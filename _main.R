# Travel Time and Traffic: Main Script

POLYGONS_ALL <- c("google_typical_route_10m", 
                  "mapbox_typical_route_10m",
                  #"gadm1",
                  #"gadm2",
                  #"gadm3",
                  #"osm_10m",
                  "estates",
                  "twitter_crashes_50m",
                  "twitter_crashes_100m",
                  "ntsa_crashes_50m",
                  "ntsa_crashes_100m")

POLYGONS_ALL <- "google_typical_route_10m"
POLYGONS_ALL <- "osm_10m"

POLYGONS_ALL <- "osm_10m"
POLYGONS_ALL <- "estates"
POLYGONS_ALL <- "gadm1"
POLYGONS_ALL <- "h3_iso_routes"
POLYGONS_ALL <- "twitter_crashes_50m"
POLYGONS_ALL <- "h3_iso_routes"
POLYGONS_ALL <- "osm_10m"

DELETE_OUTPUT <- F
RUN_CODE      <- F

# Filepaths --------------------------------------------------------------------
#### Root
if(Sys.info()[["user"]] == "rmarty"){
  db_dir <- "~/Dropbox/World Bank/IEs/Travel Time and Traffic Analysis"
  git_dir <- "~/Documents/Github/travel-time-traffic"
  sm_db_dir <- "~/Dropbox/World Bank/IEs/CrashMap-Nairobi"
  figures_dir <- "~/Dropbox/Apps/Overleaf/Travel Time and Traffic/figures"
  tables_dir <- "~/Dropbox/Apps/Overleaf/Travel Time and Traffic/tables"
}

if(Sys.info()[["user"]] == "robmarty"){
  db_dir <- "~/Dropbox/World Bank/IEs/Travel Time and Traffic Analysis"
  git_dir <- "~/Documents/Github/travel-time-traffic"
  sm_db_dir <- "~/Dropbox/World Bank/IEs/CrashMap-Nairobi"
  figures_dir <- "~/Dropbox/Apps/Overleaf/Travel Time and Traffic/figures"
  tables_dir <- "~/Dropbox/Apps/Overleaf/Travel Time and Traffic/tables"
}

#### Paths from root
data_dir         <- file.path(db_dir, "Data")
traffic_dir      <- file.path(data_dir, "Traffic")
tt_dir           <- file.path(data_dir, "Travel Time") 
traffic_tt_dir   <- file.path(data_dir, "Traffic-Travel Time-Merged")
gadm_dir         <- file.path(data_dir, "GADM")
city_traffic_dir <- file.path(data_dir, "City-Level Traffic")
analysis_data_dir <- file.path(data_dir, "analysis-data")
extracted_data_dir <- file.path(data_dir, "extracted-data")
nbo_exp_dir        <- file.path(data_dir, "Nairobi Expressway")

traffic_gg_raw_dir <- file.path(sm_db_dir, "Data", "Google Mapbox Traffic", "FinalData", "google_individual_rasters")
traffic_mb_raw_dir <- file.path(sm_db_dir, "Data", "Google Mapbox Traffic", "FinalData", "mapbox_daily_data")
waze_bq_dir <- file.path(sm_db_dir, "Data", "Waze - BigQuery", "FinalData")
tomtom_raw_dir <- file.path(sm_db_dir, "Data", "TomTom", "RawData")

git_clean_dir <- file.path(git_dir, "01_clean_data")
git_analysis_dir <- file.path(git_dir, "02_analysis")

# Remove tables/figures --------------------------------------------------------
if(DELETE_OUTPUT){
  out_files <- c(list.files(tables_dir, full.names = T, pattern = ".tex"),
                 list.files(figures_dir, full.names = T, pattern = ".png"))
  
  for(file_i in out_files) file.remove(file_i)
}


# API Keys ---------------------------------------------------------------------
if(Sys.info()[["user"]] == "rmarty"){
  api_keys_df <- read.csv(file.path("~/Dropbox", "World Bank", "Webscraping", "Files for Server", "api_keys.csv"),
                          stringsAsFactors = F)
}

# Packages ---------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(aws.s3)
library(ggrepel)
library(lubridate)
library(stringr)
library(sf)
library(haven)
library(tidyr)
library(labelled)
library(readr)
library(foreach)
library(googletraffic)
library(doParallel)
library(raster)
library(leaflet)
library(exactextractr)
library(ggplot2)
library(purrr)
library(geodata)
library(lubridate)
library(ggmap)
library(ggpubr)
library(jsonlite)
library(data.table)
library(fixest)
library(janitor)
library(osmdata)
library(DescTools)
library(ggspatial)
library(ggpmisc)
library(fixest)
library(modelsummary)
library(lubridate)
library(tidyterra)
library(pbmcapply)
library(mapboxapi)
library(forcats)
library(osrm)
library(h3jsr)
library(lwgeom)
library(tidyterra)
library(dplyr)
library(fixest)
library(purrr)
library(broom)

source("https://raw.githubusercontent.com/ramarty/fast-functions/refs/heads/master/R/functions_in_chunks.R")


# #devtools::install_github("MBalthasar/S5Processor")
# library(S5Processor)
# library(ncdf4)
# library(ggplot2)
# library(dismo)
# library(maptools)
# library(raster)
# library(geosphere)
# library(rgdal)
# library(rgeos)
# library(sp)

source(file.path(git_dir, "functions", "functions.R"))

modelsummary_tab <- function(...,
                             output = NULL){
  latex_output <- capture.output(modelsummary(..., output = "latex"))
  
  # Remove the "\begin{table}" and "\end{table}" lines
  latex_output <- latex_output[-c(1, length(latex_output))]
  latex_output <- latex_output[-1]
  
  # Print the customized LaTeX output without table environment
  sink(output)
  cat(latex_output, sep = "\n")
  sink()
}

log_margin <- function(y, x){
  
  y_min_non_zero <- min(y[y > 0], na.rm = T)
  
  y <- y / y_min_non_zero
  
  case_when(y > 0 ~ log(y),
            y == 0 ~ -x)
  
}

Mode <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mk_traffic_indicators <- function(data_df, 
                                  beta,
                                  # speed_trunk = 52.9,
                                  # speed_primary = 42.2,
                                  # speed_secondary = 34.7,
                                  # speed_tertiary = 27.7,
                                  # speed_unclassified = 26.7,
                                  # speed_residential = 21.6
                                  speed_trunk_fast = 85,
                                  speed_trunk = 55,
                                  speed_primary = 50,
                                  speed_secondary = 35,
                                  speed_tertiary = 30,
                                  speed_unclassified = 25,
                                  speed_residential = 20){
  
  data_df <- data_df %>%
    dplyr::mutate(speed_avg = 
                    speed_trunk_fast * prop_trunk_fast + 
                    speed_trunk * prop_trunk + 
                    speed_primary * prop_primary + 
                    speed_secondary * prop_secondary + 
                    speed_tertiary * prop_tertiary + 
                    speed_unclassified * prop_unclassified +
                    speed_residential * prop_residential) %>%
    dplyr::mutate(
      # Linear predictor: log(delay per km)
      # Each traffic-level coefficient is now itself a function of speed_avg,
      # combining the main effect and the speed interaction term
      CI = (beta["tl_prop_2"] + beta["tl_prop_2_speed"] * speed_avg) * tl_prop_2 +
        (beta["tl_prop_3"] + beta["tl_prop_3_speed"] * speed_avg) * tl_prop_3 +
        (beta["tl_prop_4"] + beta["tl_prop_4_speed"] * speed_avg) * tl_prop_4,
      
      # Delay factor relative to free-flow
      delay_factor = exp(CI),
      
      # Speed as a fraction of free-flow speed
      speed_multiplier = exp(-CI)
    )
}

# Code -------------------------------------------------------------------------
if(F){
  
  # Clean data -----------------------------------------------------------------
  source(file.path(git_clean_dir, "01_move_data_for_nairobi.R"))
  source(file.path(git_clean_dir, "01_prep_city_files.R"))
  
  source(file.path(git_clean_dir, "02_append_travel_time_google.R"))
  source(file.path(git_clean_dir, "02_append_travel_time_mapbox.R"))
  
  #source(file.path(git_clean_dir, "03_make_typical_routes.R"))
  
  source(file.path(git_clean_dir, "04_extract_points_to_routes.R"))
  source(file.path(git_clean_dir, "04_extract_google_traffic.R"))
  #source(file.path(git_clean_dir, "04_extract_mapbox_traffic.R"))
  #source(file.path(git_clean_dir, "04_extract_tomtom.R"))
  #source(file.path(git_clean_dir, "04_extract_waze.R"))
  
  source(file.path(git_clean_dir, "05_clean_data.R"))
  
  # Analysis -------------------------------------------------------------------
  ### Calibration
  source(file.path(git_analysis_dir, "1_calibration_regression.R"))
  source(file.path(git_analysis_dir, "1_calibration_sensitivity_by_route.R"))
  source(file.path(git_analysis_dir, "1_calibration_sensitivity_over_time.R"))
  
  ### Summary figures
  source(file.path(git_analysis_dir, "1_map_tt_tl.R"))
  source(file.path(git_analysis_dir, "1_speed_boxplots.R"))
  source(file.path(git_analysis_dir, "1_summary_boxplots.R"))
  source(file.path(git_analysis_dir, "1_route_summary.R"))
  source(file.path(git_analysis_dir, "1_route_summary_calib.R"))
  source(file.path(git_analysis_dir, "1_mapbox_map_example.R"))
  source(file.path(git_analysis_dir, "1_deviation_example_1.R"))
  source(file.path(git_analysis_dir, "1_deviation_example_2.R"))
  source(file.path(git_analysis_dir, "1_extract_traffic_example.R"))
  source(file.path(git_analysis_dir, "1_summary_boxplots_calib_sample.R"))
  
  ### Case Studies
  source(file.path(git_analysis_dir, "2_case_study_crash.R"))
  source(file.path(git_analysis_dir, "2_case_study_crash_50m.R"))
  
  source(file.path(git_analysis_dir, "2_case_study_nbo_election.R"))
  
  source(file.path(git_analysis_dir, "2_isochrone_methods.R"))
  source(file.path(git_analysis_dir, "2_isochrone_hourly_example.R"))
  source(file.path(git_analysis_dir, "2_isochrone_hex_buildings.R"))
  
  ### Additional analysis
  source(file.path(git_analysis_dir, "2_percent_route_deviate.R"))
  source(file.path(git_analysis_dir, "2_route_distance_over_time.R"))
  
  source(file.path(git_analysis_dir, "2_descriptive_summary.R"))
  
}

