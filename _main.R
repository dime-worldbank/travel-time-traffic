# Travel Time and Traffic: Main Script

POLYGONS_ALL <- c("google_typical_route_10m", 
                  #"mapbox_typical_route_10m",
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
POLYGONS_ALL <- "estates"

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
library(lubridate)
library(stringr)
library(sf)
library(haven)
library(tidyr)
library(labelled)
library(readr)
library(foreach)
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
library(fixest)
library(modelsummary)
library(lubridate)
library(tidyterra)
library(mapboxapi)
library(forcats)

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
  source(file.path(git_analysis_dir, "amount_route_deviates_overall.R"))
  source(file.path(git_analysis_dir, "amount_route_deviates_veh_level.R"))
  source(file.path(git_analysis_dir, "case_study_crash_analysis.R"))
  source(file.path(git_analysis_dir, "case_study_nbo_election.R"))
  source(file.path(git_analysis_dir, "case_study_congestion_analysis.R"))
  source(file.path(git_analysis_dir, "correlation_between_vars.R"))
  source(file.path(git_analysis_dir, "deviation_example_1.R"))
  source(file.path(git_analysis_dir, "deviation_example_2.R"))
  source(file.path(git_analysis_dir, "extract_traffic_example.R"))
  source(file.path(git_analysis_dir, "map_levels_and_speed.R"))
  source(file.path(git_analysis_dir, "percent_time_route_deviates.R"))
  source(file.path(git_analysis_dir, "reg_explain_diff_route.R"))
  source(file.path(git_analysis_dir, "reg_levels_explain_speed.R"))
  source(file.path(git_analysis_dir, "route_summary.R"))
  source(file.path(git_analysis_dir, "scatterplots_od_levels.R"))
  
  source(file.path(git_analysis_dir, "summary_boxplots.R"))
  
  
  
}

