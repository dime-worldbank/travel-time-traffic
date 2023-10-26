# Travel Time and Traffic: Main Script

# Filepaths --------------------------------------------------------------------
#### Root
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

traffic_gg_raw_dir <- file.path(sm_db_dir, "Data", "Google Mapbox Traffic", "FinalData", "google_individual_rasters")
traffic_mb_raw_dir <- file.path(sm_db_dir, "Data", "Google Mapbox Traffic", "FinalData", "mapbox_daily_data")

git_clean_dir <- file.path(git_dir, "01_clean_data")

# API Keys ---------------------------------------------------------------------
if(Sys.info()[["user"]] == "robmarty"){
  api_keys_df <- read.csv(file.path("~/Dropbox", "World Bank", "Webscraping", "Files for Server", "api_keys.csv"),
                          stringsAsFactors = F)
}

# Packages ---------------------------------------------------------------------
library(dplyr)
library(aws.s3)
library(lubridate)
library(stringr)
library(sf)
library(haven)
library(tidyr)
library(labelled)
library(readr)
library(raster)
library(leaflet)
library(exactextractr)
library(ggplot2)
library(purrr)
library(geodata)
library(lubridate)
library(ggmap)
library(ggpubr)

source(file.path(git_dir, "functions", "functions.R"))

# Code -------------------------------------------------------------------------
if(F){
  
  # Clean data -----------------------------------------------------------------
  source(file.path(git_clean_dir, "01_move_data_for_nairobi.R"))
  source(file.path(git_clean_dir, "01_prep_city_files.R"))
  
  source(file.path(git_clean_dir, "02_append_travel_time_google.R"))
  source(file.path(git_clean_dir, "02_append_travel_time_mapbox.R"))
  
  #source(file.path(git_clean_dir, "03_make_typical_routes.R"))
  
  source(file.path(git_clean_dir, "04_extract_google_traffic_city.R"))
  source(file.path(git_clean_dir, "04_extract_google_traffic_routes.R"))
  source(file.path(git_clean_dir, "04_extract_mapbox_traffic_city.R"))
  source(file.path(git_clean_dir, "04_extract_mapbox_traffic_routes.R"))
  
  source(file.path(git_clean_dir, "05_clean_route_data.R"))
  source(file.path(git_clean_dir, "05_clean_city_data.R"))
  
}

