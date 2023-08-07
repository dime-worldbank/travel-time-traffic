# Travel Time and Traffic: Main Script

# Filepaths --------------------------------------------------------------------
#### Root
if(Sys.info()[["user"]] == "robmarty"){
  db_dir <- "~/Dropbox/World Bank/IEs/Travel Time and Traffic Analysis"
  git_dir <- "~/Documents/Github/travel-time-traffic"
}

#### Paths from root
data_dir    <- file.path(db_dir, "Data")
traffic_dir <- file.path(data_dir, "Traffic")
tt_dir      <- file.path(data_dir, "Travel Time")

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
library(labelled)

# Code -------------------------------------------------------------------------


