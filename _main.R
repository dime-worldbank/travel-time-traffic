# Travel Time and Traffic: Main Script

# Filepaths --------------------------------------------------------------------
#### Root
if(Sys.info()[["user"]] == "robmarty"){
  db_dir <- "~/Dropbox/World Bank/IEs/Travel Time and Traffic Analysis"
  git_dir <- "~/Documents/Github/travel-time-traffic"
}

#### Paths from root
data_dir         <- file.path(db_dir, "Data")
traffic_dir      <- file.path(data_dir, "Traffic")
tt_dir           <- file.path(data_dir, "Travel Time") 
traffic_tt_dir   <- file.path(data_dir, "Traffic-Travel Time-Merged")
gadm_dir         <- file.path(data_dir, "GADM")
city_traffic_dir <- file.path(data_dir, "City-Level Traffic")

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

# Functions --------------------------------------------------------------------
extract_gt_to_poly <- function(r, locations_sf){
  
  count_0 <- function(x){
    sum(x == 0, na.rm = T)
  }
  
  count_1 <- function(x){
    sum(x == 1, na.rm = T)
  }
  
  count_2 <- function(x){
    sum(x==2, na.rm = T)
  }
  
  count_3 <- function(x){
    sum(x==3, na.rm = T)
  }
  
  count_4 <- function(x){
    sum(x==4, na.rm = T)
  }
  
  count_5 <- function(x){
    sum(x==5, na.rm = T)
  }
  
  ## Weird issue of counting "1s" -- numbers are way too large
  r[] <- r[] + 1
  
  locations_sf$count_0 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_1, ## count_1 because add 1 (so raw is 0)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$count_1 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_2, ## count_2 because add 1 (so raw is 1)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$count_2 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_3, ## count_3 because add 1 (so raw is 2)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$count_3 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_4, ## count_4 because add 1 (so raw is 3)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$count_4 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_5, ## count_5 because add 1 (so raw is 4)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$geometry <- NULL
  return(locations_sf)
}


calc_traffic_length <- function(polygon_sf, traffic_sf, add_by_class = F){
  
  length_all_df <- map_df(1:nrow(polygon_sf), function(i){
    
    road_i <- polygon_sf[i,]
    
    mp_sf_i_roadi <- st_intersection(road_i, traffic_sf)
    
    mp_sf_i_roadi$length_m <- mp_sf_i_roadi %>% st_length() %>% as.numeric()
    
    length_low      <- mp_sf_i_roadi$length_m[mp_sf_i_roadi$congestion %in% "low"] %>% sum()
    length_moderate <- mp_sf_i_roadi$length_m[mp_sf_i_roadi$congestion %in% "moderate"] %>% sum()
    length_heavy    <- mp_sf_i_roadi$length_m[mp_sf_i_roadi$congestion %in% "heavy"] %>% sum()
    length_severe   <- mp_sf_i_roadi$length_m[mp_sf_i_roadi$congestion %in% "severe"] %>% sum()
    
    road_i_df <- road_i
    road_i_df$geometry <- NULL 
    
    length_df <- data.frame(
      length_low = length_low,
      length_moderate = length_moderate,
      length_heavy = length_heavy,
      length_severe = length_severe
    )
    
    if(add_by_class){
      
      mp_sf_i_roadi_df <- mp_sf_i_roadi
      mp_sf_i_roadi_df$geometry <- NULL
      
      length_class_df <- mp_sf_i_roadi_wide_df <- mp_sf_i_roadi_df %>% 
        group_by(congestion, class) %>%
        dplyr::summarise(length_m = sum(length_m)) %>%
        ungroup() %>%
        pivot_wider(values_from = length_m,
                    names_from = c(congestion, class))
      
      length_df <- bind_cols(length_df, length_class_df)
      
    }
    
    length_df <- bind_cols(length_df, road_i_df)
    
    return(length_df)
  })
  
  return(length_all_df)
  
}

# Code -------------------------------------------------------------------------


