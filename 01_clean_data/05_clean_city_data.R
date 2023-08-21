# Clean City Data

unit <- "gadm2"

# Load data --------------------------------------------------------------------
gg_df <- file.path(city_traffic_dir, paste0(unit, "_individual_files_google")) %>%
  list.files(pattern = ".Rds",
             full.names = T) %>%
  map_df(readRDS)

mb_df <- file.path(city_traffic_dir, paste0(unit, "_individual_files_mapbox")) %>%
  list.files(pattern = ".Rds",
             full.names = T) %>%
  map_df(readRDS)