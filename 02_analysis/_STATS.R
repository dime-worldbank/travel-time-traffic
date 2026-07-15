
###
route26_df <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))
route26_df$date %>% summary()

###
routecalib_df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))
routecalib_df %>%
  distinct(date, hour) %>%
  group_by(hour) %>%
  dplyr::summarise(n_dates = n()) %>%
  ungroup()

routecalib_df %>%
  dplyr::filter(hour == 4) %>%
  distinct(date)

routecalib_df %>%
  dplyr::filter(hour == 20) %>%
  distinct(date)

###
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
osm_sf$name %>% str_subset("haile")
osm_sf %>%
  dplyr::filter(name %in% c("eastern bypass",
                            "haile selassie avenue",
                            "jogoo road",
                            "kenyatta avenue",
                            "langata road", 
                            "mombasa road",
                            "ngong road",
                            "outer ring road",
                            "southern bypass",
                            "thika road",
                            "uhuru highway",
                            "waiyaki way")) %>%
  st_drop_geometry() %>%
  distinct(name, fclass)



