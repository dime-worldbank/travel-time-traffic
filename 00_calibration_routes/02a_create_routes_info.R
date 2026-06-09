# Create Routes

set.seed(42)

# Load data --------------------------------------------------------------------
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds")) %>%
  st_as_sf()

grid_sf <- readRDS(file.path(data_dir, "Google Traffic 2026", "Grid", "grid_param.Rds"))
grid_sf <- grid_sf %>% st_union() %>% st_as_sf()

osm_sf <- osm_sf %>% st_intersection(grid_sf)

osm_sf <- osm_sf %>%
  dplyr::mutate(length_m = geometry %>% st_length() %>% as.numeric())

osm_sf <- osm_sf %>%
  group_by(name, fclass) %>%
  dplyr::mutate(length_m = sum(length_m)) %>%
  ungroup() %>%
  dplyr::filter(length_m >= 500)

road_df <- osm_sf %>%
  st_drop_geometry() %>%
  distinct(name, fclass, length_m)

# Extract traffic --------------------------------------------------------------
osm_sf <- osm_sf %>% st_buffer_chunks(dist = 10, chunk_size = 100)

tiff_vec <- file.path(traffic_gg_raw_dir) %>%
  list.files(pattern = "*.tiff") 

time_df <- tibble(
  tiff = tiff_vec,
  timestamp = str_extract(tiff, "\\d+"),
  datetime_utc = as.POSIXct(as.numeric(timestamp),
                            origin = "1970-01-01",
                            tz = "UTC")
) %>%
  dplyr::mutate(datetime_eat = datetime_utc %>% with_tz("Africa/Nairobi"))

time_df <- time_df %>%
  dplyr::filter(datetime_eat >= ymd("2023-06-04", tz = "Africa/Nairobi"),
                datetime_eat < ymd("2023-06-08", tz = "Africa/Nairobi"))

time_df <- time_df[1:5,]

for(i in 1:nrow(time_df)){
  
  time_df_i <- time_df[i,]
  
  OUT_DIR <- file.path(data_dir, "Travel Time Routes 2026",
                       "All Roads Traffic",
                       paste0("routes_traffic_", time_df_i$timestamp, ".Rds"))
  
  if(!file.exists(OUT_DIR)){
    
    r <- raster(file.path(traffic_gg_raw_dir, time_df_i$tiff))
    
    raster_extract_df <- exact_extract(
      r,
      osm_sf,
      function(values, coverage_fraction) {
        tibble(
          v_mean = mean(values, na.rm = T),
          n_0 = sum(values == 0, na.rm = TRUE),
          n_1 = sum(values == 1, na.rm = TRUE),
          n_2 = sum(values == 2, na.rm = TRUE),
          n_3 = sum(values == 3, na.rm = TRUE),
          n_4 = sum(values == 4, na.rm = TRUE)
        )
      }
    )
    
    osm_df <- osm_sf %>% st_drop_geometry()
    
    osm_df <- bind_cols(osm_df, raster_extract_df)
    osm_df$datetime_eat <- time_df_i$datetime_eat
    
    saveRDS(osm_df, OUT_DIR)
    
  }
}

file.path(data_dir, "Travel Time Routes 2026",
          "All Roads Traffic") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  length()

df <- file.path(data_dir, "Travel Time Routes 2026",
                "All Roads Traffic") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  map_df(readRDS)

df$v_mean[is.na(df$v_mean)] <- 0

sum_df <- df %>%
  dplyr::mutate(hour = datetime_eat %>% hour()) %>%
  dplyr::filter(hour >= 6) %>%
  group_by(name, fclass, length_m) %>%
  dplyr::summarise(v_mean = mean(v_mean),
                   n_0 = mean(n_0),
                   n_1 = mean(n_1),
                   n_2 = mean(n_2),
                   n_3 = mean(n_3),
                   n_4 = mean(n_4)) %>%
  ungroup()

sum_df <- sum_df %>%
  dplyr::filter(n_3 >= 0.5)

sum_df$random_num <- runif(nrow(sum_df))
sum_df$toward_cbd <- sample(x = c(T, F), size = nrow(sum_df), replace = T)

sum_df <- sum_df %>%
  arrange(random_num)

# Export -----------------------------------------------------------------------
saveRDS(sum_df,
        file.path(data_dir, "Travel Time Routes 2026",
                  "roads_traffic_agg.Rds"))
