# Extract Data

# Load data --------------------------------------------------------------------
typical_routes_sf <- readRDS(file.path(data_dir, "Travel Time Routes 2026", "typical_routes.Rds"))

gt_files_df <- tibble(
  filename_gt = list.files(file.path(data_dir, "Google Traffic 2026", "RawData"), pattern = "\\.tif$")
) %>%
  mutate(
    datetime_eat = as.numeric(str_extract(filename_gt, "(?<=utc)\\d+")) %>%
      as.POSIXct(origin = "1970-01-01", tz = "Africa/Nairobi"),
    date = as.Date(datetime_eat, tz = "Africa/Nairobi"),
    hour = lubridate::hour(datetime_eat)
  ) %>%
  select(-datetime_eat)

tt_files_df <- tibble(
  filename_tt = list.files(file.path(data_dir, "Travel Time Routes 2026", "Travel Time Data"), pattern = "\\.Rds$")
) %>%
  mutate(
    datetime_eat = str_extract(filename_tt, "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}") %>%
      as.POSIXct(format = "%Y-%m-%d_%H-%M-%S", tz = "Africa/Nairobi"),
    date = as.Date(datetime_eat, tz = "Africa/Nairobi"),
    hour = lubridate::hour(datetime_eat)
  ) %>%
  select(-datetime_eat)

files_df <- gt_files_df %>%
 full_join(tt_files_df, by = c("date", "hour"))

# Extract traffic to typical route ---------------------------------------------
for(i in 1:nrow(gt_files_df)){
  
  gt_files_df_i <- gt_files_df[i,]
  
  OUT_DIR <- file.path(data_dir, "Travel Time Routes 2026", "Traffic Extracted to Typical Routes",
                       paste0("gt_", gt_files_df_i$date, "-", gt_files_df_i$hour, ".Rds"))
  
  if(!file.exists(OUT_DIR)){
    r <- raster(file.path(data_dir, "Google Traffic 2026", "RawData",
                          gt_files_df_i$filename_gt))
    
    traffic_df <- extract_gt_to_poly(r, typical_routes_sf)
    traffic_df$date <- gt_files_df_i$date
    traffic_df$hour <- gt_files_df_i$hour
    
    saveRDS(traffic_df, OUT_DIR)
  }
  
}

# Merge data -------------------------------------------------------------------
traffic_df <- file.path(data_dir, "Travel Time Routes 2026", "Traffic Extracted to Typical Routes") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  map_df(readRDS)

traveltime_df <- file.path(data_dir, "Travel Time Routes 2026", "Travel Time Data") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  map_df(readRDS) %>%
  dplyr::mutate(date = query_datetime_eat %>% date(),
                hour = query_datetime_eat %>% hour())

all_df <- inner_join(traffic_df,
                     traveltime_df,
                     by = c("name", "fclass", "uid", "date", "hour"))

# Clean data -------------------------------------------------------------------

# Export -----------------------------------------------------------------------
saveRDS(all_df, file.path(extracted_data_dir, "routes_for_calibration", "google_traffic_tt.Rds"))



