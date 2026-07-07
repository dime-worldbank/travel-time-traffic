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

# Buffer typical routes --------------------------------------------------------
typical_routes_sf <- typical_routes_sf %>% st_buffer(dist = 10)

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
#### Create variables
all_df <- all_df %>%
  dplyr::mutate(count_all = count_1 + count_2 + count_3 + count_4) %>%
  
  group_by(uid) %>%
  dplyr::mutate(count_all_max = max(count_all, na.rm = T)) %>%
  ungroup() %>%
  
  dplyr::mutate(tl_prop_2   = count_2 / count_all_max,
                tl_prop_3   = count_3 / count_all_max,
                tl_prop_234 = (count_2 + count_3 + count_4) / count_all_max,
                tl_prop_34  = (          count_3 + count_4) / count_all_max,
                tl_prop_4   = (                    count_4) / count_all_max) %>%
  
  dplyr::rename(gg_tl_count_all_max = count_all_max) %>%
  dplyr::select(-c(count_0, count_1, count_2, count_3, count_4, count_all))

all_df <- all_df %>%
  dplyr::mutate(datetime = query_datetime_eat)

#### Filter variables
## If traffic levels are NA
all_df <- all_df %>%
  dplyr::filter(!is.na(tl_prop_4))

## If no variation in speed
all_df <- all_df %>%
  group_by(uid) %>%
  dplyr::mutate(speed_in_traffic_kmh_uid_mean = mean(speed_in_traffic_kmh),
                speed_in_traffic_kmh_uid_sd = sd(speed_in_traffic_kmh)) %>%
  ungroup()

all_df <- all_df %>%
  dplyr::filter(speed_in_traffic_kmh_uid_sd > 0)

# Filter -----------------------------------------------------------------------
all_df <- all_df %>%
  dplyr::filter(date >= ymd("2026-06-11"),
                date <= ymd("2026-06-18"))

# Export -----------------------------------------------------------------------
saveRDS(all_df, file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))

