# Download Google Travel Time in Monthly Chunks

readRDS_constvar <- function(path){
  df <- readRDS(path)
  df$leg_id <- NULL
  df$departure_time <- NULL
  df$arrival_time <- NULL
  return(df)
}

# Load Data --------------------------------------------------------------------
tt_sf <- list.files(file.path(tt_dir, 
                              "google_daily_data_nairobi"),
                    full.names = T,
                    pattern = "*.Rds") %>%
  lapply(readRDS_constvar) %>%
  do.call(what = "rbind")

tt_od_df <- read_csv(file.path(tt_dir, "nairobi_tt_od.csv"))
tt_od_df <- tt_od_df %>%
  dplyr::select(segment_id, road) %>%
  dplyr::rename(road_name = road) %>%
  dplyr::mutate(road_name = road_name %>% tools::toTitleCase())

# Prep Variables ---------------------------------------------------------------
## Cleanup
tt_sf <- tt_sf %>%
  mutate(time = time %>% as.character %>% ymd_hms(tz = "UTC") %>% with_tz(tzone = "Africa/Nairobi"),
         speed_kmh = (distance_m/1000) / (duration_s/60/60),
         speed_in_traffic_kmh = (distance_m/1000) / (duration_in_traffic_s/60/60),
         uid = 1:n()) %>%
  dplyr::select(-c(alternative_id)) %>%
  dplyr::rename(segment_id = locations_segment_id)

## Merge in road name
tt_sf <- tt_sf %>%
  left_join(tt_od_df, by = "segment_id")

# Export -----------------------------------------------------------------------

#### Full Data
saveRDS(tt_sf, file.path(tt_dir, "google_tt.Rds"))

#### Just Routes
# tt_noroute <- tt_sf
# tt_noroute$geometry <- NULL
# 
# tt_routeonly <- tt_sf %>%
#   dplyr::select(uid, geometry)
# 
# tt_noroute <- tt_noroute %>% 
#   dplyr::distinct(origin, destination, .keep_all = T) %>%
#   left_join(tt_routeonly, by = "uid") %>%
#   st_sf() %>%
#   dplyr::select(summary, distance_m, origin, destination, locations_segment_id,
#                 uid, geometry)
# 
# saveRDS(tt_noroute, file.path(tt_dir,
#                               "google_route_geoms.Rds"))

#### Just Data (Drop geometry)
tt_df <- tt_sf
tt_df$geometry <- NULL

# var_label(tt_df$summary) <- "Road Name"
# var_label(tt_df$distance_m) <- "Distance (Meters)"
# var_label(tt_df$distance_text) <- "Distance (Text)"
# var_label(tt_df$duration_s) <- "Typical Duration traffic (Seconds)"
# var_label(tt_df$duration_text) <- "Typical Duration traffic (Text)"
# var_label(tt_df$duration_in_traffic_s) <- "Duration in traffic (Seconds)"
# var_label(tt_df$duration_in_traffic_text) <- "Duration in traffic (Text)"
# var_label(tt_df$time) <- "Date Time"
# var_label(tt_df$origin) <- "Origin Coordinates"
# var_label(tt_df$destination) <- "Dest Coordinates"
# var_label(tt_df$locations_segment_id) <- "Segment ID"
# var_label(tt_df$road_name) <- "Main Road of Route"
# 
# var_label(tt_df$speed_kmh) <- "Average Speed (Km/hr)"
# var_label(tt_df$speed_in_traffic_kmh) <- "Average Speed in Traffic (Km/hr)"

saveRDS(tt_df, file.path(tt_dir,
                         "google_tt_data.Rds"))

