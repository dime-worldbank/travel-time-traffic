# Clean Data

# Typical Routes ===============================================================
#### Load data
mb_tl_df <- file.path(data_dir, "extracted-data", "mapbox_typical_route_10m", "mapbox_traffic_levels") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS) %>%
  
  # No variation in data across hours after here; some issue with API, so remove
  dplyr::filter(datetime < ymd_hms("2024-01-12 00:00:00", tz = "Africa/Nairobi"))

# Determines issue with data
# mb_tl_df %>%
#   dplyr::mutate(date = datetime %>% date(),
#                 hour = datetime %>% hour()) %>%
#   group_by(date, hour) %>%
#   dplyr::summarise(length_mb_low = mean(length_mb_low)) %>%
#   ungroup() %>%
#   
#   group_by(date) %>%
#   dplyr::summarise(value_sd = sd(length_mb_low)) %>%
#   ungroup()

mb_tt_df <- readRDS(file.path(data_dir, "Travel Time", "mapbox_tt_data.Rds"))

#### Merge
mb_df <- full_join(mb_tl_df, mb_tt_df, by = c("segment_id", "datetime"))

#### Cleanup
mb_df <- mb_df %>%
  dplyr::mutate(length_traffic_total = length_mb_low + length_mb_moderate + length_mb_heavy + length_mb_severe) %>%
  group_by(segment_id) %>%
  dplyr::mutate(length_traffic_total_max = max(length_traffic_total, na.rm = T),
                distance_m_mode = Mode(distance_m)) %>%
  ungroup() %>%
  
  dplyr::mutate(tl_prop_1 = length_mb_low/length_traffic_total_max,
                tl_prop_2 = length_mb_moderate/length_traffic_total_max,
                tl_prop_3 = length_mb_heavy/length_traffic_total_max,
                tl_prop_4 = length_mb_severe/length_traffic_total_max,
                
                tl_prop_1_unadj = length_mb_low/length_traffic_total,
                tl_prop_2_unadj = length_mb_moderate/length_traffic_total,
                tl_prop_3_unadj = length_mb_heavy/length_traffic_total,
                tl_prop_4_unadj = length_mb_severe/length_traffic_total) %>%
  
  dplyr::mutate(tl_and_tt_data = !is.na(tl_prop_2) & !is.na(duration_s), 
                route_change_length = (segment_id %in% c(18, 25)),
                modal_route = distance_m_mode == distance_m) %>%
  dplyr::select(-uid) %>%
  dplyr::rename(uid = segment_id)

#### Export
saveRDS(mb_df, file.path(analysis_data_dir, "mapbox_routes.Rds"))

