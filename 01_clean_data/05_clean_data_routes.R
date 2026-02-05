# Clean Data Routes

# Travel time
tt_df <- readRDS(file.path(tt_dir, "google_tt_data.Rds")) 

tt_df <- tt_df %>%
  dplyr::select(segment_id, datetime, 
                speed_kmh, speed_in_traffic_kmh,
                duration_s, duration_in_traffic_s,
                distance_m) %>%
  dplyr::rename(speed_kmh = speed_kmh,
                speed_in_traffic_kmh = speed_in_traffic_kmh,
                duration_s = duration_s,
                duration_in_traffic_s = duration_in_traffic_s,
                distance_m = distance_m) %>%
  dplyr::rename(uid = segment_id)

tt_df <- tt_df %>%
  group_by(uid) %>%
  dplyr::mutate(distance_m_mode = Mode(distance_m)) %>%
  ungroup() %>%
  dplyr::mutate(modal_route = distance_m_mode == distance_m)

# Append traffic data
tl_df <- file.path(extracted_data_dir, polygon_i, "google_traffic_levels") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  map_df(readRDS) %>%
  dplyr::mutate(count_all = count_1 + count_2 + count_3 + count_4) %>%
  
  group_by(uid) %>%
  dplyr::mutate(count_all_max = max(count_all, na.rm = T)) %>%
  ungroup() %>%
  
  dplyr::mutate(tl_prop_234 = (count_2 + count_3 + count_4) / count_all_max,
                tl_prop_34  = (          count_3 + count_4) / count_all_max,
                tl_prop_4   = (                    count_4) / count_all_max,
                tl_prop_3   = (                    count_3) / count_all_max,
                tl_prop_2   = (                    count_2) / count_all_max) %>%
  
  dplyr::rename(tl_count_all_max = count_all_max) %>%
  dplyr::select(-c(count_0, count_1, count_2, count_3, count_4, count_all)) 

# Merge
route_df <- inner_join(tl_df, tt_df, by = c("uid", "datetime"))

# Cleanup
route_df <- route_df %>%
  dplyr::mutate(date = datetime %>% date(),
                hour = datetime %>% hour()) %>%
  dplyr::filter(!is.na(datetime)) %>%
  
  # Remove dates with missing data
  dplyr::filter(date < ymd("2023-08-17")) %>%
  dplyr::filter( ! ((date >= ymd("2023-02-23")) & (date <= ymd("2023-03-16"))) )

# Export
saveRDS(route_df, file.path(analysis_data_dir, "google_routes.Rds"))






