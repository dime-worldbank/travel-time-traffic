# Clean Data: Crashes

unit <- "100m"

# Load crashes -----------------------------------------------------------------
twitter_df <- readRDS(file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds")) %>%
  st_drop_geometry()

# Traffic levels ---------------------------------------------------------------
tl_df <- file.path(data_dir, "extracted-data", 
                   paste0("twitter_crashes_", unit), 
                   "google_traffic_levels") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS) %>%
  dplyr::rename(crash_id = uid)

# Travel time -------------------------------------------------------------------
route_crash_df <- readRDS(file.path(data_dir, "points-intersect-routes", 
                                    paste0("twitter_crashes_",unit,"_google_route.Rds"))) %>%
  dplyr::rename(road_length_near_crash_m = road_length_m)

#tt_df <- readRDS(file.path(data_dir, "Travel Time", "google_tt_data.Rds")) %>%
tt_df <- readRDS(file.path(analysis_data_dir, "google_routes.Rds")) %>%
  dplyr::select(uid, datetime,
                duration_s, distance_m, 
                duration_in_traffic_s, 
                speed_kmh, speed_in_traffic_kmh,
                delay_factor_od) %>%
  dplyr::mutate(minute = datetime %>% minute()) %>%
  dplyr::filter(minute == 0) %>%
  dplyr::select(-minute) %>%
  dplyr::rename(segment_id = uid)

tt_crash_df <- route_crash_df %>%
  left_join(tt_df, by = c("segment_id"))

# Each crash can have multiple rotues running through it. Aggregate to 
# crash-time level, weighting average by length of road that is near crash
tt_crash_agg_df <- tt_crash_df %>%
  group_by(crash_id, datetime) %>%
  dplyr::summarise(speed_in_traffic_kmh = weighted.mean(speed_in_traffic_kmh, road_length_near_crash_m, na.rm = T),
                   duration_in_traffic_s = weighted.mean(duration_in_traffic_s, road_length_near_crash_m, na.rm = T),
                   delay_factor_od = weighted.mean(delay_factor_od, road_length_near_crash_m, na.rm = T),
                   distance_m_weighted = weighted.mean(distance_m, road_length_near_crash_m, na.rm = T),
                   distance_m = mean(distance_m)) %>%
  ungroup()

#### Merge + Create variables --------------------------------------------------
twitter_data_df <- tl_df %>%
  left_join(tt_crash_agg_df, by = c("crash_id", "datetime")) %>%
  left_join(twitter_df, by = "crash_id")

twitter_data_df <- twitter_data_df %>%
  dplyr::mutate(date = datetime %>% date()) %>%
  dplyr::filter(!is.na(datetime)) %>%
  dplyr::filter(date < ymd("2023-08-17")) %>%
  dplyr::filter( ! ((date >= ymd("2023-02-23")) & (date <= ymd("2023-03-16"))) )

twitter_data_df <- twitter_data_df %>%
  dplyr::mutate(crash_datetime = floor_date(crash_datetime, unit = "hour")) %>%
  dplyr::mutate(hours_since_crash = difftime(datetime, crash_datetime, units = "hours") %>% as.numeric())

twitter_data_df <- twitter_data_df %>%
  dplyr::mutate(count_all = count_1 + count_2 + count_3 + count_4) %>%
  group_by(crash_id) %>%
  dplyr::mutate(count_all_max = max(count_all, na.rm = T)) %>%
  ungroup() %>%
  
  dplyr::mutate(tl_prop_2 = count_2/count_all_max,
                tl_prop_3 = count_3/count_all_max,
                tl_prop_4 = count_4/count_all_max)

#### Export --------------------------------------------------------------------
saveRDS(twitter_data_df, file.path(analysis_data_dir, paste0("google_twitter_",unit,".Rds")))


