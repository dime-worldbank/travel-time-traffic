

# Crashes ======================================================================
unit = "50m"
for(unit in c("50m", "100m")){
  message(unit)
  
  #### Load data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ### Crashes
  twitter_df <- readRDS(file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds")) %>%
    st_drop_geometry()
  
  ### Traffic Levels
  mb_tl_df <- file.path(data_dir, "extracted-data", paste0("twitter_crashes_", unit), "mapbox_traffic_levels") %>%
    list.files(pattern = "*.Rds",
               full.names = T) %>%
    map_df(readRDS) %>%
    
    # No variation in data across hours after here; some issue with API, so remove
    dplyr::filter(datetime < ymd_hms("2024-01-12 00:00:00", tz = "Africa/Nairobi")) %>%
    
    # Fix zero issues
    dplyr::mutate(length_mb_severe = replace_na(length_mb_severe, 0))
  
  ### Travel Time
  mb_route_crash_df <- readRDS(file.path(data_dir, "points-intersect-routes", paste0("twitter_crashes_",unit,"_mapbox_route.Rds"))) %>%
    dplyr::rename(road_length_near_crash_m = road_length_m)
  
  mb_tt_df <- readRDS(file.path(data_dir, "Travel Time", "mapbox_tt_data.Rds")) %>%
    dplyr::select(segment_id, duration_s, distance_m, speed_kmh, datetime) %>%
    dplyr::mutate(minute = datetime %>% minute()) %>%
    dplyr::filter(minute == 0) %>%
    dplyr::select(-minute)
  
  mb_tt_crash_df <- mb_route_crash_df %>%
    left_join(mb_tt_df, by = c("segment_id"))
  
  # Each crash can have multiple rotues running through it. Aggregate to 
  # crash-time level, weighting average by length of road that is near crash
  mb_tt_crash_agg_df <- mb_tt_crash_df %>%
    group_by(crash_id, datetime) %>%
    dplyr::summarise(speed_kmh = weighted.mean(speed_kmh, road_length_near_crash_m),
                     duration_s = weighted.mean(duration_s, road_length_near_crash_m),
                     distance_m_weighted = weighted.mean(distance_m, road_length_near_crash_m),
                     distance_m = mean(distance_m)) %>%
    ungroup()
  
  #### Merge + Create variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  twitter_data_df <- mb_tl_df %>%
    left_join(mb_tt_crash_agg_df, by = c("crash_id", "datetime")) %>%
    left_join(twitter_df, by = "crash_id")

  twitter_data_df <- twitter_data_df %>%
    dplyr::filter(!is.na(datetime)) %>%
    dplyr::mutate(crash_datetime = floor_date(crash_datetime, unit = "hour")) %>%
    dplyr::mutate(hours_since_crash = difftime(datetime, crash_datetime, units = "hours") %>% as.numeric())
  
  twitter_data_df <- twitter_data_df %>%
    dplyr::mutate(length_traffic_total = length_mb_low + length_mb_moderate + length_mb_heavy + length_mb_severe) %>%
    group_by(crash_id) %>%
    dplyr::mutate(length_traffic_total_max = max(length_traffic_total, na.rm = T)) %>%
    ungroup() %>%
    
    dplyr::mutate(tl_prop_2 = length_mb_moderate/length_traffic_total_max,
                  tl_prop_3 = length_mb_heavy/length_traffic_total_max,
                  tl_prop_4 = length_mb_severe/length_traffic_total_max)
  
  #### Export - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  saveRDS(twitter_data_df, file.path(analysis_data_dir, paste0("mapbox_twitter_",unit,".Rds")))
}


