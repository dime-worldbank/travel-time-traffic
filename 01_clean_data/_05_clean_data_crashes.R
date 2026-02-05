

# Crashes ======================================================================
unit = "100m"
for(unit in c("100m")){ # "50m", 
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
  
  ### Traffic Levels - Crash Buffers
  dough_files <- file.path(data_dir, "extracted-data") %>%
    list.files(pattern = "doughnut100m",
               full.names = T)
  
  dough_df_vec <- map(dough_files, function(dough_files_i){
    # i=1
    # dough_files_i <- dough_files[i]
    
    buff_size_i <- basename(dough_files_i) %>% 
      str_replace_all(paste0("twitter_crashes_|_doughnut", unit), "")
    
    mb_tl_dough_df <- file.path(dough_files_i, "mapbox_traffic_levels") %>%
      list.files(pattern = "*.Rds",
                 full.names = T) %>%
      map_df(readRDS) %>%
      
      # No variation in data across hours after here; some issue with API, so remove
      dplyr::filter(datetime < ymd_hms("2024-01-12 00:00:00", tz = "Africa/Nairobi")) %>%
      
      # Fix zero issues
      dplyr::mutate(length_mb_severe = replace_na(length_mb_severe, 0)) %>%
      
      # Create variables
      dplyr::mutate(length_traffic_total = length_mb_low + length_mb_moderate + length_mb_heavy + length_mb_severe) %>%
      group_by(crash_id) %>%
      dplyr::mutate(length_traffic_total_max = max(length_traffic_total, na.rm = T)) %>%
      ungroup() %>%
      
      dplyr::mutate(tl_prop_2 = length_mb_moderate/length_traffic_total_max,
                    tl_prop_3 = length_mb_heavy/length_traffic_total_max,
                    tl_prop_4 = length_mb_severe/length_traffic_total_max) %>%
      dplyr::rename_with(
        ~ paste0(., "_doughbuff_",buff_size_i),
        -c(crash_id, datetime)
      ) 
    
  })
  
  dough_df <- dough_df_vec %>%
    purrr::reduce(
      dplyr::full_join,
      by = c("crash_id", "datetime")
    )
  
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
    left_join(dough_df, by = c("crash_id", "datetime")) %>%
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


