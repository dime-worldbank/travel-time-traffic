# Clean Data

# Load data --------------------------------------------------------------------
for(polygon_i in POLYGONS_ALL){
  print(polygon_i)
  
  #### Google Traffic Levels
  google_tl_df <- file.path(extracted_data_dir, polygon_i, "google_traffic_levels") %>%
    list.files(full.names = T,
               pattern = "*.Rds") %>%
    map_df(readRDS) %>%
    dplyr::mutate(count_all = count_1 + count_2 + count_3 + count_4) %>%
    
    group_by(uid) %>%
    dplyr::mutate(count_all_max = max(count_all)) %>%
    ungroup() %>%
    
    dplyr::mutate(gg_tl_prop_234 = (count_2 + count_3 + count_4) / count_all_max,
                  gg_tl_prop_34  = (          count_3 + count_4) / count_all_max,
                  gg_tl_prop_4   = (                    count_4) / count_all_max) %>%
    
    dplyr::rename(gg_tl_count_all_max = count_all_max) %>%
    dplyr::select(-c(count_0, count_1, count_2, count_3, count_4, count_all))
  
  #### Mapbox Traffic Levels
  mapbox_tl_df <- file.path(extracted_data_dir, polygon_i, "mapbox_traffic_levels") %>%
    list.files(full.names = T,
               pattern = "*.Rds") %>%
    map_df(readRDS) %>%
    dplyr::rename(datetime = datetime_scrape) %>%
    dplyr::mutate(length_all = low + moderate + heavy + severe) %>%
    
    group_by(uid) %>%
    dplyr::mutate(length_all_max = max(length_all)) %>%
    ungroup() %>%
    
    dplyr::mutate(mb_tl_prop_234 = (moderate + heavy + severe) / length_all_max,
                  mb_tl_prop_34  = (           heavy + severe) / length_all_max,
                  mb_tl_prop_4   = (                   severe) / length_all_max) %>%
    
    dplyr::rename(mb_tl_length_all_max = length_all_max) %>%
    dplyr::select(-c(low, moderate, heavy, severe, length_all))
  
  #### TomTom
  tomtom_df <- file.path(extracted_data_dir, polygon_i, "tomtom") %>%
    list.files(full.names = T,
               pattern = "*.Rds") %>%
    map_df(readRDS) %>%
    dplyr::mutate(timeSet = timeSet - 2) %>%
    dplyr::mutate(datetime = paste(date_from, timeSet) %>% ymd_h(tz = "Africa/Nairobi")) %>%
    dplyr::select(-c(timeSet, date_from, date_to)) %>%
    rename_with(~paste0("tmtm_", .), -c("datetime", "uid"))
  
  #### Waze
  waze_df <- file.path(extracted_data_dir, polygon_i, "waze") %>%
    list.files(full.names = T,
               pattern = "*.Rds") %>%
    map_df(readRDS) %>%
    dplyr::rename(datetime = ts) %>%
    dplyr::mutate(wz_delay_sum_min = delay_sum_s / 60) %>%
    dplyr::select(-delay_sum_s)
  
  #### Make Wide
  data_wide_df <- google_tl_df %>%
    full_join(mapbox_tl_df, by = c("uid", "datetime")) %>%
    full_join(tomtom_df, by = c("uid", "datetime")) %>%
    full_join(waze_df, by = c("uid", "datetime"))
  
  #### Make Long
  data_long_df <- bind_rows(
    google_tl_df %>% 
      pivot_longer(cols = -c("uid", "datetime")) %>%
      dplyr::mutate(source = "Google Traffic Levels"),
    
    mapbox_tl_df %>% 
      pivot_longer(cols = -c("uid", "datetime")) %>%
      dplyr::mutate(source = "Mapbox Traffic Levels"),
    
    tomtom_df %>% 
      pivot_longer(cols = -c("uid", "datetime")) %>%
      dplyr::mutate(source = "TomTom"),
    
    waze_df %>% 
      pivot_longer(cols = -c("uid", "datetime")) %>%
      dplyr::mutate(source = "Waze")
  )
  
  # Add in Travel Time ---------------------------------------------------------
  if(str_detect(polygon_i, "typical_route")){
    
    #### Google
    google_tt_df <- readRDS(file.path(tt_dir,
                                      "google_tt_data.Rds")) 
    
    google_tt_df <- google_tt_df %>%
      dplyr::select(segment_id, datetime, 
                    speed_kmh, speed_in_traffic_kmh,
                    duration_s, duration_in_traffic_s,
                    distance_m) %>%
      dplyr::rename(gg_speed_kmh = speed_kmh,
                    gg_speed_in_traffic_kmh = speed_in_traffic_kmh,
                    gg_duration_s = duration_s,
                    gg_duration_in_traffic_s = duration_in_traffic_s,
                    gg_distance_m = distance_m) %>%
      dplyr::rename(uid = segment_id)
    
    #### Mapbox
    mapbox_tt_df <- readRDS(file.path(tt_dir, "mapbox_tt_data.Rds"))
    
    mapbox_tt_df <- mapbox_tt_df %>%
      dplyr::select(segment_id, datetime, 
                    speed_kmh,
                    duration_s,
                    distance_m) %>%
      dplyr::rename(mb_speed_in_traffic_kmh = speed_kmh,
                    mb_duration_in_traffic_s = duration_s,
                    mb_distance_m = distance_m) %>%
      dplyr::rename(uid = segment_id)
    
    #### Make Wide
    tt_df <- full_join(google_tt_df, mapbox_tt_df,
                       by = c("uid", "datetime"))
    
    data_wide_df <- data_wide_df %>%
      full_join(tt_df, by = c("uid", "datetime"))
    
    #### Make Long
    tt_long_df <- bind_rows(
      google_tt_df %>% 
        pivot_longer(cols = -c("uid", "datetime")) %>%
        dplyr::mutate(source = "Google Travel Time"),
      
      mapbox_tt_df %>% 
        pivot_longer(cols = -c("uid", "datetime")) %>%
        dplyr::mutate(source = "Mapbox Travel Time")
    )
    
    data_long_df <- bind_rows(data_long_df,
                              tt_long_df)
    
  }
  
  # For points, merge in travel time routes ------------------------------------
  if(polygon_i %in% c("ntsa_crashes_50m", "ntsa_crashes_100m")){
    
    #### Load Point / Route Intersections
    gg_inter_df <- readRDS(file.path(data_dir, "points-intersect-routes", 
                                     paste0(polygon_i, "_", "google","_route",".Rds")))
    
    mb_inter_df <- readRDS(file.path(data_dir, "points-intersect-routes", 
                                     paste0(polygon_i, "_", "google","_route",".Rds")))
    
    #### Google
    google_tt_df <- readRDS(file.path(tt_dir,
                                      "google_tt_data.Rds")) 
    
    google_tt_df <- google_tt_df %>%
      dplyr::select(segment_id, datetime, 
                    speed_kmh, speed_in_traffic_kmh,
                    duration_s, duration_in_traffic_s,
                    distance_m) %>%
      dplyr::rename(gg_speed_kmh = speed_kmh,
                    gg_speed_in_traffic_kmh = speed_in_traffic_kmh,
                    gg_duration_s = duration_s,
                    gg_duration_in_traffic_s = duration_in_traffic_s,
                    gg_distance_m = distance_m) 
    
    #### Mapbox
    mapbox_tt_df <- readRDS(file.path(tt_dir, "mapbox_tt_data.Rds"))
    
    mapbox_tt_df <- mapbox_tt_df %>%
      dplyr::select(segment_id, datetime, 
                    speed_kmh,
                    duration_s,
                    distance_m) %>%
      dplyr::rename(mb_speed_in_traffic_kmh = speed_kmh,
                    mb_duration_in_traffic_s = duration_s,
                    mb_distance_m = distance_m) 
    
    #### Merge / Aggregate
    gg_dt <- full_join(google_tt_df, gg_inter_df, 
                       by = c("segment_id"),
                       relationship = "many-to-many") %>%
      as.data.table()
    
    gg_sum_dt <- gg_dt[, .(gg_speed_kmh_mean = mean(gg_speed_kmh),
                           gg_speed_in_traffic_kmh_mean = mean(gg_speed_in_traffic_kmh),
                           gg_duration_s_mean = mean(gg_duration_s),
                           gg_duration_in_traffic_s_mean = mean(gg_duration_in_traffic_s),
                           gg_distance_m_mean = mean(gg_distance_m),
                           gg_speed_kmh_wmean = weighted.mean(gg_speed_kmh, w = road_length_m),
                           gg_speed_in_traffic_kmh_wmean = weighted.mean(gg_speed_in_traffic_kmh, w = road_length_m),
                           gg_duration_s_wmean = weighted.mean(gg_duration_s, w = road_length_m),
                           gg_duration_in_traffic_s_wmean = weighted.mean(gg_duration_in_traffic_s, w = road_length_m),
                           gg_distance_m_wmean = weighted.mean(gg_distance_m, w = road_length_m)),
                       by = .(crash_id, datetime)]
    
    gg_sum_df <- gg_sum_dt %>%
      as.data.frame() %>%
      dplyr::rename(uid = crash_id)
    
    
    mb_df <- full_join(mapbox_tt_df, mb_inter_df, 
                       by = c("segment_id"),
                       relationship = "many-to-many") %>%
      as.data.table()
    
    mb_sum_dt <- mb_df[, .(mb_speed_in_traffic_kmh_mean = mean(mb_speed_in_traffic_kmh),
                           mb_duration_in_traffic_s_mean = mean(mb_duration_in_traffic_s),
                           mb_distance_m_mean = mean(mb_distance_m),
                           mb_speed_in_traffic_kmh_wmean = weighted.mean(mb_speed_in_traffic_kmh, w = road_length_m),
                           mb_duration_in_traffic_s_wmean = weighted.mean(mb_duration_in_traffic_s, w = road_length_m),
                           mb_distance_m_wmean = weighted.mean(mb_distance_m, w = road_length_m)),
                       by = .(crash_id, datetime)]
    
    mb_sum_df <- mb_sum_dt %>%
      as.data.frame() %>%
      dplyr::rename(uid = crash_id)
    
    #### Add to main datasets
    data_wide_df <- data_wide_df %>%
      full_join(gg_sum_df, by = c("uid", "datetime")) %>%
      full_join(mb_sum_df, by = c("uid", "datetime"))  
    
    data_long_df <- bind_rows(data_long_df,
                              
                              gg_sum_df %>% 
                                pivot_longer(cols = -c("uid", "datetime")) %>%
                                dplyr::mutate(source = "Google Travel Time"),
                              
                              mb_sum_df %>% 
                                pivot_longer(cols = -c("uid", "datetime")) %>%
                                dplyr::mutate(source = "Mapbox Travel Time")
    )
    
    
    
    
  }
  
  # Add in attributes ----------------------------------------------------------
  if(polygon_i %in% c("ntsa_crashes_500m", "ntsa_crashes_100m")){
    roi_df <- readRDS(file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds")) %>%
      dplyr::select(crash_id, datetime, no, contains("veh_type")) %>%
      dplyr::rename(crash_datetime = datetime,
                    uid = crash_id) %>%
      st_drop_geometry()
  }
  
  if(polygon_i == "google_typical_route_10m"){
    roi_df <- readRDS(file.path(tt_dir, "google_tt_data.Rds")) %>%
      dplyr::select(segment_id, road_name) %>%
      distinct() %>%
      dplyr::rename(uid = segment_id)
  }
  
  if(polygon_i == "mapbox_typical_route_10m"){
    roi_df <- readRDS(file.path(tt_dir, "mapbox_tt_data.Rds")) %>%
      dplyr::select(segment_id, road_name) %>%
      distinct() %>%
      dplyr::rename(uid = segment_id)
  }
  
  if(polygon_i == "gadm1"){
    roi_df <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds")) %>%
      dplyr::select(GID_1, NAME_1) %>%
      st_drop_geometry() %>%
      distinct() %>%
      dplyr::rename(uid = GID_1)
  }
  
  if(polygon_i == "gadm2"){
    roi_df <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_2_pk.rds")) %>%
      dplyr::select(GID_2, NAME_1, NAME_2) %>%
      st_drop_geometry() %>%
      distinct() %>%
      dplyr::rename(uid = GID_2)
  }
  
  if(polygon_i == "gadm3"){
    roi_df <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_3_pk.rds")) %>%
      dplyr::select(GID_3, NAME_1, NAME_2, NAME_3) %>%
      st_drop_geometry() %>%
      distinct() %>%
      dplyr::rename(uid = GID_3)
  }
  
  data_long_df <- data_long_df %>% 
    left_join(roi_df, by = "uid")
  
  data_wide_df <- data_wide_df %>% 
    left_join(roi_df, by = "uid")
  
  # Export ---------------------------------------------------------------------
  saveRDS(data_long_df, file.path(analysis_data_dir, 
                                  paste0(polygon_i, "_long.Rds")))
  
  write_dta(data_long_df, file.path(analysis_data_dir, 
                                    paste0(polygon_i, "_long.dta")))
  
  
  saveRDS(data_wide_df, file.path(analysis_data_dir, 
                                  paste0(polygon_i, "_wide.Rds")))
  
  write_dta(data_wide_df, file.path(analysis_data_dir, 
                                    paste0(polygon_i, "_wide.dta")))
}

