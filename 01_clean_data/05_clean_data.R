# Clean Data

# Load data --------------------------------------------------------------------
for(polygon_i in POLYGONS_ALL){
  print(polygon_i)
  
  #### Append Data
  data_wide_df <- file.path(extracted_data_dir, polygon_i, "google_traffic_levels") %>%
    list.files(full.names = T,
               pattern = "*.Rds") %>%
    map_df(readRDS) %>%
    dplyr::mutate(count_all = count_1 + count_2 + count_3 + count_4) %>%
    
    group_by(uid) %>%
    dplyr::mutate(count_all_max = max(count_all, na.rm = T)) %>%
    ungroup() %>%
    
    dplyr::mutate(gg_tl_prop_234 = (count_2 + count_3 + count_4) / count_all_max,
                  gg_tl_prop_34  = (          count_3 + count_4) / count_all_max,
                  gg_tl_prop_4   = (                    count_4) / count_all_max) %>%
    
    dplyr::rename(gg_tl_count_all_max = count_all_max) %>%
    dplyr::select(-c(count_0, count_1, count_2, count_3, count_4, count_all))
  
  #### Make Long
  data_long_df <- google_tl_df %>% 
    pivot_longer(cols = -c("uid", "datetime")) %>%
    dplyr::mutate(source = "Google Traffic Levels")
  
  # Add in Travel Time ---------------------------------------------------------
  if(str_detect(polygon_i, "typical_route")){
    
    #### Google
    google_tt_df <- readRDS(file.path(tt_dir, "google_tt_data.Rds")) 
    
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
    
    if(polygon_i %in% c("ntsa_crashes_50m", "ntsa_crashes_100m")){
      gg_inter_df <- gg_inter_df %>%
        as.data.frame() %>%
        dplyr::rename(uid = crash_id)
      
      mb_inter_df <- mb_inter_df %>%
        as.data.frame() %>%
        dplyr::rename(uid = crash_id)
    }
  
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
    # Memory intensive, so merge/aggregate for each event
    
    gg_sum_df <- map_df(unique(gg_inter_df$uid), function(uid){
      print(uid)
      
      gg_inter_df_i <- gg_inter_df[gg_inter_df$uid %in% uid,]
      
      gg_dt_i <- full_join(google_tt_df, gg_inter_df_i, 
                         by = c("segment_id"),
                         relationship = "many-to-many") %>%
        dplyr::filter(!is.na(uid)) %>%
        as.data.table()
      
      gg_sum_dt_i <- gg_dt_i[, .(gg_speed_kmh_mean = mean(gg_speed_kmh),
                             gg_speed_in_traffic_kmh_mean = mean(gg_speed_in_traffic_kmh),
                             gg_duration_s_mean = mean(gg_duration_s),
                             gg_duration_in_traffic_s_mean = mean(gg_duration_in_traffic_s),
                             gg_distance_m_mean = mean(gg_distance_m),
                             gg_speed_kmh_wmean = weighted.mean(gg_speed_kmh, w = road_length_m),
                             gg_speed_in_traffic_kmh_wmean = weighted.mean(gg_speed_in_traffic_kmh, w = road_length_m),
                             gg_duration_s_wmean = weighted.mean(gg_duration_s, w = road_length_m),
                             gg_duration_in_traffic_s_wmean = weighted.mean(gg_duration_in_traffic_s, w = road_length_m),
                             gg_distance_m_wmean = weighted.mean(gg_distance_m, w = road_length_m)),
                         by = .(uid, datetime)] %>%
        as.data.frame()
      
      return(gg_sum_dt_i)
    })
    
    mb_sum_df <- map_df(unique(mb_inter_df$uid), function(uid){
      print(uid)
      
      mb_inter_df_i <- mb_inter_df[mb_inter_df$uid %in% uid,]
      
      mb_df_i <- full_join(mapbox_tt_df, mb_inter_df_i, 
                         by = c("segment_id"),
                         relationship = "many-to-many") %>%
        dplyr::filter(!is.na(uid)) %>%
        as.data.table()
      
      mb_sum_dt_i <- mb_df_i[, .(mb_speed_in_traffic_kmh_mean = mean(mb_speed_in_traffic_kmh),
                             mb_duration_in_traffic_s_mean = mean(mb_duration_in_traffic_s),
                             mb_distance_m_mean = mean(mb_distance_m),
                             mb_speed_in_traffic_kmh_wmean = weighted.mean(mb_speed_in_traffic_kmh, w = road_length_m),
                             mb_duration_in_traffic_s_wmean = weighted.mean(mb_duration_in_traffic_s, w = road_length_m),
                             mb_distance_m_wmean = weighted.mean(mb_distance_m, w = road_length_m)),
                         by = .(uid, datetime)] %>%
        as.data.frame()
      
      return(mb_sum_dt_i)
    })

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
  if(polygon_i %in% c("ntsa_crashes_50m", "ntsa_crashes_100m")){
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
  
  # Final merge and export -----------------------------------------------------
  
  #### Long
  data_long_df <- data_long_df %>% 
    left_join(roi_df, by = "uid")
  
  saveRDS(data_long_df, file.path(analysis_data_dir, 
                                  paste0(polygon_i, "_long.Rds")))
  
  # write_dta(data_long_df, file.path(analysis_data_dir, 
  #                                   paste0(polygon_i, "_long.dta")))
  
  rm(data_long_df)
  gc()
  
  #### Wide
  data_wide_df <- data_wide_df %>% 
    left_join(roi_df, by = "uid")
  
  saveRDS(data_wide_df, file.path(analysis_data_dir, 
                                  paste0(polygon_i, "_wide.Rds")))
  
  # write_dta(data_wide_df, file.path(analysis_data_dir, 
  #                                   paste0(polygon_i, "_wide.dta")))
  
  rm(data_wide_df)
  gc()

}

