# Clean Data

# Load data --------------------------------------------------------------------
for(polygon_i in POLYGONS_ALL){
  
  print(polygon_i)
  
  # Load/Clean Google Traffic Levels -------------------------------------------
  google_tl_df <- file.path(extracted_data_dir, polygon_i, "google_traffic_levels") %>%
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
    dplyr::select(-c(count_0, count_1, count_2, count_3, count_4, count_all)) %>%
    
    # No google traffic data beyond this date; all zeros
    dplyr::filter(datetime < ymd("2023-08-17"))
  
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
      dplyr::mutate(gg_duration_min = gg_duration_s / 60,
                    gg_duration_in_traffic_min = gg_duration_in_traffic_s / 60,
                    gg_distance_km = gg_distance_m / 1000) %>%
      dplyr::rename(uid = segment_id)
    
    google_tl_df <- google_tl_df %>%
      full_join(google_tt_df, by = c("uid", "datetime"))
    
  }
  
  # For points, merge in travel time routes ------------------------------------
  if(polygon_i %in% c("ntsa_crashes_50m", "ntsa_crashes_100m")){
    
    #### Load Point / Route Intersections
    gg_inter_df <- readRDS(file.path(data_dir, "points-intersect-routes", 
                                     paste0(polygon_i, "_", "google","_route",".Rds")))
    
    if(polygon_i %in% c("ntsa_crashes_50m", "ntsa_crashes_100m")){
      gg_inter_df <- gg_inter_df %>%
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
        as.data.frame() %>%
        dplyr::mutate(gg_duration_min_mean = gg_duration_s_mean / 60,
                      gg_duration_in_traffic_min_mean = gg_duration_in_traffic_s_mean / 60,
                      gg_distance_km_mean = gg_distance_m_mean / 1000,
                      gg_duration_min_wmean = gg_duration_s_wmean / 60,
                      gg_duration_in_traffic_min_wmean = gg_duration_in_traffic_s_wmean / 60,
                      gg_distance_km_wmean = gg_distance_m_wmean / 1000)
      
      return(gg_sum_dt_i)
    })
    
    #### Add to main datasets
    google_tl_df <- google_tl_df %>%
      full_join(gg_sum_df, by = c("uid", "datetime")) 
    
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
  
  #### Merge
  google_tl_df <- google_tl_df %>% 
    left_join(roi_df, by = "uid")
  
  # Export ---------------------------------------------------------------------
  saveRDS(google_tl_df, file.path(analysis_data_dir, 
                                  paste0(polygon_i, "_wide.Rds")))
  
  rm(data_wide_df)
  gc()
}

