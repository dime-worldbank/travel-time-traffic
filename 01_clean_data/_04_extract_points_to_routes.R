# Extract Points to Route

## NTSA Crashes
rtc_sf      <- readRDS(file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds"))
rtc_50m_sf  <- st_buffer(rtc_sf, dist = 50)
rtc_100m_sf <- st_buffer(rtc_sf, dist = 100)

## Twitter Crashes
twitter_sf      <- readRDS(file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds"))
twitter_50m_sf  <- st_buffer(twitter_sf, dist = 50)
twitter_100m_sf <- st_buffer(twitter_sf, dist = 100)

#### Load route 
route_sf <- readRDS(file.path(tt_dir, "google_typical_route.Rds")) %>%
  dplyr::select(segment_id)

for(polygon in c("ntsa_crashes_50m",
                 "ntsa_crashes_100m",
                 "twitter_crashes_50m",
                 "twitter_crashes_100m")){
  
  #### Load
  if(polygon %in% "ntsa_crashes_50m"){
    roi_sf <- rtc_50m_sf %>%
      dplyr::select(crash_id)
    id_var <- "crash_id"
  }
  
  if(polygon %in% "ntsa_crashes_100m"){
    roi_sf <- rtc_100m_sf %>%
      dplyr::select(crash_id)
    id_var <- "crash_id"
  }
  
  if(polygon %in% "twitter_crashes_50m"){
    roi_sf <- twitter_50m_sf %>%
      dplyr::select(crash_id)
    id_var <- "crash_id"
  }
  
  if(polygon %in% "twitter_crashes_100m"){
    roi_sf <- twitter_100m_sf %>%
      dplyr::select(crash_id)
    id_var <- "crash_id"
  }
  
  #### Intersect Points and Route
  roi_route_df <- map_df(1:nrow(roi_sf), function(i){
    print(i)
    
    roi_i_sf <- roi_sf[i,]
    roi_i_route_df <- st_intersection(roi_i_sf, route_sf) 
    roi_i_route_df$road_length_m <- roi_i_route_df %>% st_length() %>% as.numeric()
    
    roi_i_route_df <- roi_i_route_df %>%
      st_drop_geometry()
    
    return(roi_i_route_df)
  })
  
  saveRDS(roi_route_df,
          file.path(data_dir, "points-intersect-routes", 
                    paste0(polygon, "_", "google_route",".Rds")))
  
}






