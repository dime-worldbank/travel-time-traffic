# Extract Points to Route

## NTSA Crashes
# rtc_sf      <- readRDS(file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds"))
# rtc_50m_sf  <- st_buffer(rtc_sf, dist = 50)
# rtc_100m_sf <- st_buffer(rtc_sf, dist = 100)

## Twitter Crashes
twitter_sf      <- readRDS(file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds"))
twitter_50m_sf  <- st_buffer(twitter_sf, dist = 50)
twitter_100m_sf <- st_buffer(twitter_sf, dist = 100)

#### Load route 
route_sf <- readRDS(file.path(tt_dir, "mapbox_typical_route.Rds")) %>%
  dplyr::select(segment_id)

buff_sizes_50m <- seq(from = 100, to = 2000, by = 50)
buff_sizes_100m <- seq(from = 200, to = 2000, by = 100)

for(polygon in c("twitter_crashes_50m",
                 "twitter_crashes_100m",
                 paste0("twitter_crashes_",buff_sizes_50m,"m_doughnut50m"),
                 paste0("twitter_crashes_",buff_sizes_100m,"m_doughnut100m"))){
  
  message(polygon)
  
  buff_name <- polygon %>% str_replace_all("twitter_crashes_", "")
  
  #### Load
  twitter_sf <- readRDS(file.path(data_dir, "Twitter Crashes", "FinalData", paste0("crashes_twitter_",buff_name,".Rds")))
  
  roi_sf <- twitter_sf %>%
    dplyr::select(crash_id)
  id_var <- "crash_id"
  
  
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
                    paste0(polygon, "_", "mapbox_route",".Rds")))
  
}






