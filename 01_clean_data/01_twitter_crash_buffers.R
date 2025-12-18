# Twitter Crash Buffers

## Twitter Crashes
twitter_sf      <- readRDS(file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds"))
twitter_sf <- twitter_sf %>%
  dplyr::filter(crash_datetime >= ymd_hms("2023-05-20 00:00:00", tz = "Africa/Nairobi"))

for(buffer_m in seq(from = 50, to = 3000, by = 50)){
  message(buffer_m)
  
  twitter_buff_sf <- st_buffer(twitter_sf, dist = buffer_m)
  
  twitter_buff_small_50m_sf <- st_buffer(twitter_sf, dist = buffer_m - 50)
  twitter_buff_small_100m_sf <- st_buffer(twitter_sf, dist = buffer_m - 100)
  
  twitter_buff_doughnut_50m_sf <- map_df(1:nrow(twitter_buff_sf), function(i){
    st_difference(twitter_buff_sf[i,], twitter_buff_small_50m_sf[i,])
  }) %>%
    dplyr::select(-c(crash_id.1, crash_datetime.1))
  
  twitter_buff_doughnut_100m_sf <- map_df(1:nrow(twitter_buff_sf), function(i){
    st_difference(twitter_buff_sf[i,], twitter_buff_small_100m_sf[i,])
  }) %>%
    dplyr::select(-c(crash_id.1, crash_datetime.1))
  
  saveRDS(twitter_buff_sf,
          file.path(data_dir, "Twitter Crashes", "FinalData",
                    paste0("crashes_twitter_", buffer_m, "m.Rds")))
  
  saveRDS(twitter_buff_doughnut_50m_sf,
          file.path(data_dir, "Twitter Crashes", "FinalData",
                    paste0("crashes_twitter_", buffer_m, "m_doughnut50m.Rds")))
  
  saveRDS(twitter_buff_doughnut_100m_sf,
          file.path(data_dir, "Twitter Crashes", "FinalData",
                    paste0("crashes_twitter_", buffer_m, "m_doughnut100m.Rds")))
}

