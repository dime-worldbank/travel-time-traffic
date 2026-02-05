# Twitter crash buffers

# Load twitter data ------------------------------------------------------------
twitter_sf <- readRDS(file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds"))
twitter_sf <- twitter_sf %>%
  dplyr::filter(crash_datetime >= ymd_hms("2023-05-20 00:00:00", tz = "Africa/Nairobi"))

twitter_buff_sf <- twitter_sf %>%
  st_buffer(dist = 100)

# Load other data --------------------------------------------------------------
estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))
cbd_sf <- estates_sf %>%
  dplyr::filter(Name == "Central Business District")

osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))

# Stats ------------------------------------------------------------------------
#### Distance to CBD
twitter_sf$dist_cbd_m <- st_distance(twitter_sf, cbd_sf) %>% as.numeric()

#### Largest road type
twitter_sf$osm_fclass_largest <- map_vec(1:nrow(twitter_buff_sf), function(i){
  inter_tf <- st_intersects(osm_sf, twitter_buff_sf[i,], sparse = FALSE) %>% as.vector()
  largest_road_type <- osm_sf$fclass[inter_tf] %>% 
    min() # motorway = 1 ... unclassified = max
  
  return(largest_road_type)
})

# Export -----------------------------------------------------------------------
twitter_df <- twitter_sf %>%
  st_drop_geometry() %>%
  dplyr::select(crash_id, dist_cbd_m, osm_fclass_largest)

saveRDS(twitter_df, file.path(data_dir, "Twitter Crashes", "FinalData", "crashes_twitter_attributes.Rds"))


