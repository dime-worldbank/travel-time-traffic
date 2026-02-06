# Extract Mapbox Traffic

# Load data --------------------------------------------------------------------

## Typical Routes
rt_typ_google_sf      <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))
rt_typ_google_10buff_sf <- st_buffer(rt_typ_google_sf, dist = 10)

rt_typ_mapbox_sf      <- readRDS(file.path(tt_dir, "mapbox_typical_route.Rds"))
rt_typ_mapbox_10buff_sf <- st_buffer(rt_typ_mapbox_sf, dist = 10)

## NTSA Crashes
rtc_sf      <- readRDS(file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds"))
rtc_50m_sf  <- st_buffer(rtc_sf, dist = 50)
rtc_100m_sf <- st_buffer(rtc_sf, dist = 100)

## Twitter Crashes
twitter_sf      <- readRDS(file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds"))
twitter_50m_sf  <- st_buffer(twitter_sf, dist = 50)
twitter_100m_sf <- st_buffer(twitter_sf, dist = 100)

## OSM
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_10m.Rds"))

## Estates
estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))

## Iso Routes
h3_iso_routes_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "appended_routes", "iso_10m_routes.Rds"))

# Setup parallel cores ---------------------------------------------------------

#myCluster <- makeCluster(4, type = "FORK") 

#myCluster <- makeCluster(3, type = "FORK") 

#registerDoParallel(myCluster)

# Extract data -----------------------------------------------------------------
tiff_vec <- file.path(traffic_gg_raw_dir) %>%
  list.files(pattern = "*.tiff") 

length(tiff_vec)

for(file_i in tiff_vec){
  #foreach(file_i=tiff_vec, .combine='c', .inorder=FALSE) %dopar% {
  
  r <- rast(file.path(traffic_gg_raw_dir, file_i))
  
  file_i_str <- file_i %>% str_replace_all(".tiff|gt_nairobi_utc", "")
  
  for(polygon in POLYGONS_ALL){
    
    if(polygon == "h3_iso_routes"){
      roi_sf <- h3_iso_routes_sf %>%
        dplyr::select(route_id)
      id_var <- "route_id"
    }
    
    if(polygon %in% "osm_10m"){
      roi_sf <- osm_sf %>%
        dplyr::select(uid)
      id_var <- "uid"
    }
    
    if(polygon %in% "estates"){
      roi_sf <- estates_sf %>%
        dplyr::select(uid)
      id_var <- "uid"
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
    
    if(polygon %in% "google_typical_route_10m"){
      roi_sf <- rt_typ_google_10buff_sf %>%
        dplyr::select(segment_id)
      id_var <- "segment_id"
    }
    
    if(polygon == "gadm1"){
      roi_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds")) %>%
        dplyr::select(GID_1) 
      id_var <- "GID_1"
    } 
    
    if(polygon == "gadm2"){
      roi_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_2_pk.rds")) %>%
        dplyr::select(GID_2) 
      id_var <- "GID_2"
    }
    
    if(polygon == "gadm3"){
      roi_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_3_pk.rds")) %>%
        dplyr::select(GID_3) 
      id_var <- "GID_3"
    } 
    
    OUT_DIR <- file.path(extracted_data_dir,
                         polygon,
                         "google_traffic_levels")
    dir.create(file.path(extracted_data_dir, polygon))
    dir.create(file.path(extracted_data_dir, polygon, "google_traffic_levels"))
    
    FILE_NAME <- paste("google_traffic_levels", polygon, file_i_str, sep = "_") %>% paste0(".Rds")
    
    OUT_PATH <- file.path(OUT_DIR, FILE_NAME)
    
    if(!file.exists(OUT_PATH)){
      print(file_i)
      
      datetime_i <- file_i %>%
        str_replace_all("gt_nairobi_utc", "") %>%
        str_replace_all(".tiff", "") %>%
        as.numeric() %>%
        as_datetime(tz = "UTC") %>%
        round_date(unit = "30 minutes") %>%
        with_tz(tzone = "Africa/Nairobi")
      
      roi_sf$uid <- roi_sf[[id_var]]
      if(id_var != "uid") roi_sf[[id_var]] <- NULL
      
      google_df <- extract_gt_to_poly(r, roi_sf)
      google_df$datetime <- datetime_i
      
      saveRDS(google_df, OUT_PATH)
      
    }
  } 
}










