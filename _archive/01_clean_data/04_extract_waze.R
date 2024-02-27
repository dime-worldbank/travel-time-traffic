# Extract Waze: Routes

# Load data --------------------------------------------------------------------

## Typical Routes
rt_typ_google_sf      <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))
rt_typ_google_10buff_sf <- st_buffer(rt_typ_google_sf, dist = 10)

rt_typ_mapbox_sf      <- readRDS(file.path(tt_dir, "mapbox_typical_route.Rds"))
rt_typ_mapbox_10buff_sf <- st_buffer(rt_typ_mapbox_sf, dist = 10)

## Crashes
rtc_sf      <- readRDS(file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds"))
rtc_50m_sf  <- st_buffer(rtc_sf, dist = 50)
rtc_100m_sf <- st_buffer(rtc_sf, dist = 100)

# Cleanup jams -----------------------------------------------------------------
for(year in 2019:2023){
  
  jams_yr_sf <- readRDS(file.path(waze_bq_dir, paste0("waze_jams_",year,".Rds")))
  st_crs(jams_yr_sf) <- 4326
  
  jams_yr_sf <- jams_yr_sf %>%
    dplyr::mutate(ts = ts %>% floor_date(unit = "hour"))
  
  for(polygon in POLYGONS_ALL){
    
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
    
    if(polygon %in% "mapbox_typical_route_10m"){
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
    
    for(poly_i in 1:nrow(roi_sf)){
      
      roi_sf_i <- roi_sf[poly_i,]
      
      OUT_DIR <- file.path(extracted_data_dir,
                           polygon,
                           "waze")
      dir.create(file.path(extracted_data_dir, polygon))
      dir.create(file.path(extracted_data_dir, polygon, "waze"))
      
      FILE_NAME <- paste("waze", polygon, year, roi_sf_i[[id_var]], sep = "_") %>% paste0(".Rds")
      
      OUT_PATH <- file.path(OUT_DIR, FILE_NAME)
      
      if(!file.exists(OUT_PATH)){
        
        jams_i_sf <- st_intersection(jams_yr_sf, roi_sf_i)
        
        jams_i_df <- jams_i_sf %>%
          st_drop_geometry() %>%
          group_by(ts) %>%
          dplyr::summarise(delay_sum_s = sum(delay)) %>%
          ungroup()
        
        jams_i_df$uid <- roi_sf_i[[id_var]]
        
        saveRDS(jams_i_df, OUT_PATH)
        
      }
    } 
  }
  
  rm(jams_yr_sf)
  gc()
}









