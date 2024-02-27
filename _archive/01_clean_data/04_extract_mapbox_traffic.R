# Extract Mapbox Traffic

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

# Extract data -----------------------------------------------------------------
mapbox_files <- file.path(traffic_mb_raw_dir) %>%
  list.files(pattern = "*.Rds") 

for(file_i in mapbox_files){
  
  mapbox_sf <- readRDS(file.path(traffic_mb_raw_dir, file_i))
  
  date_i_str <- file_i %>%
    str_replace_all(".Rds|mp_nairobi_", "") 
  
  if(length(mapbox_sf) > 1){
    
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
                             "mapbox_traffic_levels")
        dir.create(file.path(extracted_data_dir, polygon))
        dir.create(file.path(extracted_data_dir, polygon, "mapbox_traffic_levels"))
        
        FILE_NAME <- paste("mapbox_traffic_levels", polygon, date_i_str, roi_sf_i[[id_var]], sep = "_") %>% paste0(".Rds")
        
        OUT_PATH <- file.path(OUT_DIR, FILE_NAME)
        
        if(!file.exists(OUT_PATH)){

          mapbox_i_sf <- st_intersection(mapbox_sf, roi_sf_i)
          
          mapbox_i_sf$length_m <- mapbox_i_sf %>% st_length() %>% as.numeric()
          
          mapbox_i_df <- mapbox_i_sf %>%
            st_drop_geometry() %>%
            dplyr::mutate(datetime_scrape = datetime_scrape %>% 
                            with_tz(tzone = "Africa/Nairobi") %>%
                            floor_date(unit = "hours")) %>%
            group_by(datetime_scrape, congestion) %>%
            dplyr::summarise(length_m = sum(length_m)) %>%
            ungroup() %>%
            pivot_wider(id_cols = datetime_scrape,
                        names_from = congestion,
                        values_from = length_m)
          
          if("low"      %in% names(mapbox_i_df)) mapbox_i_df$low[is.na(mapbox_i_df$low)]           <- 0
          if("moderate" %in% names(mapbox_i_df)) mapbox_i_df$moderate[is.na(mapbox_i_df$moderate)] <- 0
          if("heavy"    %in% names(mapbox_i_df)) mapbox_i_df$heavy[is.na(mapbox_i_df$heavy)]       <- 0
          if("severe"   %in% names(mapbox_i_df)) mapbox_i_df$severe[is.na(mapbox_i_df$severe)]     <- 0
          
          mapbox_i_df$uid <- roi_sf_i[[id_var]]
          
          saveRDS(mapbox_i_df, OUT_PATH)
          
        }
      } 
    }
  }
}








