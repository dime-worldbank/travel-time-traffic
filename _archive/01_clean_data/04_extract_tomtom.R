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

# Extract data -----------------------------------------------------------------
tomtom_files <- tomtom_raw_dir %>%
  list.files()

for(file_i in tomtom_files){
  
  tomtom_sf <- read_sf(file.path(tomtom_raw_dir, file_i))
  
  date_range_df <- tomtom_sf$dateRanges[1] %>% fromJSON()
  
  date_from_to_str <- paste0(date_range_df$from, "_", date_range_df$to) %>%
    str_replace_all("-", "_")
  
  tomtom_sf$mapsVersions <- NULL
  tomtom_sf
  tomtom_sf <- tomtom_sf[-1,]
  
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
                           "tomtom")
      dir.create(file.path(extracted_data_dir, polygon))
      dir.create(file.path(extracted_data_dir, polygon, "tomtom"))
      
      FILE_NAME <- paste("tomtom", polygon, date_from_to_str, roi_sf_i[[id_var]], sep = "_") %>% paste0(".Rds")
      
      OUT_PATH <- file.path(OUT_DIR, FILE_NAME)
      
      if(!file.exists(OUT_PATH)){
        
        tomtom_i_sf <- st_intersection(tomtom_sf, roi_sf_i)
        
        if(nrow(tomtom_i_sf) > 0){
          
          tomtom_i_long_df <- map_df(1:nrow(tomtom_i_sf), function(i){
            
            speed_df_i <- fromJSON(tomtom_i_sf$segmentTimeResults[i]) 
            
            q_df <- map_df(1:length(speed_df_i$speedPercentiles), function(j){
              q_values <- speed_df_i$speedPercentiles[j][[1]]
              q_names <- paste0("q", seq(5,95,5))
              
              q_i_df <- data.frame(t(q_values))
              names(q_i_df) <- q_names
              
              return(q_i_df)
            })
            
            speed_df_i$speedPercentiles <- NULL
            speed_df_i <- bind_cols(speed_df_i, q_df)
            
            speed_df_i$speedLimit <- tomtom_i_sf$speedLimit[i]
            speed_df_i$distance   <- tomtom_i_sf$distance[i]
            speed_df_i$frc        <- tomtom_i_sf$frc[i]
            
            return(speed_df_i)
          })
          
          tomtom_i_df <- tomtom_i_long_df %>%
            dplyr::filter(sampleSize >= 1) %>%
            group_by(timeSet) %>%
            summarise(havgspeed_wmean = weighted.mean(x = harmonicAverageSpeed, w = sampleSize),
                      avgspeed_wmean         = weighted.mean(x = averageSpeed,         w = sampleSize),
                      q10_wmean = weighted.mean(x = q10, w = sampleSize),
                      q20_wmean = weighted.mean(x = q20, w = sampleSize),
                      q30_wmean = weighted.mean(x = q30, w = sampleSize),
                      q40_wmean = weighted.mean(x = q40, w = sampleSize),
                      q50_wmean = weighted.mean(x = q50, w = sampleSize),
                      q60_wmean = weighted.mean(x = q60, w = sampleSize),
                      q70_wmean = weighted.mean(x = q70, w = sampleSize),
                      q80_wmean = weighted.mean(x = q80, w = sampleSize),
                      q90_wmean = weighted.mean(x = q90, w = sampleSize),
                      q95_wmean = weighted.mean(x = q95, w = sampleSize),
                      
                      sl_o_havgspeed_wmean = weighted.mean(x = (harmonicAverageSpeed - speedLimit), w = sampleSize),
                      sl_o_avgspeed_wmean         = weighted.mean(x = (averageSpeed - speedLimit),         w = sampleSize),
                      sl_o_q10_wmean = weighted.mean(x = (q10 - speedLimit), w = sampleSize),
                      sl_o_q20_wmean = weighted.mean(x = (q20 - speedLimit), w = sampleSize),
                      sl_o_q30_wmean = weighted.mean(x = (q30 - speedLimit), w = sampleSize),
                      sl_o_q40_wmean = weighted.mean(x = (q40 - speedLimit), w = sampleSize),
                      sl_o_q50_wmean = weighted.mean(x = (q50 - speedLimit), w = sampleSize),
                      sl_o_q60_wmean = weighted.mean(x = (q60 - speedLimit), w = sampleSize),
                      sl_o_q70_wmean = weighted.mean(x = (q70 - speedLimit), w = sampleSize),
                      sl_o_q80_wmean = weighted.mean(x = (q80 - speedLimit), w = sampleSize),
                      sl_o_q90_wmean = weighted.mean(x = (q90 - speedLimit), w = sampleSize),
                      sl_o_q95_wmean = weighted.mean(x = (q95 - speedLimit), w = sampleSize),
                      
                      sample_size_sum = sum(sampleSize))
          
          tomtom_i_df$uid <- roi_sf_i[[id_var]]
          tomtom_i_df$date_from <- date_range_df$from
          tomtom_i_df$date_to   <- date_range_df$to
          
          saveRDS(tomtom_i_df, OUT_PATH)
          
        }
      } 
    }
  }
}









