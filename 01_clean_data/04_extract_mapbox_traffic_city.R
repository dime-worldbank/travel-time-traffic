# Extract Google Traffic to City

for(unit in c("gadm1", "gadm2", "gadm3")){
  
  # Load unit ------------------------------------------------------------------
  if(unit == "gadm1") unit_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds")) 
  if(unit == "gadm2") unit_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_2_pk.rds")) 
  if(unit == "gadm3") unit_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_3_pk.rds"))
  
  # Loop through traffic -------------------------------------------------------
  rds_vec <- file.path(traffic_dir, 
                       "mapbox_daily_data_nairobi") %>%
    list.files(pattern = "*.Rds",
               full.names = T) 
  
  for(rds_i in rds_vec){
    
    mp_sf <- readRDS(rds_i)
    mp_sf <- mp_sf %>%
      dplyr::mutate(datetime_scrape = datetime_scrape %>% 
                      floor_date(unit = "30 minutes") %>%
                      as.character())
    
    for(datetime_i in unique(mp_sf$datetime_scrape)){
      
      ## Prep name
      filename_i <- datetime_i %>% 
        as.character() %>% 
        paste0("mb_", .) %>%
        str_replace_all("[:punct:]", "_") %>%
        str_replace_all(" ", "_") %>%
        paste0(".Rds")
      
      OUT_PATH <- file.path(city_traffic_dir, paste0(unit, "_individual_files_mapbox"), filename_i)
      
      if(!file.exists(OUT_PATH)){
        print(OUT_PATH)
        
        mp_sf_i <- mp_sf[mp_sf$datetime_scrape %in% datetime_i,]

        traffic_df <- calc_traffic_length(unit_sf, mp_sf_i, add_by_class = T)
        
        traffic_df$datetime <- datetime_i
        traffic_df$polygon_area_m2 <- unit_sf %>% st_area() %>% as.numeric()
        
        saveRDS(traffic_df, OUT_PATH)
        
      }
    }
  }
}

