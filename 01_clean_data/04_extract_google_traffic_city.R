# Extract Google Traffic to City

unit <- "gadm1"

for(unit in c("gadm1", "gadm2", "gadm3")){
  
  # Load unit ------------------------------------------------------------------
  if(unit == "gadm1") unit_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds")) 
  if(unit == "gadm2") unit_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_2_pk.rds")) 
  if(unit == "gadm3") unit_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_3_pk.rds"))
  
  # Loop through traffic -------------------------------------------------------
  tiff_vec <- file.path(traffic_dir, 
                        "google_individual_rasters_nairobi") %>%
    list.files(pattern = "*.tiff")
  
  for(tiff_i in tiff_vec){
    
    tiff_datetime_i <- tiff_i %>%
      str_replace_all("gt_nairobi_utc", "") %>%
      str_replace_all(".tiff", "") %>%
      as.numeric() %>%
      as_datetime(tz = "UTC") %>%
      round_date(unit = "30 minutes") %>%
      with_tz(tzone = "Africa/Nairobi")
    
    ## Prep name
    filename_i <- tiff_datetime_i %>%
      as.character() %>% 
      paste0("gg_", .) %>%
      str_replace_all("[:punct:]", "_") %>%
      str_replace_all(" ", "_") %>%
      paste0(".Rds")
    
    OUT_PATH <- file.path(city_traffic_dir, paste0(unit, "_individual_files_google"), filename_i)
    
    if(!file.exists(OUT_PATH)){
      print(OUT_PATH)
      
      traffic_r <- raster(file.path(traffic_dir, "google_individual_rasters_nairobi", tiff_i))
      
      traffic_df <- extract_gt_to_poly(traffic_r, unit_sf)
      
      traffic_df$datetime <- tiff_datetime_i
      traffic_df$polygon_area_m2 <- unit_sf %>% st_area() %>% as.numeric()
      
      saveRDS(traffic_df, OUT_PATH)
      
    }
  }
}

