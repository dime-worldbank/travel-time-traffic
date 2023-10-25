# Extract Google Traffic

BUFFER_M <- 10

# Load/prep data ---------------------------------------------------------------
## Travel time
tt_sf <- readRDS(file.path(tt_dir, "mapbox_tt.Rds"))

## Traffic
rds_vec <- file.path(traffic_mb_raw_dir) %>%
  list.files(pattern = "*.Rds",
             full.names = T) 

## Typical Routes
rt_typ_google_sf      <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))
rt_typ_google_buff_sf <- st_buffer(rt_typ_google_sf, dist = BUFFER_M)
rt_typ_google_buff_sf$route_area_m2_ggtyp <- rt_typ_google_buff_sf %>% st_area() %>% as.numeric()

rt_typ_mapbox_sf      <- readRDS(file.path(tt_dir, "mapbox_typical_route.Rds"))
rt_typ_mapbox_buff_sf <- st_buffer(rt_typ_mapbox_sf, dist = BUFFER_M)
rt_typ_mapbox_buff_sf$route_area_m2_mptyp <- rt_typ_mapbox_buff_sf %>% st_area() %>% as.numeric()

# Extract  data ----------------------------------------------------------------
rds_i <- rds_vec[1]
for(rds_i in rds_vec){
  
  mp_sf <- readRDS(rds_i)
  if(length(mp_sf) > 1){
    mp_sf <- mp_sf %>%
      dplyr::mutate(datetime_scrape = datetime_scrape %>% floor_date(unit = "30 minutes"))
    
    for(datetime_i in as.character(unique(mp_sf$datetime_scrape))){
      
      tt_sf_i <- tt_sf[tt_sf$datetime %in% ymd_hms(datetime_i, tz = "Africa/Nairobi"),]
      
      if(nrow(tt_sf_i) > 0){
        
        ## Prep name
        filename_i <- datetime_i %>% 
          as.character() %>% 
          paste0("mb_", .) %>%
          str_replace_all("[:punct:]", "_") %>%
          str_replace_all(" ", "_") %>%
          paste0(paste0("_buffer", BUFFER_M, "m")) %>%
          paste0(".Rds")
        
        OUT_PATH <- file.path(traffic_tt_dir, "mapbox_individual_files_routes", filename_i)
        
        if(!file.exists(OUT_PATH)){
          print(OUT_PATH)
          
          ## Grab Mapbox Traffic for Hour i
          mp_sf_i <- mp_sf[mp_sf$datetime_scrape %in% ymd_hms(datetime_i, tz = "Africa/Nairobi"),]
          
          ## Buffer actual route
          tt_sf_buff_i <- st_buffer(tt_sf_i, dist = BUFFER_M)
          tt_sf_buff_i$route_area_m2_mprte <- tt_sf_buff_i %>% st_area() %>% as.numeric()
          
          ## Extract traffic length
          rt_rte_mapbox_df <- calc_traffic_length(tt_sf_buff_i, mp_sf_i)
          rt_typ_google_df <- calc_traffic_length(rt_typ_google_buff_sf, mp_sf_i)
          rt_typ_mapbox_df <- calc_traffic_length(rt_typ_mapbox_buff_sf, mp_sf_i)
          
          ## Cleanup dataframes
          rt_typ_google_df <- rt_typ_google_df %>%
            dplyr::select(segment_id, contains("length_"), route_area_m2_ggtyp) %>%
            dplyr::rename_at(vars(contains("length_")), . %>% str_replace_all("length_", "length_ggtyp_"))
          
          rt_typ_mapbox_df <- rt_typ_mapbox_df %>%
            dplyr::select(segment_id, contains("length_"), route_area_m2_mptyp) %>%
            dplyr::rename_at(vars(contains("length_")), . %>% str_replace_all("length_", "length_mptyp_"))
          
          rt_rte_mapbox_df <- rt_rte_mapbox_df %>%
            dplyr::select(segment_id, contains("length_"), route_area_m2_mprte) %>%
            dplyr::rename_at(vars(contains("length_")), . %>% str_replace_all("length_", "length_mprte_"))
          
          ## Merge Together
          tt_data <- tt_sf_i
          tt_data$geometry <- NULL
          
          mp_all_df <- tt_data %>%
            left_join(rt_typ_google_df, by = "segment_id") %>%
            left_join(rt_typ_mapbox_df, by = "segment_id") %>%
            left_join(rt_rte_mapbox_df, by = "segment_id")
          
          ## Export
          saveRDS(mp_all_df, OUT_PATH)
          
        }
      }
    }
  }
}


