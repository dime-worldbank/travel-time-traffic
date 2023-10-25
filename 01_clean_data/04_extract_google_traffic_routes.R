# Extract Google Traffic

BUFFER_M <- 10

# Load/prep data ---------------------------------------------------------------
## Travel time
tt_sf <- readRDS(file.path(tt_dir, "google_tt.Rds"))

## Traffic
tiff_vec <- file.path(traffic_gg_raw_dir) %>%
  list.files(pattern = "*.tiff") 

tiff_datetime <- tiff_vec %>%
  str_replace_all("gt_nairobi_utc", "") %>%
  str_replace_all(".tiff", "") %>%
  as.numeric() %>%
  as_datetime(tz = "UTC") %>%
  round_date(unit = "30 minutes") %>%
  with_tz(tzone = "Africa/Nairobi")

## Typical Routes
rt_typ_google_sf      <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))
rt_typ_google_buff_sf <- st_buffer(rt_typ_google_sf, dist = BUFFER_M)

rt_typ_mapbox_sf      <- readRDS(file.path(tt_dir, "mapbox_typical_route.Rds"))
rt_typ_mapbox_buff_sf <- st_buffer(rt_typ_mapbox_sf, dist = BUFFER_M)

# Extract  data ----------------------------------------------------------------
datetime_i <- unique(tt_sf$datetime)[22000] # For testing

#### Loop through datetimes
for(datetime_i in unique(tt_sf$datetime)){
  
  traffic_file <- tiff_vec[tiff_datetime %in% datetime_i]
  
  #### Check if traffic file exists
  if(length(traffic_file) > 0){
    
    ## Prep name
    filename_i <- datetime_i %>% 
      as.character() %>% 
      paste0("gg_", .) %>%
      str_replace_all("[:punct:]", "_") %>%
      str_replace_all(" ", "_") %>%
      paste0(paste0("_buffer", BUFFER_M, "m")) %>%
      paste0(".Rds")
    
    OUT_PATH <- file.path(traffic_tt_dir, "google_individual_files_routes", filename_i)
    
    #### Check if file already created
    if(!file.exists(OUT_PATH)){
      print(OUT_PATH)
      
      ## Road traffic raster
      traffic_r <- raster(file.path(traffic_gg_raw_dir, traffic_file))
      
      ## Prep route
      tt_sf_i      <- tt_sf[tt_sf$datetime %in% datetime_i,]
      tt_sf_buff_i <- st_buffer(tt_sf_i, dist = BUFFER_M)
      
      ## Extract Google Traffic
      rt_typ_google_df <- extract_gt_to_poly(traffic_r, rt_typ_google_buff_sf)
      rt_typ_mapbox_df <- extract_gt_to_poly(traffic_r, rt_typ_mapbox_buff_sf)
      tt_i_df          <- extract_gt_to_poly(traffic_r, tt_sf_buff_i)
      
      ## Area of Route Polygons
      rt_typ_google_df$route_area_m2_ggtyp <- rt_typ_google_buff_sf %>% st_area() %>% as.numeric()
      rt_typ_mapbox_df$route_area_m2_mptyp <- rt_typ_mapbox_buff_sf %>% st_area() %>% as.numeric()
      tt_i_df$route_area_m2_ggrte          <- tt_sf_buff_i          %>% st_area() %>% as.numeric()
      
      ## Cleanup dataframes
      rt_typ_google_df <- rt_typ_google_df %>%
        dplyr::select(segment_id, contains("count_"), route_area_m2_ggtyp) %>%
        dplyr::rename_at(vars(contains("count_")), . %>% str_replace_all("count_", "count_ggtyp_"))
      
      rt_typ_mapbox_df <- rt_typ_mapbox_df %>%
        dplyr::select(segment_id, contains("count_"), route_area_m2_mptyp) %>%
        dplyr::rename_at(vars(contains("count_")), . %>% str_replace_all("count_", "count_mptyp_"))
      
      tt_i_df <- tt_i_df %>%
        dplyr::select(segment_id, contains("count_"), route_area_m2_ggrte) %>%
        dplyr::rename_at(vars(contains("count_")), . %>% str_replace_all("count_", "count_ggrte_"))
      
      ## Merge Together
      tt_data <- tt_sf_i
      tt_data$geometry <- NULL
      
      gt_all_df <- tt_data %>%
        left_join(rt_typ_google_df, by = "segment_id") %>%
        left_join(rt_typ_mapbox_df, by = "segment_id") %>%
        left_join(tt_i_df, by = "segment_id")
      
      ## Export
      saveRDS(gt_all_df, OUT_PATH)
    }
  }
}







