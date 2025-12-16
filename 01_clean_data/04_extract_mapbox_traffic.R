# Extract Google Traffic

BUFFER_M <- 10

# Load/prep data ---------------------------------------------------------------
## Traffic
rds_vec_all <- file.path(traffic_mb_raw_dir) %>%
  list.files(pattern = "*.Rds",
             full.names = T) 

## Typical Routes
rt_typ_mapbox_sf      <- readRDS(file.path(tt_dir, "mapbox_typical_route.Rds"))
rt_typ_mapbox_buff_sf <- st_buffer(rt_typ_mapbox_sf, dist = BUFFER_M)
rt_typ_mapbox_buff_sf$route_area_m2_mptyp <- rt_typ_mapbox_buff_sf %>% st_area() %>% as.numeric()

## OSM
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_10m.Rds"))

## Estates
estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))

## GADM1
nbo_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds"))

## Twitter Crashes
twitter_sf      <- readRDS(file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds"))
twitter_sf <- twitter_sf %>%
  dplyr::filter(crash_datetime >= ymd_hms("2023-05-20 00:00:00", tz = "Africa/Nairobi"))
twitter_50m_sf  <- st_buffer(twitter_sf, dist = 50)
twitter_100m_sf <- st_buffer(twitter_sf, dist = 100)


# Extract  data ----------------------------------------------------------------
rds_i <- rds_vec_all[1]
dataset = "mapbox_typical_route_10m"

for(dataset in "osm_10m"){
  
  if(dataset == "gadm1"){
    polyline_sf <- nbo_sf
    uid_var <- "GID_1"
    chunk_size <- 100
    rds_vec <- rds_vec_all
  }
  
  if(dataset == "twitter_crashes_50m"){
    polyline_sf <- twitter_50m_sf
    uid_var <- "crash_id"
    chunk_size <- 100
    rds_vec <- rds_vec_all
    rds_vec <- rds_vec[basename(rds_vec) <= "mp_nairobi_2023_07_13.Rds"]
  }
  
  if(dataset == "twitter_crashes_100m"){
    polyline_sf <- twitter_100m_sf
    uid_var <- "crash_id"
    chunk_size <- 100
    rds_vec <- rds_vec_all
    rds_vec <- rds_vec[basename(rds_vec) <= "mp_nairobi_2023_07_13.Rds"]
  }
  
  if(dataset == "estates"){
    polyline_sf <- estates_sf
    uid_var <- "uid"
    chunk_size <- 50
    rds_vec <- rds_vec_all
  }
  
  if(dataset == "mapbox_typical_route_10m"){
    polyline_sf <- rt_typ_mapbox_buff_sf
    uid_var <- "segment_id"
    chunk_size <- 10
    rds_vec <- rds_vec_all
  }
  
  if(dataset == "osm_10m"){
    polyline_sf <- osm_sf
    uid_var <- "uid"
    chunk_size <- 200
    rds_vec <- rds_vec_all
  }
  
  rds_vec <- sample(rds_vec)
  rds_vec <- sample(rds_vec)
  
  for(rds_i in rds_vec){
    
    mp_sf <- readRDS(rds_i)
    
    if(length(mp_sf) > 1){
      mp_sf <- mp_sf %>%
        dplyr::mutate(datetime_scrape = datetime_scrape %>% floor_date(unit = "30 minutes"))
      
      for(datetime_i in as.character(unique(mp_sf$datetime_scrape))){
        
        if(nchar(datetime_i) == 10){
          datetime_i <- paste0(datetime_i, "00:00:00")
        }
        
        ## Prep name
        filename_i <- datetime_i %>% 
          as.character() %>% 
          paste0("mb_", .) %>%
          str_replace_all("[:punct:]", "_") %>%
          str_replace_all(" ", "_") %>%
          paste0(paste0("_buffer", BUFFER_M, "m")) %>%
          paste0(".Rds")
        
        OUT_PATH <- file.path(data_dir, "extracted-data", dataset, "mapbox_traffic_levels", filename_i)
        
        if(!file.exists(OUT_PATH)){
          print(OUT_PATH)
          
          ## Grab Mapbox Traffic for Hour i
          mp_sf_i <- mp_sf[mp_sf$datetime_scrape %in% ymd_hms(datetime_i, tz = "Africa/Nairobi"),]
          
          if(dataset %in% c("mapbox_typical_route_10m", "gadm1")){
            rt_mapbox_df <- calc_traffic_length(polyline_sf, mp_sf_i)
          } else{
            rt_mapbox_df <- calc_traffic_length_all(polyline_sf, mp_sf_i, uid_var, chunk_size)
          }
          #

          rt_mapbox_clean_df <- rt_mapbox_df %>%
            dplyr::select(!!uid_var, contains("length_")) %>%
            dplyr::rename_at(vars(contains("length_")), . %>% str_replace_all("length_", "length_mb_"))
          
          rt_mapbox_clean_df$datetime <- ymd_hms(datetime_i, tz = "Africa/Nairobi")
          
          ## Export
          saveRDS(rt_mapbox_clean_df, OUT_PATH)
          
        }
      }
    }
  }
}
#}


