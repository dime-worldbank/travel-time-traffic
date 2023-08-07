# Download Google Travel Time in Monthly Chunks

# Download Function ------------------------------------------------------------
import_routes <- function(s3_key){
  # Grab file name
  object <- paste0("s3://",s3_key)
  save_object(object=object,file="temp.zip")
  unzip("temp.zip")
  file <- unzip("temp.zip",list=T)
  file <- file[!endsWith(file$Name, "/"),] 
  file_name <- file$Name
  
  # Import File
  route_sf <- geojson_sf(file_name)
  
  print(file_name)
  
  # Remove files
  unlink(strsplit(file_name[1],"/")[[1]][1], recursive=T)
  file.remove("temp.zip")
  
  return(route_sf)
}

# Keys -------------------------------------------------------------------------
Sys.setenv("AWS_ACCESS_KEY_ID" = api_keys_df$Key[(api_keys_df$Account %in% "robmarty3@gmail.com") & (api_keys_df$Service %in% "AWS_ACCESS_KEY_ID")],
           "AWS_SECRET_ACCESS_KEY" = api_keys_df$Key[(api_keys_df$Account %in% "robmarty3@gmail.com") & (api_keys_df$Service %in% "AWS_SECRET_ACCESS_KEY")],
           "AWS_DEFAULT_REGION" = "us-east-2")

#### Grab file names 
s3_files <- get_bucket(bucket="wb-dime-traveltimes", max=Inf, url_style="path", prefix="dakar_senegal/routes/")
get_s3_keys <- function(i, s3_files) s3_files[i]$Contents$Key
s3_keys <- lapply(1:length(s3_files), get_s3_keys, s3_files) %>% unlist
s3_keys <- s3_keys[!endsWith(s3_keys, "/")] 
s3_keys <- paste0("wb-dime-traveltimes/", s3_keys)

s3_keys_datetime <- s3_keys %>%
  str_replace_all(".*dakar_routes_", "") %>%
  str_replace_all(".*routes_dakar_brt_", "") %>%
  str_replace_all("_r.*", "") %>%
  str_replace_all(".zip", "") %>%
  str_replace_all("_", "-") %>%
  ymd_hms() %>%
  as.character()

s3_keys_date <- s3_keys_datetime %>% as.Date()

# s3_files <- get_bucket(bucket="wb-dime-traveltimes", max=Inf, url_style="path", 
#                        prefix="dakar_senegal/routes/routes_dakar_brt_2023_07")


# Download ---------------------------------------------------------------------
# month_starts <- seq(from="2021-05-01" %>% as.Date(),
#                     to = Sys.Date() - 32,
#                     by = "month") %>% as.character()

dates_all <- s3_keys_date %>% as.Date() %>% unique() %>% sort()

# Don't scrape last date
dates_all <- dates_all[-length(dates_all)]

for(date_i in dates_all){
  print(paste(date_i, "-----------------------------------------------"))
  
  OUT_PATH <- file.path(tt_dir, 
                        "google_daily_data_dakar",
                        paste0("google_tt_",date_i,".Rds"))
  
  if(!file.exists(OUT_PATH)){
    
    s3_keys_date_i <- s3_keys[s3_keys_date %in% date_i]
    
    route_date_sf_i <- lapply(s3_keys_date_i, function(file_i){
      print(file_i)
      
      if(file_i %>% str_detect(".zip")){
        route_sf_i <- import_routes(file_i)
      }
      
      if(file_i %>% str_detect(".geojson")){
        
        route_sf_i <- aws.s3::s3read_using(read_sf, 
                                           object = paste0("s3://",file_i))
        route_sf_i$time <- route_sf_i$time %>% ymd_hms() %>% as.character()
        route_sf_i$alternative_id <- route_sf_i$alternative_id %>% as.character()
        route_sf_i$distance_m <- route_sf_i$distance_m %>% as.numeric()
        route_sf_i$duration_s <- route_sf_i$duration_s %>% as.numeric()
        route_sf_i$duration_in_traffic_s <- route_sf_i$duration_in_traffic_s %>% as.numeric()
        route_sf_i$segment_id <- route_sf_i$segment_id %>% as.character()
        
      }
      
      return(route_sf_i)
    }) %>%
      bind_rows()
    
    saveRDS(route_date_sf_i, OUT_PATH)
    
    
  }
  
  
}

# for(month_start_i in month_starts){
#   print(paste(month_start_i, "-----------------------------------------------"))
#   
#   OUT_PATH <- file.path(dropbox_file_path, 
#                         "Data",
#                         "Travel Time",
#                         "Google Travel Time",
#                         "FinalData",
#                         "monthly_data",
#                         paste0("google_tt_",month_start_i,".Rds"))
#   
#   if(!file.exists(OUT_PATH)){
#     
#     month_end_i <- month_start_i %>% str_replace_all("01$", "32")
#     
#     s3_keys_month <- s3_keys[s3_keys_date >= month_start_i & s3_keys_date <= month_end_i]
#     
#     routes_sf <- lapply(s3_keys_month, import_routes) %>% do.call(what="rbind")
#     
#     saveRDS(routes_sf, OUT_PATH)
#     
#   }
#   
# }
# 

