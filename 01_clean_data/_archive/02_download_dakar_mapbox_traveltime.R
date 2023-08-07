# Download Mapbox Travel Time in Monthly Chunks

# Download Function ------------------------------------------------------------
import_geojson_from_s3 <- function(s3_key){
  
  Sys.sleep(0.2)
  
  # Where are we?
  print(s3_key)
  
  # Grab file name
  object <- paste0("s3://",s3_key)
  
  # Extract geoJSON
  route_sf <- object %>% 
    get_object() %>%
    rawToChar() %>%
    geojson_sf()
  
  return(route_sf)
}

# Keys -------------------------------------------------------------------------
Sys.setenv("AWS_ACCESS_KEY_ID" = api_keys_df$Key[(api_keys_df$Account %in% "robmarty3@gmail.com") & (api_keys_df$Service %in% "AWS_ACCESS_KEY_ID")],
           "AWS_SECRET_ACCESS_KEY" = api_keys_df$Key[(api_keys_df$Account %in% "robmarty3@gmail.com") & (api_keys_df$Service %in% "AWS_SECRET_ACCESS_KEY")],
           "AWS_DEFAULT_REGION" = "us-east-2")

#### Grab file names
s3_files <- get_bucket(bucket="wb-dime-traveltimes", max=Inf, url_style="path", prefix="dakar/mapbox_routes/")
get_s3_keys <- function(i, s3_files) s3_files[i]$Contents$Key
s3_keys <- lapply(1:length(s3_files), get_s3_keys, s3_files) %>% unlist
s3_keys <- s3_keys[!endsWith(s3_keys, "/")] 
s3_keys <- paste0("wb-dime-traveltimes/", s3_keys)

s3_keys_date <- s3_keys %>%
  str_replace_all(".*mapbox_routes_routes_", "") %>%
  str_replace_all(".zip", "") %>%
  str_replace_all("_", "-") %>%
  ymd_hms() %>%
  as.character()

# Download ---------------------------------------------------------------------
# month_starts <- seq(from="2019-01-01" %>% as.Date(),
#                     to = "2022-12-01" %>% as.Date(),
#                     by = "month") %>% as.character()

latest_scraped <- file.path(dropbox_file_path, 
                            "Data",
                            "Travel Time",
                            "Mapbox Travel Time",
                            "FinalData",
                            "monthly_data") %>%
  list.files() %>%
  str_replace_all("mapbox_tt_", "") %>%
  str_replace_all(".Rds", "") %>%
  max()

day_starts <- seq(from = latest_scraped %>% as.Date(),
                  to = Sys.Date() - 1,
                  by = "day") %>%
  as.character()

for(day_start_i in day_starts){
  print(paste(day_start_i, "-----------------------------------------------"))
  
  s3_keys_day <- s3_keys[as.Date(s3_keys_date) %in% ymd(day_start_i)]
  
  routes_sf <- lapply(s3_keys_day, import_geojson_from_s3) %>% do.call(what="rbind")
  
  saveRDS(routes_sf, file.path(dropbox_file_path, 
                               "Data",
                               "Travel Time",
                               "Mapbox Travel Time",
                               "FinalData",
                               "daily_data",
                               paste0("mapbox_tt_",day_start_i,".Rds")))
  
}


