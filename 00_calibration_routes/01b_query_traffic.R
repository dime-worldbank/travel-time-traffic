# Scrape Data

source("~/Documents/github/travel-time-traffic/_main.R")

# Load data --------------------------------------------------------------------
grid_df <- readRDS(file.path(data_dir, "Google Traffic 2026", "Grid", "grid_param.Rds"))

# Setup ------------------------------------------------------------------------
api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv")

hr_now <- hour(Sys.time())

EVEN_HOURS <- seq(from = 0, to = 23, by = 2)

if(hr_now %in% EVEN_HOURS){
  google_key <- api_keys %>%
    dplyr::filter(Service == "Google Routes API",
                  Account == "randrewm039@gmail.com") %>%
    pull(Key) %>%
    as.character()
} else{
  google_key <- api_keys %>%
    dplyr::filter(Service == "Google Routes API",
                  Account == "robmarty3@gmail.com") %>%
    pull(Key) %>%
    as.character()
}

Sys.setenv("AWS_ACCESS_KEY_ID" = as.character(api_keys$Key[(api_keys$Service %in% "AWS_ACCESS_KEY_ID") & (api_keys$Account %in% "robmarty3@gmail.com")]),
           "AWS_SECRET_ACCESS_KEY" = as.character(api_keys$Key[(api_keys$Service %in% "AWS_SECRET_ACCESS_KEY") & (api_keys$Account %in% "robmarty3@gmail.com")]),
           "AWS_DEFAULT_REGION" = "us-east-2")

# Scrape data ------------------------------------------------------------------
utc_time <- Sys.time() %>% 
  with_tz(tzone = "UTC") %>%
  as.numeric() %>%
  substring(1,10)

r <- gt_make_raster_from_grid(grid_param_df  = grid_df,
                              webshot_delay  = 15,
                              google_key     = google_key,
                              return_list_of_rasters = T,
                              print_progress = T)

r_small <- lapply(r, function(x) {
  message("Agg")
  raster::aggregate(x, fact = 3, fun = modal, na.rm = TRUE)
})

r_m <- gt_mosaic(r_small)

OUT_DIR <- file.path(data_dir, "Google Traffic 2026", "RawData", 
                     paste0("gt_nairobi_utc", utc_time, ".tif"))

writeRaster(r_m, OUT_DIR)




#### TEST PARAMS
if(F){
  rr <- gt_make_raster(location = c(40.712778, -74.006111),
                      height = 10000,
                      width = 10000,
                      zoom = 17,
                      webshot_delay = 20,
                      google_key = google_key)
  
  ## Plot
  r_df <- rasterToPoints(rr, spatial = TRUE) %>% as.data.frame()
  names(r_df) <- c("value", "x", "y")
  
  ggplot() +
    geom_raster(data = r_df, 
                aes(x = x, y = y, 
                    fill = as.factor(value))) +
    labs(fill = "Traffic\nLevel") +
    scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
    coord_quickmap() + 
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color="white"))
  
}

quit(save="no")