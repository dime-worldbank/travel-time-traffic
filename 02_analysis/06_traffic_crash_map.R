# Traffic Crash Map

# Load data --------------------------------------------------------------------
gadm_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_3_pk.rds"))

g_files <- file.path(tt_google_raw_dir) %>%
  list.files(pattern = "*.tiff")

g_datetime <- g_files %>%
  str_replace_all("gt_nairobi_utc|.tiff", "") %>%
  as.numeric() %>%
  as_datetime() %>%
  with_tz(tzone = "Africa/Nairobi") %>%
  as.data.frame() %>%
  dplyr::rename(datetime = ".") %>%
  mutate(dow = wday(datetime, label = T),
         date = datetime %>% as.Date(),
         hour = datetime %>% hour())
g_datetime$file_name <- g_files

g_datetime <- g_datetime %>%
  dplyr::filter(hour %in% 19:20,
                !(dow %in% c("Sun", "Sat")),
                date > ymd("2022-01-01"))

r <- raster(file.path(traffic_dir, "google_individual_rasters_nairobi", g_datetime$file_name))
r[] <- r[] + 1
r[][r[] %in% 1] <- NA

clean_mean <- function(x, ...){
  x <- x + 1
  x[x == 1] <- NA
  return(x)
}

rs <- file.path(tt_google_raw_dir, g_datetime$file_name) %>%
  head(2) %>%
  stack()
rs_a <- aggregate(rs, fact = 4, fun = max, na.rm = T)
rs_mean <- calc(rs_a, fun = clean_mean)

# Map --------------------------------------------------------------------------
r_df <- rasterToPoints(rs_mean, spatial = TRUE) %>% as.data.frame()
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