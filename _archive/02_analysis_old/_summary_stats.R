# Summary stats

# Typical routes ---------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

route_df$datetime %>% min()
route_df$datetime %>% max()

route_26_df <- route_df %>%
  dplyr::filter(all_26_route %in% 1)

route_26_df$datetime %>% min()
route_26_df$datetime %>% max()

# Crashes ----------------------------------------------------------------------
rtc_sf <- readRDS(file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds"))

# Min date starts eight weeks after, as require pre-trends
min_datetime <- min(route_df$datetime) #+ 60*60*24 * 7 * 8
max_datetime <- max(route_df$datetime)

rtc_sf <- rtc_sf[rtc_sf$datetime >= min_datetime,]
rtc_sf <- rtc_sf[rtc_sf$datetime <= max_datetime,]

min(rtc_sf$datetime)
max(rtc_sf$datetime)

# Route length -----------------------------------------------------------------
mode_sf <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))

summary(mode_sf$distance_m/1000)

# Pixel size -------------------------------------------------------------------
r <- traffic_gg_raw_dir %>%
  list.files(pattern = ".tif",
             full.names = T) %>%
  head(1) %>%
  raster()
res(r)[1] * 111.12 * 1000

(14853/2.38)/1000
(24700/2.38)/1000

# Number of wards --------------------------------------------------------------
nbo3_df   <- readRDS(file.path(analysis_data_dir, "gadm3_wide.Rds"))

nbo3_df$NAME_3 %>% unique() %>% length()
