# Summary stats

# Typical routes ---------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

route_df$datetime %>% min()
route_df$datetime %>% max()

# Crashes ----------------------------------------------------------------------
rtc_sf <- readRDS(file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds"))

# Min date starts eight weeks after, as require pre-trends
min_datetime <- min(route_df$datetime) #+ 60*60*24 * 7 * 8
max_datetime <- max(route_df$datetime)

rtc_sf <- rtc_sf[rtc_sf$datetime >= min_datetime,]
rtc_sf <- rtc_sf[rtc_sf$datetime <= max_datetime,]

min(rtc_sf$datetime)
max(rtc_sf$datetime)




