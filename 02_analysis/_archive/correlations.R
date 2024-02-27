
tt_df <- readRDS(file.path(analysis_data_dir, "route_data_wide.Rds"))

tt_df$mapbox_speed_in_traffic_kmh

tt_df %>%
  ggplot(aes(x = google_speed_in_traffic_kmh, 
             y = mapbox_speed_in_traffic_kmh)) +
  geom_point()

