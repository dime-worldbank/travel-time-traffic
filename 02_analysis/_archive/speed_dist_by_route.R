# Speed Distribution

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  dplyr::filter(!is.na(gg_speed_in_traffic_kmh)) %>%
  mutate(uid = factor(uid)) %>%
  
  group_by(uid) %>%
  mutate(gg_speed_in_traffic_kmh_med = median(gg_speed_in_traffic_kmh),
         gg_distance_km_med = median(gg_distance_km),
         gg_duration_in_traffic_min_med = median(gg_duration_in_traffic_min)) %>%
  ungroup()

df %>%
  ggplot(aes(x = gg_speed_in_traffic_kmh,
             y = reorder(uid, gg_speed_in_traffic_kmh_med) )) +
  geom_boxplot() +
  theme_classic2() +
  labs(x = "Average Speed (km/h)",
       y = "O-D Pair")

df %>%
  ggplot(aes(x = gg_duration_in_traffic_min,
             y = reorder(uid, gg_duration_in_traffic_min_med) )) +
  geom_boxplot() +
  theme_classic2() +
  labs(x = "Duration (min)",
       y = "O-D Pair")
