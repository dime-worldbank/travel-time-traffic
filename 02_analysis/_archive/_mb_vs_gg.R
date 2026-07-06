

df <- readRDS(file.path(analysis_data_dir, "mapbox_typical_route_10m_wide.Rds"))

df$gg_duration_in_traffic_s
df$mb_duration_in_traffic_s

df$gg_speed_in_traffic_kmh

df %>%
  dplyr::filter(datetime >= ymd("2023-08-06"),
                datetime <= ymd("2023-08-12")) %>%
  dplyr::select(datetime, uid, gg_speed_in_traffic_kmh, mb_speed_in_traffic_kmh) %>%
  pivot_longer(cols = -c(datetime, uid)) %>%
  ggplot() +
  geom_line(aes(x = datetime,
                y = value,
                color = name)) +
  facet_wrap(~uid,
             scales = "free_y") +
  labs(title = "Average speeds across routes for example week",
       subtitle = "gg = google; mb = mapbox")


