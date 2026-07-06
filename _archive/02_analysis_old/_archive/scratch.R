
tt_df <- readRDS(file.path(tt_dir,
                           "google_tt_data.Rds"))

tt_df %>%
  filter(segment_id == 1) %>%
  mutate(date = datetime %>% date()) %>%
  filter(date %in% ymd("2022-06-01")) %>%
  ggplot() +
  geom_line(aes(x = datetime,
                y = speed_in_traffic_kmh)) +
  theme_classic2() +
  labs(x = "Date and time",
       y = "Speed (km/h)")
