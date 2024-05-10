# When change

route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

route_df %>%
  dplyr::filter(!(uid %in% 3:4),
                all_26_route %in% 1) %>%
  group_by(uid) %>%
  mutate(gg_distance_km_sd = sd(gg_distance_km)) %>%
  ungroup() %>%
  filter(gg_distance_km_sd > 0) %>%
  ggplot() +
  geom_point(aes(x = gg_distance_km,
                 y = gg_duration_min)) +
  facet_wrap(~uid,
             scales = "free")


route_df %>%
  dplyr::filter(!(uid %in% 3:4),
                all_26_route %in% 1) %>%
  group_by(uid) %>%
  mutate(gg_distance_km_sd = sd(gg_distance_km)) %>%
  ungroup() %>%
  filter(gg_distance_km_sd > 0) %>%
  ggplot() +
  geom_boxplot(aes(x = factor(gg_distance_km),
                 y = gg_duration_min)) +
  facet_wrap(~uid,
             scales = "free")

