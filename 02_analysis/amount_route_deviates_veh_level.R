# Deviate Route

p_theme <- theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = 8),
                 axis.title.x = element_text(size = 8),
                 plot.title = element_text(face = "bold", size = 10),
                 axis.text = element_text(color = "black"))

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# Prep data --------------------------------------------------------------------
df <- df %>%
  dplyr::filter(!is.na(gg_distance_m)) %>%
  group_by(uid) %>%
  mutate(gg_distance_m_mode = Mode(gg_distance_m)) %>%
  ungroup() %>%
  mutate(gg_diff_mode = abs(gg_distance_m - gg_distance_m_mode) > 100)

# Difference -------------------------------------------------------------------
p_distance <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_distance_km = mean(gg_distance_km)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_distance_km) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = true - false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  labs(title = "A. Difference in average distance (km)",
       x = "Distance modal route - Avg distance non-modal route",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

p_duration <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_duration_in_traffic_min = mean(gg_duration_in_traffic_min)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_duration_in_traffic_min) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = true - false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  labs(title = "B. Difference in average duration (minutes)",
       x = "Avg duration modal route - Avg duration non-modal route",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

p_speed <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_speed_in_traffic_kmh = mean(gg_speed_in_traffic_kmh)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_speed_in_traffic_kmh) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = true - false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  labs(title = "C. Difference in average speed (km/h)",
       x = "Avg speed modal route - Avg speed non-modal route",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

# Percent difference -----------------------------------------------------------
p_distance_per <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_distance_km = mean(gg_distance_km)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_distance_km) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = (true - false)/false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "D. Percent difference in average distance",
       x = "% difference in average modal and non-modal distance",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

p_duration_per <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_duration_in_traffic_min = mean(gg_duration_in_traffic_min)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_duration_in_traffic_min) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = (true - false)/false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "E. Percent difference in average duration",
       x = "% difference in average modal and non-modal duration",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

p_speed_per <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_speed_in_traffic_kmh = mean(gg_speed_in_traffic_kmh)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_speed_in_traffic_kmh) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = (true - false)/false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "F. Percent difference in average speed",
       x = "% difference in average modal and non-modal speed",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

p     <- ggarrange(p_distance,     p_duration,     p_speed, nrow = 1)
p_per <- ggarrange(p_distance_per, p_duration_per, p_speed_per, nrow = 1)

p <- ggarrange(p, p_per, ncol = 1)

p_a <- annotate_figure(p, 
                       top = text_grob("Difference in indicators between typical and other routes, across O-D pairs", 
                                       color = "black", face = "bold", size = 14))

ggsave(p_a, 
       filename = file.path(figures_dir, "dev_modal_nonmodal_diff.png"),
       height = 4, width = 10)



