# Deviate Route

p_theme <- theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = 8),
                 axis.title.x = element_text(size = 8),
                 plot.title = element_text(face = "bold", size = 10),
                 axis.text = element_text(color = "black"))

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# Prep data --------------------------------------------------------------------
df <- df %>%
  dplyr::filter(!is.na(gg_distance_m),
                all_26_route %in% 1) %>%
  group_by(uid) %>%
  mutate(gg_distance_m_mode = Mode(gg_distance_m)) %>%
  ungroup() %>%
  mutate(gg_diff_mode = abs(gg_distance_m - gg_distance_m_mode) > 100)

# Difference -------------------------------------------------------------------
df <- df %>%
  group_by(uid) %>%
  dplyr::mutate(gg_distance_km_modal             = mean(gg_distance_km[gg_diff_mode == F]),
                gg_duration_in_traffic_min_modal = mean(gg_duration_in_traffic_min[gg_diff_mode == F]),
                gg_speed_in_traffic_kmh_modal    = mean(gg_speed_in_traffic_kmh[gg_diff_mode == F])) %>%
  ungroup() %>%
  mutate(gg_distance_km_diff             = gg_distance_km - gg_distance_km_modal,
         gg_duration_in_traffic_min_diff = gg_duration_in_traffic_min - gg_duration_in_traffic_min_modal,
         gg_speed_in_traffic_kmh_diff    = gg_speed_in_traffic_kmh - gg_speed_in_traffic_kmh_modal) %>%
  mutate(gg_distance_km_pc             = gg_distance_km_diff / gg_distance_km_modal,
         gg_duration_in_traffic_min_pc = gg_duration_in_traffic_min_diff / gg_duration_in_traffic_min_modal,
         gg_speed_in_traffic_kmh_pc    = gg_speed_in_traffic_kmh_diff / gg_speed_in_traffic_kmh_modal) %>%
  dplyr::filter(gg_diff_mode %in% T) #%>%
  #dplyr::filter(abs(gg_distance_km_diff) >= 0.1)

p <- df %>%
  dplyr::select(uid, gg_distance_km_diff, gg_duration_in_traffic_min_diff, gg_speed_in_traffic_kmh_diff) %>%
  pivot_longer(cols = -uid) %>%
  dplyr::mutate(name_clean = case_when(
    name == "gg_distance_km_diff" ~ "A. Difference in distance (km)",
    name == "gg_duration_in_traffic_min_diff" ~ "B. Differene in duration (minutes)",
    name == "gg_speed_in_traffic_kmh_diff" ~ "C. Difference in speed"
  )) %>%
  ggplot() +
  geom_histogram(aes(x = value),
                 color = "dodgerblue",
                 fill = "dodgerblue",
                 bins = 100) +
  geom_vline(xintercept = 0, color = "red") +
  facet_wrap(~name_clean,
             scales = "free") +
  labs(x = "Difference from modal route",
       y = "N observations") +
  theme_classic2() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

p_pc <- df %>%
  dplyr::select(uid, gg_distance_km_pc, gg_duration_in_traffic_min_pc, gg_speed_in_traffic_kmh_pc) %>%
  pivot_longer(cols = -uid) %>%
  dplyr::mutate(name_clean = case_when(
    name == "gg_distance_km_pc" ~ "D. Percent difference in distance",
    name == "gg_duration_in_traffic_min_pc" ~ "E. Percent difference in duration",
    name == "gg_speed_in_traffic_kmh_pc" ~ "F. Percent difference in speed"
  )) %>%
  ggplot() +
  geom_histogram(aes(x = value),
                 color = "dodgerblue",
                 fill = "dodgerblue",
                 bins = 100) +
  geom_vline(xintercept = 0, color = "red") +
  facet_wrap(~name_clean,
             scales = "free") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Percent difference from modal route",
       y = "N observations") +
  theme_classic2() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

p <- ggarrange(p, p_pc, ncol = 1)

# p_a <- annotate_figure(p, 
#                        top = text_grob("Difference in indicators between typical and other routes, across O-D pairs", 
#                                        color = "black", face = "bold", size = 14))

ggsave(p, 
       filename = file.path(figures_dir, "dev_modal_nonmodal_diff_overall.png"),
       height = 3.75, width = 10)
