# Scatter plots between OD and traffic level indicators

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  mutate(uid = paste0("Route: ", uid) %>%
           factor(levels = paste0("Route: ", 1:26))) %>%
  dplyr::filter(all_26_route %in% T)

df %>%
  ggplot(aes(x = gg_tl_prop_234,
             y = gg_speed_in_traffic_kmh)) +
  geom_point(size = 0.5,
             alpha = 0.5) +
  geom_smooth(method = "lm",
              se = F,
              color = "orange") +
  stat_cor(aes(label = ..r.label..),
           method = "pearson",
           color = "red",
           size = 3,
           label.x = 0.3) +
  labs(x = "Proportion traffic level 2-4",
       y = "Average\nspeed\n(km/h)",
       title = "Average speed vs traffic levels") +
  facet_wrap(~uid, scales = "free_y") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

ggsave(filename = file.path(figures_dir, "scatter_speed_levels_time.png"),
       height = 6, width = 10)

df %>%
  ggplot(aes(x = gg_tl_prop_234,
             y = gg_duration_in_traffic_min)) +
  geom_point(size = 0.5,
             alpha = 0.5) +
  geom_smooth(method = "lm",
              se = F,
              color = "orange") +
  stat_cor(aes(label = ..r.label..),
           method = "pearson",
           color = "red",
           size = 3,
           label.x = 0.35) +
  labs(x = "Proportion traffic level 2-4",
       y = "Travel\nduration\n(min)",
       title = "Travel duration vs traffic levels") +
  facet_wrap(~uid, scales = "free_y") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

ggsave(filename = file.path(figures_dir, "scatter_tt_levels_time.png"),
       height = 6, width = 10)
