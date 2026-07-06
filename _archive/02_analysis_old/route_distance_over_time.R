# Route distance over time

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

p <- route_df %>%
  ggplot(aes(x = datetime,
             y = gg_distance_km)) +
  geom_col(fill = "gray20") +
  facet_wrap(~uid,
             ncol = 3) +
  labs(x = NULL,
       y = "Kilometers",
       title = "Route distance (km) over time") +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"),
        strip.background = element_blank())

ggsave(p, 
       filename = file.path(figures_dir, "route_over_time.png"),
       height = 8, width = 9)
