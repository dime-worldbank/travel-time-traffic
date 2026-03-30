# Route distance over time

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

route_df$gg_distance_km[route_df$gg_distance_km >= 20] <- 20

p <- route_df %>%
  dplyr::filter(gg_distance_km <= 20) %>%
  ggplot(aes(x = datetime,
             y = gg_distance_km)) +
  geom_col(fill = "gray20",
           width = 60*60*7) +
  facet_wrap(~uid,
             ncol = 13) +
  labs(x = NULL,
       y = "Kilometers",
       title = "Route distance (km) over time") +
  scale_x_datetime(
    breaks = as.POSIXct(c("2022-07-28", "2023-04-01")),
    date_labels = "%Y-%m",
  ) +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"),
        strip.background = element_blank(),
        axis.text.x = element_text(size=5, angle = 0))

ggsave(p, 
       filename = file.path(figures_dir, "route_over_time.png"),
       height = 3, width = 9)
