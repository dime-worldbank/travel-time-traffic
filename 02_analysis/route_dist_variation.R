# Route Distance Variation

# Load data --------------------------------------------------------------------
tt_df <- readRDS(file.path(traffic_tt_dir, "route_data_long.Rds"))

tt_df %>%
  ggplot() +
  geom_boxplot(aes(y = segment_id,
                   x = distance_m,
                   color = source))
