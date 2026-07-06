set.seed(42)
# Load both datasets fresh ---------------------------------------------------------
route_df    <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))
route_df_v2 <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))
# Compute 99th percentile speed per route (uid) -------------------------------------
route_p99_speed_byclass <- route_df %>%
  dplyr::filter(hour %in% 1:4) %>%
  group_by(uid) %>%
  summarise(
    fclass = first(fclass),
    p99_speed = quantile(speed_in_traffic_kmh, probs = 0.99, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(uid = as.character(uid))

route_p99_speed_v2 <- route_df_v2 %>%
  dplyr::filter(hour %in% 1:4) %>%
  group_by(uid) %>%
  summarise(
    fclass = "Long panel of\n26 routes",
    p99_speed = quantile(speed_in_traffic_kmh, probs = 0.99, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(uid = as.character(uid))

combined_p99 <- bind_rows(route_p99_speed_byclass, route_p99_speed_v2)

combined_p99 <- combined_p99 %>%
  mutate(fclass_label = case_when(
    fclass == "Long panel of\n26 routes" ~ fclass,
    TRUE ~ str_to_title(fclass)
  ))

fclass_levels <- c("Unclassified", "Residential", "Tertiary", "Secondary",
                   "Primary", "Trunk", "Long panel of\n26 routes")
combined_p99 <- combined_p99 %>%
  mutate(fclass_label = factor(fclass_label, levels = fclass_levels))
print(combined_p99, n = 100)

# Check both ends of the range to calibrate bracket placement
min_speed <- min(combined_p99$p99_speed, na.rm = TRUE)
max_speed <- max(combined_p99$p99_speed, na.rm = TRUE)
min_speed
max_speed

# Boxplot: speed on x-axis (via coord_flip), categories on y-axis --------------------
# Bracket sits to the LEFT of the boxes but still >= 0, in the gap between the axis 
# origin and the lowest data point
ggplot(combined_p99, aes(x = fclass_label, y = p99_speed, fill = fclass_label)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6) +
  annotate("segment", x = 0.8, xend = 6.2, y = 8, yend = 8,
           linewidth = 0.6, color = "gray40") +
  annotate("segment", x = 0.8, xend = 0.8, y = 8, yend = 12,
           linewidth = 0.6, color = "gray40") +
  annotate("segment", x = 6.2, xend = 6.2, y = 8, yend = 12,
           linewidth = 0.6, color = "gray40") +
  annotate("text", x = 3.5, y = 7, label = "Routes\nused for\ncalibration",
           size = 3.5, fontface = "italic", hjust = 1) +
  scale_y_continuous(limits = c(0, NA)) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "99th percentile speed (km/h)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(10, 20, 10, 40),
        axis.text = element_text(color = "black", size = 12))
ggsave(filename = file.path(figures_dir, "p99_speed_boxplot_byclass_and_v2.png"),
       height = 4, width = 8)
#       title = "Distribution of free-flow speeds across routes"

#### Stats
combined_p99 %>%
  group_by(fclass_label) %>%
  dplyr::summarise(p99_speed = mean(p99_speed)) %>%
  ungroup()


osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
osm_sf %>%
  st_drop_geometry() %>%
  dplyr::filter(fclass %in% "trunk") %>%
  pull(name)
