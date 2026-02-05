# Summary Boxplots

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  dplyr::filter(all_26_route %in% 1)

# Figure -----------------------------------------------------------------------
# --- Step 1: Prepare the data for reordering and plotting ---
plot_data <- df %>%
  dplyr::select(uid,
                gg_speed_in_traffic_kmh, gg_duration_in_traffic_min, gg_distance_km,
                gg_tl_prop_2, gg_tl_prop_3, gg_tl_prop_4) %>%
  
  # Reorder the 'uid' factor levels
  dplyr::mutate(uid = as.factor(uid)) %>%
  dplyr::mutate(uid = forcats::fct_reorder(
    .f = uid,
    .x = gg_speed_in_traffic_kmh,
    .fun = median,
    .desc = F
  ))

# --- Step 2: Create a transparent data frame to force the x-axis limit (0.5) ---
# Create one placeholder row for each uid and each traffic level variable.
placeholder_data <- plot_data %>%
  dplyr::select(uid) %>%
  distinct(uid) %>%
  tidyr::crossing(name = c("A. Traffic Speed (km/h)",
                           "B. Travel Duration (min)",
                           "C. Travel Distance (km)",
                           "D. Prop. Route Traffic Level 2",
                           "E. Prop. Route Traffic Level 3",
                           "F. Prop. Route Traffic Level 4")) %>%
  dplyr::mutate(value = 0.5) %>% # Force the x-value to be 0.5
  dplyr::mutate(name = name %>%
  factor(levels = c("A. Traffic Speed (km/h)",
                    "B. Travel Duration (min)",
                    "C. Travel Distance (km)",
                    "D. Prop. Route Traffic Level 2",
                    "E. Prop. Route Traffic Level 3",
                    "F. Prop. Route Traffic Level 4")))

# --- Step 3: Reshape the main data for plotting ---
long_data <- plot_data %>%
  pivot_longer(cols = -uid,
               names_to = "name",
               values_to = "value") %>%
  
  # Rename the facets and re-level 'name' factor (as in your original code)
  dplyr::mutate(name = case_when(
    name == "gg_speed_in_traffic_kmh" ~ "A. Traffic Speed (km/h)",
    name == "gg_duration_in_traffic_min" ~ "B. Travel Duration (min)", 
    name == "gg_distance_km" ~ "C. Travel Distance (km)",
    name == "gg_tl_prop_2" ~ "D. Prop. Route Traffic Level 2",
    name == "gg_tl_prop_3" ~ "E. Prop. Route Traffic Level 3",
    name == "gg_tl_prop_4" ~ "F. Prop. Route Traffic Level 4"
  ),
  name = name %>%
    factor(levels = c("A. Traffic Speed (km/h)",
                      "B. Travel Duration (min)",
                      "C. Travel Distance (km)",
                      "D. Prop. Route Traffic Level 2",
                      "E. Prop. Route Traffic Level 3",
                      "F. Prop. Route Traffic Level 4")))


# --- Step 4: Create the plot by combining the layers ---
long_data %>%
  ggplot(aes(y = uid, x = value)) +
  
  # Layer 1: The invisible points to set the scale range (data = placeholder_data)
  # This must be the first layer to set the scale range correctly.
  # Note: The 'uid' factor levels in placeholder_data must match the reordered levels in long_data.
  geom_point(data = placeholder_data,
             aes(y = uid, x = value),
             alpha = 0) + # Make the points completely transparent
  
  # Layer 2: The boxplots (data = long_data)
  geom_boxplot(outlier.size = 0.1,
               outlier.color = "firebrick2",
               fill = "gray",
               color = "black",
               size = 0.2) +
  
  # Faceting: scales = "free_x" uses the extent of the data points in each facet.
  facet_wrap(~name, scales = "free_x", nrow = 2) +
  
  # Labels and theming
  labs(x = NULL,
       y = "Route ID") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 8))

ggsave(filename = file.path(figures_dir, "summary_boxplots.png"),
       height = 6, width = 9)


#### Stats
q_df <- df %>%
  dplyr::select(uid,
                gg_speed_in_traffic_kmh,
                gg_tl_prop_2, gg_tl_prop_3, gg_tl_prop_4) %>%
  pivot_longer(-uid) %>%
  group_by(name, uid) %>%
  dplyr::summarise(q25 = quantile(value, 0.25, na.rm = T),
                   q50 = quantile(value, 0.5, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = name,
              values_from = c(q25, q50))

mean(q_df$q50_gg_tl_prop_2)
mean(q_df$q50_gg_tl_prop_3)
mean(q_df$q50_gg_tl_prop_4)

table(q_df$q25_gg_tl_prop_2 > 0)
table(q_df$q25_gg_tl_prop_3 == 0)
table(q_df$q25_gg_tl_prop_4 == 0)

mean(q_df$q25_gg_tl_prop_2 > 0)
mean(q_df$q25_gg_tl_prop_3 == 0)
mean(q_df$q25_gg_tl_prop_4 == 0)
