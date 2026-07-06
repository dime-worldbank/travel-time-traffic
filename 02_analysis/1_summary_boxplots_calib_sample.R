# Summary Boxplots

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))
df <- df %>%
  dplyr::mutate(prop_trunk = ifelse(fclass == "trunk", 1, 0),
                prop_primary = ifelse(fclass == "primary", 1, 0),
                prop_secondary = ifelse(fclass == "secondary", 1, 0),
                prop_tertiary = ifelse(fclass == "tertiary", 1, 0),
                prop_residential = ifelse(fclass == "residential", 1, 0),
                prop_unclassified = ifelse(fclass == "unclassified", 1, 0)) %>%
  dplyr::mutate(                uid = uid %>% factor() %>% as.numeric()) %>%
  dplyr::rename(speed_kmh = speed_in_traffic_kmh)

beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

df <- df %>%
  group_by(uid) %>%
  dplyr::mutate(duration_in_traffic_s_minimum = quantile(duration_in_traffic_s, 0.05, na.rm = TRUE) %>% as.numeric()) %>%
  ungroup() %>%
  dplyr::mutate(duration_pc      = (duration_in_traffic_s - duration_in_traffic_s_minimum) / duration_in_traffic_s_minimum,
                delay_factor_od  = duration_pc + 1)

df <- df %>%
  mk_traffic_indicators(beta)

# df <- df %>%
#   dplyr::filter(modal_route %in% T)

# Figure -----------------------------------------------------------------------
# --- Step 1: Prepare the data for reordering and plotting ---
plot_data <- df %>%
  dplyr::mutate(duration_min = duration_in_traffic_s/60,
                distance_km = distance_m / 1000) %>%
  dplyr::select(uid,
                speed_kmh, duration_min, distance_km, delay_factor_od,
                tl_prop_2, tl_prop_3, tl_prop_4,
                delay_factor) %>%
  
  # Reorder the 'uid' factor levels
  dplyr::mutate(uid = as.factor(uid)) %>%
  dplyr::mutate(uid = forcats::fct_reorder(
    .f = uid,
    .x = speed_kmh,
    .fun = median,
    .desc = F
  ))

# --- Step 2: Create a transparent data frame to force the x-axis limit (0.5) ---
# Create one placeholder row for each uid and each traffic level variable.
placeholder_data <- plot_data %>%
  dplyr::select(uid) %>%
  distinct(uid) %>%
  tidyr::crossing(name = c("A. Prop. Route Traffic Level 2",
                           "B. Prop. Route Traffic Level 3",
                           "C. Prop. Route Traffic Level 4",
                           #"D. Delay Factor\n(Traffic level data)",
                           "D. Travel Time (min)",
                           "E. Travel Distance (km)",
                           "F. Traffic Speed (km/h)"#
                           
                           #,
                           #"H. Delay Factor\n(O-D data)"
  )) %>%
  dplyr::mutate(value = 0.5) %>% # Force the x-value to be 0.5
  dplyr::mutate(name = name %>%
                  factor(levels = c("A. Prop. Route Traffic Level 2",
                                    "B. Prop. Route Traffic Level 3",
                                    "C. Prop. Route Traffic Level 4",
                                    #"D. Delay Factor\n(Traffic level data)",
                                    "D. Travel Time (min)",
                                    "E. Travel Distance (km)",
                                    "F. Traffic Speed (km/h)")))
#"H. Delay Factor\n(O-D data)")))

# --- Step 3: Reshape the main data for plotting ---
long_data <- plot_data %>%
  pivot_longer(cols = -uid,
               names_to = "name",
               values_to = "value") %>%
  
  # Rename the facets and re-level 'name' factor (as in your original code)
  dplyr::mutate(name = case_when(
    name == "tl_prop_2" ~ "A. Prop. Route Traffic Level 2",
    name == "tl_prop_3" ~ "B. Prop. Route Traffic Level 3",
    name == "tl_prop_4" ~ "C. Prop. Route Traffic Level 4",
    name == "duration_min" ~ "D. Travel Time (min)", 
    name == "distance_km" ~ "E. Travel Distance (km)",
    name == "speed_kmh" ~ "F. Traffic Speed (km/h)",
    name == "delay_factor" ~ "A. Delay Factor\n(Traffic level data)",
    name == "delay_factor_od" ~ "B. Delay Factor\n(O-D data)",
  ),
  name = name %>%
    factor(levels = c("A. Prop. Route Traffic Level 2",
                      "B. Prop. Route Traffic Level 3",
                      "C. Prop. Route Traffic Level 4",
                      "D. Travel Time (min)",
                      "E. Travel Distance (km)",
                      "F. Traffic Speed (km/h)",
                      "A. Delay Factor\n(Traffic level data)",
                      "B. Delay Factor\n(O-D data)"
    )))

# long_data <- long_data %>%
#   dplyr::mutate(
#     value = dplyr::if_else(
#       name %in% c("D. Delay Factor\n(Traffic level data)",
#                   "H. Delay Factor\n(O-D data)") & value > 10,
#       10,
#       value
#     )
#   )

long_data <- long_data %>%
  mutate(value = if_else(
    name %in% c("D. Delay Factor\n(Traffic level data)",
                "H. Delay Factor\n(O-D data)"),
    pmin(value, 10),
    value
  ))

# --- Step 4: Create the plot by combining the layers ---
long_data %>%
  dplyr::filter(!(name %in% c("A. Delay Factor\n(Traffic level data)",
                              "B. Delay Factor\n(O-D data)"))) %>%
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
        axis.text.y = element_text(size = 7))

ggsave(filename = file.path(figures_dir, "summary_boxplots_calib_sample.png"),
       height = 10, width = 10)


