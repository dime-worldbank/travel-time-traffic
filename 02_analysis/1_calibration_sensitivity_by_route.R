# Sensitivity over time

# Calibration data =============================================================

# Load data --------------------------------------------------------------------
route_calib_df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))

route_calib_df <- route_calib_df %>%
  dplyr::mutate(speed_kmh = speed_in_traffic_kmh,
                tt_hour_per_km = (duration_in_traffic_s/60/60) / (distance_m/1000),
                prop_ge_2 = tl_prop_2 + tl_prop_3 + tl_prop_4,
                prop_ge_3 = tl_prop_3 + tl_prop_4,
                prop_ge_4 = tl_prop_4) %>%
  dplyr::mutate(speed_kmh_ln = log(speed_kmh),
                tt_hour_per_km_ln = log(tt_hour_per_km)) %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(),
                date = datetime %>% date())

route_calib_df <- route_calib_df %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh[hour %in% 1:4], prob = 0.99, na.rm = T) %>% as.numeric()) %>%
  ungroup()

route_calib_df <- route_calib_df %>%
  filter(hour >= 6, hour <= 21) %>%
  dplyr::mutate(uid = uid %>% factor() %>% as.numeric() %>% as.character())

# Regressions ------------------------------------------------------------------
lm_route_calib_df <- map_df(unique(route_calib_df$uid), function(uid_i){
  
  feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | date_week, 
        vcov = "hetero", data = route_calib_df[route_calib_df$uid %in% uid_i,]) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                  uid = uid_i,
                  fclass = route_calib_df$fclass[route_calib_df$uid %in% uid_i][1],
                  speed_kmh_uid_max = route_calib_df$speed_kmh_uid_max[route_calib_df$uid %in% uid_i][1]) %>%
    dplyr::filter(!(variable %in% c("(Intercept)")))
  
})

lm_route_calib_df <- lm_route_calib_df %>%
  dplyr::mutate(fclass = fclass %>% as.character() %>% tools::toTitleCase() %>%
                  factor(levels = c("Trunk", "Primary", "Secondary",
                                    "Tertiary", "Unclassified", "Residential"))) %>%
  group_by(uid) %>%
  dplyr::mutate(b_max = max(abs(b))) %>%
  ungroup() %>%
  dplyr::mutate(variable = case_when(
    variable == "tl_prop_2" ~ "Prop. Traffic Level 2",
    variable == "tl_prop_3" ~ "Prop. Traffic Level 3",
    variable == "tl_prop_4" ~ "Prop. Traffic Level 4"
  ))

lm_route_sub_calib_df <- lm_route_calib_df %>%
  dplyr::filter(b_max <= 10)

p_calib <- lm_route_sub_calib_df %>%
  dplyr::mutate(uid = forcats::fct_reorder(uid, speed_kmh_uid_max)) %>%
  ggplot(aes(xmin = x2_5_percent,
             xmax = x97_5_percent,
             x = b,
             y = uid,
             color = speed_kmh_uid_max)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_linerange() +
  geom_point() +
  facet_wrap(~variable, ncol = 3) +
  labs(x = "Coef (+/- 95% CI)",
       y = "Route ID",
       color = "Free-Flow\nSpeed (km/h)",
       title = "A. Calibration sample") +
  lims(x = c(-6, 10)) +
  scale_color_gradientn(
    colours = c("#1B9E77", "#2C7BB6", "#D7191C"),
    limits = c(10, 85)
  ) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(face = "bold"))

# 26 route data ================================================================

# Load data --------------------------------------------------------------------
route_26_df <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))

route_26_df <- route_26_df %>%
  dplyr::mutate(speed_kmh = speed_in_traffic_kmh,
                tt_hour_per_km = (duration_in_traffic_s/60/60) / (distance_m/1000),
                prop_ge_2 = tl_prop_2 + tl_prop_3 + tl_prop_4,
                prop_ge_3 = tl_prop_3 + tl_prop_4,
                prop_ge_4 = tl_prop_4) %>%
  dplyr::mutate(speed_kmh_ln = log(speed_kmh),
                tt_hour_per_km_ln = log(tt_hour_per_km)) %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(),
                date = datetime %>% date())

route_26_df <- route_26_df %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh[hour %in% 1:4], prob = 0.99, na.rm = T) %>% as.numeric()) %>%
  ungroup()

route_26_df <- route_26_df %>%
  filter(hour >= 6, hour <= 21) %>%
  dplyr::mutate(uid = uid %>% factor() %>% as.numeric() %>% as.character(),
                date_week = date %>% floor_date(unit = "weeks"))

# Regressions ------------------------------------------------------------------
lm_route_26_df <- map_df(unique(route_26_df$uid), function(uid_i){
  
  feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | date_week, 
        vcov = "hetero", data = route_26_df[route_26_df$uid %in% uid_i,]) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                  uid = uid_i,
                  fclass = route_26_df$fclass[route_26_df$uid %in% uid_i][1],
                  speed_kmh_uid_max = route_26_df$speed_kmh_uid_max[route_26_df$uid %in% uid_i][1]) %>%
    dplyr::filter(!(variable %in% c("(Intercept)")))
  
})

lm_route_26_df <- lm_route_26_df %>%
  # dplyr::mutate(fclass = fclass %>% as.character() %>% tools::toTitleCase() %>%
  #                 factor(levels = c("Trunk", "Primary", "Secondary",
  #                                   "Tertiary", "Unclassified", "Residential"))) %>%
  group_by(uid) %>%
  dplyr::mutate(b_max = max(abs(b))) %>%
  ungroup() %>%
  dplyr::mutate(variable = case_when(
    variable == "tl_prop_2" ~ "Prop. Traffic Level 2",
    variable == "tl_prop_3" ~ "Prop. Traffic Level 3",
    variable == "tl_prop_4" ~ "Prop. Traffic Level 4"
  ))

lm_route_sub_26_df <- lm_route_26_df %>%
  dplyr::filter(b_max <= 10)

p_26 <- lm_route_sub_26_df %>%
  dplyr::mutate(uid = forcats::fct_reorder(uid, speed_kmh_uid_max)) %>%
  ggplot(aes(xmin = x2_5_percent,
             xmax = x97_5_percent,
             x = b,
             y = uid,
             color = speed_kmh_uid_max)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_linerange() +
  geom_point() +
  facet_wrap(~variable, ncol = 3) +
  labs(x = "Coef (+/- 95% CI)",
       y = "Route ID",
       color = "Free-Flow\nSpeed (km/h)",
       title = "B. Long panel of 26 routes") +
  lims(x = c(-6, 10)) +
  scale_color_gradientn(
    colours = c("#1B9E77", "#2C7BB6", "#D7191C"),
    limits = c(10, 85)
  ) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(face = "bold"))

# Export =======================================================================
p <- ggarrange(p_calib, p_26, ncol = 1, common.legend = T, legend = "right")

ggsave(p, filename = file.path(figures_dir, "coef_by_route.png"),
       height = 8, width = 7)

