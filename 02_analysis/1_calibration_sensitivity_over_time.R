# Sensitivity over time

# Calibration sample ===========================================================

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
                date = datetime %>% date()) %>%
  dplyr::mutate(date_week = case_when(
    date_week == 1 ~ ymd("2026-06-11"),
    date_week == 2 ~ ymd("2026-07-08")
  )) %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh[hour %in% 1:4], prob = 0.99, na.rm = T) %>% as.numeric()) %>%
  ungroup()

route_calib_df <- route_calib_df %>%
  filter(hour >= 6, hour <= 21)

route_variation_calib_df <- route_calib_df %>%
  group_by(uid) %>%
  summarise(share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
            share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
            .groups = "drop")
routes_keep_calib <- route_variation_calib_df %>%
  filter(share_prop3_gt0 >= 0.05, share_prop4_gt0 >= 0.05) %>%
  pull(uid)
route_calib_df <- route_calib_df %>% filter(uid %in% routes_keep_calib)

# Regression - Pooled ----------------------------------------------------------
lm_pooled_calib_df <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid,
                      vcov = ~uid, data = route_calib_df) %>%
  confint() %>%
  as.data.frame() %>%
  clean_names() %>%
  rownames_to_column(var = "variable") %>%
  dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2) %>%
  dplyr::mutate(var_clean = case_when(
    variable == "tl_prop_2" ~ "Prop. Traffic Level 2",
    variable == "tl_prop_3" ~ "Prop. Traffic Level 3",
    variable == "tl_prop_4" ~ "Prop. Traffic Level 4",
  ))

lm_pooled_calib_df

# Regressions - Weekly ---------------------------------------------------------
# lm_pooled_calib_df <- lm_pooled_calib_df %>%
#   group_by(date_week) %>%
#   dplyr::mutate(date_week_ndays = date %>% unique() %>% length(),
#                 n_routes = uid %>% unique() %>% length()) %>%
#   ungroup() %>%
#   dplyr::filter(date_week_ndays == 7)

lm_week_calib_df <- map_df(unique(route_calib_df$date_week), function(week_i){
  
  feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid,
        vcov = ~uid, data = route_calib_df[route_calib_df$date_week %in% week_i,]) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                  week = week_i)
  
})

lm_week_calib_df <- lm_week_calib_df %>%
  dplyr::mutate(var_clean = case_when(
    variable == "tl_prop_2" ~ "Prop. Traffic Level 2",
    variable == "tl_prop_3" ~ "Prop. Traffic Level 3",
    variable == "tl_prop_4" ~ "Prop. Traffic Level 4",
  )) 

p_calib <- lm_week_calib_df %>%
  ggplot(aes(x = week,
             y = b,
             ymin = x2_5_percent,
             ymax = x97_5_percent)) +
  geom_rect(data = lm_pooled_calib_df,
            aes(ymin = x2_5_percent, ymax = x97_5_percent,
                xmin = as.Date(-Inf), xmax = as.Date(Inf)),
            fill = "gray50", alpha = 0.2, inherit.aes = FALSE) +
  geom_hline(data = lm_pooled_calib_df,
             aes(yintercept = b, linetype = "Pooled estimate"),
             color = "gray30") +
  geom_point() +
  geom_linerange() +
  facet_wrap(~var_clean) +
  scale_linetype_manual(values = c("Pooled estimate" = "dashed"), name = NULL) +
  # scale_x_date(
  #   date_breaks = "2 month",
  #   date_labels = "%b"   # Jan, Feb, Mar, ...
  # ) +
  xlim(ymd("2026-06-01"), ymd("2026-07-18")) +
  #ylim(-1, 8) +
  labs(x = "Week",
       y = "Coef (+/- 95% CI)",
       title = "A. Calibration sample"
  ) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

p_calib

# 26 Routes ====================================================================

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))

route_df <- route_df %>%
  dplyr::mutate(speed_kmh = speed_in_traffic_kmh,
                tt_hour_per_km = (duration_in_traffic_s/60/60) / (distance_m/1000),
                prop_ge_2 = tl_prop_2 + tl_prop_3 + tl_prop_4,
                prop_ge_3 = tl_prop_3 + tl_prop_4,
                prop_ge_4 = tl_prop_4) %>%
  dplyr::mutate(speed_kmh_ln = log(speed_kmh),
                tt_hour_per_km_ln = log(tt_hour_per_km)) %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(),
                date = datetime %>% date()) %>%
  dplyr::mutate(date_week = floor_date(date, unit = "weeks")) %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh[hour %in% 1:4], prob = 0.99, na.rm = T) %>% as.numeric()) %>%
  ungroup()

route_df <- route_df %>%
  filter(hour >= 6, hour <= 21)

route_variation_df <- route_df %>%
  group_by(uid) %>%
  summarise(share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
            share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
            .groups = "drop")
routes_keep <- route_variation_df %>%
  filter(share_prop3_gt0 >= 0.05, share_prop4_gt0 >= 0.05) %>%
  pull(uid)
#route_df <- route_df %>% dplyr::filter(uid %in% routes_keep)

# Regression - Pooled ----------------------------------------------------------
lm_pooled_df <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid,
                      vcov = ~uid, data = route_df) %>%
  confint() %>%
  as.data.frame() %>%
  clean_names() %>%
  rownames_to_column(var = "variable") %>%
  dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2) %>%
  dplyr::mutate(var_clean = case_when(
    variable == "tl_prop_2" ~ "Prop. Traffic Level 2",
    variable == "tl_prop_3" ~ "Prop. Traffic Level 3",
    variable == "tl_prop_4" ~ "Prop. Traffic Level 4",
  ))

lm_pooled_df

# Regressions - Weekly ---------------------------------------------------------
route_df <- route_df %>%
  group_by(date_week) %>%
  dplyr::mutate(date_week_ndays = date %>% unique() %>% length(),
                n_routes = uid %>% unique() %>% length()) %>%
  ungroup() %>%
  dplyr::filter(date_week_ndays == 7)

route_13_df <- route_df %>% dplyr::filter(n_routes == 13)
route_26_df <- route_df %>% dplyr::filter(n_routes == 26)

lm_week_13_df <- map_df(unique(route_13_df$date_week), function(week_i){
  
  feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid,
        vcov = ~uid, data = route_13_df[route_13_df$date_week %in% week_i,]) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                  week = week_i)
  
})

lm_week_26_df <- map_df(unique(route_26_df$date_week), function(week_i){
  
  feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid,
        vcov = ~uid, data = route_26_df[route_26_df$date_week %in% week_i,]) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                  week = week_i)
  
})

lm_week_13_df <- lm_week_13_df %>%
  dplyr::mutate(var_clean = case_when(
    variable == "tl_prop_2" ~ "Prop. Traffic Level 2",
    variable == "tl_prop_3" ~ "Prop. Traffic Level 3",
    variable == "tl_prop_4" ~ "Prop. Traffic Level 4",
  )) %>%
  dplyr::mutate(n_routes = "13 Routes")

lm_week_26_df <- lm_week_26_df %>%
  dplyr::mutate(var_clean = case_when(
    variable == "tl_prop_2" ~ "Prop. Traffic Level 2",
    variable == "tl_prop_3" ~ "Prop. Traffic Level 3",
    variable == "tl_prop_4" ~ "Prop. Traffic Level 4",
  )) %>%
  dplyr::mutate(n_routes = "26 Routes")

lm_week_df <- bind_rows(lm_week_13_df,
                        lm_week_26_df)

p_26 <- lm_week_df %>%
  ggplot(aes(x = week,
             y = b,
             ymin = x2_5_percent,
             ymax = x97_5_percent,
             color = n_routes)) +
  geom_rect(data = lm_pooled_df,
            aes(ymin = x2_5_percent, ymax = x97_5_percent,
                xmin = as.Date(-Inf), xmax = as.Date(Inf)),
            fill = "gray50", alpha = 0.2, inherit.aes = FALSE) +
  geom_hline(data = lm_pooled_df,
             aes(yintercept = b, linetype = "Pooled estimate"),
             color = "gray30") +
  geom_point() +
  geom_linerange() +
  facet_wrap(~var_clean) +
  scale_color_manual(values = c("orange",
                                "dodgerblue")) +
  scale_linetype_manual(values = c("Pooled estimate" = "dashed"), name = NULL) +
  scale_x_date(
    date_breaks = "2 month",
    date_labels = "%b"   # Jan, Feb, Mar, ...
  ) +
  labs(x = "Week",
       y = "Coef (+/- 95% CI)",
       color = "N Routes",
       title = "B. Long panel of 26 routes"
  ) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# Export =======================================================================

p <- ggarrange(p_calib, p_26, ncol = 1)

ggsave(p, filename = file.path(figures_dir, "coef_by_time.png"),
       height = 8, width = 9)




