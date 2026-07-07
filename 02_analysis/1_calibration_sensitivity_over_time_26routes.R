# Sensitivity over time

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
                date = datetime %>% date())

route_df <- route_df %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh[hour %in% 1:4], prob = 0.99, na.rm = T) %>% as.numeric()) %>%
  ungroup()

route_df <- route_df %>%
  filter(hour >= 6, hour <= 21)



# route_df <- route_df %>%
#   dplyr::mutate(rm_date = (date >= ymd("2022-12-24")) & (date <= ymd("2023-01-07"))) %>%
#   dplyr::filter(rm_date %in% F)

route_df <- route_df %>%
  dplyr::mutate(date_week = floor_date(date, unit = "weeks"))

# Regression - Pooled ----------------------------------------------------------
lm_pooled_df <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid + date_week,
                      vcov = ~uid + date_week, data = route_df) %>%
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

lm_week_df %>%
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
             aes(yintercept = b),
             linetype = "dashed", color = "gray30") +
  geom_point() +
  geom_linerange() +
  facet_wrap(~var_clean) +
  scale_color_manual(values = c("orange",
                                "dodgerblue")) +
  scale_x_date(
    date_breaks = "2 month",
    date_labels = "%b"   # Jan, Feb, Mar, ...
  ) +
  labs(x = "Week",
       y = "Coef (+/- 95% CI)",
       color = "N Routes",
  ) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(filename = file.path(figures_dir, "coef_weekly_26routes.png"),
       height = 3.5, width= 11.5)








