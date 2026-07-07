# Sensitivity over time

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))

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
  filter(hour >= 6, hour <= 21) %>%
  dplyr::mutate(uid = uid %>% factor() %>% as.numeric() %>% as.character())

# Threshold at 0.05 (computed on daytime hours only, matching 1_calibration_regression.R)
route_variation_df <- route_df %>%
  group_by(uid) %>%
  summarise(share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
            share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
            .groups = "drop")

# Regressions ------------------------------------------------------------------
lm_route_df <- map_df(unique(route_df$uid), function(uid_i){
  
  feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4, 
        vcov = "hetero", data = route_df[route_df$uid %in% uid_i,]) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                  uid = uid_i,
                  fclass = route_df$fclass[route_df$uid %in% uid_i][1],
                  speed_kmh_uid_max = route_df$speed_kmh_uid_max[route_df$uid %in% uid_i][1]) %>%
    dplyr::filter(!(variable %in% c("(Intercept)")))
  
})

lm_route_df <- lm_route_df %>%
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

lm_route_sub_df <- lm_route_df %>%
  dplyr::filter(b_max <= 10)

p <- lm_route_sub_df %>%
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
       color = "Free-Flow\nSpeed (km/h)") +
  lims(x = c(-6, 10)) +
  scale_color_viridis_c(
    option = "viridis",   # or "magma", "plasma", "inferno", "cividis", "turbo"
    end = 0.95
  ) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(face = "bold"))

ggsave(p, filename = file.path(figures_dir, "coef_by_route.png"),
       height = 4, width = 7)
