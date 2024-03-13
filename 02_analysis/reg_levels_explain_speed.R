# Google Mapbox Scatterplot

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# OLS --------------------------------------------------------------------------
df <- df %>%
  dplyr::mutate(gg_speed_in_traffic_kmh = log(gg_speed_in_traffic_kmh),
                gg_duration_in_traffic_min = log(gg_duration_in_traffic_min),
                gg_tl_prop_234 = log(gg_tl_prop_234),
                gg_tl_prop_34 = log(gg_tl_prop_34),
                gg_tl_prop_4 = log(gg_tl_prop_4))

lm_speed_1 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_prop_234 | uid, data = df)
lm_speed_2 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_prop_34 | uid, data = df)
lm_speed_3 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_prop_4 | uid, data = df)

lm_dur_1 <- feols(gg_duration_in_traffic_min ~ gg_tl_prop_234 | uid, data = df)
lm_dur_2 <- feols(gg_duration_in_traffic_min ~ gg_tl_prop_34 | uid, data = df)
lm_dur_3 <- feols(gg_duration_in_traffic_min ~ gg_tl_prop_4 | uid, data = df)

modelsummary_tab(list("Speed (km/h), log" = lm_speed_1,
                      "Speed (km/h), log" = lm_speed_2,
                      "Speed (km/h), log" = lm_speed_3,
                      "Duration (min), log" = lm_dur_1,
                      "Duration (min), log" = lm_dur_2,
                      "Duration (min), log" = lm_dur_3),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("gg_tl_prop_234" = "Prop route traffic level 2 - 4, log",
                              "gg_tl_prop_34" = "Prop route traffic level 3 - 4, log",
                              "gg_tl_prop_4" = "Prop route traffic level 4, log"),
                 gof_map = c("nobs", "adj.r.squared", "r2.within.adjusted"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6,
                                    'Route FE', "Y", "Y", "Y", "Y", "Y", "Y"),
                 output = file.path(tables_dir,
                                    "ols_gg_speed_dur_traffic.tex"))

