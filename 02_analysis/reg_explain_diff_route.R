# Deviate Route

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# Prep data --------------------------------------------------------------------
df <- df %>%
  group_by(uid) %>%
  mutate(gg_distance_m_mode = Mode(gg_distance_m, na.rm = T)) %>%
  ungroup() %>%
  mutate(gg_diff_mode = abs(gg_distance_m - gg_distance_m_mode) > 100) %>%
  mutate(diff_from_mode_km = (gg_distance_m - gg_distance_m_mode) / 1000,
         hour = datetime %>% hour(),
         dow = datetime %>% wday)

# Regressions: Binary ----------------------------------------------------------
lm1 <- feols(gg_distance_km             ~ gg_diff_mode | uid + hour + dow, data = df) 
lm2 <- feols(gg_duration_in_traffic_min ~ gg_diff_mode | uid + hour + dow, data = df) 
lm3 <- feols(gg_speed_in_traffic_kmh    ~ gg_diff_mode | uid + hour + dow, data = df) 
lm4 <- feols(gg_duration_in_traffic_min ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 
lm5 <- feols(gg_speed_in_traffic_kmh    ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 

modelsummary_tab(list("Distance (km)" = lm1,
                      "Duration (min)" = lm2,
                      "Speed (km/h)" = lm3,
                      "Duration (min)" = lm4,
                      "Speed (km/h)" = lm5),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("gg_diff_modeTRUE" = "Route Different: Binary",
                              "diff_from_mode_km" = "Route Difference: Kilometers"),
                 gof_map = c("nobs", "adj.r.squared"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5,
                                    'Route FE', "Y", "Y", "Y", "Y", "Y",
                                    'Hour FE', "Y", "Y", "Y", "Y", "Y",
                                    'Day of Week FE', "Y", "Y", "Y", "Y", "Y"),
                 output = file.path(tables_dir,
                                    "reg_route_dev.tex"))
