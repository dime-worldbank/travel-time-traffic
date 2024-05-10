# Deviate Route

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# Prep data --------------------------------------------------------------------
df <- df %>%
  dplyr::filter(all_26_route %in% 1) %>%
  dplyr::filter(!(uid %in% 3:4)) %>%
  
  group_by(uid) %>%
  mutate(gg_distance_m_mode = Mode(gg_distance_m, na.rm = T)) %>%
  ungroup() %>%
  mutate(gg_diff_mode = abs(gg_distance_m - gg_distance_m_mode) > 100) %>%
  mutate(diff_from_mode_km = (gg_distance_m - gg_distance_m_mode) / 1000,
         hour = datetime %>% hour(),
         dow = datetime %>% wday)

# Regressions: OD --------------------------------------------------------------
# lm1 <- feols(gg_distance_km             ~ gg_diff_mode | uid + hour + dow, data = df) 
# lm2 <- feols(gg_duration_in_traffic_min ~ gg_diff_mode | uid + hour + dow, data = df) 
# lm3 <- feols(gg_speed_in_traffic_kmh    ~ gg_diff_mode | uid + hour + dow, data = df) 
# 
# lm4 <- feols(gg_duration_in_traffic_min ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 
# lm5 <- feols(gg_speed_in_traffic_kmh    ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 

lm1 <- feols(gg_distance_km             ~ gg_diff_mode | uid, data = df) 
lm2 <- feols(gg_duration_in_traffic_min ~ gg_diff_mode | uid, data = df) 
lm3 <- feols(gg_speed_in_traffic_kmh    ~ gg_diff_mode | uid, data = df) 

lm4 <- feols(gg_duration_in_traffic_min ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,]) 
lm5 <- feols(gg_speed_in_traffic_kmh    ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,]) 

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
                                    'O-D Pair FE', "Y", "Y", "Y", "Y", "Y"),
                 output = file.path(tables_dir,
                                    "reg_route_dev.tex"))

# 'Hour FE', "Y", "Y", "Y", "Y", "Y",
# 'Day of Week FE', "Y", "Y", "Y", "Y", "Y"

# Regressions: traffic ---------------------------------------------------------
# lm1 <- feols(gg_tl_prop_234 ~ gg_diff_mode | uid + hour + dow, data = df) 
# lm2 <- feols(gg_tl_prop_34  ~ gg_diff_mode | uid + hour + dow, data = df) 
# lm3 <- feols(gg_tl_prop_4   ~ gg_diff_mode | uid + hour + dow, data = df) 
# lm4 <- feols(gg_tl_mean     ~ gg_diff_mode | uid + hour + dow, data = df) 
# lm5 <- feols(gg_tl_max      ~ gg_diff_mode | uid + hour + dow, data = df) 
# 
# lm6 <- feols(gg_tl_prop_234 ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 
# lm7 <- feols(gg_tl_prop_34    ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 
# lm8 <- feols(gg_tl_prop_4    ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 
# lm9 <- feols(gg_tl_mean    ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 
# lm10 <- feols(gg_tl_max    ~ diff_from_mode_km | uid + hour + dow, data = df[df$gg_diff_mode %in% T,]) 

lm1 <- feols(gg_tl_prop_234 ~ gg_diff_mode | uid, data = df, vcov = "hetero") 
lm2 <- feols(gg_tl_prop_34  ~ gg_diff_mode | uid, data = df, vcov = "hetero") 
lm3 <- feols(gg_tl_prop_4   ~ gg_diff_mode | uid, data = df, vcov = "hetero") 
lm4 <- feols(gg_tl_mean     ~ gg_diff_mode | uid, data = df, vcov = "hetero") 
lm5 <- feols(gg_tl_max      ~ gg_diff_mode | uid, data = df, vcov = "hetero") 

lm6 <- feols(gg_tl_prop_234 ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 
lm7 <- feols(gg_tl_prop_34    ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 
lm8 <- feols(gg_tl_prop_4    ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 
lm9 <- feols(gg_tl_mean    ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 
lm10 <- feols(gg_tl_max    ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 

modelsummary_tab(list("2-4 Traffic" = lm1,
                      "2-4 Traffic" = lm6,
                      
                      "3-4 Traffic" = lm2,
                      "3-4 Traffic" = lm7,
                      
                      "4 Traffic" = lm3,
                      "4 Traffic" = lm8,
                      
                      "Avg Traffic" = lm4,
                      "Avg Traffic" = lm9,
                      
                      "Max Traffic" = lm5,
                      "Max Traffic" = lm10),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("gg_diff_modeTRUE" = "Route Different: Binary",
                              "diff_from_mode_km" = "Route Difference: Kilometers"),
                 gof_map = c("nobs", "adj.r.squared"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10,
                                    'O-D Pair FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
                 output = file.path(tables_dir,
                                    "reg_route_dev_traffic.tex"))

# 'Hour FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
# 'Day of Week FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"

# Regressions: traffic as indep var --------------------------------------------
lm1 <- feols(gg_diff_mode ~  gg_tl_prop_234 | uid, data = df, vcov = "hetero") 
lm2 <- feols(gg_diff_mode ~  gg_tl_prop_34  | uid, data = df, vcov = "hetero") 
lm3 <- feols(gg_diff_mode ~  gg_tl_prop_4   | uid, data = df, vcov = "hetero") 
lm4 <- feols(gg_diff_mode ~  gg_tl_mean     | uid, data = df, vcov = "hetero") 
lm5 <- feols(gg_diff_mode ~  gg_tl_max      | uid, data = df, vcov = "hetero") 

lm6  <- feols(diff_from_mode_km ~  gg_tl_prop_234 | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 
lm7  <- feols(diff_from_mode_km ~  gg_tl_prop_34  | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 
lm8  <- feols(diff_from_mode_km ~  gg_tl_prop_4   | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 
lm9  <- feols(diff_from_mode_km ~  gg_tl_mean     | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 
lm10 <- feols(diff_from_mode_km ~  gg_tl_max      | uid, data = df[df$gg_diff_mode %in% T,], vcov = "hetero") 

modelsummary_tab(list("Route Different: Binary" = lm1,
                      "Route Different: Binary" = lm2,
                      "Route Different: Binary" = lm3,
                      "Route Different: Binary" = lm4,
                      "Route Different: Binary" = lm5,
                      "Route Different: Kilometers" = lm6,
                      "Route Different: Kilometers" = lm7,
                      "Route Different: Kilometers" = lm8,
                      "Route Different: Kilometers" = lm9,
                      "Route Different: Kilometers" = lm10),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("gg_tl_prop_234" = "Prop 2-4 Traffic",
                              "gg_tl_prop_34" = "Prop 3-4 Traffic",
                              "gg_tl_prop_4" = "Prop 4 Traffic",
                              "gg_tl_mean" = "Average Traffic",
                              "gg_tl_max" = "Maximum Traffic"),
                 gof_map = c("nobs", "adj.r.squared"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10,
                                    'O-D Pair FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
                 output = file.path(tables_dir,
                                    "reg_route_dev_traffic_alt.tex"))

