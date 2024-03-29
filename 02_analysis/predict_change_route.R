# Change in Route

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  mutate(diff_from_mode_km = (gg_distance_m - gg_distance_m_mode) / 1000,
         hour = datetime %>% hour(),
         dow = datetime %>% wday)

lm1 <- feols(gg_diff_mode ~ gg_tl_prop_234 | uid + hour + dow, data = df) 
lm2 <- feols(gg_diff_mode ~ gg_tl_prop_34 | uid + hour + dow, data = df) 
lm3 <- feols(gg_diff_mode ~ gg_tl_prop_4 | uid + hour + dow, data = df) 

modelsummary_tab(list("Diff Route" = lm1,
                      "Diff Route" = lm2,
                      "Diff Route" = lm3),
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