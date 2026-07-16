# Predicted vs Observed Delay Factor

# Load data --------------------------------------------------------------------
route_26_df  <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))
route_cal_df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))

beta_df <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Cleanup calibrated routes ----------------------------------------------------

## Delay factor from OD data
route_cal_df <- route_cal_df %>%
  group_by(uid) %>%
  dplyr::mutate(duration_in_traffic_s_minimum = duration_in_traffic_s[hour %in% 1:4] %>%
                  quantile(0.01, na.rm = T) %>%
                  as.numeric()) %>%
  ungroup() %>%
  dplyr::mutate(duration_pc = (duration_in_traffic_s - duration_in_traffic_s_minimum)/duration_in_traffic_s_minimum,
                delay_factor_od = duration_pc + 1)

## fclass
route_cal_df <- route_cal_df %>%
  dplyr::mutate(fclass = case_when(
    name == "thika road" ~ "trunk_fast",
    name == "southern bypass" ~ "trunk_fast",
    name == "mombasa road" ~ "trunk_fast",
    TRUE ~ fclass
  )) %>%
  dplyr::mutate(prop_trunk_fast = ifelse(fclass == "trunk_fast", 1, 0),
                prop_trunk = ifelse(fclass == "trunk", 1, 0),
                prop_primary = ifelse(fclass == "primary", 1, 0),
                prop_secondary = ifelse(fclass == "secondary", 1, 0),
                prop_tertiary = ifelse(fclass == "tertiary", 1, 0),
                prop_residential = ifelse(fclass == "residential", 1, 0),
                prop_unclassified = ifelse(fclass == "unclassified", 1, 0))

## uid
route_cal_df <- route_cal_df %>%
  dplyr::mutate(uid = uid %>% as.factor() %>% as.numeric())

# Delay factor [from calibration] ----------------------------------------------
route_26_df  <- mk_traffic_indicators(route_26_df, beta)
route_cal_df <- mk_traffic_indicators(route_cal_df, beta)

# Delay factor [from 26 routes] ------------------------------------------------
route_26_df <- route_26_df %>%
  dplyr::mutate(delay_factor_26r = exp(tl_prop_2 * 1.378 + tl_prop_3 * 2.869 + tl_prop_4 * 5.799))

route_cal_df <- route_cal_df %>%
  dplyr::mutate(delay_factor_26r = exp(tl_prop_2 * 1.378 + tl_prop_3 * 2.869 + tl_prop_4 * 5.799))

# Regression -------------------------------------------------------------------


# Figures ----------------------------------------------------------------------
route_cal_df %>%
  ggplot() +
  geom_point(aes(x = route_cal_df,
                 y = delay_factor_od)) +
  facet_wrap(~uid)





