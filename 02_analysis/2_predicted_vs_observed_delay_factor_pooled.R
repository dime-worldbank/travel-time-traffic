# Predicted vs Observed Delay Factor: Pooled Scatterplots
#
# Companion to 2_predicted_vs_observed_delay_factor.R. That script summarizes
# fit within each route separately (lm-based, per-route R^2/RMSE). Here we
# instead pool all observations within each sample and compute R^2/RMSE
# directly against the raw predictions -- the same approach used in
# 1_calibration_sensitivity_kfold_60routes.R -- so the two are comparable.

# Load data --------------------------------------------------------------------
route_26_df  <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))
route_cal_df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))

beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

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

# Delay factor [from calibration] -----------------------------------------------
route_26_df  <- mk_traffic_indicators(route_26_df, beta)
route_cal_df <- mk_traffic_indicators(route_cal_df, beta)

# Pooled fit statistics ----------------------------------------------------------
# Same formulas as 1_calibration_sensitivity_kfold_60routes.R: raw comparison of
# observed vs. predicted delay factor, pooling all observations within a sample
# (no per-route fitted line), so bias/scale error shows up directly in R^2/RMSE.
r2_fun <- function(obs, pred) {
  keep <- is.finite(obs) & is.finite(pred)
  obs <- obs[keep]; pred <- pred[keep]
  1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
}
rmse_fun <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))

fit_stats_df <- tibble(
  sample = c("Within-sample: Calibration sample (60 routes)",
             "Out-of-sample: Long panel (26 routes)"),
  r2 = c(r2_fun(route_cal_df$delay_factor_od, route_cal_df$delay_factor),
         r2_fun(route_26_df$delay_factor_od, route_26_df$delay_factor)),
  rmse = c(rmse_fun(route_cal_df$delay_factor_od, route_cal_df$delay_factor),
           rmse_fun(route_26_df$delay_factor_od, route_26_df$delay_factor)),
  n_obs = c(sum(is.finite(route_cal_df$delay_factor_od) & is.finite(route_cal_df$delay_factor)),
            sum(is.finite(route_26_df$delay_factor_od) & is.finite(route_26_df$delay_factor)))
) %>%
  dplyr::mutate(label = paste0("R^2 = ", sprintf("%.2f", r2), "\nRMSE = ", sprintf("%.2f", rmse),
                                "\nN = ", format(n_obs, big.mark = ",")))

print(fit_stats_df)

# Scatterplots -------------------------------------------------------------------
AXIS_MAX <- 8

p_cal <- route_cal_df %>%
  ggplot(aes(x = delay_factor_od, y = delay_factor)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.1, size = 0.6, color = "dodgerblue") +
  geom_label(data = fit_stats_df %>% dplyr::filter(sample == "Within-sample: Calibration sample (60 routes)"),
             aes(x = -Inf, y = Inf, label = label),
             hjust = -0.1, vjust = 1.2, size = 3.5, color = "gray20",
             fill = "white", alpha = 0.85, label.size = NA, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(0, AXIS_MAX), ylim = c(0, AXIS_MAX)) +
  labs(x = "Observed Delay Factor (from O-D Travel Time Data)",
       y = "Predicted Delay Factor\n(from Traffic Levels)",
       title = "A. Within-sample: Calibration sample (60 routes)") +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"))

p_26 <- route_26_df %>%
  ggplot(aes(x = delay_factor_od, y = delay_factor)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.1, size = 0.6, color = "darkorange") +
  geom_label(data = fit_stats_df %>% dplyr::filter(sample == "Out-of-sample: Long panel (26 routes)"),
             aes(x = -Inf, y = Inf, label = label),
             hjust = -0.1, vjust = 1.2, size = 3.5, color = "gray20",
             fill = "white", alpha = 0.85, label.size = NA, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(0, AXIS_MAX), ylim = c(0, AXIS_MAX)) +
  labs(x = "Observed Delay Factor (from O-D Travel Time Data)",
       y = "Predicted Delay Factor\n(from Traffic Levels)",
       title = "B. Out-of-sample: Long panel (26 routes)") +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"))

p_pooled <- ggarrange(p_cal, p_26, ncol = 2)

p_pooled

ggsave(p_pooled, filename = file.path(figures_dir, "observed_vs_predicted_delay_pooled.png"),
       height = 5, width = 11)

# Fit statistics by observed delay factor bin -------------------------------------
# Fixed-width bins of the observed delay factor; R^2 and RMSE (pooled, as above)
# and N computed separately within each bin, to see whether fit varies with
# congestion severity. N is reported since R^2 in particular can be unstable
# in bins with few observations or little within-bin variance.
bin_breaks <- c(0:8, Inf)
bin_labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8+")

compute_bin_fit <- function(df, sample_label){
  df %>%
    dplyr::mutate(delay_bin = cut(delay_factor_od, breaks = bin_breaks, labels = bin_labels,
                                   right = FALSE, include.lowest = TRUE)) %>%
    dplyr::filter(!is.na(delay_bin)) %>%
    group_by(delay_bin) %>%
    summarise(n = dplyr::n(),
              r2 = r2_fun(delay_factor_od, delay_factor),
              rmse = rmse_fun(delay_factor_od, delay_factor),
              .groups = "drop") %>%
    dplyr::mutate(sample = sample_label)
}

bin_fit_df <- dplyr::bind_rows(
  compute_bin_fit(route_cal_df, "Within-sample: Calibration sample (60 routes)"),
  compute_bin_fit(route_26_df, "Out-of-sample: Long panel (26 routes)")
) %>%
  dplyr::mutate(sample = factor(sample, levels = c("Within-sample: Calibration sample (60 routes)",
                                                     "Out-of-sample: Long panel (26 routes)")))

print(bin_fit_df, n = Inf)

bin_fit_long_df <- bin_fit_df %>%
  tidyr::pivot_longer(cols = c(r2, rmse), names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = recode(metric, r2 = "R-squared", rmse = "RMSE"))

p_bin_fit <- bin_fit_long_df %>%
  ggplot(aes(x = delay_bin, y = value)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_col(fill = "gray70") +
  geom_text(aes(label = paste0("N=", format(n, big.mark = ","))),
            angle = 90, hjust = -0.1, size = 2.7, color = "gray20") +
  facet_grid(metric ~ sample, scales = "free") +
  labs(x = "Observed Delay Factor Bin",
       y = NULL,
       title = "Fit by observed delay factor bin") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

p_bin_fit

ggsave(p_bin_fit, filename = file.path(figures_dir, "observed_vs_predicted_delay_pooled_by_bin.png"),
       height = 6, width = 10)
