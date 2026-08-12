# Calibration Sensitivity: Stratified subsample stability

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
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh[hour %in% 1:4], prob = 0.99, na.rm = T) %>% as.numeric(),
                # Free-flow duration baseline (needed for observed delay factor below);
                # computed here, before filtering to hours 6-21, so hours 1-4 are still available.
                duration_in_traffic_s_minimum = quantile(duration_in_traffic_s[hour %in% 1:4], prob = 0.01, na.rm = T) %>% as.numeric()) %>%
  ungroup() %>%
  filter(hour >= 6, hour <= 21)

# Free-flow speed is centered on the full, un-thresholded set of routes -- same
# as 1_calibration_regression.R (center_speed_v1) -- so that main-effect
# coefficients here are on the same footing as Table 2 col. 6. Centering after
# the route-inclusion filter instead would shift the main effects by
# delta_k * (center_full - center_filtered), since the interaction slope is
# unchanged but the centering constant it's multiplied against differs.
center_speed <- route_df %>% distinct(uid, speed_kmh_uid_max) %>% pull(speed_kmh_uid_max) %>% mean(na.rm = TRUE)
route_df <- route_df %>%
  dplyr::mutate(speed_kmh_uid_max_c = speed_kmh_uid_max - center_speed,
                # Observed delay factor: how much slower than free-flow, per observation
                delay_factor_od = (duration_in_traffic_s - duration_in_traffic_s_minimum) / duration_in_traffic_s_minimum + 1)

# Route-inclusion threshold: match column 6 of Table
# \ref{tab:ols_calibration_threshold_x_speed_centered_calib} (thresh_0.05_speed in
# 1_calibration_regression.R), whose coefficients are saved to coefs.Rds and used
# to compute the delay factor throughout the paper. A route is retained only if it
# reaches traffic levels 3 and 4 in at least 5% of its observations; routes that
# essentially never reach those levels contribute no identifying variation for the
# level-3 and level-4 coefficients. Keeping this consistent with the main-text
# figure (2_predicted_vs_observed_delay_factor.R) means both exercises resample
# from the same estimation sample the deployed calibration uses.
ROUTE_INCLUSION_THRESHOLD <- 0.05

routes_keep <- route_df %>%
  group_by(uid) %>%
  summarise(share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
            share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::filter(share_prop3_gt0 >= ROUTE_INCLUSION_THRESHOLD,
                share_prop4_gt0 >= ROUTE_INCLUSION_THRESHOLD) %>%
  pull(uid)

route_df <- route_df %>% dplyr::filter(uid %in% routes_keep)

# 1. Re-estimate the calibration equation (column 6 of                       --
#    ols_calibration_threshold_x_speed_centered_calib.tex: threshold = 0.05, --
#    traffic-level proportions interacted with centered free-flow speed --  --
#    equivalently column 2 of ols_calibration_centered_calib_vs_longpanel)   --
#    repeatedly on random subsamples of routes, stratified by road-class     --
#    group, to check how stable coefficients are to which routes are         --
#    included.                                                               --
boot_fml <- as.formula(
  "tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 + tl_prop_2:speed_kmh_uid_max_c + tl_prop_3:speed_kmh_uid_max_c + tl_prop_4:speed_kmh_uid_max_c | uid"
)

var_clean_map <- c(
  "tl_prop_2" = "Prop. Traffic Level 2",
  "tl_prop_3" = "Prop. Traffic Level 3",
  "tl_prop_4" = "Prop. Traffic Level 4",
  "tl_prop_2:speed_kmh_uid_max_c" = "Prop. TL 2 x Speed (centered)",
  "tl_prop_3:speed_kmh_uid_max_c" = "Prop. TL 3 x Speed (centered)",
  "tl_prop_4:speed_kmh_uid_max_c" = "Prop. TL 4 x Speed (centered)"
)

N_DRAWS <- 500
SUBSAMPLE_SHARE <- 0.5

var_clean_levels <- unname(var_clean_map)

# Predicted delay factor from a draw's fitted coefficients ---------------------
# Uses the route's own observed traffic-level proportions and centered free-flow
# speed (both available for every route, in- or out-of-sample); route fixed
# effects are intentionally excluded, since the delay factor is meant to be a
# portable function of traffic levels + speed, not tied to a specific route.
predict_delay_factor <- function(mod, data){
  b <- coef(mod)
  ci <- (b["tl_prop_2"] + b["tl_prop_2:speed_kmh_uid_max_c"] * data$speed_kmh_uid_max_c) * data$tl_prop_2 +
    (b["tl_prop_3"] + b["tl_prop_3:speed_kmh_uid_max_c"] * data$speed_kmh_uid_max_c) * data$tl_prop_3 +
    (b["tl_prop_4"] + b["tl_prop_4:speed_kmh_uid_max_c"] * data$speed_kmh_uid_max_c) * data$tl_prop_4
  exp(ci)
}

rmse_fun <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))

# Out-of-sample R^2 (1 - SS_res/SS_tot): can go negative if predictions are
# worse than just guessing the mean, unlike a simple cor(obs, pred)^2.
r2_fun <- function(obs, pred){
  keep <- is.finite(obs) & is.finite(pred)
  obs <- obs[keep]; pred <- pred[keep]
  1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
}

lm_pooled_boot_df <- feols(boot_fml, vcov = ~uid, data = route_df) %>%
  confint() %>%
  as.data.frame() %>%
  clean_names() %>%
  rownames_to_column(var = "variable") %>%
  dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2) %>%
  dplyr::mutate(var_clean = factor(var_clean_map[variable], levels = var_clean_levels))

# Stratification groups for the 50/50 splits, matching
# 2_predicted_vs_observed_delay_factor.R. The route-inclusion threshold leaves
# only 2 residential and 2 trunk_fast routes -- too few to stratify on their
# own, since a 50/50 split would put a single route on each side. We therefore
# pool residential with unclassified, and trunk_fast with trunk, giving five
# strata of 8-9 routes each.
routes_by_class <- route_df %>%
  distinct(uid, fclass) %>%
  dplyr::mutate(strata = dplyr::case_when(
    fclass %in% c("residential", "unclassified") ~ "residential/unclassified",
    fclass %in% c("trunk", "trunk_fast")         ~ "trunk/trunk_fast",
    TRUE                                          ~ fclass
  ))

set.seed(42)
boot_results <- map(1:N_DRAWS, function(draw_i){

  routes_i <- routes_by_class %>%
    group_by(strata) %>%
    dplyr::slice_sample(prop = SUBSAMPLE_SHARE) %>%
    ungroup() %>%
    pull(uid)

  mod_i <- feols(boot_fml, vcov = ~uid, data = route_df[route_df$uid %in% routes_i,])

  coef_df_i <- mod_i %>%
    coef() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::rename(b = 2) %>%
    dplyr::mutate(draw = draw_i)

  # (1) Delay factor for all routes, from this draw's fitted coefficients ------
  delay_factor_pred_i <- predict_delay_factor(mod_i, route_df)
  in_sample_i <- route_df$uid %in% routes_i

  # (2) RMSE and R^2 between observed and estimated delay factor, computed     --
  #     separately pooling all within-sample and all out-of-sample observations
  fit_df_i <- tibble(
    draw = draw_i,
    sample = c("Within-sample", "Out-of-sample"),
    n_obs = c(sum(in_sample_i), sum(!in_sample_i)),
    rmse = c(rmse_fun(route_df$delay_factor_od[in_sample_i], delay_factor_pred_i[in_sample_i]),
             rmse_fun(route_df$delay_factor_od[!in_sample_i], delay_factor_pred_i[!in_sample_i])),
    r2 = c(r2_fun(route_df$delay_factor_od[in_sample_i], delay_factor_pred_i[in_sample_i]),
           r2_fun(route_df$delay_factor_od[!in_sample_i], delay_factor_pred_i[!in_sample_i]))
  )

  list(coef = coef_df_i, fit = fit_df_i)

})

lm_boot_df <- map_df(boot_results, "coef") %>%
  dplyr::mutate(var_clean = factor(var_clean_map[variable], levels = var_clean_levels))

fit_boot_df <- map_df(boot_results, "fit")

# Share of draws whose coefficient falls within the pooled estimate's 95% CI ---
pct_in_ci_df <- lm_boot_df %>%
  dplyr::left_join(lm_pooled_boot_df %>%
                      dplyr::select(variable, ci_low = x2_5_percent, ci_high = x97_5_percent),
                    by = "variable") %>%
  group_by(var_clean) %>%
  summarise(pct_in_ci = mean(b >= ci_low & b <= ci_high, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::mutate(var_clean = factor(var_clean, levels = var_clean_levels),
                label = paste0(round(pct_in_ci * 100, 1), "% of draws\nwithin pooled 95% CI"))

# 2. Plot the distribution of coefficients across draws ------------------------
p_boot <- lm_boot_df %>%
  ggplot(aes(x = b)) +
  geom_rect(data = lm_pooled_boot_df,
            aes(xmin = x2_5_percent, xmax = x97_5_percent,
                ymin = -Inf, ymax = Inf, fill = "95% CI of pooled estimate"),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_vline(data = lm_pooled_boot_df,
             aes(xintercept = b, linetype = "Pooled estimate"),
             color = "gray30") +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "white", alpha = 0.8) +
  geom_label(data = pct_in_ci_df,
             aes(x = Inf, y = Inf, label = label),
             hjust = 1.05, vjust = 1.3, size = 3, color = "gray30",
             fill = "white", alpha = 0.75, label.size = NA, inherit.aes = FALSE) +
  facet_wrap(~var_clean, scales = "free", ncol = 3) +
  scale_linetype_manual(values = c("Pooled estimate" = "dashed"), name = NULL) +
  scale_fill_manual(values = c("95% CI of pooled estimate" = "gray50"), name = NULL) +
  labs(x = "Coefficient",
       y = paste0("Count (of ", N_DRAWS, " draws)"),
       title = "Calibration sensitivity: stability across random subsamples of routes",
       subtitle = paste0(SUBSAMPLE_SHARE * 100, "% of routes randomly resampled within each road class, ", N_DRAWS, " draws")
  ) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

p_boot

ggsave(p_boot, filename = file.path(figures_dir, "coef_by_subsample.png"),
       height = 6, width = 9)

# 3. Plot within-sample vs. out-of-sample delay-factor accuracy, and the gap ---
gap_panel <- "Gap [Out-of-sample - Within-sample]"

# A small share of draws produce extreme negative out-of-sample R^2 (a badly
# generalizing road-class split). For legibility, the left column's x-axis is
# floored at LEFT_R2_MIN; this excludes a small number of observations there.
# The gap column (right) is left on its own free scale, since gap values can
# be considerably more negative and flooring it there would cut much more.
LEFT_R2_MIN <- -0.5

fit_dist_df <- fit_boot_df %>%
  tidyr::pivot_longer(cols = c(rmse, r2), names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = recode(metric, rmse = "RMSE", r2 = "R-squared"),
                panel = "Within- vs. out-of-sample") %>%
  dplyr::select(draw, metric, panel, sample, value)

fit_gap_df <- fit_boot_df %>%
  dplyr::select(draw, sample, rmse, r2) %>%
  tidyr::pivot_wider(id_cols = draw, names_from = sample, values_from = c(rmse, r2)) %>%
  dplyr::transmute(draw,
                    rmse = `rmse_Out-of-sample` - `rmse_Within-sample`,
                    r2   = `r2_Out-of-sample` - `r2_Within-sample`) %>%
  tidyr::pivot_longer(cols = c(rmse, r2), names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = recode(metric, rmse = "RMSE", r2 = "R-squared"),
                panel = gap_panel)

n_trimmed_left <- sum(fit_dist_df$metric == "R-squared" & fit_dist_df$value < LEFT_R2_MIN)

fit_dist_df <- fit_dist_df %>% dplyr::filter(!(metric == "R-squared" & value < LEFT_R2_MIN))

panel_levels <- c("Within- vs. out-of-sample", gap_panel)

fit_dist_df <- fit_dist_df %>% dplyr::mutate(panel = factor(panel, levels = panel_levels))
fit_gap_df  <- fit_gap_df  %>% dplyr::mutate(panel = factor(panel, levels = panel_levels))

# Anchor point so the left column's R-squared panel always starts at exactly
# LEFT_R2_MIN, even if no histogram bin happens to reach that far left.
left_r2_anchor_df <- tibble(metric = factor("R-squared", levels = c("RMSE", "R-squared")),
                             panel = factor("Within- vs. out-of-sample", levels = panel_levels),
                             value = LEFT_R2_MIN)

p_fit <- ggplot() +
  geom_vline(data = fit_gap_df %>% dplyr::distinct(metric, panel),
             aes(xintercept = 0), linetype = "dashed", color = "gray30") +
  geom_histogram(data = fit_dist_df, aes(x = value, fill = sample),
                  bins = 30, color = "white", alpha = 0.6, position = "identity") +
  geom_histogram(data = fit_gap_df, aes(x = value),
                  bins = 30, fill = "gray50", color = "white", alpha = 0.85) +
  geom_blank(data = left_r2_anchor_df, aes(x = value)) +
  facet_grid(metric ~ panel, scales = "free") +
  scale_fill_manual(values = c("Within-sample" = "dodgerblue",
                                "Out-of-sample" = "darkorange"),
                     name = NULL) +
  labs(x = "Value",
       y = paste0("Count (of ", N_DRAWS, " draws)"),
       title = "Delay factor accuracy: within-sample vs. out-of-sample",
       subtitle = paste0("Each of ", N_DRAWS, " draws refits the calibration equation on a random ",
                          SUBSAMPLE_SHARE * 100, "% stratified (by road class) subsample of routes.\n",
                          "Gap = Out-of-sample - Within-sample (RMSE or R-squared).")
  ) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"))

p_fit

ggsave(p_fit, filename = file.path(figures_dir, "delay_factor_fit_by_subsample.png"),
       height = 6, width = 9)
