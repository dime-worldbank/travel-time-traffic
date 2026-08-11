# Predicted vs Observed Delay Factor

set.seed(42)

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

# Delay factor [from calibration] ----------------------------------------------
route_26_df  <- mk_traffic_indicators(route_26_df, beta)
route_cal_df <- mk_traffic_indicators(route_cal_df, beta)

# Delay factor [from 26 routes] ------------------------------------------------
route_26_df <- route_26_df %>%
  dplyr::mutate(delay_factor_26r = exp(tl_prop_2 * 1.378 + tl_prop_3 * 2.869 + tl_prop_4 * 5.799))

route_cal_df <- route_cal_df %>%
  dplyr::mutate(delay_factor_26r = exp(tl_prop_2 * 1.378 + tl_prop_3 * 2.869 + tl_prop_4 * 5.799))

# Max delay factor per route, ordering -----------------------------------------
route_26_df <- route_26_df %>%
  group_by(uid) %>%
  dplyr::mutate(delay_factor_max = max(delay_factor)) %>%
  ungroup() %>%
  mutate(uid = fct_reorder(as.factor(uid), delay_factor_max, .desc = TRUE))

route_cal_df <- route_cal_df %>%
  group_by(uid) %>%
  dplyr::mutate(delay_factor_max = max(delay_factor)) %>%
  ungroup() %>%
  mutate(uid = fct_reorder(as.factor(uid), delay_factor_max, .desc = TRUE))

# Regression -------------------------------------------------------------------
lm_cal <- feols(delay_factor ~ delay_factor_od | uid, data = route_cal_df, vcov = ~uid)
lm_26r <- feols(delay_factor ~ delay_factor_od | uid, data = route_26_df, vcov = ~uid)

etable(
  list(
    "Calibration Sample" = lm_cal,
    "Long-Panel Sample"  = lm_26r
  ),
  dict = c(
    delay_factor = "Delay Factor, Predicted from Traffic Levels",
    delay_factor_od = "Observed Delay Factor from O-D Travel Time Data",
    uid = "Route"
  ),
  extralines = list(
    "-Observations" = c(
      format(nobs(lm_cal), big.mark = ","),
      format(nobs(lm_26r), big.mark = ",")
    ),
    "-N Routes" = c(lm_cal$fixef_sizes[["uid"]],
                    lm_26r$fixef_sizes[["uid"]]),
    "-_Sample" = c("Calibration", "Long-Panel")
  ),
  fitstat = ~ ar2 + r2 + wr2 + rmse,
  style.tex = style.tex(fixef.title = "\\midrule", fixef.suffix = " FE"),
  replace = TRUE,
  float = FALSE,
  file = file.path(
    tables_dir,
    "delay_factor_observed_vs_predicted.tex"
  )
)


# Panel A: RMSE, MAE, and correlation across routes, by group -------------------
# Three groups per route, all using the same specification -- traffic-level
# proportions interacted with mean-centered free-flow speed, route fixed
# effects (column 2 of Table \ref{tab:ols_calibration_centered_calib_vs_longpanel}
# / 1_calibration_sensitivity_kfold_60routes.R) -- so they are comparable:
#  (a) Within-Sample [Calibration Sample]: model fit on all 58 calibration
#      routes, evaluated on those same routes.
#  (b) Out-of-Sample [Calibration Sample, repeated 50/50 splits]: 500 draws, each randomly
#      holding out 50% of calibration routes (stratified by road class); the
#      model is fit on the other 50% and applied to the held-out routes. Each
#      route's metric is averaged across the ~250 draws in which it was held out.
#  (c) Out-of-Sample [Long-Panel Sample of 26 Routes]: the (a) model applied to
#      the independent long-panel sample, never used in estimation.

## Prep calibration sample for the kfold-spec model ------------------------------
route_cal_kf_df <- route_cal_df %>%
  dplyr::mutate(speed_kmh = speed_in_traffic_kmh,
                tt_hour_per_km = (duration_in_traffic_s/60/60) / (distance_m/1000),
                tt_hour_per_km_ln = log(tt_hour_per_km),
                hour = datetime %>% hour()) %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh[hour %in% 1:4], prob = 0.99, na.rm = T) %>% as.numeric()) %>%
  ungroup() %>%
  filter(hour >= 6, hour <= 21)

center_speed_cal <- route_cal_kf_df %>% distinct(uid, speed_kmh_uid_max) %>% pull(speed_kmh_uid_max) %>% mean(na.rm = TRUE)
route_cal_kf_df <- route_cal_kf_df %>% dplyr::mutate(speed_kmh_uid_max_c = speed_kmh_uid_max - center_speed_cal)

## Prep long panel using the SAME centering constant as the calibration sample --
route_26_kf_df <- route_26_df %>%
  dplyr::mutate(speed_kmh = speed_in_traffic_kmh,
                hour = datetime %>% hour()) %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh[hour %in% 1:4], prob = 0.99, na.rm = T) %>% as.numeric()) %>%
  ungroup() %>%
  filter(hour >= 6, hour <= 21) %>%
  dplyr::mutate(speed_kmh_uid_max_c = speed_kmh_uid_max - center_speed_cal)

kfold_fml <- as.formula(
  "tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 + tl_prop_2:speed_kmh_uid_max_c + tl_prop_3:speed_kmh_uid_max_c + tl_prop_4:speed_kmh_uid_max_c | uid"
)

predict_delay_factor <- function(mod, data){
  b <- coef(mod)
  ci <- (b["tl_prop_2"] + b["tl_prop_2:speed_kmh_uid_max_c"] * data$speed_kmh_uid_max_c) * data$tl_prop_2 +
    (b["tl_prop_3"] + b["tl_prop_3:speed_kmh_uid_max_c"] * data$speed_kmh_uid_max_c) * data$tl_prop_3 +
    (b["tl_prop_4"] + b["tl_prop_4:speed_kmh_uid_max_c"] * data$speed_kmh_uid_max_c) * data$tl_prop_4
  exp(ci)
}

rmse_fun <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
mae_fun  <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)
cor_fun  <- function(obs, pred) cor(obs, pred, use = "complete.obs")

## (a) Within-sample: fit on all 58 routes, evaluate on those same routes -------
mod_full <- feols(kfold_fml, vcov = ~uid, data = route_cal_kf_df)
route_cal_kf_df <- route_cal_kf_df %>% dplyr::mutate(pred_full = predict_delay_factor(mod_full, route_cal_kf_df))

a_df <- route_cal_kf_df %>%
  group_by(uid) %>%
  summarise(n = dplyr::n(),
            rmse = rmse_fun(delay_factor_od, pred_full),
            mae = mae_fun(delay_factor_od, pred_full),
            cor = cor_fun(delay_factor_od, pred_full),
            .groups = "drop") %>%
  dplyr::mutate(group = "(a) Within-Sample\n[Calibration Sample]")

## (b) Out-of-sample: 500-draw k-fold within the calibration sample -------------
N_DRAWS <- 500
SUBSAMPLE_SHARE <- 0.5

routes_by_class_cal <- route_cal_kf_df %>% distinct(uid, fclass)

# Fixed-width bins of observed delay factor, for panel C
bin_breaks <- c(0:5, Inf)
bin_labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", ">5")

set.seed(42)
b_results <- map(1:N_DRAWS, function(draw_i){

  routes_i <- routes_by_class_cal %>%
    group_by(fclass) %>%
    dplyr::slice_sample(prop = SUBSAMPLE_SHARE) %>%
    ungroup() %>%
    pull(uid)

  mod_i <- feols(kfold_fml, vcov = ~uid, data = route_cal_kf_df[route_cal_kf_df$uid %in% routes_i,])
  pred_i <- predict_delay_factor(mod_i, route_cal_kf_df)
  held_out_i <- !(route_cal_kf_df$uid %in% routes_i)

  held_out_df_i <- route_cal_kf_df[held_out_i,] %>%
    dplyr::mutate(pred = pred_i[held_out_i])

  route_summary_i <- held_out_df_i %>%
    group_by(uid) %>%
    summarise(rmse = rmse_fun(delay_factor_od, pred),
              mae = mae_fun(delay_factor_od, pred),
              cor = cor_fun(delay_factor_od, pred),
              .groups = "drop") %>%
    dplyr::mutate(draw = draw_i)

  # Panel C: pooled RMSE/MAE by observed delay factor bin, for this draw
  bin_summary_i <- held_out_df_i %>%
    dplyr::mutate(delay_bin = cut(delay_factor_od, breaks = bin_breaks, labels = bin_labels,
                                   right = FALSE, include.lowest = TRUE)) %>%
    dplyr::filter(!is.na(delay_bin)) %>%
    group_by(delay_bin) %>%
    summarise(n = dplyr::n(),
              rmse = rmse_fun(delay_factor_od, pred),
              mae = mae_fun(delay_factor_od, pred),
              .groups = "drop") %>%
    dplyr::mutate(draw = draw_i)

  # Panel B: raw held-out observed/predicted pairs, draw 1 only
  scatter_i <- if (draw_i == 1) held_out_df_i %>% dplyr::select(uid, delay_factor_od, pred) else NULL

  list(route_summary = route_summary_i, bin_summary = bin_summary_i, scatter = scatter_i)

})

b_draws_df <- map_df(b_results, "route_summary")
bin_draws_df <- map_df(b_results, "bin_summary")
draw1_scatter_df <- b_results[[1]]$scatter

b_df <- b_draws_df %>%
  group_by(uid) %>%
  summarise(n = dplyr::n(),
            rmse = mean(rmse, na.rm = TRUE),
            mae = mean(mae, na.rm = TRUE),
            cor = mean(cor, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::mutate(group = "(b) Out-of-Sample\n[Calibration Sample: 500 repeated\n50/50 splits, averaged]")

## (c) Out-of-sample: long panel of 26 routes, never used in estimation ---------
route_26_kf_df <- route_26_kf_df %>% dplyr::mutate(pred_full = predict_delay_factor(mod_full, route_26_kf_df))

c_df <- route_26_kf_df %>%
  group_by(uid) %>%
  summarise(n = dplyr::n(),
            rmse = rmse_fun(delay_factor_od, pred_full),
            mae = mae_fun(delay_factor_od, pred_full),
            cor = cor_fun(delay_factor_od, pred_full),
            .groups = "drop") %>%
  dplyr::mutate(group = "(c) Out-of-Sample\n[Long-Panel Sample of 26 Routes]")

## Combine and plot ---------------------------------------------------------------
group_levels <- c("(a) Within-Sample\n[Calibration Sample]",
                   "(b) Out-of-Sample\n[Calibration Sample: 500 repeated\n50/50 splits, averaged]",
                   "(c) Out-of-Sample\n[Long-Panel Sample of 26 Routes]")

route_fit_df <- dplyr::bind_rows(a_df, b_df, c_df) %>%
  dplyr::mutate(group = factor(group, levels = rev(group_levels)))

route_fit_long_df <- route_fit_df %>%
  tidyr::pivot_longer(cols = c(rmse, mae, cor), names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = recode(metric, rmse = "RMSE", mae = "MAE", cor = "Correlation"),
                metric = factor(metric, levels = c("RMSE", "MAE", "Correlation")))

# Single combined label per box (avoids the p25/median/p75 labels overlapping
# each other when their values are close together).
fit_summary <- route_fit_long_df %>%
  group_by(group, metric) %>%
  summarise(p25 = quantile(value, 0.25, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            p75 = quantile(value, 0.75, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::mutate(label = paste0("P25=", sprintf("%.2f", p25),
                                "  Med=", sprintf("%.2f", median),
                                "  P75=", sprintf("%.2f", p75)))

TEXT_NUDGE <- -0.35

p_box <- route_fit_long_df %>%
  ggplot(aes(x = value, y = group)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, fill = "gray90") +
  geom_jitter(height = 0.12, width = 0, alpha = 0.5, size = 1.2, color = "dodgerblue") +
  geom_text(data = fit_summary, aes(x = -Inf, label = label),
            color = "red", size = 2.8, hjust = -0.02,
            position = position_nudge(y = TEXT_NUDGE)) +
  facet_wrap(~metric, scales = "free_x", ncol = 3) +
  labs(x = NULL, y = NULL,
       title = "A. Distribution across routes of RMSE, MAE, and correlation\nbetween observed and predicted delay factor") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

p_box

# Panel B: scatterplot for one 50/50 split draw (held-out routes only) --------
AXIS_MAX <- 5

draw1_stats <- tibble(
  rmse = rmse_fun(draw1_scatter_df$delay_factor_od, draw1_scatter_df$pred),
  mae  = mae_fun(draw1_scatter_df$delay_factor_od, draw1_scatter_df$pred),
  cor  = cor_fun(draw1_scatter_df$delay_factor_od, draw1_scatter_df$pred)
) %>%
  dplyr::mutate(label = paste0("RMSE = ", sprintf("%.2f", rmse),
                                "\nMAE = ", sprintf("%.2f", mae),
                                "\nCorrelation = ", sprintf("%.2f", cor)))

p_draw1 <- draw1_scatter_df %>%
  ggplot(aes(x = delay_factor_od, y = pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.15, size = 0.6, color = "dodgerblue") +
  geom_label(data = draw1_stats, aes(x = -Inf, y = Inf, label = label),
             hjust = -0.05, vjust = 1.2, size = 3.2, color = "gray20",
             fill = "white", alpha = 0.85, label.size = NA, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(0, AXIS_MAX), ylim = c(0, AXIS_MAX)) +
  labs(x = "Observed Delay Factor",
       y = "Predicted Delay Factor",
       title = "B. Observed vs. predicted delay factor: one 50/50 split\n(held-out routes only)") +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"))

# Panel C: distribution of RMSE by observed delay factor bin, across draws -----
p_bin_rmse <- bin_draws_df %>%
  dplyr::mutate(delay_bin = factor(delay_bin, levels = bin_labels)) %>%
  ggplot(aes(x = delay_bin, y = rmse)) +
  geom_boxplot(outlier.size = 0.5, fill = "gray90") +
  labs(x = "Observed Delay Factor Bin",
       y = "RMSE",
       title = "C. Distribution of RMSE by observed delay factor bin\n(500 repeated 50/50 splits, held-out routes only)") +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"))

p_bc <- ggarrange(p_draw1, p_bin_rmse, ncol = 2)

p_main <- ggarrange(p_box, p_bc, ncol = 1, heights = c(0.45, 0.55))

p_main

ggsave(p_main, filename = file.path(figures_dir, "observed_vs_predicted_delay_main.png"),
       height = 8, width = 11.3)

ggsave(p_main, filename = file.path(figures_dir, "figure_4.png"),
       height = 8, width = 11.3)

# Scatterplots -----------------------------------------------------------------
pcal <- route_cal_df %>%
  ggplot() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  geom_point(aes(x = delay_factor_od,
                 y = delay_factor)) +
  ylim(0, 8) +
  xlim(0, 8) +
  labs(y = "Delay Factor,\nPredicted from Traffic Levels\n[Calibrated using calibration\nsample of 60 routes]",
       x = "Observed Delay\nFactor from O-D \nTravel Time Data",
       title = "A. Delay factor calibrated from 60 route sample compared against\nobserved delay factor on same sample") +
  facet_wrap(~uid, ncol = 10) +
  theme(strip.background = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold"))

p26 <- route_26_df %>%
  ggplot() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  geom_point(aes(x = delay_factor_od,
                 y = delay_factor)) +
  ylim(0, 8) +
  xlim(0, 8) +
  labs(y = "Delay Factor,\nPredicted from Traffic Levels\n[Calibrated using calibration\nsample of 60 routes]",
       x = "Observed Delay\nFactor from O-D \nTravel Time Data",
       title = "B. Delay factor calibrated from 60 route sample compared against\nobserved delay factor on long-panel of 26 routes") +
  facet_wrap(~uid, ncol = 10) +
  theme(strip.background = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold"))

p <- ggarrange(pcal, p26, ncol = 1, heights = c(0.6, 0.4))

ggsave(p, filename = file.path(figures_dir, "delay_factor_observed_vs_predicted_scatter.png"),
       height = 10,
       width = 8)

