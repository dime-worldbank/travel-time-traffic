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


# Panel A: pooled RMSE and correlation, by group -------------------------------
# All groups use the same specification -- traffic-level proportions interacted
# with mean-centered free-flow speed, route fixed effects (column 2 of Table
# \ref{tab:ols_calibration_centered_calib_vs_longpanel} /
# 1_calibration_sensitivity_kfold_60routes.R) -- so they are comparable.
#
# Every metric here is POOLED across routes: one RMSE / correlation
# computed over all observations on the relevant side of a split, rather than
# computed per route and then summarised. This matches panel B and the
# appendix figure (delay_factor_fit_by_subsample.png), and unlike a per-route
# metric it retains the between-route level error -- the dimension a change in
# the calibration coefficients actually moves.
#
#  (a) and (b) come from the SAME 500 draws, each randomly holding out 50% of
#      calibration routes (stratified by road-class group; see routes_by_class_cal
#      below) and fitting the model on the other 50%. They differ only in which
#      observations are pooled:
#        (a) Within-Sample: the routes the draw was trained on.
#        (b) Out-of-Sample: the routes held out of that draw.
#      Each draw contributes one pooled value, so the boxplots show the
#      distribution across the 500 splits. Using the same draws for both keeps
#      the estimator identical across (a) and (b), so the only difference is
#      trained-on vs. held-out -- evaluating (a) against coefficients fit on all
#      retained routes would confound the two with a change in sample size.
#  (c) Out-of-Sample [Long-Panel Sample of 26 Routes]: the model fit on all
#      retained calibration routes, applied to the independent long-panel
#      sample, never used in estimation. The long panel is evaluated in full --
#      the route-inclusion threshold below governs estimation, not evaluation.
#      This is a single pooled value rather than a distribution, so it is drawn
#      as a reference line.
#
# Note that predictions deliberately exclude the route fixed effects (see
# predict_delay_factor below), so no route-specific parameter is ever fit to a
# trained-on route. The (a) vs. (b) contrast therefore tests only whether the
# six pooled slopes travel to new routes, and should be read as a coefficient-
# stability check rather than as a test for overfitting.

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

## Route-inclusion threshold ---------------------------------------------------
# Match column 6 of Table \ref{tab:ols_calibration_threshold_x_speed_centered_calib}
# (thresh_0.05_speed in 1_calibration_regression.R), whose coefficients are saved
# to coefs.Rds and used to compute the delay factor throughout the paper. A route
# is retained only if it reaches traffic level 3 and traffic level 4 in at least
# 5% of its observations; routes that essentially never reach those levels
# contribute no identifying variation for the level-3 and level-4 coefficients.
# Applying it here means the figure validates the same estimation sample the
# deployed calibration uses.
ROUTE_INCLUSION_THRESHOLD <- 0.05

routes_keep_cal <- route_cal_kf_df %>%
  group_by(uid) %>%
  summarise(share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
            share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::filter(share_prop3_gt0 >= ROUTE_INCLUSION_THRESHOLD,
                share_prop4_gt0 >= ROUTE_INCLUSION_THRESHOLD) %>%
  pull(uid)

route_cal_kf_df <- route_cal_kf_df %>% dplyr::filter(uid %in% routes_keep_cal)

N_CAL_ROUTES <- dplyr::n_distinct(route_cal_kf_df$uid)

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

## Model fit on all retained routes -- used for panel (c) only ----------------
mod_full <- feols(kfold_fml, vcov = ~uid, data = route_cal_kf_df)

## (a) and (b): 500-draw 50/50 splits within the calibration sample -------------
N_DRAWS <- 500
SUBSAMPLE_SHARE <- 0.5

# Stratification groups for the 50/50 splits. The route-inclusion threshold
# leaves only 2 residential and 2 trunk_fast routes -- too few to stratify on
# their own, since a 50/50 split would put a single route on each side. We
# therefore pool residential with unclassified, and trunk_fast with trunk,
# giving five strata of 8-9 routes each.
routes_by_class_cal <- route_cal_kf_df %>%
  distinct(uid, fclass) %>%
  dplyr::mutate(strata = dplyr::case_when(
    fclass %in% c("residential", "unclassified") ~ "residential/unclassified",
    fclass %in% c("trunk", "trunk_fast")         ~ "trunk/trunk_fast",
    TRUE                                          ~ fclass
  ))

# Fixed-width bins of observed delay factor, for panel C
bin_breaks <- c(0:5, Inf)
bin_labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", ">5")

set.seed(42)
b_results <- map(1:N_DRAWS, function(draw_i){

  routes_i <- routes_by_class_cal %>%
    group_by(strata) %>%
    dplyr::slice_sample(prop = SUBSAMPLE_SHARE) %>%
    ungroup() %>%
    pull(uid)

  mod_i <- feols(kfold_fml, vcov = ~uid, data = route_cal_kf_df[route_cal_kf_df$uid %in% routes_i,])
  pred_i <- predict_delay_factor(mod_i, route_cal_kf_df)
  held_out_i <- !(route_cal_kf_df$uid %in% routes_i)

  held_out_df_i <- route_cal_kf_df[held_out_i,] %>%
    dplyr::mutate(pred = pred_i[held_out_i])

  in_sample_df_i <- route_cal_kf_df[!held_out_i,] %>%
    dplyr::mutate(pred = pred_i[!held_out_i])

  # Same draw, same fitted coefficients -- the only difference between the two
  # summaries is whether the routes were trained on or held out. Metrics are
  # pooled over all observations on that side, giving one value per draw.
  pooled_summary <- function(df) dplyr::tibble(
    draw = draw_i,
    n = nrow(df),
    n_routes = dplyr::n_distinct(df$uid),
    rmse = rmse_fun(df$delay_factor_od, df$pred),
    cor  = cor_fun(df$delay_factor_od, df$pred)
  )

  pooled_out_i <- pooled_summary(held_out_df_i)
  pooled_in_i  <- pooled_summary(in_sample_df_i)

  # Panel C: pooled RMSE by observed delay factor bin, for this draw
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

  # Keep the draw's route split so panel B can be rebuilt for a chosen draw
  # without replaying the whole sequence of random draws.
  list(pooled_out = pooled_out_i, pooled_in = pooled_in_i,
       bin_summary = bin_summary_i, routes = routes_i)

})

bin_draws_df <- map_df(b_results, "bin_summary")

a_df <- map_df(b_results, "pooled_in") %>%
  dplyr::mutate(group = "(a) Within-Sample\n[Calibration Sample: routes the\ndraw was trained on]")

b_df <- map_df(b_results, "pooled_out") %>%
  dplyr::mutate(group = "(b) Out-of-Sample\n[Calibration Sample: routes\nheld out of the draw]")

## Representative split for panel B --------------------------------------------
# Panel B illustrates a single split, so it should show a typical one rather than
# whichever draw happened to come first. Pick the draw whose pooled out-of-sample
# RMSE and correlation are jointly closest to their medians across all draws
# (distances standardised so neither metric dominates), then refit that draw.
rep_draw <- b_df %>%
  dplyr::mutate(dist = abs(rmse - median(rmse, na.rm = TRUE)) / sd(rmse, na.rm = TRUE) +
                        abs(cor - median(cor, na.rm = TRUE)) / sd(cor, na.rm = TRUE)) %>%
  dplyr::slice_min(dist, n = 1) %>%
  dplyr::pull(draw)

rep_routes <- b_results[[rep_draw]]$routes
rep_mod <- feols(kfold_fml, vcov = ~uid,
                 data = route_cal_kf_df[route_cal_kf_df$uid %in% rep_routes, ])
rep_held_out <- !(route_cal_kf_df$uid %in% rep_routes)

rep_scatter_df <- route_cal_kf_df[rep_held_out, ] %>%
  dplyr::mutate(pred = predict_delay_factor(rep_mod, route_cal_kf_df)[rep_held_out]) %>%
  dplyr::select(uid, delay_factor_od, pred)

## (c) Out-of-sample: long panel of 26 routes, never used in estimation ---------
route_26_kf_df <- route_26_kf_df %>% dplyr::mutate(pred_full = predict_delay_factor(mod_full, route_26_kf_df))

# Pooled over all long-panel observations, to match (a) and (b). Drawn as a
# reference line, since it is one value rather than a distribution.
c_pooled_df <- dplyr::tibble(
  metric = c("RMSE", "Correlation"),
  value = c(rmse_fun(route_26_kf_df$delay_factor_od, route_26_kf_df$pred_full),
            cor_fun(route_26_kf_df$delay_factor_od, route_26_kf_df$pred_full))
) %>%
  dplyr::mutate(metric = factor(metric, levels = c("RMSE", "Correlation")),
                label = paste0("(c) Long-Panel = ", sprintf("%.2f", value)))

## Combine and plot ---------------------------------------------------------------
group_levels <- c("(a) Within-Sample\n[Calibration Sample: routes the\ndraw was trained on]",
                   "(b) Out-of-Sample\n[Calibration Sample: routes\nheld out of the draw]")

fit_draws_df <- dplyr::bind_rows(a_df, b_df) %>%
  dplyr::mutate(group = factor(group, levels = rev(group_levels)))

fit_long_df <- fit_draws_df %>%
  tidyr::pivot_longer(cols = c(rmse, cor), names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = recode(metric, rmse = "RMSE", cor = "Correlation"),
                metric = factor(metric, levels = c("RMSE", "Correlation")))

# Single combined label per box (avoids the p25/median/p75 labels overlapping
# each other when their values are close together).
fit_summary <- fit_long_df %>%
  group_by(group, metric) %>%
  summarise(p25 = quantile(value, 0.25, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            p75 = quantile(value, 0.75, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::mutate(label = paste0("P25=", sprintf("%.2f", p25),
                                "  Med=", sprintf("%.2f", median),
                                "  P75=", sprintf("%.2f", p75)))

# A handful of splits sit far outside the bulk of the distribution -- draws in
# which both residential routes land in the held-out half, leaving that class
# absent from estimation. Trim the display range so the boxplots stay legible.
# The P25/median/P75 labels above are computed on all 500 draws regardless, and
# the RMSE cap is kept above the (c) reference line so that line stays visible.
RMSE_DISPLAY_MAX <- 1.5
COR_DISPLAY_MIN  <- 0.4

n_rmse_trimmed <- sum(fit_long_df$metric == "RMSE" & fit_long_df$value > RMSE_DISPLAY_MAX, na.rm = TRUE)
n_cor_trimmed  <- sum(fit_long_df$metric == "Correlation" & fit_long_df$value < COR_DISPLAY_MIN, na.rm = TRUE)

fit_long_plot_df <- fit_long_df %>%
  dplyr::filter(!(metric == "RMSE" & value > RMSE_DISPLAY_MAX),
                !(metric == "Correlation" & value < COR_DISPLAY_MIN))

TEXT_NUDGE <- -0.30

p_box <- fit_long_plot_df %>%
  ggplot(aes(x = value, y = group)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, fill = "gray90") +
  geom_jitter(height = 0.12, width = 0, alpha = 0.18, size = 0.8, color = "dodgerblue") +
  geom_vline(data = c_pooled_df,
             aes(xintercept = value,
                 linetype = "(c) Out-of-Sample [Long-Panel Sample of 26 Routes], pooled"),
             color = "red3", linewidth = 0.6) +
  # Right-aligned just left of the line, so the label never runs off the panel
  # when the (c) value sits near the right edge of a facet's free x scale.
  geom_text(data = c_pooled_df, aes(x = value, y = Inf, label = label),
            color = "red3", size = 2.7, hjust = 1.05, vjust = 1.4,
            inherit.aes = FALSE) +
  scale_x_continuous(expand = expansion(mult = 0.08)) +
  geom_text(data = fit_summary, aes(x = -Inf, label = label),
            color = "gray20", size = 2.8, hjust = -0.02,
            position = position_nudge(y = TEXT_NUDGE)) +
  facet_wrap(~metric, scales = "free_x", ncol = 2) +
  scale_linetype_manual(values = c("(c) Out-of-Sample [Long-Panel Sample of 26 Routes], pooled" = "dashed"),
                        name = NULL) +
  labs(x = NULL, y = NULL,
       title = paste0("A. Pooled RMSE and correlation between observed and predicted delay factor,\n",
                      "across ", N_DRAWS, " repeated 50/50 route splits")) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"))

p_box

# Panel B: scatterplot for one 50/50 split draw (held-out routes only) --------
AXIS_MAX <- 5

rep_stats <- tibble(
  rmse = rmse_fun(rep_scatter_df$delay_factor_od, rep_scatter_df$pred),
  cor  = cor_fun(rep_scatter_df$delay_factor_od, rep_scatter_df$pred)
) %>%
  dplyr::mutate(label = paste0("RMSE = ", sprintf("%.2f", rmse),
                                "\nCorrelation = ", sprintf("%.2f", cor)))

p_rep <- rep_scatter_df %>%
  ggplot(aes(x = delay_factor_od, y = pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.15, size = 0.6, color = "dodgerblue") +
  geom_label(data = rep_stats, aes(x = -Inf, y = Inf, label = label),
             hjust = -0.05, vjust = 1.2, size = 3.2, color = "gray20",
             fill = "white", alpha = 0.85, label.size = NA, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(0, AXIS_MAX), ylim = c(0, AXIS_MAX)) +
  labs(x = "Observed Delay Factor",
       y = "Predicted Delay Factor",
       title = "B. Observed vs. predicted delay factor:\na representative 50/50 split (held-out routes only)") +
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

p_bc <- ggarrange(p_rep, p_bin_rmse, ncol = 2)

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

