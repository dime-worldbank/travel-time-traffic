library(tidyverse)
library(fixest)
library(janitor)
library(lubridate)
library(broom)

# ==================================================================================
# Dataset 1: route_df (by-class, 57 routes)
# ==================================================================================
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
  filter(hour >= 6, hour <= 21)

route_df <- route_df %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_3_max = quantile(tl_prop_3, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_4_max = quantile(tl_prop_4, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_4_sd = sd(tl_prop_4, na.rm = T) %>% as.numeric()) %>%
  ungroup()

route_variation_df <- route_df %>%
  group_by(uid) %>%
  summarise(
    fclass = first(fclass),
    within_sd_prop3 = sd(tl_prop_3, na.rm = TRUE),
    within_sd_prop4 = sd(tl_prop_4, na.rm = TRUE),
    share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
    share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
    .groups = "drop"
  )

# ==================================================================================
# Dataset 2: route_df_v2 (26 O-D routes, "Long Panel")
# ==================================================================================
route_df_v2 <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))

route_df_v2 <- route_df_v2 %>%
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

route_df_v2 <- route_df_v2 %>%
  filter(hour >= 6, hour <= 21)

route_df_v2 <- route_df_v2 %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_3_max = quantile(tl_prop_3, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_4_max = quantile(tl_prop_4, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_4_sd = sd(tl_prop_4, na.rm = T) %>% as.numeric()) %>%
  ungroup()

route_variation_df_v2 <- route_df_v2 %>%
  group_by(uid) %>%
  summarise(
    within_sd_prop3 = sd(tl_prop_3, na.rm = TRUE),
    within_sd_prop4 = sd(tl_prop_4, na.rm = TRUE),
    share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
    share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
    .groups = "drop"
  )

# ==================================================================================
# Shared helper functions
# ==================================================================================
all_classes <- c("trunk", "primary", "secondary", "tertiary", "residential", "unclassified")

joint_f_level <- function(mod, level) {
  main_term <- paste0("tl_prop_", level)
  interact_term <- paste0("tl_prop_", level, ":speed_kmh_uid_max")
  if (!(interact_term %in% names(coef(mod)))) return(c(stat = NA, p = NA))
  w <- tryCatch(fixest::wald(mod, paste0(main_term, "|", interact_term)),
                error = function(e) NULL)
  if (is.null(w)) return(c(stat = NA, p = NA))
  c(stat = unname(w$stat), p = unname(w$p))
}

fit_pair <- function(df_sub) {
  mod_plain <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid,
                     vcov = ~ uid, data = df_sub)
  mod_speed <- feols(
    tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 +
      tl_prop_2:speed_kmh_uid_max + tl_prop_3:speed_kmh_uid_max + tl_prop_4:speed_kmh_uid_max | uid,
    vcov = ~ uid, data = df_sub)
  list(plain = mod_plain, speed = mod_speed)
}

compute_stats <- function(mod, df_sub, has_class = TRUE) {
  fw2 <- joint_f_level(mod, 2)
  fw3 <- joint_f_level(mod, 3)
  fw4 <- joint_f_level(mod, 4)
  
  base_stats <- tibble(
    n_obs = nrow(df_sub),
    n_routes = n_distinct(df_sub$uid),
    p95_speed = median(df_sub$speed_kmh_uid_max, na.rm = TRUE),
    joint_f_prop2_stat = unname(fw2["stat"]),
    joint_f_prop2_pval = unname(fw2["p"]),
    joint_f_prop3_stat = unname(fw3["stat"]),
    joint_f_prop3_pval = unname(fw3["p"]),
    joint_f_prop4_stat = unname(fw4["stat"]),
    joint_f_prop4_pval = unname(fw4["p"])
  )
  
  if (has_class) {
    n_by_class <- df_sub %>% distinct(uid, fclass) %>% count(fclass) %>%
      complete(fclass = all_classes, fill = list(n = 0)) %>%
      deframe()
    base_stats <- base_stats %>%
      mutate(
        n_routes_trunk = n_by_class[["trunk"]],
        n_routes_primary = n_by_class[["primary"]],
        n_routes_secondary = n_by_class[["secondary"]],
        n_routes_tertiary = n_by_class[["tertiary"]],
        n_routes_residential = n_by_class[["residential"]],
        n_routes_unclassified = n_by_class[["unclassified"]]
      )
  }
  base_stats
}

# ==================================================================================
# Build models: route_df (thresholds 0, 0.02, 0.05) + route_df_v2 (threshold 0.05 only)
# ==================================================================================
thresholds <- c(0, 0.02, 0.05)

threshold_results <- purrr::map(thresholds, function(thresh) {
  routes_keep <- route_variation_df %>%
    filter(share_prop3_gt0 >= thresh, share_prop4_gt0 >= thresh) %>%
    pull(uid)
  df_sub <- route_df %>% filter(uid %in% routes_keep)
  mods <- fit_pair(df_sub)
  list(
    plain = list(model = mods$plain, stats = compute_stats(mods$plain, df_sub, has_class = TRUE)),
    speed = list(model = mods$speed, stats = compute_stats(mods$speed, df_sub, has_class = TRUE))
  )
})
names(threshold_results) <- paste0("thresh_", thresholds)

routes_keep_v2 <- route_variation_df_v2 %>%
  filter(share_prop3_gt0 >= 0.05, share_prop4_gt0 >= 0.05) %>%
  pull(uid)
df_sub_v2_05 <- route_df_v2 %>% filter(uid %in% routes_keep_v2)
mods_v2_05 <- fit_pair(df_sub_v2_05)
stats_v2_05_plain <- compute_stats(mods_v2_05$plain, df_sub_v2_05, has_class = FALSE)
stats_v2_05_speed <- compute_stats(mods_v2_05$speed, df_sub_v2_05, has_class = FALSE)

models_combined <- list(
  "thresh_0_plain"     = threshold_results[["thresh_0"]]$plain$model,
  "thresh_0_speed"     = threshold_results[["thresh_0"]]$speed$model,
  "thresh_0.02_plain"  = threshold_results[["thresh_0.02"]]$plain$model,
  "thresh_0.02_speed"  = threshold_results[["thresh_0.02"]]$speed$model,
  "thresh_0.05_plain"  = threshold_results[["thresh_0.05"]]$plain$model,
  "thresh_0.05_speed"  = threshold_results[["thresh_0.05"]]$speed$model,
  "v2_thresh_0.05_plain" = mods_v2_05$plain,
  "v2_thresh_0.05_speed" = mods_v2_05$speed
)

stats_combined_list <- list(
  "thresh_0_plain"     = threshold_results[["thresh_0"]]$plain$stats,
  "thresh_0_speed"     = threshold_results[["thresh_0"]]$speed$stats,
  "thresh_0.02_plain"  = threshold_results[["thresh_0.02"]]$plain$stats,
  "thresh_0.02_speed"  = threshold_results[["thresh_0.02"]]$speed$stats,
  "thresh_0.05_plain"  = threshold_results[["thresh_0.05"]]$plain$stats,
  "thresh_0.05_speed"  = threshold_results[["thresh_0.05"]]$speed$stats,
  "v2_thresh_0.05_plain" = stats_v2_05_plain,
  "v2_thresh_0.05_speed" = stats_v2_05_speed
)

stats_combined_df <- bind_rows(stats_combined_list, .id = "model_id") %>%
  mutate(model_id = factor(model_id, levels = names(models_combined))) %>%
  arrange(model_id)

print(stats_combined_df, width = Inf)
write.csv(stats_combined_df, file.path(tables_dir, "traffic_level_combined_diagnostics.csv"), row.names = FALSE)

# ==================================================================================
# Regression table: ols_calibration_threshold_x_speed.tex
# ==================================================================================
my_style = style.tex(tpt = TRUE, notes.tpt.intro = "\\footnotesize")
setFixest_etable(style.tex = my_style)

dict = c(tt_hour_per_km_ln = "Travel time (hours) per kilometer, logged",
         tl_prop_2 = "Prop traffic level 2",
         tl_prop_3 = "Prop traffic level 3",
         tl_prop_4 = "Prop traffic level 4",
         "tl_prop_2:speed_kmh_uid_max" = "Prop traffic level 2 $\\times$ Speed (km/h)",
         "tl_prop_3:speed_kmh_uid_max" = "Prop traffic level 3 $\\times$ Speed (km/h)",
         "tl_prop_4:speed_kmh_uid_max" = "Prop traffic level 4 $\\times$ Speed (km/h)",
         uid = "Route")
setFixest_dict(dict)

stat_row_c <- function(varname, digits = 3) {
  vals <- stats_combined_df %>% pull(!!sym(varname))
  ifelse(is.na(vals), "", as.character(round(vals, digits)))
}
stat_row_c_pval <- function(varname) {
  vals <- stats_combined_df %>% pull(!!sym(varname))
  ifelse(is.na(vals), "", ifelse(vals < 0.001, "<0.001", as.character(round(vals, 3))))
}

dataset_row <- c(rep("Calibration", 6), rep("Long Panel", 2))
threshold_row <- c(as.character(rep(thresholds, each = 2)), "0.05", "0.05")

esttex(models_combined[[1]], models_combined[[2]], models_combined[[3]], models_combined[[4]],
       models_combined[[5]], models_combined[[6]], models_combined[[7]], models_combined[[8]],
       float = F,
       replace = T,
       extralines = list(
         
         "_ \\midrule \\emph{Sample}" = rep("", 8),
         "_Dataset"                   = dataset_row,
         "_Route-Inclusion Threshold" = threshold_row,
         "_N Obs"                     = stat_row_c("n_obs", 0),
         "_N Routes"                  = stat_row_c("n_routes", 0),
         "_95th Pct. Speed (km/h)"    = stat_row_c("p95_speed", 1),
         
         "_ \\midrule \\emph{Model Fit}" = rep("", 8),
         "_Joint F-stat (Prop2 = Prop2xSpeed = 0)" = stat_row_c("joint_f_prop2_stat", 1),
         "_Joint F p-value (Prop2)"   = stat_row_c_pval("joint_f_prop2_pval"),
         "_Joint F-stat (Prop3 = Prop3xSpeed = 0)" = stat_row_c("joint_f_prop3_stat", 1),
         "_Joint F p-value (Prop3)"   = stat_row_c_pval("joint_f_prop3_pval"),
         "_Joint F-stat (Prop4 = Prop4xSpeed = 0)" = stat_row_c("joint_f_prop4_stat", 1),
         "_Joint F p-value (Prop4)"   = stat_row_c_pval("joint_f_prop4_pval")
       ),
       notes = "Route-Inclusion Threshold indicates the minimum share of a route's observed hours in which the route shows any exposure to traffic level 3 and any exposure to traffic level 4; routes falling below this threshold on either measure are excluded from the estimation sample. Higher thresholds retain routes with more reliable within-route variation in high-congestion states, at the cost of a smaller sample.",
       file = file.path(tables_dir, "ols_calibration_threshold_x_speed.tex"))


# ==================================================================================
# Save calibration coefficients from column (6): thresh_0.05_speed
# ==================================================================================
beta <- coef(models_combined[["thresh_0.05_speed"]])
beta

saveRDS(beta, file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))


# ==================================================================================
# Figure: Predicted coefficients by speed -- Column (6) vs. Column (7)
# ==================================================================================
mean_speed_col7 <- mean(df_sub_v2_05$speed_kmh_uid_max, na.rm = TRUE)
mean_speed_col7

# Predicted coefficients from column (6): thresh_0.05_speed (Calibration, w/ interaction)
mod_col6 <- models_combined[["thresh_0.05_speed"]]
vcov_col6 <- vcov(mod_col6)

predict_coef_at_speed <- function(mod, vcov_mat, level, speed_val) {
  main_term <- paste0("tl_prop_", level)
  interact_term <- paste0("tl_prop_", level, ":speed_kmh_uid_max")
  
  b_main <- coef(mod)[main_term]
  b_int  <- coef(mod)[interact_term]
  estimate <- b_main + b_int * speed_val
  
  var_main <- vcov_mat[main_term, main_term]
  var_int  <- vcov_mat[interact_term, interact_term]
  cov_main_int <- vcov_mat[main_term, interact_term]
  se <- sqrt(var_main + (speed_val^2) * var_int + 2 * speed_val * cov_main_int)
  
  tibble(traffic_level = paste0("Level ", level, " Traffic"), speed = speed_val,
         estimate = unname(estimate), se = se,
         conf.low = unname(estimate) - 1.96*se, conf.high = unname(estimate) + 1.96*se)
}

speed_seq <- seq(0, 60, by = 1)
levels_seq <- c(2, 3, 4)

col6_curve <- purrr::cross_df(list(level = levels_seq, speed = speed_seq)) %>%
  purrr::pmap_dfr(function(level, speed) predict_coef_at_speed(mod_col6, vcov_col6, level, speed))

# Point where the predicted curve intersects the Long Panel mean speed -----------
col6_at_mean_speed <- purrr::map_dfr(levels_seq, function(level) {
  predict_coef_at_speed(mod_col6, vcov_col6, level, mean_speed_col7)
})

# Reference coefficients from column (7): v2_thresh_0.05_plain (Long Panel, no interaction)
mod_col7 <- models_combined[["v2_thresh_0.05_plain"]]

col7_ref <- tidy(mod_col7) %>%
  filter(term %in% c("tl_prop_2", "tl_prop_3", "tl_prop_4")) %>%
  mutate(traffic_level = recode(term,
                                tl_prop_2 = "Level 2 Traffic",
                                tl_prop_3 = "Level 3 Traffic",
                                tl_prop_4 = "Level 4 Traffic"),
         conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error)

# Figure ----------------------------------------------------------------------------
# Note: scales default to shared/fixed across facets (no scales = "free_y")
ggplot() +
  geom_ribbon(data = col6_curve, aes(x = speed, ymin = conf.low, ymax = conf.high, fill = traffic_level),
              alpha = 0.15) +
  geom_line(data = col6_curve, aes(x = speed, y = estimate, color = traffic_level), linewidth = 0.9) +
  geom_hline(data = col7_ref, aes(yintercept = estimate, color = traffic_level),
             linetype = "dashed", linewidth = 0.7) +
  geom_rect(data = col7_ref, aes(ymin = conf.low, ymax = conf.high, xmin = -Inf, xmax = Inf, fill = traffic_level),
            alpha = 0.08) +
  geom_vline(xintercept = mean_speed_col7, linetype = "dotted", color = "gray30", linewidth = 0.7) +
  geom_point(data = col6_at_mean_speed, aes(x = speed, y = estimate, color = traffic_level),
             size = 3) +
  annotate("text", x = mean_speed_col7, y = Inf, label = "Long Panel\nmean speed",
           vjust = 1.3, hjust = 1.05, size = 3, color = "gray30", fontface = "italic") +
  facet_wrap(~ traffic_level) +
  labs(x = "Route 95th-pct. speed (km/h)",
       y = "Predicted coefficient",
       title = "Predicted traffic-level coefficients by speed: Calibration (col. 6) vs. Long Panel (col. 7)",
       subtitle = "Solid line + shaded band: Calibration, speed-varying (95% CI).\nDashed line + band: Long Panel, flat (95% CI)",
       color = "Traffic Level", fill = "Traffic Level") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = file.path(figures_dir, "coef_by_speed_col6_vs_col7.png"),
       height = 5, width = 10)