library(tidyverse)
library(fixest)
library(janitor)
library(lubridate)

# Load data (Version 2: 26 O-D routes) --------------------------------------------
route_df_v2 <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))

# Clean data -----------------------------------------------------------------------
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

# Restrict to 6am-9pm (hour 6 through 21 inclusive) BEFORE computing route-level 
# stats, so all downstream diagnostics reflect this restricted window
route_df_v2 <- route_df_v2 %>%
  filter(hour >= 6, hour <= 21)

route_df_v2 <- route_df_v2 %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_3_max = quantile(tl_prop_3, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_4_max = quantile(tl_prop_4, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_4_sd = sd(tl_prop_4, na.rm = T) %>% as.numeric()) %>%
  ungroup()

# Route-level variation diagnostics (used to build the threshold-based samples) ---
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
# Table: Pooled + Pooled-with-Speed-Interaction, across 3 thresholds (6 models)
# 6am-9pm restricted sample -- V2 (O-D routes)
# ==================================================================================
thresholds <- c(0, 0.02, 0.05)

joint_f_level <- function(mod, level) {
  main_term <- paste0("tl_prop_", level)
  interact_term <- paste0("tl_prop_", level, ":speed_kmh_uid_max")
  
  if (!(interact_term %in% names(coef(mod)))) {
    return(c(stat = NA, p = NA))
  }
  
  w <- tryCatch(fixest::wald(mod, paste0(main_term, "|", interact_term)),
                error = function(e) NULL)
  if (is.null(w)) return(c(stat = NA, p = NA))
  c(stat = unname(w$stat), p = unname(w$p))
}

threshold_results_v2 <- purrr::map(thresholds, function(thresh) {
  
  routes_keep <- route_variation_df_v2 %>%
    filter(share_prop3_gt0 >= thresh, share_prop4_gt0 >= thresh) %>%
    pull(uid)
  
  df_sub <- route_df_v2 %>% filter(uid %in% routes_keep)
  
  mod_plain <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid,
                     vcov = ~ uid, data = df_sub)
  
  mod_speed <- feols(
    tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 +
      tl_prop_2:speed_kmh_uid_max + tl_prop_3:speed_kmh_uid_max + tl_prop_4:speed_kmh_uid_max | uid,
    vcov = ~ uid, data = df_sub)
  
  compute_stats <- function(mod, df_sub) {
    fw2 <- joint_f_level(mod, 2)
    fw3 <- joint_f_level(mod, 3)
    fw4 <- joint_f_level(mod, 4)
    
    tibble(
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
  }
  
  list(
    plain = list(model = mod_plain, stats = compute_stats(mod_plain, df_sub)),
    speed = list(model = mod_speed, stats = compute_stats(mod_speed, df_sub))
  )
})
names(threshold_results_v2) <- paste0("thresh_", thresholds)

models_v2 <- list()
stats_v2_list <- list()
for (t in paste0("thresh_", thresholds)) {
  models_v2[[paste0(t, "_plain")]] <- threshold_results_v2[[t]]$plain$model
  models_v2[[paste0(t, "_speed")]] <- threshold_results_v2[[t]]$speed$model
  stats_v2_list[[paste0(t, "_plain")]] <- threshold_results_v2[[t]]$plain$stats
  stats_v2_list[[paste0(t, "_speed")]] <- threshold_results_v2[[t]]$speed$stats
}

stats_v2_df <- bind_rows(stats_v2_list, .id = "model_id") %>%
  mutate(model_id = factor(model_id, levels = names(models_v2))) %>%
  arrange(model_id)

print(stats_v2_df, width = Inf)
write.csv(stats_v2_df, file.path(tables_dir, "traffic_level_threshold_x_speed_diagnostics_v2_peak.csv"), row.names = FALSE)

# Regression table -----------------------------------------------------------------
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

stat_row_v2 <- function(varname, digits = 3) {
  vals <- stats_v2_df %>% pull(!!sym(varname))
  ifelse(is.na(vals), "", as.character(round(vals, digits)))
}
stat_row_v2_pval <- function(varname) {
  vals <- stats_v2_df %>% pull(!!sym(varname))
  ifelse(is.na(vals), "", ifelse(vals < 0.001, "<0.001", as.character(round(vals, 3))))
}

esttex(models_v2[[1]], models_v2[[2]], models_v2[[3]], models_v2[[4]],
       models_v2[[5]], models_v2[[6]],
       float = F,
       replace = T,
       extralines = list(
         
         "_ \\midrule \\emph{Sample}" = rep("", 6),
         "_Route-Inclusion Threshold"  = as.character(rep(thresholds, each = 2)),
         "_N Obs"                     = stat_row_v2("n_obs", 0),
         "_N Routes"                  = stat_row_v2("n_routes", 0),
         "_95th Pct. Speed (km/h)"    = stat_row_v2("p95_speed", 1),
         
         "_ \\midrule \\emph{Model Fit}" = rep("", 6),
         "_Joint F-stat (Prop2 = Prop2xSpeed = 0)" = stat_row_v2("joint_f_prop2_stat", 1),
         "_Joint F p-value (Prop2)"   = stat_row_v2_pval("joint_f_prop2_pval"),
         "_Joint F-stat (Prop3 = Prop3xSpeed = 0)" = stat_row_v2("joint_f_prop3_stat", 1),
         "_Joint F p-value (Prop3)"   = stat_row_v2_pval("joint_f_prop3_pval"),
         "_Joint F-stat (Prop4 = Prop4xSpeed = 0)" = stat_row_v2("joint_f_prop4_stat", 1),
         "_Joint F p-value (Prop4)"   = stat_row_v2_pval("joint_f_prop4_pval")
       ),
       file = file.path(tables_dir, "ols_calibration_threshold_x_speed_v2.tex"))