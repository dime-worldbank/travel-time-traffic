library(tidyverse)
library(broom)

# Median speed of the Long Panel (V2) routes used in column (7) -------------------
median_speed_col7 <- median(df_sub_v2_05$speed_kmh_uid_max, na.rm = TRUE)
median_speed_col7

# ==================================================================================
# Predicted coefficients from column (4): thresh_0.02_speed (By-Class, w/ interaction)
# ==================================================================================
mod_col4 <- models_combined[["thresh_0.02_speed"]]
vcov_col4 <- vcov(mod_col4)

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
  
  tibble(traffic_level = paste0("Level ", level), speed = speed_val,
         estimate = unname(estimate), se = se,
         conf.low = unname(estimate) - 1.96*se, conf.high = unname(estimate) + 1.96*se)
}

speed_seq <- seq(0, 60, by = 1)
levels_seq <- c(2, 3, 4)

col4_curve <- purrr::cross_df(list(level = levels_seq, speed = speed_seq)) %>%
  purrr::pmap_dfr(function(level, speed) predict_coef_at_speed(mod_col4, vcov_col4, level, speed))

# Point where the predicted curve intersects the Long Panel median speed ----------
col4_at_median_speed <- purrr::map_dfr(levels_seq, function(level) {
  predict_coef_at_speed(mod_col4, vcov_col4, level, median_speed_col7)
})

# ==================================================================================
# Reference coefficients from column (7): v2_thresh_0.05_plain (Long Panel, no interaction)
# ==================================================================================
mod_col7 <- models_combined[["v2_thresh_0.05_plain"]]

col7_ref <- tidy(mod_col7) %>%
  filter(term %in% c("tl_prop_2", "tl_prop_3", "tl_prop_4")) %>%
  mutate(traffic_level = recode(term,
                                tl_prop_2 = "Level 2",
                                tl_prop_3 = "Level 3",
                                tl_prop_4 = "Level 4"),
         conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error)

# ==================================================================================
# Figure
# ==================================================================================
ggplot() +
  geom_ribbon(data = col4_curve, aes(x = speed, ymin = conf.low, ymax = conf.high, fill = traffic_level),
              alpha = 0.15) +
  geom_line(data = col4_curve, aes(x = speed, y = estimate, color = traffic_level), linewidth = 0.9) +
  geom_hline(data = col7_ref, aes(yintercept = estimate, color = traffic_level),
             linetype = "dashed", linewidth = 0.7) +
  geom_rect(data = col7_ref, aes(ymin = conf.low, ymax = conf.high, xmin = -Inf, xmax = Inf, fill = traffic_level),
            alpha = 0.08) +
  geom_vline(xintercept = median_speed_col7, linetype = "dotted", color = "gray30", linewidth = 0.7) +
  geom_point(data = col4_at_median_speed, aes(x = speed, y = estimate, color = traffic_level),
             size = 3) +
  annotate("text", x = median_speed_col7, y = Inf, label = "Long Panel\nmedian speed",
           vjust = 1.3, hjust = 1.05, size = 3, color = "gray30", fontface = "italic") +
  facet_wrap(~ traffic_level, scales = "free_y") +
  labs(x = "Route 95th-pct. speed (km/h)",
       y = "Predicted coefficient",
       title = "Predicted traffic-level coefficients by speed: Calibration (col. 4) vs. Long Panel (col. 7)",
       subtitle = "Solid line + shaded band: Calibration, speed-varying (95% CI).\nDashed line + band: Long Panel, flat (95% CI)",
       color = "Traffic Level", fill = "Traffic Level") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = file.path(figures_dir, "coef_by_speed_col4_vs_col7.png"),
       height = 5, width = 10)