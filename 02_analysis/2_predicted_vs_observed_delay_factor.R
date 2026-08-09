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

# Route-level fit statistics ---------------------------------------------------
# Within-route regressions of the observed delay factor on the estimated delay
# factor. Used to flag poorly fitting routes in the regression table below, and
# for the boxplots further down.
cal_df <- map_df(unique(route_cal_df$uid), function(uid_i){
  route_cal_df_i <- route_cal_df[route_cal_df$uid == uid_i,]

  lm1 <- lm(delay_factor_od ~ delay_factor, data = route_cal_df_i)
  data.frame(uid = uid_i,
             fclass = route_cal_df_i$fclass[1],
             delay_factor_sd = route_cal_df_i$delay_factor %>% sd(),
             delay_factor_od_sd = route_cal_df_i$delay_factor_od %>% sd(),
             delay_factor_p95 = route_cal_df_i$delay_factor %>% quantile(0.95, na.rm = T) %>% as.numeric(),
             delay_factor_od_p95 = route_cal_df_i$delay_factor_od %>% quantile(0.95, na.rm = T) %>% as.numeric(),
             delay_factor_od_iqr = route_cal_df_i$delay_factor_od %>% IQR(na.rm = T) %>% as.numeric(),

             r2 = summary(lm1)$r.squared,
             rmse = sqrt(mean(residuals(lm1)^2)))
}) %>%
  dplyr::mutate(fclass = ifelse(fclass == "trunk_fast", "trunk", fclass),
                type = "Calibration Sample [60 Routes]")

r26_df <- map_df(unique(route_26_df$uid), function(uid_i){
  route_26_df_i <- route_26_df[route_26_df$uid == uid_i,]

  lm1 <- lm(delay_factor_od ~ delay_factor, data = route_26_df_i)
  data.frame(uid = uid_i,
             delay_factor_sd = route_26_df_i$delay_factor %>% sd(),
             delay_factor_od_sd = route_26_df_i$delay_factor_od %>% sd(),
             delay_factor_p95 = route_26_df_i$delay_factor %>% quantile(0.95, na.rm = T) %>% as.numeric(),
             delay_factor_od_p95 = route_26_df_i$delay_factor_od %>% quantile(0.95, na.rm = T) %>% as.numeric(),
             delay_factor_od_iqr = route_26_df_i$delay_factor_od %>% IQR(na.rm = T) %>% as.numeric(),
             r2 = summary(lm1)$r.squared,
             rmse = sqrt(mean(residuals(lm1)^2)))
}) %>%
  dplyr::mutate(type = "Long-Panel Sample [26 Routes]")

# Regression -------------------------------------------------------------------
lm_cal <- feols(delay_factor ~ delay_factor_od | uid, data = route_cal_df, vcov = ~uid)
lm_26r <- feols(delay_factor ~ delay_factor_od | uid, data = route_26_df, vcov = ~uid)

etable(
  list(
    "Calibration Sample" = lm_cal,
    "Long-Panel Sample"  = lm_26r
  ),
  dict = c(
    delay_factor = "Delay Factor, Estimated from Traffic Levels",
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


# Boxplots ---------------------------------------------------------------------
#### Make dataframes
combined_r2 <- bind_rows(cal_df %>% mutate(type = "[Within Sample]\nCalibration sample"),
                         r26_df %>% mutate(type = "[Out of Sample]\nLong panel of\n26 routes"))

#### Boxplots
TEXT_NUDGE <- -0.4

r2_summary <- combined_r2 %>%
  group_by(type) %>%
  summarise(
    p25 = quantile(r2, 0.25, na.rm = TRUE),
    median = median(r2, na.rm = TRUE),
    p75 = quantile(r2, 0.75, na.rm = TRUE)
  )

rmse_summary <- combined_r2 %>%
  group_by(type) %>%
  summarise(
    p25 = quantile(rmse, 0.25, na.rm = TRUE),
    median = median(rmse, na.rm = TRUE),
    p75 = quantile(rmse, 0.75, na.rm = TRUE)
  )

p_box_r2 <- combined_r2 %>%
  ggplot(aes(x = r2, y = type)) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    fill = "gray90"
  ) +
  geom_jitter(
    height = 0.12,
    width = 0,
    alpha = 0.5,
    size = 1.5,
    color = "dodgerblue"
  ) +
  geom_text(
    data = r2_summary,
    aes(x = p25, label = sprintf("%.2f", p25)),
    color = "red",
    size = 3,
    position = position_nudge(y = TEXT_NUDGE)
  ) +
  geom_text(
    data = r2_summary,
    aes(x = median, label = sprintf("%.2f", median)),
    color = "red",
    size = 3,
    position = position_nudge(y = TEXT_NUDGE)
  ) +
  geom_text(
    data = r2_summary,
    aes(x = p75, label = sprintf("%.2f", p75)),
    color = "red",
    size = 3,
    position = position_nudge(y = TEXT_NUDGE)
  ) +
  labs(
    x = expression(R^2),
    y = NULL
  ) +
  xlim(0, 1) +
  theme_classic()

p_box_rmse <- combined_r2 %>%
  ggplot(aes(x = rmse, y = type)) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    fill = "gray90"
  ) +
  geom_jitter(
    height = 0.12,
    width = 0,
    alpha = 0.5,
    size = 1.5,
    color = "dodgerblue"
  ) +
  geom_text(
    data = rmse_summary,
    aes(x = p25, label = sprintf("%.2f", p25)),
    color = "red",
    size = 3,
    position = position_nudge(y = TEXT_NUDGE,
                              x = -0.05)
  ) +
  geom_text(
    data = rmse_summary,
    aes(x = median, label = sprintf("%.2f", median)),
    color = "red",
    size = 3,
    position = position_nudge(y = TEXT_NUDGE)
  ) +
  geom_text(
    data = rmse_summary,
    aes(x = p75, label = sprintf("%.2f", p75)),
    color = "red",
    size = 3,
    position = position_nudge(y = TEXT_NUDGE)
  ) +
  labs(
    x = "RMSE",
    y = NULL
  ) +
  theme_classic() +
  xlim(0, 1)

p_box <- ggarrange(
  p_box_r2,
  p_box_rmse,
  ncol = 1
)

p_box <- annotate_figure(
  p_box,
  top = text_grob(
    expression(bold("A. Distribution of within-route " * R^2 * " and RMSE")),
    hjust = 0,
    x = 0
  )
)

# Performance by congestion ----------------------------------------------------
p_r2 <- ggplot() +
  stat_smooth(
    data = cal_df,
    aes(x = delay_factor_od_p95, y = r2),
    method = "loess",
    span = 0.8,
    se = TRUE,
    color = "black",
    fill = "grey70",
    linewidth = 1
  ) +
  geom_point(
    data = cal_df,
    aes(
      x = delay_factor_od_p95,
      y = r2,
      color = "Calibration sample"
    )
  ) +
  geom_point(
    data = r26_df,
    aes(
      x = delay_factor_od_p95,
      y = r2,
      color = "Long panel of\n26 routes"
    )
  ) +
  scale_color_manual(values = c("black", "darkorange")) +
  labs(x = "Delay Factor (Observed), 95th Percentile",
       y = expression(R^2),
       color = "Data",
       title = expression(bold("B."~R^2))) +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(size = 11))

p_rmse <- ggplot() +
  stat_smooth(
    data = cal_df,
    aes(x = delay_factor_od_p95, y = rmse),
    method = "loess",
    span = 0.8,
    se = TRUE,
    color = "black",
    fill = "grey70",
    linewidth = 1
  ) +
  geom_point(
    data = cal_df,
    aes(
      x = delay_factor_od_p95,
      y = rmse,
      color = "Calibration sample"
    )
  ) +
  geom_point(
    data = r26_df,
    aes(
      x = delay_factor_od_p95,
      y = rmse,
      color = "Long panel of\n26 routes"
    )
  ) +
  scale_color_manual(values = c("black", "darkorange")) +
  labs(x = "Delay Factor (Observed), 95th Percentile",
       y = "RMSE",
       color = "Data",
       title = "C. RMSE") +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(size = 11))

p <- annotate_figure(
  ggarrange(
    p_r2, p_rmse,
    common.legend = TRUE,
    legend = "right"
  ),
  top = text_grob(
    expression(
      bolditalic("Comparing within-route " * R^2 * 
                   " and RMSE of observed and estimated delay factor")
    ),
    size = 12,
    hjust = 0,
    x = 0,
    just = "left"
  )
)

# Arrange: Main ----------------------------------------------------------------
p_blank <- ggplot() + theme_void()
p_top <- ggarrange(p_blank, p_box, p_blank, nrow = 1, widths = c(0.1, 0.8, 0.1))

p_main <- ggarrange(p_top, p, ncol = 1)

ggsave(p_main,
       filename = file.path(figures_dir, "observed_vs_predicted_delay_main.png"),
       height = 7, width = 9)

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
  labs(y = "Delay Factor,\nEstimated from Traffic Levels\n[Calibrated using calibration\nsample of 60 routes]",
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
  labs(y = "Delay Factor,\nEstimated from Traffic Levels\n[Calibrated using calibration\nsample of 60 routes]",
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

