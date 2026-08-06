# Predicted vs Observed Delay Factor

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
    delay_factor = "Delay Factor, Estimated from Traffic Levels",
    delay_factor_od = "Observed Delay Factor from O-D Travel Time Data",
    uid = "Route"
  ),
  extralines = list(
    "-Observations" = c(
      format(nobs(lm_cal), big.mark = ","),
      format(nobs(lm_26r), big.mark = ",")
    ),
    "-N Routes" = c(lm_cal$fixef_sizes[["uid"]], lm_26r$fixef_sizes[["uid"]]),
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

#### Make dataframes
combined_r2 <- bind_rows(cal_df %>% mutate(type = "Calibration sample"),
                         r26_df %>% mutate(type = "Long panel of\n26 routes"))

p_box_r2 <- combined_r2 %>%
  ggplot() +
  geom_boxplot(aes(x = r2,
                   y = type))

p_box_rmse <- combined_r2 %>%
  ggplot() +
  geom_boxplot(aes(x = rmse,
                   y = type))

p_bax <- ggarrange(p_box_r2,
                   p_box_rmse, ncol = 1)
p_bax

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
       y = "R^2",
       color = "Data",
       title = "A. R^2") +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"))

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
       title = "B. RMSE") +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"))

p <- annotate_figure(
  ggarrange(p_r2, p_rmse,
            common.legend = TRUE, legend = "right"),
  top = text_grob(
    "Comparing within route R^2 RMSE of observed and estimated delay factor\nwith 95th percentile of observed delay",
    #face = "bold",
    size = 14,
    hjust = 0,
    x = 0
  )
)

p

# Scatterplots -----------------------------------------------------------------
AXIS_MAX <- 3

route_cal_df %>%
  ggplot(aes(x = delay_factor_od, y = delay_factor)) +
  geom_point() +
  coord_cartesian(xlim = c(0, AXIS_MAX), ylim = c(0, AXIS_MAX))

route_26_df %>%
  ggplot(aes(x = delay_factor_od, y = delay_factor)) +
  geom_point(size = 0.1) +
  coord_cartesian(xlim = c(0, AXIS_MAX), ylim = c(0, AXIS_MAX))








ggplot(combined_r2, aes(x = fclass_label, y = r2, fill = fclass_label)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6) +
  annotate("segment", x = 0.8, xend = 6.2, y = 0.95, yend = 0.95,
           linewidth = 0.6, color = "gray40") +
  annotate("segment", x = 0.8, xend = 0.8, y = 0.95, yend = 1.05,
           linewidth = 0.6, color = "gray40") +
  annotate("segment", x = 6.2, xend = 6.2, y = 0.95, yend = 1.05,
           linewidth = 0.6, color = "gray40") +
  annotate("text", x = 3.5, y = 1.08, label = "Routes\nused for\ncalibration",
           size = 3.5, fontface = "italic", hjust = 0) +
  scale_y_continuous(limits = c(0, 1.2), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = expression(R^2*" (Observed vs. Predicted Delay Factor)")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(10, 40, 10, 10),
        axis.text = element_text(color = "black", size = 12))
ggsave(filename = file.path(figures_dir, "r2_boxplot_byclass_and_v2.png"),
       height = 4, width = 8)

# Figures ----------------------------------------------------------------------
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

