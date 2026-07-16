# Long Sample - testing different calibration coefficients

# Load data --------------------------------------------------------------------
route_df <- readRDS(
  file.path(analysis_data_dir, "google_routes.Rds")
)

beta_df <- readRDS(
  file.path(data_dir, "Calibration Coefficients", "coefs.Rds")
)

# Delay factors ----------------------------------------------------------------
route_df <- route_df %>%
  mk_traffic_indicators(beta) %>%
  dplyr::mutate(
    delay_factor_within =
      exp(tl_prop_2 * 1.378 +
            tl_prop_3 * 2.869 +
            tl_prop_4 * 5.799),
    dur_per_km = duration_in_traffic_s / distance_m
  )

# Table: ------------------------------------------------------------------------
lm1 <- feols(
  delay_factor_within ~ delay_factor,
  vcov = ~uid,
  data = route_df
)

lm2 <- feols(
  delay_factor_within ~ delay_factor | uid,
  vcov = ~uid,
  data = route_df
)

lm3 <- feols(
  duration_in_traffic_s ~ delay_factor_within | uid,
  vcov = ~uid,
  data = route_df
)

lm4 <- feols(
  duration_in_traffic_s ~ delay_factor | uid,
  vcov = ~uid,
  data = route_df
)


# Register a custom fit statistic for the number of routes
fixest::fitstat_register(
  "nroutes",
  function(x) dplyr::n_distinct(route_df$uid),
  "N Routes"
)

etable(
  list(
    "Without Route FE" = lm1,
    "With Route FE" = lm2,
    "Duration ~ Within DF" = lm3,
    "Duration ~ Calib DF" = lm4
  ),
  dict = c(
    delay_factor = "Delay Factor: Calibration Sample",
    delay_factor_within = "Delay Factor: Within Sample",
    duration_in_traffic_s = "Duration in Traffic (sec)",
    uid = "Route"
  ),
  extralines = list(
    "_^Route FEs" = c("No", "Yes", "Yes", "Yes")
  ),
  # Places N Routes immediately after Observations
  fitstat = ~ n + nroutes + r2 + ar2,
  drop.section = "fixef",
  replace = TRUE,
  float = FALSE,
  file = file.path(
    tables_dir,
    "longpanel_calib_compare.tex"
  )
)

# Figure: Delay factors --------------------------------------------------------
route_levels <- paste0("Route ID: ", 1:26)

route_plot_df <- route_df %>%
  dplyr::mutate(
    uid = factor(
      paste0("Route ID: ", uid),
      levels = route_levels
    )
  )

route_stats_df <- route_df %>%
  dplyr::group_by(uid) %>%
  dplyr::summarise(
    r2 = summary(lm(delay_factor_within ~ delay_factor))$r.squared,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    uid = factor(
      paste0("Route ID: ", uid),
      levels = route_levels
    ),
    x = 0.4,
    y = 9.6,
    label = sprintf("R² = %.3f", r2)
  )

p <- route_plot_df %>%
  ggplot(aes(x = delay_factor,
             y = delay_factor_within)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_point() +
  geom_text(
    data = route_stats_df,
    aes(x = x,
        y = y,
        label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = -2,
    size = 3
  ) +
  coord_cartesian(
    xlim = c(0, 15),
    ylim = c(0, 15)
  ) +
  labs(
    x = "Delay Factor: Calibration Sample",
    y = "Delay\nFactor:\nWithin\nSample"
  ) +
  facet_wrap(~uid) +
  theme(
    strip.background = element_blank(),
    axis.title.y = element_text(
      vjust = 0.5,
      angle = 0
    )
  )

ggsave(
  filename = file.path(
    figures_dir,
    "longpanel_calib_compare.png"
  ),
  plot = p,
  width = 8,
  height = 7
)

# Figure: Duration ~ Delay factor, calibration- and within-sample (lm3 & lm4) ----
sample_colors <- c(
  "Calibration Sample" = "#2a78d6",
  "Within Sample"      = "#eb6834"
)

route_stats_duration_df <- dplyr::bind_rows(
  route_df %>%
    dplyr::group_by(uid) %>%
    dplyr::summarise(
      r2 = summary(lm(dur_per_km ~ delay_factor))$r.squared,
      .groups = "drop"
    ) %>%
    dplyr::mutate(sample_type = "Calibration Sample"),
  route_df %>%
    dplyr::group_by(uid) %>%
    dplyr::summarise(
      r2 = summary(lm(dur_per_km ~ delay_factor_within))$r.squared,
      .groups = "drop"
    ) %>%
    dplyr::mutate(sample_type = "Within Sample")
) %>%
  dplyr::mutate(
    uid = factor(
      paste0("Route ID: ", uid),
      levels = route_levels
    ),
    sample_type = factor(sample_type, levels = names(sample_colors)),
    label = sprintf("R² = %.3f", r2)
  ) %>%
  dplyr::arrange(uid, sample_type) %>%
  dplyr::group_by(uid) %>%
  dplyr::mutate(label_vjust = 1.4 + 1.6 * (dplyr::row_number() - 1)) %>%
  dplyr::ungroup()

route_plot_duration_df <- dplyr::bind_rows(
  route_plot_df %>%
    dplyr::transmute(uid, dur_per_km,
                     delay_factor_value = delay_factor,
                     sample_type = "Calibration Sample"),
  route_plot_df %>%
    dplyr::transmute(uid, dur_per_km,
                     delay_factor_value = delay_factor_within,
                     sample_type = "Within Sample")
) %>%
  dplyr::mutate(sample_type = factor(sample_type, levels = names(sample_colors)))

p_duration <- route_plot_duration_df %>%
  ggplot(aes(x = delay_factor_value,
             y = dur_per_km,
             color = sample_type)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_text(
    data = route_stats_duration_df,
    aes(x = -Inf, y = Inf, label = label, color = sample_type, vjust = label_vjust),
    inherit.aes = FALSE,
    hjust = -1,
    size = 2.7,
    show.legend = FALSE
  ) +
  scale_color_manual(values = sample_colors, name = NULL) +
  labs(
    x = "Delay Factor",
    y = "Travel\ntime\n(seconds)\nper\nmeter"
  ) +
  facet_wrap(~uid) +
  xlim(0, 5) +
  ylim(0.04, 0.25) +
  theme(
    strip.background = element_blank(),
    legend.position = "bottom",
    axis.title.y = element_text(
      vjust = 0.5,
      angle = 0
    )
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

ggsave(
  filename = file.path(
    figures_dir,
    "longpanel_calib_compare_duration.png"
  ),
  plot = p_duration,
  width = 8,
  height = 7.5
)

# Figure: Boxplot of per-route R² by sample --------------------------------------
p_r2_box <- route_stats_duration_df %>%
  ggplot(aes(x = sample_type, y = r2, fill = sample_type)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.08, alpha = 0.6, size = 1.5) +
  scale_fill_manual(values = sample_colors, guide = "none") +
  labs(
    x = NULL,
    y = expression(R^2)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold")
  )

ggsave(
  filename = file.path(
    figures_dir,
    "longpanel_calib_compare_duration_r2_boxplot.png"
  ),
  plot = p_r2_box,
  width = 5,
  height = 5
)
