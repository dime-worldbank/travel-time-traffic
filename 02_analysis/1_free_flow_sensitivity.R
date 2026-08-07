# Free Flow Sensitivity

# Elsewhere in the paper a route's free-flow speed is the 99th percentile of
# observed speed during night hours (1-4am) -- equivalently, the 1st percentile
# of travel time. This script recomputes that definition alongside alternatives
# (minimum travel time, and the 2nd through 5th percentiles) and shows how
# sensitive the resulting free-flow speed is to that choice.

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))

route_df <- route_df %>%
  dplyr::mutate(speed_kmh = speed_in_traffic_kmh,
                hour = datetime %>% hour())

# Free-flow speed definitions --------------------------------------------------
# Each definition is a percentile of travel time. Because speed is inversely
# related to travel time, the p-th percentile of travel time is the (100-p)-th
# percentile of speed; the definitions are applied to speed, as in the rest of
# the paper. Night hours (1-4am) are held fixed across definitions.
ff_class_levels <- c("trunk", "primary", "secondary", "tertiary",
                     "residential", "unclassified")

ff_def_baseline <- "1st pct."

ff_defs_df <- tibble::tribble(
  ~def_label,  ~speed_prob,
  "Minimum",   1.00,
  "1st pct.",  0.99,
  "2nd pct.",  0.98,
  "3rd pct.",  0.97,
  "4th pct.",  0.96,
  "5th pct.",  0.95
)

ff_df <- map_df(1:nrow(ff_defs_df), function(i){

  route_df %>%
    group_by(uid, fclass) %>%
    dplyr::summarise(speed_ff = quantile(speed_kmh[hour %in% 1:4],
                                         prob = ff_defs_df$speed_prob[i],
                                         na.rm = T) %>% as.numeric(),
                     .groups = "drop") %>%
    dplyr::mutate(def_label  = ff_defs_df$def_label[i],
                  speed_prob = ff_defs_df$speed_prob[i])

})

ff_df <- ff_df %>%
  dplyr::mutate(def_label = factor(def_label, levels = ff_defs_df$def_label),
                fclass    = fclass %>% as.character() %>% factor(levels = ff_class_levels)) %>%
  dplyr::filter(!is.na(speed_ff))

#### Percent difference from the definition used in the paper
ff_base_df <- ff_df %>%
  dplyr::filter(def_label %in% ff_def_baseline) %>%
  dplyr::select(uid, speed_ff_base = speed_ff)

ff_df <- ff_df %>%
  left_join(ff_base_df, by = "uid") %>%
  dplyr::mutate(pct_diff = 100 * (speed_ff - speed_ff_base) / speed_ff_base)

# Summary table ----------------------------------------------------------------
ff_summary_df <- bind_rows(

  ff_df %>%
    group_by(def_label) %>%
    dplyr::summarise(fclass        = "All routes",
                     n_routes      = n(),
                     speed_mean    = mean(speed_ff, na.rm = T),
                     speed_median  = median(speed_ff, na.rm = T),
                     pct_diff_mean = mean(pct_diff, na.rm = T),
                     .groups = "drop"),

  ff_df %>%
    group_by(def_label, fclass) %>%
    dplyr::summarise(n_routes      = n(),
                     speed_mean    = mean(speed_ff, na.rm = T),
                     speed_median  = median(speed_ff, na.rm = T),
                     pct_diff_mean = mean(pct_diff, na.rm = T),
                     .groups = "drop") %>%
    dplyr::mutate(fclass = as.character(fclass))

) %>%
  arrange(fclass, def_label)

print(ff_summary_df, n = Inf)

write.csv(ff_summary_df,
          file.path(tables_dir, "free_flow_sensitivity.csv"),
          row.names = FALSE)

# Figure -----------------------------------------------------------------------
#### Highlight the definition used in the paper
ff_fill_values <- setNames(
  ifelse(ff_defs_df$def_label %in% ff_def_baseline, "dodgerblue", "gray80"),
  ff_defs_df$def_label
)

#### Median difference, labelled just above each boxplot. label_y is the highest
#### plotted value (whisker or outlier), so the label never sits on top of a point.
ff_median_df <- ff_df %>%
  group_by(fclass, def_label) %>%
  dplyr::summarise(pct_diff_median = median(pct_diff, na.rm = T),
                   label_y = max(pct_diff, na.rm = T),
                   .groups = "drop")

ff_theme <- theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"))

p <- ff_df %>%
  ggplot(aes(x = def_label,
             y = pct_diff,
             fill = def_label)) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.3) +
  geom_boxplot(outlier.size = 0.5, linewidth = 0.3) +
  geom_text(data = ff_median_df,
            aes(x = def_label, y = label_y,
                label = sprintf("%.1f", pct_diff_median)),
            vjust = -0.7, size = 2.5, color = "gray20", inherit.aes = FALSE) +
  facet_wrap(~fclass, ncol = 3) +
  scale_fill_manual(values = ff_fill_values, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.09))) +
  labs(x = "Percentile of travel time used to define free flow",
       y = "% difference in free-flow speed",
       title = paste0("Difference in free-flow speed relative to the ",
                      ff_def_baseline, " definition"),
       subtitle = "Median values reported above the boxplots") +
  ff_theme

ggsave(p, filename = file.path(figures_dir, "free_flow_sensitivity.png"),
       height = 6.5, width = 9)
