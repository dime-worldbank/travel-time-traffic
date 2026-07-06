# Google Mapbox Scatterplots

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  dplyr::filter(all_26_route %in% 1)

# Create variables -------------------------------------------------------------
df <- df %>%
  group_by(uid) %>%
  dplyr::mutate(gg_speed_in_traffic_kmh_max = quantile(gg_speed_in_traffic_kmh, 0.99, na.rm = T),
                gg_duration_in_traffic_min_min = quantile(gg_duration_in_traffic_min, 0.01, na.rm = T))

df <- df %>%
  dplyr::mutate(ti_speed = gg_tl_prop_2 * 1.086 + gg_tl_prop_3 * 3.956 + gg_tl_prop_4 * 5.979,
                ti_dur   = gg_tl_prop_2 * 0.9958 + gg_tl_prop_3 * 4.329 + gg_tl_prop_4 * 6.231,
                speed_pc = (gg_speed_in_traffic_kmh - gg_speed_in_traffic_kmh_max)/gg_speed_in_traffic_kmh_max*100,
                dur_pc = (gg_duration_in_traffic_min - gg_duration_in_traffic_min_min)/gg_duration_in_traffic_min_min*100,
                distance_pc = (gg_distance_m - gg_distance_m_mode)/gg_distance_m_mode*100) %>%
  dplyr::mutate(uid = factor(uid)) %>%
  dplyr::filter(!is.na(gg_tl_prop_2))

df <- df %>%
  dplyr::mutate(ti_speed_group = case_when(
    ti_speed == 0 ~ "0",
    ti_speed <= 0.1 ~ "0 - 0.1",
    ti_speed <= 0.2 ~ "0.1 - 0.2",
    ti_speed <= 0.3 ~ "0.2 - 0.3",
    ti_speed <= 0.4 ~ "0.3 - 0.4",
    ti_speed <= 0.5 ~ "0.4 - 0.5",
    ti_speed <= 0.6 ~ "0.5 - 0.6",
    ti_speed <= 0.7 ~ "0.6 - 0.7",
    ti_speed <= 0.8 ~ "0.7 - 0.8",
    ti_speed <= 0.9 ~ "0.8 - 0.9",
    ti_speed <= 1.0 ~ "0.9 - 1.0",
    ti_speed > 1.0 ~ ">1.0"
  )) %>%
  dplyr::mutate(ti_dur_group = case_when(
    ti_dur == 0 ~ "0",
    ti_dur <= 0.1 ~ "0 - 0.1",
    ti_dur <= 0.2 ~ "0.1 - 0.2",
    ti_dur <= 0.3 ~ "0.2 - 0.3",
    ti_dur <= 0.4 ~ "0.3 - 0.4",
    ti_dur <= 0.5 ~ "0.4 - 0.5",
    ti_dur <= 0.6 ~ "0.5 - 0.6",
    ti_dur <= 0.7 ~ "0.6 - 0.7",
    ti_dur <= 0.8 ~ "0.7 - 0.8",
    ti_dur <= 0.9 ~ "0.8 - 0.9",
    ti_dur <= 1.0 ~ "0.9 - 1.0",
    ti_dur > 1.0 ~ ">1.0"
  )) %>%
  dplyr::mutate(ti_speed_group = ti_speed_group %>%
                  factor(levels = c("0",
                                    "0 - 0.1",
                                    "0.1 - 0.2",
                                    "0.2 - 0.3",
                                    "0.3 - 0.4",
                                    "0.4 - 0.5",
                                    "0.5 - 0.6",
                                    "0.6 - 0.7",
                                    "0.7 - 0.8",
                                    "0.8 - 0.9",
                                    "0.9 - 1.0",
                                    ">1.0"))) %>%
  dplyr::mutate(ti_dur_group = ti_dur_group %>%
                  factor(levels = c("0",
                                    "0 - 0.1",
                                    "0.1 - 0.2",
                                    "0.2 - 0.3",
                                    "0.3 - 0.4",
                                    "0.4 - 0.5",
                                    "0.5 - 0.6",
                                    "0.6 - 0.7",
                                    "0.7 - 0.8",
                                    "0.8 - 0.9",
                                    "0.9 - 1.0",
                                    ">1.0")))

# Within route standard deviation ----------------------------------------------
speed_df <- df %>%
  group_by(uid, ti_speed_group) %>%
  dplyr::summarise(stdev = sd(gg_speed_in_traffic_kmh, na.rm = T),
                   pc_mean = mean(speed_pc),
                   pc_median = median(speed_pc)) %>%
  ungroup() %>%
  dplyr::rename(ti = ti_speed_group) %>%
  dplyr::mutate(metric = "Speed (km/h)")

dur_df <- df %>%
  group_by(uid, ti_speed_group) %>%
  dplyr::summarise(stdev = sd(gg_duration_in_traffic_min, na.rm = T),
                   pc_mean = mean(dur_pc),
                   pc_median = median(dur_pc)) %>%
  ungroup() %>%
  dplyr::rename(ti = ti_speed_group) %>%
  dplyr::mutate(metric = "Duration (minutes)")

distance_df <- df %>%
  group_by(uid, ti_speed_group) %>%
  dplyr::summarise(stdev = sd(gg_distance_km, na.rm = T),
                   pc_mean = mean(distance_pc),
                   pc_median = median(distance_pc)) %>%
  ungroup() %>%
  dplyr::rename(ti = ti_speed_group) %>%
  dplyr::mutate(metric = "Distance (km)")

# Figures ----------------------------------------------------------------------
p1 <- bind_rows(
  dur_df,
  speed_df,
  distance_df
) %>%
  dplyr::mutate(metric = paste0(metric, " Std. Dev.")) %>%
  dplyr::mutate(metric = metric %>%
                  factor(levels = c("Speed (km/h) Std. Dev.",
                                    "Duration (minutes) Std. Dev.",
                                    "Distance (km) Std. Dev."))) %>%
  ggplot(aes(x = stdev,
             y = ti)) +
  geom_boxplot(outliers = F,
               fill = "gray") +
  facet_wrap(~metric,
             scales = "free_x") +
  labs(x = "Std. Dev.",
       y = "Travel Index",
       caption = "Outliers not shown",
       title = "B. Distribution of Within Route Standard Deviation of Indicators") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

###
p2 <- bind_rows(
  dur_df,
  speed_df,
  distance_df
) %>%
  dplyr::mutate(metric = paste0("% Change ", metric)) %>%
  dplyr::mutate(metric = metric %>%
                  factor(levels = c("% Change Speed (km/h)",
                                    "% Change Duration (minutes)",
                                    "% Change Distance (km)"))) %>%
  ggplot(aes(x = pc_mean,
             y = ti)) +
  geom_boxplot(outliers = F,
               fill = "gray") +
  facet_wrap(~metric,
             scales = "free_x") +
  labs(x = "% Change",
       y = "Travel Index",
       title = "A. Distribution of Within Route Average Percent Change in Indicators",
       subtitle = " - % change in speed calculated relative to within route 99th percentile\n - % change in duration calculated relative to within route 1st percentile\n - % change in distanced calculated relative to mode distance") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

p <- ggarrange(p2, p1, ncol = 1)

ggsave(p,
       filename = file.path(figures_dir, "dist_boxplots.png"),
       height = 8, width = 8)


# df %>%
#   dplyr::select(uid, ti_speed_group, 
#                 distance_pc, speed_pc, dur_pc) %>%
#   pivot_longer(cols = -c(uid, ti_speed_group)) %>%
#   
#   ggplot(aes(x = value,
#              y = ti_speed_group)) +
#   geom_boxplot() +
#   facet_wrap(~name,
#              scales = "free_x") +
#   labs(x = "% Change",
#        y = "Travel Index",
#        title = "Distribution of Percent Change")






