# Google Mapbox Scatterplots

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  dplyr::filter(all_26_route %in% 1)

# Create variables -------------------------------------------------------------
df <- df %>%
  group_by(uid) %>%
  dplyr::mutate(gg_speed_in_traffic_kmh_max = quantile(gg_speed_in_traffic_kmh, 0.95, na.rm = T),
                gg_duration_in_traffic_min_max = quantile(gg_duration_in_traffic_min, 0.95, na.rm = T))

df <- df %>%
  dplyr::mutate(ti_speed = gg_tl_prop_2 * 1.086 + gg_tl_prop_3 * 3.956 + gg_tl_prop_4 * 5.979,
                ti_dur   = gg_tl_prop_2 * 0.9958 + gg_tl_prop_3 * 4.329 + gg_tl_prop_4 * 6.231,
                speed_pc = (gg_speed_in_traffic_kmh - gg_speed_in_traffic_kmh_max)/gg_speed_in_traffic_kmh_max*100,
                dur_pc = (gg_duration_in_traffic_min - gg_duration_in_traffic_min_max)/gg_duration_in_traffic_min_max*100) %>%
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
  dplyr::mutate(metric = "Speed")

dur_df <- df %>%
  group_by(uid, ti_dur_group) %>%
  dplyr::summarise(stdev = sd(gg_duration_in_traffic_min, na.rm = T),
                   pc_mean = mean(speed_pc),
                   pc_median = median(speed_pc)) %>%
  ungroup() %>%
  dplyr::rename(ti = ti_dur_group) %>%
  dplyr::mutate(metric = "Duration")

# Figures ----------------------------------------------------------------------
### Std Dev
dur_df %>%
  ggplot(aes(x = stdev,
             y = ti)) +
  geom_boxplot()

speed_df %>%
  ggplot(aes(x = stdev,
             y = ti)) +
  geom_boxplot()

###
dur_df %>%
  ggplot(aes(x = pc_median,
             y = ti)) +
  geom_boxplot()

speed_df %>%
  ggplot(aes(x = pc_median,
             y = ti)) +
  geom_boxplot()

###
dur_df %>%
  ggplot(aes(x = pc_mean,
             y = ti)) +
  geom_boxplot()

speed_df %>%
  ggplot(aes(x = pc_mean,
             y = ti)) +
  geom_boxplot()








###
df %>%
  ggplot(aes(x = speed_pc,
             y = ti_speed_group)) +
  geom_boxplot() 

df %>%
  ggplot(aes(x = dur_pc,
             y = ti_dur_group)) +
  geom_boxplot() 




df %>%
  ggplot(aes(x = gg_speed_in_traffic_kmh,
             y = ti_speed_group)) +
  geom_boxplot() +
  facet_wrap(~uid)

df %>%
  ggplot(aes(x = gg_duration_in_traffic_min,
             y = ti_dur_group)) +
  geom_boxplot() +
  facet_wrap(~uid)
