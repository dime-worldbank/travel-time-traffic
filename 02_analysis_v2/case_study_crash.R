# Crash Regressions

# Load data --------------------------------------------------------------------
crash_50m_df <- readRDS(file.path(analysis_data_dir, "mapbox_twitter_50m.Rds"))
crash_100m_df <- readRDS(file.path(analysis_data_dir, "mapbox_twitter_100m.Rds"))
crash_df <- readRDS(file.path(analysis_data_dir, "mapbox_twitter_100m.Rds"))

# Clean data -------------------------------------------------------------------
crash_df <- crash_df %>%
  dplyr::mutate(date = datetime %>% hour(),
                hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(),
                crash_dow = crash_datetime %>% lubridate::wday(),
                duration_min = duration_s / 60,
                ti = tl_prop_2*2 + tl_prop_3*3 + tl_prop_4*4,
                ti = replace_na(ti, 0),
                ti_ttsample = case_when(
                  is.na(speed_kmh) ~ NA,
                  TRUE ~ ti
                ))
                
hours_since_df <- crash_df %>%
  dplyr::filter(hours_since_crash <= 10,
                hours_since_crash >= -10) %>%
  dplyr::select(crash_id, hours_since_crash, hour) %>%
  dplyr::rename(hour_of_day_since_crash = hours_since_crash)

hours_since_same_dow_df <- crash_df %>%
  dplyr::filter(hours_since_crash <= 10,
                hours_since_crash >= -10) %>%
  dplyr::select(crash_id, hours_since_crash, dow, hour) %>%
  dplyr::rename(hour_of_day_since_crash_same_dow = hours_since_crash)

crash_df <- crash_df %>%
  left_join(hours_since_df, by = c("crash_id", "hour")) %>%
  left_join(hours_since_same_dow_df, by = c("crash_id", "hour", "dow")) %>%
  dplyr::filter(!is.na(hour_of_day_since_crash_same_dow)) %>%
  dplyr::mutate(crash_day = case_when(
    (hours_since_crash <= 10) & (hours_since_crash >= -10) ~ 1,
    TRUE ~ 0
  ))

crash_df <- crash_df %>%
  dplyr::mutate(hour_of_day_since_crash = hour_of_day_since_crash_same_dow)

# Restrict sample --------------------------------------------------------------
crash_df <- crash_df %>%
  dplyr::filter(hours_since_crash <= 10,
                hours_since_crash >= -24*7*4) %>%
  group_by(crash_id) %>%
  dplyr::mutate(n_obs = n()) %>%
  ungroup() %>%
  dplyr::filter(n_obs >= 21*2)

n_crash_tl <- crash_df %>%
  dplyr::filter(!is.na(ti)) %>%
  pull(crash_id) %>%
  unique() %>%
  length()

n_crash_tt <- crash_df %>%
  dplyr::filter(!is.na(speed_kmh)) %>%
  pull(crash_id) %>%
  unique() %>%
  length()

# Regressions ------------------------------------------------------------------
lm_to_df <- function(lm_i){
  lm_i %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "hour_of_day_since_crash") %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                  hour_of_day_since_crash = hour_of_day_since_crash %>%
                    str_replace_all("hour_of_day_since_crash", "") %>%
                    str_replace_all("crash_day", "") %>%
                    str_replace_all(":", "") %>%
                    as.numeric())
}

#### Regressions
lm_ti_df <- feols(
  ti ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Traffic Index\n[N Crashes = ", n_crash_tl, "]"))

lm_ti_ttsample_df <- feols(
  ti_ttsample ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Traffic Index\n[N Crashes = ", n_crash_tt, "]"))

lm_speed_df <- feols(
  speed_kmh ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Speed (km/h)\n[N Crashes = ", n_crash_tt, "]"))

lm_duration_df <- feols(
  duration_min ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Duration (minutes)\n[N Crashes = ", n_crash_tt, "]"))

lm_all_df <- bind_rows(lm_ti_df,
                       lm_ti_ttsample_df,
                       lm_speed_df,
                       lm_duration_df)

lm_all_df %>%
  dplyr::mutate(dv = dv %>% fct_rev()) %>%
  ggplot(aes(x = hour_of_day_since_crash,
             y = b,
             ymin = x2_5_percent,
             ymax = x97_5_percent)) +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_vline(xintercept = -1, color = "red", linetype = "dotted") +
  geom_linerange() +
  geom_point() +
  facet_wrap(~dv, scales = "free_y") +
  labs(x = "Hours Since Crash",
       y = "Coef (+/i 95% CI)") +
  theme_classic2() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_blank())

