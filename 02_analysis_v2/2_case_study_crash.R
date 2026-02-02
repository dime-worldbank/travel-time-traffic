# Crash Regressions

# Load data --------------------------------------------------------------------
crash_df <- readRDS(file.path(analysis_data_dir, "mapbox_twitter_100m.Rds"))
crash_attr_df <- readRDS(file.path(data_dir, "Twitter Crashes", "FinalData", 
                                   "crashes_twitter_attributes.Rds"))

beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Clean data -------------------------------------------------------------------
crash_df <- crash_df %>%
  mutate(
    # Linear predictor: log(delay per km)
    CI = beta["tl_prop_2"] * tl_prop_2 +
      beta["tl_prop_3"] * tl_prop_3 +
      beta["tl_prop_4"] * tl_prop_4,
    
    # Delay factor relative to green
    delay_factor = exp(CI),
    
    # Speed as a fraction of green speed
    speed_multiplier = exp(-CI)
  ) %>%
  dplyr::mutate(date = datetime %>% hour(),
                hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(),
                crash_dow = crash_datetime %>% lubridate::wday(),
                duration_min = duration_s / 60,
                duration_min_ln = log(duration_min),
                speed_kmh_ln = log(speed_kmh),
                delay_factor_ttsample = case_when(
                  is.na(speed_kmh) ~ NA,
                  TRUE ~ delay_factor
                ))

hours_since_df <- crash_df %>%
  dplyr::filter(hours_since_crash <= 10,
                hours_since_crash >= -10) %>%
  dplyr::select(crash_id, hours_since_crash, hour) %>%
  dplyr::rename(hour_of_day_since_crash = hours_since_crash) %>%
  distinct()

hours_since_same_dow_df <- crash_df %>%
  dplyr::filter(hours_since_crash <= 10,
                hours_since_crash >= -10) %>%
  dplyr::select(crash_id, hours_since_crash, dow, hour) %>%
  dplyr::rename(hour_of_day_since_crash_same_dow = hours_since_crash) %>%
  distinct()

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

crash_df <- crash_df %>%
  left_join(crash_attr_df, by = "crash_id")

# Restrict sample --------------------------------------------------------------
crash_df <- crash_df %>%
  dplyr::filter(hours_since_crash <= 10,
                hours_since_crash >= -24*7*4) %>%
  group_by(crash_id) %>%
  dplyr::mutate(n_obs = n()) %>%
  ungroup() %>%
  dplyr::filter(n_obs >= 21*2)

n_crash_tl <- crash_df %>%
  dplyr::filter(!is.na(delay_factor)) %>%
  pull(crash_id) %>%
  unique() %>%
  length()

n_crash_tt <- crash_df %>%
  dplyr::filter(!is.na(speed_kmh)) %>%
  pull(crash_id) %>%
  unique() %>%
  length()

##
crash_type1_df <- crash_df %>%
  dplyr::filter(osm_fclass_largest %in% c("trunk"))

crash_type2_df <- crash_df %>%
  dplyr::filter((osm_fclass_largest %in% c("primary", "secondary")))

crash_type3_df <- crash_df %>%
  dplyr::filter((osm_fclass_largest %in% c("tertiary", "residential")))

n_crash_tl_type1 <- crash_type1_df %>%
  dplyr::filter(!is.na(delay_factor)) %>%
  pull(crash_id) %>%
  unique() %>%
  length()

n_crash_tl_type2 <- crash_type2_df %>%
  dplyr::filter(!is.na(delay_factor)) %>%
  pull(crash_id) %>%
  unique() %>%
  length()

n_crash_tl_type3 <- crash_type3_df %>%
  dplyr::filter(!is.na(delay_factor)) %>%
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

#### Regressions: Main - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
lm_delayfactor_df <- feols(
  delay_factor ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Traffic Level: Delay Factor\n[N Crashes = ", n_crash_tl, "]"))

lm_delayfactor_ttsample_df <- feols(
  delay_factor_ttsample ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Traffic Level: Delay Factor\n[N Crashes = ", n_crash_tt, "]"))

lm_duration_df <- feols(
  duration_min_ln ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Duration, Logged\n[N Crashes = ", n_crash_tt, "]"))

lm_all_df <- bind_rows(lm_delayfactor_df,
                       lm_delayfactor_ttsample_df,
                       lm_duration_df)

#### Regressions: Sample V2 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
lm_delayfactor_1_df <- feols(
  delay_factor ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_type1_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Trunk Roads\nTraffic Level: Delay Factor\n[N Crashes = ", n_crash_tl_type1, "]"),
                type = 1)

lm_delayfactor_2_df <- feols(
  delay_factor ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_type2_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Primary & Secondary Roads\nTraffic Level: Delay Factor\n[N Crashes = ", n_crash_tl_type2, "]"),
                type = 2)

lm_delayfactor_3_df <- feols(
  delay_factor ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
  data = crash_type3_df,
  cluster = ~crash_id
) %>%
  lm_to_df() %>%
  dplyr::mutate(dv = paste0("Tertiary & Unclassified Roads\nTraffic Level: Delay Factor\n[N Crashes = ", n_crash_tl_type3, "]"),
                type = 3)


lm_type_df <- bind_rows(lm_delayfactor_1_df,
                        lm_delayfactor_2_df,
                        lm_delayfactor_3_df)

# Figures ----------------------------------------------------------------------
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
  facet_wrap(~dv) +
  labs(x = "Hours Since Crash",
       y = "Coef (+/- 95% CI)") +
  theme_classic2() +
  theme(strip.text = element_text(face = "bold"),
        panel.background = element_rect(fill = "gray95", color = NA),
        strip.background = element_blank())

ggsave(filename = file.path(figures_dir, "lm_crash.png"),
       height = 3,
       width = 9)

lm_type_df %>%
  dplyr::mutate(dv = fct_reorder(dv, type)) %>%
  ggplot(aes(x = hour_of_day_since_crash,
             y = b,
             ymin = x2_5_percent,
             ymax = x97_5_percent)) +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_vline(xintercept = -1, color = "red", linetype = "dotted") +
  geom_linerange() +
  geom_point() +
  facet_wrap(~dv) +
  labs(x = "Hours Since Crash",
       y = "Coef (+/- 95% CI)") +
  theme_classic2() +
  theme(strip.text = element_text(face = "bold"),
        panel.background = element_rect(fill = "gray95", color = NA),
        strip.background = element_blank())

ggsave(filename = file.path(figures_dir, "lm_crash_type.png"),
       height = 3,
       width = 9)

