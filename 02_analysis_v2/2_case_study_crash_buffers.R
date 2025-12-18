# Crash Regressions

# Load data --------------------------------------------------------------------
crash_df <- readRDS(file.path(analysis_data_dir, "mapbox_twitter_100m.Rds"))

beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Make long --------------------------------------------------------------------
crash_df <- crash_df %>%
  dplyr::select(crash_id, datetime, crash_datetime, hours_since_crash, contains("tl_prop")) %>%
  pivot_longer(cols = -c(crash_id, datetime, crash_datetime, hours_since_crash)) %>%
  dplyr::mutate(buffer = name %>% str_replace_all("tl_prop_2_doughbuff_|tl_prop_3_doughbuff_|tl_prop_4_doughbuff_|m", "") %>%
                  as.numeric() %>%
                  replace_na(100),
                variable = name %>% str_replace_all("_doughbuff_.*", "")) %>%
  pivot_wider(id_cols = c(crash_id, datetime, crash_datetime, hours_since_crash, buffer),
              names_from = variable,
              values_from = value)

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
                crash_dow = crash_datetime %>% lubridate::wday())

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
  ungroup() #%>%
  #dplyr::filter(n_obs >= 21*2)

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
lm_df <- map_df(unique(crash_df$buffer), function(buffer_i){
  message(buffer_i)
  feols(
    delay_factor ~ i(hour_of_day_since_crash, crash_day, ref = -1) | crash_id,
    data = crash_df[crash_df$buffer == buffer_i,],
    cluster = ~crash_id
  ) %>%
    lm_to_df() %>%
    dplyr::mutate(buffer = buffer_i)
})

lm_df %>%
  #dplyr::filter(buffer )
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
        strip.background = element_blank()) +
  facet_wrap(~buffer)

ggsave(filename = file.path(figures_dir, "lm_crash_buffers.png"),
       height = 3,
       width = 9)

