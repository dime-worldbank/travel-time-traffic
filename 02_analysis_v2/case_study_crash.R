

crash_df <- readRDS(file.path(analysis_data_dir, "mapbox_twitter_50m.Rds"))

crash_df$crash_id %>% unique() %>% length()
crash_df$duration_s %>% is.na %>% table()
crash_df$tl_prop_2 %>% is.na %>% table()

# Full sample
crash_df <- crash_df %>%
  dplyr::filter(abs(hours_since_crash) <= 10) %>%
  group_by(crash_id) %>%
  dplyr::mutate(n_obs = n()) %>%
  ungroup() %>%
  dplyr::filter(n_obs %in% 21)

crash_df <- crash_df %>%
  dplyr::filter(abs(hours_since_crash) <= 10) %>%
  dplyr::mutate(hours_since_crash_fct = hours_since_crash %>%
                  factor() %>%
                  relevel(ref = "-1"),
                hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday()) %>%
  dplyr::mutate(ti = tl_prop_2*2 + tl_prop_3*3 + tl_prop_4*4,
                ti = replace_na(ti, 0)) %>%
  group_by(crash_id) %>%
  dplyr::mutate(ti_baseline = ti[hours_since_crash %in% -1],
                crash_hour = hour[hours_since_crash == 0]) %>%
  ungroup()

lm_df <- feols(ti ~ hours_since_crash_fct | crash_id + dow + hour, data = crash_df) %>%
  confint() %>%
  as.data.frame() %>%
  clean_names() %>%
  rownames_to_column(var = "time_since_crash") %>%
  dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                time_since_crash = time_since_crash %>%
                  str_replace_all("hours_since_crash_fct", "") %>%
                  as.numeric())

lm_df %>%
  ggplot(aes(x = time_since_crash,
             y = b,
             ymin = x2_5_percent,
             ymax = x97_5_percent)) +
  geom_linerange() +
  geom_point()

crash_df %>% 
  ggplot(aes(x = hours_since_crash,
             y = ti)) +
  geom_line() +
  geom_vline(xintercept = -1, color = "red") +
  facet_wrap(~crash_id) 
