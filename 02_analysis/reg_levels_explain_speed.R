# Google Mapbox Scatterplot

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  dplyr::filter(all_26_route %in% 1)

# Prep data --------------------------------------------------------------------
df <- df %>%
  dplyr::mutate(gg_speed_in_traffic_kmh = log_margin(gg_speed_in_traffic_kmh, 0.001),
                gg_duration_in_traffic_min = log_margin(gg_duration_in_traffic_min, 0.001),
                gg_tl_prop_234 = log_margin(gg_tl_prop_234, 0.001),
                gg_tl_prop_34 = log_margin(gg_tl_prop_34, 0.001),
                gg_tl_prop_4 = log_margin(gg_tl_prop_4, 0.001),
                gg_tl_max = log_margin(gg_tl_max, 0.1),
                gg_tl_mean = log_margin(gg_tl_max, 0.1))

# OLS: Pooled ------------------------------------------------------------------
#lm_speed_1 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_prop_234 | uid, data = df, vcov = ~uid)
#lm_speed_2 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_prop_34 | uid, data = df, vcov = ~uid)
#lm_speed_3 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_prop_4 | uid, data = df, vcov = ~uid)
lm_speed_4 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_prop_4 + gg_tl_prop_3 + gg_tl_prop_2 | uid, data = df, vcov = ~uid)
#lm_speed_5 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_mean | uid, data = df, vcov = ~uid)
#lm_speed_6 <- feols(gg_speed_in_traffic_kmh ~ gg_tl_max | uid, data = df, vcov = ~uid)

#lm_dur_1 <- feols(gg_duration_in_traffic_min ~ gg_tl_prop_234 | uid, data = df, vcov = ~uid)
#lm_dur_2 <- feols(gg_duration_in_traffic_min ~ gg_tl_prop_34 | uid, data = df, vcov = ~uid)
#lm_dur_3 <- feols(gg_duration_in_traffic_min ~ gg_tl_prop_4 | uid, data = df, vcov = ~uid)
lm_dur_4 <- feols(gg_duration_in_traffic_min ~ gg_tl_prop_4 + gg_tl_prop_3 + gg_tl_prop_2 | uid, data = df, vcov = ~uid)
#lm_dur_5 <- feols(gg_duration_in_traffic_min ~ gg_tl_mean | uid, data = df, vcov = ~uid)
#lm_dur_6 <- feols(gg_duration_in_traffic_min ~ gg_tl_max | uid, data = df, vcov = ~uid)

# OLS: Pooled ------------------------------------------------------------------
lm_speed_all_df <- map_df(unique(df$uid), function(uid_i){
  
  feols(gg_speed_in_traffic_kmh ~ gg_tl_prop_4 + gg_tl_prop_3 + gg_tl_prop_2, 
                      data = df[df$uid %in% uid_i,]) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent)/2) %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(uid = uid_i) %>%
    dplyr::filter(variable != "(Intercept)")
  
})

lm_dur_all_df <- map_df(unique(df$uid), function(uid_i){
  
  feols(gg_duration_in_traffic_min ~ gg_tl_prop_4 + gg_tl_prop_3 + gg_tl_prop_2, 
        data = df[df$uid %in% uid_i,]) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent)/2) %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(uid = uid_i) %>%
    dplyr::filter(variable != "(Intercept)")
  
})

lm_speed_all_df %>%
  ggplot(aes(xmin = x2_5_percent,
             xmax = x97_5_percent,
             x = b,
             y = uid,
             color = variable)) +
  geom_linerange() +
  geom_point()

lm_dur_all_df %>%
  ggplot(aes(xmin = x2_5_percent,
             xmax = x97_5_percent,
             x = b,
             y = uid,
             color = variable)) +
  geom_hline(aes(yintercept = uid), 
             color = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0) +
  geom_linerange() +
  geom_point() +
  theme_classic2() +
  labs(x = "Coef (+/- 95% CI)",
       y = "Segment ID")

# Table: Pooled ----------------------------------------------------------------
#### Table Settings
my_style = style.tex(tpt = TRUE, 
                     notes.tpt.intro = "\\footnotesize")
setFixest_etable(style.tex = my_style)

dict = c(gg_speed_in_traffic_kmh = "Speed (km/h), log",
         gg_duration_in_traffic_min = "Duration (min), log",
         gg_tl_prop_234 = "Prop route traffic level 2 - 4, log",
         gg_tl_prop_34 = "Prop route traffic level 3 - 4, log",
         gg_tl_prop_2 = "Prop route traffic level 2, log",
         gg_tl_prop_3 = "Prop route traffic level 3, log",
         gg_tl_prop_4 = "Prop route traffic level 4, log",
         gg_tl_mean = "Traffic average, log",
         gg_tl_max = "Traffic maximum, log",
         uid = "Route")
setFixest_dict(dict)


#### Table
file.remove(file.path(tables_dir, "ols_gg_speed_dur_traffic.tex"))
esttex(lm_speed_1, lm_speed_2, lm_speed_3, lm_speed_4, lm_speed_5, lm_speed_6,
       lm_dur_1, lm_dur_2, lm_dur_3, lm_dur_4, lm_dur_5, lm_dur_6,
       float = F,
       file = file.path(tables_dir,
                        "ols_gg_speed_dur_traffic.tex"))

