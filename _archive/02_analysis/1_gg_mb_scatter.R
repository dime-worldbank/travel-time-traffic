# Google Mapbox Scatterplot

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide_clean.Rds"))

# OLS --------------------------------------------------------------------------
lm_speed_1 <- feols(gg_speed_in_traffic_kmh ~ log(gg_tl_prop_234) | uid, data = df)
lm_speed_2 <- feols(gg_speed_in_traffic_kmh ~ log(gg_tl_prop_34) | uid, data = df)
lm_speed_3 <- feols(gg_speed_in_traffic_kmh ~ log(gg_tl_prop_4) | uid, data = df)

lm_speed_4 <- feols(mb_speed_in_traffic_kmh ~ log(mb_tl_prop_234) | uid, data = df)
lm_speed_5 <- feols(mb_speed_in_traffic_kmh ~ log(mb_tl_prop_34) | uid, data = df)
lm_speed_6 <- feols(mb_speed_in_traffic_kmh ~ log(mb_tl_prop_4) | uid, data = df)

lm_dur_1 <- feols(gg_duration_in_traffic_min ~ log(gg_tl_prop_234) | uid, data = df)
lm_dur_2 <- feols(gg_duration_in_traffic_min ~ log(gg_tl_prop_34) | uid, data = df)
lm_dur_3 <- feols(gg_duration_in_traffic_min ~ log(gg_tl_prop_4) | uid, data = df)

lm_dur_4 <- feols(mb_duration_in_traffic_min ~ log(mb_tl_prop_234) | uid, data = df)
lm_dur_5 <- feols(mb_duration_in_traffic_min ~ log(mb_tl_prop_34) | uid, data = df)
lm_dur_6 <- feols(mb_duration_in_traffic_min ~ log(mb_tl_prop_4) | uid, data = df)

modelsummary_tab(list("Speed (km/h)" = lm_speed_1,
                      "Speed (km/h)" = lm_speed_2,
                      "Speed (km/h)" = lm_speed_3,
                      "Duration (min)" = lm_dur_1,
                      "Duration (min)" = lm_dur_2,
                      "Duration (min)" = lm_dur_3),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("log(gg_tl_prop_234)" = "Prop route traffic level 2 - 4",
                              "log(gg_tl_prop_34)" = "Prop route traffic level 3 - 4",
                              "log(gg_tl_prop_4)" = "Prop route traffic level 4"),
                 gof_map = c("nobs", "adj.r.squared", "r2.within.adjusted"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6,
                                    'Route FE', "Y", "Y", "Y", "Y", "Y", "Y",
                                    "Source", "Google", "Google", "Google", "Google", "Google", "Google"),
                 output = file.path(tables_dir,
                                    "ols_gg_speed_dur_traffic.tex"))

modelsummary_tab(list("Speed (km/h)" = lm_speed_4,
                      "Speed (km/h)" = lm_speed_5,
                      "Speed (km/h)" = lm_speed_6,
                      "Duration (min)" = lm_dur_4,
                      "Duration (min)" = lm_dur_5,
                      "Duration (min)" = lm_dur_6),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("log(mb_tl_prop_234)" = "Prop route traffic level 2 - 4",
                              "log(mb_tl_prop_34)" = "Prop route traffic level 3 - 4",
                              "log(mb_tl_prop_4)" = "Prop route traffic level 4"),
                 gof_map = c("nobs", "adj.r.squared", "r2.within.adjusted"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6,
                                    'Route FE', "Y", "Y", "Y", "Y", "Y", "Y",
                                    "Source", "Mapbox", "Mapbox", "Mapbox", "Mapbox", "Mapbox", "Mapbox"),
                 output = file.path(tables_dir,
                                    "ols_mb_speed_dur_traffic.tex"))

# Correlation Dist -------------------------------------------------------------
b_df <- map_df(unique(df$uid), function(uid_i){
  
  df_i <- df[df$uid %in% uid_i,]
  
  gg_df <- lm(gg_speed_in_traffic_kmh ~ gg_tl_prop_234, data = df_i) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    mutate(b = (x2_5_percent + x97_5_percent) / 2,
           source = "Google") 
  gg_df <- gg_df[2,]
  
  mb_df <- lm(mb_speed_in_traffic_kmh ~ mb_tl_prop_234, data = df_i) %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    mutate(b = (x2_5_percent + x97_5_percent) / 2,
           source = "Mapbox") 
  mb_df <- mb_df[2,]
  
  out_df <- bind_rows(gg_df, mb_df)
  out_df$uid <- uid_i
  
  return(out_df)
})

b_df %>%
  ggplot(aes(ymin = x2_5_percent,
             ymax = x97_5_percent,
             y = b,
             x = uid)) +
  geom_linerange() +
  geom_point() + 
  facet_wrap(~source)




# Scatterplot: all -------------------------------------------------------------
p_theme <- theme(strip.background = element_blank(),
                 plot.title = element_text(face = "bold"),
                 axis.text = element_text(size = 8))

p_gg <- df %>%
  ggplot(aes(x = gg_tl_prop_234,
             y = gg_speed_in_traffic_kmh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "orange", size = 1) +
  facet_wrap(~uid, ncol = 9) +
  labs(x = "Proportion of route with traffic",
       y = "Average speed",
       title = "A. Google") +
  theme_classic2() +
  p_theme

p_mb <- df %>%
  ggplot(aes(x = mb_tl_prop_234,
             y = mb_speed_in_traffic_kmh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "orange", size = 1) +
  facet_wrap(~uid, ncol = 9) +
  labs(x = "Proportion of route with traffic",
       y = "Average speed",
       title = "B. Mapbox") +
  theme_classic2() +
  p_theme

p <- ggarrange(p_gg, p_mb, ncol = 1)

ggsave(p, filename = file.path(figures_dir, "gg_mb_scatter.png"),
       height = 6, width = 10)

# Scatterplot: best ------------------------------------------------------------
# Or with most traffic?
# Scatter and example trend over time?
p_theme <- theme(strip.background = element_blank(),
                 plot.title = element_text(face = "bold"),
                 axis.text = element_text(size = 8))

p_gg <- df %>%
  ggplot(aes(x = gg_tl_prop_234,
             y = gg_speed_in_traffic_kmh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "orange", size = 1) +
  facet_wrap(~uid, ncol = 9) +
  labs(x = "Proportion of route with traffic",
       y = "Average speed",
       title = "A. Google") +
  theme_classic2() +
  p_theme

p_mb <- df %>%
  ggplot(aes(x = mb_tl_prop_234,
             y = mb_speed_in_traffic_kmh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "orange", size = 1) +
  facet_wrap(~uid, ncol = 9) +
  labs(x = "Proportion of route with traffic",
       y = "Average speed",
       title = "B. Mapbox") +
  theme_classic2() +
  p_theme

p <- ggarrange(p_gg, p_mb, ncol = 1)

ggsave(p, filename = file.path(figures_dir, "gg_mb_scatter.png"),
       height = 6, width = 10)



