# Regression

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "mapbox_routes.Rds"))

# Clean data -------------------------------------------------------------------
route_df <- route_df %>%
  dplyr::filter(tl_and_tt_data %in% T,
                #route_change_length %in% F,
                modal_route %in% T) %>%
  dplyr::mutate(speed_kmh = (distance_m/1000) /(duration_s/60/60),
                tt_hour_per_km = (duration_s/60/60) / (distance_m/1000) ) %>%
  dplyr::mutate(speed_kmh_ln = log(speed_kmh),
                tt_hour_per_km_ln = log(tt_hour_per_km)) %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday())

# Determine routes with common support -----------------------------------------
route_df <- route_df %>%
  group_by(uid) %>%
  dplyr::mutate(n_obs = n(),
                prop_2_n = sum(tl_prop_2 > 0),
                prop_3_n = sum(tl_prop_3 > 0),
                prop_4_n = sum(tl_prop_4 > 0),
                
                prop_2_sd = sd(tl_prop_2),
                prop_3_sd = sd(tl_prop_3),
                prop_4_sd = sd(tl_prop_4),
                
                prop_4_min = min(tl_prop_4),
                prop_4_max = max(tl_prop_4)) %>%
  ungroup() %>%
  dplyr::mutate(prop_2_prop = prop_2_n / n_obs,
                prop_3_prop = prop_3_n / n_obs,
                prop_4_prop = prop_4_n / n_obs)

agg_df <- route_df %>%
  distinct(uid, .keep_all = T) %>%
  dplyr::select(uid, 
                prop_2_prop, prop_3_prop, prop_4_prop,
                prop_2_n, prop_3_n, prop_4_n,
                prop_2_sd, prop_3_sd, prop_4_sd,
                prop_4_min, prop_4_max)

agg_sub_df <- agg_df %>%
  dplyr::filter(prop_4_prop >= 0.05,
                prop_4_sd >= 0.01)

# Pooled regressions -----------------------------------------------------------
lm1 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid + hour + dow, 
             data = route_df)
lm1

a <- route_df %>%
  dplyr::filter(tl_prop_4 > 0) %>%
  pull(tl_prop_3)
mean(a == 0)

a <- route_df %>%
  dplyr::filter(tl_prop_3 > 0) %>%
  pull(tl_prop_4)
mean(a == 0)

route_common1_df <- route_df %>% dplyr::filter(prop_4_prop >= 0.05, prop_4_sd >= 0.01)
route_common1_df$uid %>% unique() %>% length()
lm2 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
             data = route_common1_df)

lm2

route_common2_df <- route_df %>% dplyr::filter(prop_4_prop >= 0.1, prop_4_sd >= 0.02)
route_common2_df$uid %>% unique() %>% length()
lm3 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
             data = route_common2_df)

lm3

# A [Xpp increase] in [color] corresponds to [x% increase] in travel time
exp(lm2$coeftable$Estimate * 0.1)-1
exp(lm2$coeftable$Estimate * 0.2)-1

# In table, from OSM, show how common:
mean(osm_df$gg_tl_prop_2 >= 0.1)
mean(osm_df$gg_tl_prop_3 >= 0.1)
mean(osm_df$gg_tl_prop_4 >= 0.1)

# Separate regressions ---------------------------------------------------------
lm_sep_df <- map_df(unique(route_common2_df$uid), function(uid_i){
  feols(gg_tt_hour_per_km_ln ~ gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 | uid, 
        data = route_common2_df[route_common2_df$uid == uid_i,]) %>%
    confint() %>%
    clean_names() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
                  uid = uid_i) 
})

lm_sep_df %>%
  ggplot() +
  geom_boxplot(aes(x = b,
                   y = variable))

library(lme4)
m_re <- lmer(
  gg_tt_hour_per_km_ln ~ 
    gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 +
    (gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 | uid + hour + dow),
  data = route_common1_df
)

blups <- ranef(m_re)$uid %>%
  as.data.frame() %>%
  tibble::rownames_to_column("uid") %>%
  pivot_longer(
    cols = -uid,
    names_to = "variable",
    values_to = "u_hat"
  )

fixef_df <- tibble(
  variable = names(fixef(m_re)),
  beta = fixef(m_re)
)

blups_full <- blups %>%
  left_join(fixef_df, by = "variable") %>%
  mutate(
    beta_road = beta + u_hat
  ) %>%
  filter(variable %in% c(
    "gg_tl_prop_2",
    "gg_tl_prop_3",
    "gg_tl_prop_4"
  ))

blups_full %>%
  filter(variable == "gg_tl_prop_4") %>%
  arrange(beta_road) %>%
  mutate(uid = factor(uid, levels = uid)) %>%
  ggplot(aes(x = beta_road, y = uid)) +
  geom_vline(
    xintercept = fixef(m_re)["gg_tl_prop_4"],
    linetype = "dashed",
    color = "gray40"
  ) +
  geom_point(size = 2) +
  labs(
    x = "Effect of dark red share on log travel time per km",
    y = "Road",
    title = "Road-specific (BLUP) effects of dark red congestion",
    subtitle = "Dashed line shows pooled estimate"
  )


# OSM predict ------------------------------------------------------------------
beta <- coef(lm2)

osm_df <- osm_df %>%
  mutate(
    # Linear predictor: log(delay per km)
    CI = beta["gg_tl_prop_2"] * gg_tl_prop_2 +
      beta["gg_tl_prop_3"] * gg_tl_prop_3 +
      beta["gg_tl_prop_4"] * gg_tl_prop_4,
    
    # Delay factor relative to green
    delay_factor = exp(CI),
    
    # Speed as a fraction of green speed
    speed_multiplier = exp(-CI)
  )

osm_df$delay_factor %>% summary()

# delay_factor = 1: green / free-flow conditions
# delay_factor = 2: travel time is 2x green conditions
osm_road_df <- osm_df %>%
  group_by(name, gg_tl_count_all_max, fclass) %>%
  dplyr::summarise(delay_factor = mean(delay_factor)) %>%
  ungroup() %>%
  dplyr::mutate(delay_factor_length = delay_factor * gg_tl_count_all_max)

# ASSUMES: road length equates to importance: assumes longer roads tend to have more vehicles





a <- predict(object = lm2, newdata = osm_df)

osm_df$gg_tl_prop_2 %>% is.na %>% table()
osm_df$gg_tl_prop_3 %>% is.na %>% table()
osm_df$gg_tl_prop_4 %>% is.na %>% table()


# Separate regression ----------------------------------------------------------






