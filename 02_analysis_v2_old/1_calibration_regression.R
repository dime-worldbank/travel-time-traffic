# Regression

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "mapbox_routes.Rds"))
osm_df   <- readRDS(file.path(analysis_data_dir, "mapbox_osm_10m.Rds"))

# Clean data -------------------------------------------------------------------
route_df <- route_df %>%
  dplyr::filter(tl_and_tt_data %in% T) %>%
  dplyr::filter(#route_change_length %in% F,
    modal_route %in% T) %>%
  dplyr::mutate(speed_kmh = (distance_m/1000) /(duration_s/60/60),
                tt_hour_per_km = (duration_s/60/60) / (distance_m/1000),
                tl_prop_4 = replace_na(tl_prop_4, 0),
                worst_color = case_when(
                  tl_prop_4 > 0 ~ 4,
                  tl_prop_3 > 0 ~ 3,
                  tl_prop_2 > 0 ~ 2,
                  TRUE ~ 1
                ),
                worst_color_2 = as.numeric(worst_color == 2),
                worst_color_3 = as.numeric(worst_color == 3),
                worst_color_4 = as.numeric(worst_color == 4),
                
                prop_ge_2 = tl_prop_2 + tl_prop_3 + tl_prop_4,
                prop_ge_3 = tl_prop_3 + tl_prop_4,
                prop_ge_4 = tl_prop_4) %>%
  dplyr::mutate(speed_kmh_ln = log(speed_kmh),
                tt_hour_per_km_ln = log(tt_hour_per_km)) %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(),
                date = datetime %>% date()) %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = max(speed_kmh)) %>%
  ungroup() %>%
  dplyr::mutate(speed_kmh_uid_over50 = (speed_kmh_uid_max >= 50))

osm_df <- osm_df %>%
  dplyr::mutate(tl_prop_4 = replace_na(tl_prop_4, 0))

route_df %>%
  distinct(uid, speed_kmh_uid_max) %>%
  pull(speed_kmh_uid_max) %>%
  hist()

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
lm_all <- lm(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 + factor(uid), data = route_df)

lev <- hatvalues(lm_all)

n <- nrow(route_df)
p <- lm_all$coefficients %>% length()
route_df[(lev > 2*p / n),] %>% nrow() # Removed
route_rm_infl_df <- route_df[!(lev > 2*p / n),]

#### Proportions
lm_prop_1 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
                   vcov = ~ uid,
                   data = route_rm_infl_df)

route_over50_df <- route_rm_infl_df %>% dplyr::filter(speed_kmh_uid_max >= 50)
lm_prop_2 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
                   vcov = ~ uid,
                   data = route_over50_df)

route_under50_df <- route_rm_infl_df %>% dplyr::filter(speed_kmh_uid_max < 50)
lm_prop_3 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
                   vcov = ~ uid,
                   data = route_under50_df)

#### Worst Color
lm_worst_1 <- feols(tt_hour_per_km_ln ~ worst_color_2 + worst_color_3 + worst_color_4 | uid, 
                    vcov = ~ uid,
                    data = route_rm_infl_df)

lm_worst_2 <- feols(tt_hour_per_km_ln ~ worst_color_2 + worst_color_3 + worst_color_4 | uid, 
                    vcov = ~ uid,
                    data = route_over50_df)

lm_worst_3 <- feols(tt_hour_per_km_ln ~ worst_color_2 + worst_color_3 + worst_color_4 | uid, 
                    vcov = ~ uid,
                    data = route_under50_df)

#### N Routes
n_routes         <- route_df$uid %>% unique() %>% length()
n_routes_over50 <- route_over50_df$uid %>% unique() %>% length()
n_routes_under50 <- route_under50_df$uid %>% unique() %>% length()

# Export coefficients ----------------------------------------------------------
beta <- coef(lm_prop_1)

saveRDS(beta, file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Export regression table ------------------------------------------------------
my_style = style.tex(tpt = TRUE, 
                     notes.tpt.intro = "\\footnotesize")
setFixest_etable(style.tex = my_style)

dict = c(tt_hour_per_km_ln = "Travel time per hour, logged",
         
         tl_prop_2 = "Prop traffic level 2",
         tl_prop_3 = "Prop traffic level 3",
         tl_prop_4 = "Prop traffic level 4",
         
         uid = "Route")
setFixest_dict(dict)

#### Table
#file.remove(file.path(tables_dir, "ols_calibration.tex"))
esttex(lm_prop_1, lm_prop_2, lm_prop_3,
       float = F,
       replace = T,
       extralines = list(
         
         "_ \\midrule \\emph{Routes Used}" = 
           c("", "", "", "", "", ""), 
         
         "_Max Route Speed" = c("Any", "$\\ge 50$", "< 50"),
         
         "_N Routes" = c(n_routes, n_routes_over50, n_routes_under50),
         
         "_ \\midrule \\emph{Indep. Var. Averages}" = 
           c("", "", ""), 
         
         "_Prop traffic level 2" = c(
           route_df$tl_prop_2 %>% mean() %>% round(3),
           route_over50_df$tl_prop_2 %>% mean() %>% round(3),
           route_under50_df$tl_prop_2 %>% mean() %>% round(3)),
         "_Prop traffic level 3" = c(
           route_df$tl_prop_3 %>% mean() %>% round(3),
           route_over50_df$tl_prop_3 %>% mean() %>% round(3),
           route_under50_df$tl_prop_3 %>% mean() %>% round(3)),
         "_Prop traffic level 4" = c(
           route_df$tl_prop_4 %>% mean() %>% round(3),
           route_over50_df$tl_prop_4 %>% mean() %>% round(3),
           route_under50_df$tl_prop_4 %>% mean() %>% round(3))
       ),
       file = file.path(tables_dir, "ols_calibration.tex"))

# Merginal effects table --------------------------------------------------------
# cat("\\tabular{llll} \n ")
# cat("Percentage point increase in traffic level & \\multicolumn{3}{c}{\\% increase in travel time} \\\\ \n ")
# for(pp_i in c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)){
#   
#   cat(pp_i, " & ")
#   
#   per_change <- (exp(lm2$coeftable$Estimate * pp_i/100)-1)*100
#   per_change <- round(per_change, 2) %>% paste0("\\%") %>% paste(collapse = " & ")
#   per_change %>% paste0(" \\\\ \n ") %>% cat()
# }

# me_df <- map_df(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), function(pp_i){
#   pc <- (exp(lm2$coeftable$Estimate * pp_i/100)-1)*100
#   pc_df <- pc %>% t() %>% as.data.frame()
#   names(pc_df) <- c("tl_2", "tl_3", "tl_4")
#   pc_df$pp <- pp_i
#   pc_df
# }) %>%
#   pivot_longer(cols = -pp)
# 
# me_df %>%
#   ggplot(aes(x = pp,
#              y = value,
#              color = name)) +
#   geom_line() +
#   geom_point()
# 
# 
# # A [Xpp increase] in [color] corresponds to [x% increase] in travel time
# (exp(lm2$coeftable$Estimate * 0.1)-1)*100
# (exp(lm2$coeftable$Estimate * 0.2)-1)*100
# 
# # In table, from OSM, show how common:
# mean(osm_df$tl_prop_2 >= 0.1)
# mean(osm_df$tl_prop_3 >= 0.1)
# mean(osm_df$tl_prop_4 >= 0.1)

# # Other stats ------------------------------------------------------------------
# a <- route_df %>%
#   dplyr::filter(tl_prop_4 > 0) %>%
#   pull(tl_prop_3)
# mean(a > 0)
# 
# a <- route_df %>%
#   dplyr::filter(tl_prop_3 > 0) %>%
#   pull(tl_prop_4)
# mean(a > 0)
# 
# # Separate regressions ---------------------------------------------------------
# # lm_sep_df <- map_df(unique(route_common2_df$uid), function(uid_i){
# #   feols(gg_tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
# #         data = route_common2_df[route_common2_df$uid == uid_i,]) %>%
# #     confint() %>%
# #     clean_names() %>%
# #     rownames_to_column(var = "variable") %>%
# #     dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2,
# #                   uid = uid_i) 
# # })
# # 
# # lm_sep_df %>%
# #   ggplot() +
# #   geom_boxplot(aes(x = b,
# #                    y = variable))
# # 
# # library(lme4)
# # m_re <- lmer(
# #   gg_tt_hour_per_km_ln ~ 
# #     gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 +
# #     (gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 | uid + hour + dow),
# #   data = route_common1_df
# # )
# # 
# # blups <- ranef(m_re)$uid %>%
# #   as.data.frame() %>%
# #   tibble::rownames_to_column("uid") %>%
# #   pivot_longer(
# #     cols = -uid,
# #     names_to = "variable",
# #     values_to = "u_hat"
# #   )
# # 
# # fixef_df <- tibble(
# #   variable = names(fixef(m_re)),
# #   beta = fixef(m_re)
# # )
# # 
# # blups_full <- blups %>%
# #   left_join(fixef_df, by = "variable") %>%
# #   mutate(
# #     beta_road = beta + u_hat
# #   ) %>%
# #   filter(variable %in% c(
# #     "gg_tl_prop_2",
# #     "gg_tl_prop_3",
# #     "gg_tl_prop_4"
# #   ))
# # 
# # blups_full %>%
# #   filter(variable == "gg_tl_prop_4") %>%
# #   arrange(beta_road) %>%
# #   mutate(uid = factor(uid, levels = uid)) %>%
# #   ggplot(aes(x = beta_road, y = uid)) +
# #   geom_vline(
# #     xintercept = fixef(m_re)["gg_tl_prop_4"],
# #     linetype = "dashed",
# #     color = "gray40"
# #   ) +
# #   geom_point(size = 2) +
# #   labs(
# #     x = "Effect of dark red share on log travel time per km",
# #     y = "Road",
# #     title = "Road-specific (BLUP) effects of dark red congestion",
# #     subtitle = "Dashed line shows pooled estimate"
# #   )
# 
# # OSM predict ------------------------------------------------------------------
# beta <- coef(lm_prop_1)
# 
# osm_df <- osm_df %>%
#   mutate(
#     # Linear predictor: log(delay per km)
#     CI = beta["tl_prop_2"] * tl_prop_2 +
#       beta["tl_prop_3"] * tl_prop_3 +
#       beta["tl_prop_4"] * tl_prop_4,
#     
#     # Delay factor relative to green
#     delay_factor = exp(CI),
#     
#     # Speed as a fraction of green speed
#     speed_multiplier = exp(-CI)
#   )
# 
# osm_df$delay_factor %>% hist()
# 
# osm_sub_df <- osm_df %>%
#   arrange(-delay_factor) %>%
#   head(10)
# 
# # 1.5 = 50% more delay per km than green
# # 3 = 3x the delay per km than green
# 
# # delay_factor = 1: green / free-flow conditions
# # delay_factor = 2: travel time is 2x green conditions
# osm_road_df <- osm_df %>%
#   group_by(name, length_traffic_total_max, fclass) %>%
#   dplyr::summarise(delay_factor = mean(delay_factor)) %>%
#   ungroup() %>%
#   dplyr::mutate(delay_factor_length = delay_factor * length_traffic_total_max)
# 
# # ASSUMES: road length equates to importance: assumes longer roads tend to have more vehicles
# 
# 
