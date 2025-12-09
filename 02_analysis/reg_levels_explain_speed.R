# Google Mapbox Scatterplots

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  dplyr::filter(all_26_route %in% 1)

# Prep data --------------------------------------------------------------------
df$gg_tl_prop_4[df$gg_tl_prop_4 > 0] %>% summary()
mean(df$gg_tl_prop_4 >= 0.1, na.rm = T)
quantile(df$gg_tl_prop_4, 0.9, na.rm = T)

df <- df %>%
  dplyr::mutate(count_2 = count_2 / 1000,
                count_3 = count_3 / 1000,
                count_4 = count_4 / 1000) %>%
  dplyr::mutate(gg_speed_in_traffic_kmh_ln = log(gg_speed_in_traffic_kmh),
                gg_duration_in_traffic_min_ln = log(gg_duration_in_traffic_min),
                gg_tl_prop_234_logm0_1 = log_margin(gg_tl_prop_234, 0.1),
                gg_tl_prop_34_logm0_1 = log_margin(gg_tl_prop_34, 0.1),
                gg_tl_prop_2_logm0_1 = log_margin(gg_tl_prop_2, 0.1),
                gg_tl_prop_3_logm0_1 = log_margin(gg_tl_prop_3, 0.1),
                gg_tl_prop_4_logm0_1 = log_margin(gg_tl_prop_4, 0.1),
                gg_tl_prop_2_sq = gg_tl_prop_2^2,
                gg_tl_prop_3_sq = gg_tl_prop_3^2,
                gg_tl_prop_4_sq = gg_tl_prop_4^2,
                
                count_2_sq = count_2^2,
                count_3_sq = count_3^2,
                count_4_sq = count_4^2) %>%
  dplyr::filter(!is.na(gg_tl_prop_2))

# OLS: Prop --------------------------------------------------------------------
lm_speed_prop_1 <- feols(gg_speed_in_traffic_kmh_ln ~ gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 | uid, data = df, vcov = ~uid)
lm_speed_prop_2 <- feols(gg_speed_in_traffic_kmh_ln ~ gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 +
                      gg_tl_prop_2_sq + gg_tl_prop_3_sq + gg_tl_prop_4_sq | uid, data = df, vcov = ~uid)

lm_dur_prop_1 <- feols(gg_duration_in_traffic_min_ln ~ gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 | uid, data = df, vcov = ~uid)
lm_dur_prop_2 <- feols(gg_duration_in_traffic_min_ln ~ gg_tl_prop_2 + gg_tl_prop_3 + gg_tl_prop_4 +
                    gg_tl_prop_2_sq + gg_tl_prop_3_sq + gg_tl_prop_4_sq | uid, data = df, vcov = ~uid)

# OLS: Count -------------------------------------------------------------------
lm_speed_count_1 <- feols(gg_speed_in_traffic_kmh_ln ~ count_2 + count_3 + count_4 | uid, data = df, vcov = ~uid)
lm_speed_count_2 <- feols(gg_speed_in_traffic_kmh_ln ~ count_2 + count_3 + count_4 +
                      count_2_sq + count_3_sq + count_4_sq | uid, data = df, vcov = ~uid)

lm_dur_count_1 <- feols(gg_duration_in_traffic_min_ln ~ count_2 + count_3 + count_4 | uid, data = df, vcov = ~uid)
lm_dur_count_2 <- feols(gg_duration_in_traffic_min_ln ~ count_2 + count_3 + count_4 +
                    count_2_sq + count_3_sq + count_4_sq | uid, data = df, vcov = ~uid)

# OLS: Pooled ------------------------------------------------------------------
# lm_speed_all_df <- map_df(unique(df$uid), function(uid_i){
#   
#   feols(gg_speed_in_traffic_kmh_ln ~ gg_tl_prop_4 + gg_tl_prop_3 + gg_tl_prop_2, 
#         data = df[df$uid %in% uid_i,]) %>%
#     confint() %>%
#     as.data.frame() %>%
#     clean_names() %>%
#     dplyr::mutate(b = (x2_5_percent + x97_5_percent)/2) %>%
#     rownames_to_column(var = "variable") %>%
#     dplyr::mutate(uid = uid_i) %>%
#     dplyr::filter(variable != "(Intercept)")
#   
# })
# 
# lm_dur_all_df <- map_df(unique(df$uid), function(uid_i){
#   
#   feols(gg_duration_in_traffic_min_ln ~ gg_tl_prop_4 + gg_tl_prop_3 + gg_tl_prop_2, 
#         data = df[df$uid %in% uid_i,]) %>%
#     confint() %>%
#     as.data.frame() %>%
#     clean_names() %>%
#     dplyr::mutate(b = (x2_5_percent + x97_5_percent)/2) %>%
#     rownames_to_column(var = "variable") %>%
#     dplyr::mutate(uid = uid_i) %>%
#     dplyr::filter(variable != "(Intercept)")
#   
# })
# 
# lm_speed_all_df %>%
#   ggplot(aes(xmin = x2_5_percent,
#              xmax = x97_5_percent,
#              x = b,
#              y = uid,
#              color = variable)) +
#   geom_linerange() +
#   geom_point()
# 
# lm_dur_all_df %>%
#   ggplot(aes(xmin = x2_5_percent,
#              xmax = x97_5_percent,
#              x = b,
#              y = uid,
#              color = variable)) +
#   geom_hline(aes(yintercept = uid), 
#              color = "grey85", linewidth = 0.3) +
#   geom_vline(xintercept = 0) +
#   geom_linerange() +
#   geom_point() +
#   theme_classic2() +
#   labs(x = "Coef (+/- 95% CI)",
#        y = "Segment ID")

# Breaking point ---------------------------------------------------------------
#### Speed
lm_speed_2_df <- lm_speed_2 %>%
  coefficients() %>%
  t() %>%
  as.data.frame()

speed_ver_2 <- -lm_speed_2_df$gg_tl_prop_2/(2*lm_speed_2_df$gg_tl_prop_2_sq)
speed_ver_3 <- -lm_speed_2_df$gg_tl_prop_3/(2*lm_speed_2_df$gg_tl_prop_3_sq)
speed_ver_4 <- -lm_speed_2_df$gg_tl_prop_4/(2*lm_speed_2_df$gg_tl_prop_4_sq)

prop_over_vert_speed_2 <- (mean(df$gg_tl_prop_2 > speed_ver_2) * 100) %>% round(3) %>% paste0("\\%")
prop_over_vert_speed_3 <- (mean(df$gg_tl_prop_3 > speed_ver_3) * 100) %>% round(3) %>% paste0("\\%")
prop_over_vert_speed_4 <- (mean(df$gg_tl_prop_4 > speed_ver_4) * 100) %>% round(3) %>% paste0("\\%")

#### Duration
lm_dur_2_df <- lm_dur_2 %>%
  coefficients() %>%
  t() %>%
  as.data.frame()

dur_ver_2 <- -lm_dur_2_df$gg_tl_prop_2/(2*lm_dur_2_df$gg_tl_prop_2_sq)
dur_ver_3 <- -lm_dur_2_df$gg_tl_prop_3/(2*lm_dur_2_df$gg_tl_prop_3_sq)
dur_ver_4 <- -lm_dur_2_df$gg_tl_prop_4/(2*lm_dur_2_df$gg_tl_prop_4_sq)

prop_over_vert_dur_2 <- (mean(df$gg_tl_prop_2 > dur_ver_2) * 100) %>% round(3) %>% paste0("\\%")
prop_over_vert_dur_3 <- (mean(df$gg_tl_prop_3 > dur_ver_3) * 100) %>% round(3) %>% paste0("\\%")
prop_over_vert_dur_4 <- (mean(df$gg_tl_prop_4 > dur_ver_4) * 100) %>% round() %>% paste0("\\%")

# Table: Pooled ----------------------------------------------------------------
#### Table Settings
my_style = style.tex(tpt = TRUE, 
                     notes.tpt.intro = "\\footnotesize")
setFixest_etable(style.tex = my_style)

dict = c(gg_speed_in_traffic_kmh_ln = "Speed (km/h), log",
         gg_duration_in_traffic_min_ln = "Duration (min), log",
         gg_tl_prop_234 = "Prop route traffic level 2 - 4",
         gg_tl_prop_34 = "Prop route traffic level 3 - 4",
         
         gg_tl_prop_2 = "Prop route traffic level 2",
         gg_tl_prop_3 = "Prop route traffic level 3",
         gg_tl_prop_4 = "Prop route traffic level 4",
         
         gg_tl_prop_2_sq = "Prop route traffic level 2, Squared",
         gg_tl_prop_3_sq = "Prop route traffic level 3, Squared",
         gg_tl_prop_4_sq = "Prop route traffic level 4, Squared",
         
         gg_tl_mean = "Traffic average",
         gg_tl_max = "Traffic maximum",
         uid = "Route")
setFixest_dict(dict)

#### Table
file.remove(file.path(tables_dir, "ols_gg_speed_dur_traffic.tex"))
esttex(lm_speed_1, lm_speed_2, 
       lm_dur_1, lm_dur_2,
       float = F,
       extralines = list(

         "_ \\midrule \\emph{Quadratic Model Vertex (Turning Point)}" = 
           c("", "", "", ""), # Another visual header for the second block
         
         "_ -$\\frac{\\beta_1}{2*\\beta_2}$ [Prop. traffic level 2]" = 
           c("NA",
             round(speed_ver_2,3),
             "NA",
             round(dur_ver_2,3)),
         
         "_ -$\\frac{\\beta_1}{2*\\beta_2}$ [Prop. traffic level 3]" = 
           c("NA",
             round(speed_ver_3,3),
             "NA",
             round(dur_ver_3,3)),
         
         "_ -$\\frac{\\beta_1}{2*\\beta_2}$ [Prop. traffic level 4]" = 
           c("NA",
             round(speed_ver_4,3),
             "NA",
             round(dur_ver_4,3)),
         
         "_\\% of Obs $>$ -$\\frac{\\beta_1}{2*\\beta_2}$ [Prop. traffic level 2]" = 
           c("NA",
             prop_over_vert_speed_2,
             "NA",
             prop_over_vert_dur_2),
         
         "_\\% of Obs $>$ -$\\frac{\\beta_1}{2*\\beta_2}$ [Prop. traffic level 3]" = 
           c("NA",
             prop_over_vert_speed_3,
             "NA",
             prop_over_vert_dur_3),
         
         "_\\% of Obs $>$ -$\\frac{\\beta_1}{2*\\beta_2}$ [Prop. traffic level 4]" = 
           c("NA",
             prop_over_vert_speed_4,
             "NA",
             prop_over_vert_dur_4)
       ),
       file = file.path(tables_dir,
                        "ols_gg_speed_dur_traffic.tex"))


