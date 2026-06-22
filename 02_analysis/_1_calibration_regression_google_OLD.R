# Regression

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))

# Clean data -------------------------------------------------------------------
route_df <- route_df %>%
  #dplyr::filter(modal_route %in% T) %>%
  dplyr::mutate(speed_kmh = speed_in_traffic_kmh,
                tt_hour_per_km = (duration_in_traffic_s/60/60) / (distance_m/1000),
                prop_ge_2 = tl_prop_2 + tl_prop_3 + tl_prop_4,
                prop_ge_3 = tl_prop_3 + tl_prop_4,
                prop_ge_4 = tl_prop_4) %>%
  dplyr::mutate(speed_kmh_ln = log(speed_kmh),
                tt_hour_per_km_ln = log(tt_hour_per_km)) %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(),
                date = datetime %>% date()) %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh, prob = 0.95, na.rm = T) %>% as.numeric()) %>%
  ungroup() 

speed_df <- route_df %>%
  distinct(uid, speed_kmh_uid_max) %>%
  arrange(speed_kmh_uid_max)

median(speed_df$speed_kmh_uid_max)
table(speed_df$speed_kmh_uid_max >= 53.4)

route_df <- route_df %>%
  dplyr::mutate(speed_kmh_uid_over_med = (speed_kmh_uid_max >= 55)) %>%
  dplyr::mutate(tl_prop_2_omed = tl_prop_2 * speed_kmh_uid_over_med,
                tl_prop_3_omed = tl_prop_3 * speed_kmh_uid_over_med,
                tl_prop_4_omed = tl_prop_4 * speed_kmh_uid_over_med)

route_df <- route_df %>%
  dplyr::mutate(tl_prop_2_sq = tl_prop_2^2,
                tl_prop_3_sq = tl_prop_3^2,
                tl_prop_4_sq = tl_prop_4^2)

# Pooled regressions -----------------------------------------------------------
#### Proportions
lm_prop_1 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
                   vcov = ~ uid,
                   data = route_df)

route_omed_df <- route_df %>% dplyr::filter(speed_kmh_uid_over_med %in% T)
lm_prop_2 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
                   vcov = ~ uid,
                   data = route_omed_df)

route_umed_df <- route_df %>% dplyr::filter(speed_kmh_uid_over_med %in% F)
lm_prop_3 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, 
                   vcov = ~ uid,
                   data = route_umed_df)

lm_prop_4 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 +
                     tl_prop_2_omed + tl_prop_3_omed + tl_prop_4_omed | uid, 
                   vcov = ~ uid,
                   data = route_df)

#### Proportions: Squared
lm_prop_1_sq <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 +
                     tl_prop_2_sq + tl_prop_3_sq + tl_prop_4_sq | uid, 
                   vcov = ~ uid,
                   data = route_df)

#### N Routes
n_routes      <- route_df$uid      %>% unique() %>% length()
n_routes_omed <- route_omed_df$uid %>% unique() %>% length()
n_routes_umed <- route_umed_df$uid %>% unique() %>% length()

# Export coefficients ----------------------------------------------------------
beta <- coef(lm_prop_1)

saveRDS(beta, file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Export regression table ------------------------------------------------------
my_style = style.tex(tpt = TRUE, 
                     notes.tpt.intro = "\\footnotesize")
setFixest_etable(style.tex = my_style)

dict = c(tt_hour_per_km_ln = "Travel time (hours) per kilometer, logged",
         
         tl_prop_2 = "Prop traffic level 2",
         tl_prop_3 = "Prop traffic level 3",
         tl_prop_4 = "Prop traffic level 4",
         
         tl_prop_2_sq = "Prop traffic level 2, Squared",
         tl_prop_3_sq = "Prop traffic level 3, Squared",
         tl_prop_4_sq = "Prop traffic level 4, Squared",
         
         tl_prop_2_omed = "Prop traffic level 2 $\\times$ 95$^{\\text{th}}$ Perc. Speed $\\ge$ 55km/h",
         tl_prop_3_omed = "Prop traffic level 3 $\\times$ 95$^{\\text{th}}$ Perc. Speed $\\ge$ 55km/h",
         tl_prop_4_omed = "Prop traffic level 4 $\\times$ 95$^{\\text{th}}$ Perc. Speed $\\ge$ 55km/h",
         
         uid = "Route")
setFixest_dict(dict)

#### Table
#file.remove(file.path(tables_dir, "ols_calibration.tex"))
esttex(lm_prop_1, lm_prop_1_sq, lm_prop_2, lm_prop_3, lm_prop_4,
       float = F,
       replace = T,
       extralines = list(
         
         "_ \\midrule \\emph{Routes Used}" = 
           c("", "", "", "", ""), 
         
         "_Max Route Speed (km/h)" = c("Any", "Any", "$\\ge 55$", "< 55", "Any"),
         
         "_N Routes" = c(n_routes, n_routes, n_routes_omed, n_routes_umed, n_routes),
         
         "_ \\midrule \\emph{Indep. Var. Averages}" = 
           c("", "", "", "", ""), 
         
         "_Prop traffic level 2" = c(
           route_df$tl_prop_2 %>% mean() %>% round(3),
           route_df$tl_prop_2 %>% mean() %>% round(3),
           route_omed_df$tl_prop_2 %>% mean() %>% round(3),
           route_umed_df$tl_prop_2 %>% mean() %>% round(3),
           route_df$tl_prop_2 %>% mean() %>% round(3)),
         "_Prop traffic level 3" = c(
           route_df$tl_prop_3 %>% mean() %>% round(3),
           route_df$tl_prop_3 %>% mean() %>% round(3),
           route_omed_df$tl_prop_3 %>% mean() %>% round(3),
           route_umed_df$tl_prop_3 %>% mean() %>% round(3),
           route_df$tl_prop_3 %>% mean() %>% round(3)),
         "_Prop traffic level 4" = c(
           route_df$tl_prop_4 %>% mean() %>% round(3),
           route_df$tl_prop_4 %>% mean() %>% round(3),
           route_omed_df$tl_prop_4 %>% mean() %>% round(3),
           route_umed_df$tl_prop_4 %>% mean() %>% round(3),
           route_df$tl_prop_4 %>% mean() %>% round(3))
       ),
       file = file.path(tables_dir, "ols_calibration.tex"))

# Regression each route --------------------------------------------------------

coef_df <- route_df %>%
  group_by(uid) %>%
  group_modify(~ {
    m <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4, data = .x)
    tidy(m, conf.int = TRUE)
  }) %>%
  ungroup() %>%
  filter(term %in% c("tl_prop_2", "tl_prop_3", "tl_prop_4")) %>%
  mutate(
    traffic_level = recode(term,
                           tl_prop_2 = "Level 2",
                           tl_prop_3 = "Level 3",
                           tl_prop_4 = "Level 4"
    )
  )

# Create ordering based on Level 2 coefficient magnitude
uid_order <- coef_df %>%
  filter(traffic_level == "Level 2") %>%
  arrange(desc(abs(estimate))) %>%
  pull(uid)

# Apply ordering to full dataframe
coef_df <- coef_df %>%
  mutate(uid = factor(uid, levels = uid_order))

ggplot(coef_df, aes(x = estimate, y = uid)) +
  geom_point(size = 0.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  facet_wrap(~ traffic_level) +
  geom_vline(xintercept = 0, linetype = "solid") +   # solid zero line
  labs(
    x = "Coefficient (+/- 95% CI)",
    y = "Route ID",
    title = "Heterogeneity in regression coefficients across routes"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),  # light gray panel
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(face = "bold", size = 10)
  )

ggsave(filename = file.path(figures_dir, "reg_coef_hetero.png"),
       height = 3, width = 7)

# Elasticites ------------------------------------------------------------------
library(tidyverse)

beta <- c(
  tl2 = 1.307,
  tl3 = 3.338,
  tl4 = 6.308
)

tt_table <- expand_grid(
  prop = seq(0, 1, by = 0.1),
  traffic_level = names(beta)
) %>%
  mutate(
    tt_multiplier = exp(beta[traffic_level] * prop)
  ) %>%
  pivot_wider(
    names_from = traffic_level,
    values_from = tt_multiplier
  ) %>%
  mutate(
    across(-prop, ~ round(.x, 2))
  ) %>%
  mutate(tex = paste(prop, " & ", tl2, " & ", tl3, " & ", tl4, " \\\\ \n"))

sink(file.path(tables_dir, "prop_tl_ttincrease.tex"))
cat("\\begin{tabular}{l | lll} \n")
cat("\\hline \n")
cat(" & \\multicolumn{3}{c}{Delay factor when proportion} \\\\ \n")
cat(" & \\multicolumn{3}{c}{of road is at specified traffic level} \\\\ \n")
cat("Proportion & 2 (Medium) & 3 (High) & 4 (Severe) \\\\ \n")
cat("\\hline \n")
tt_table$tex %>% cat()
cat("\\hline \n")
cat("\\end{tabular} ")
sink()

