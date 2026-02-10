# Nairobi election

# Load data --------------------------------------------------------------------
route_df    <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))
nbo_df      <- readRDS(file.path(analysis_data_dir, "google_gadm1.Rds"))
estates_df  <- readRDS(file.path(analysis_data_dir, "google_estates.Rds"))

beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Make daily data --------------------------------------------------------------
route_df <- route_df %>%
  dplyr::mutate(duration_in_traffic_min = duration_in_traffic_s/60,
                distance_km = distance_m / 1000) %>%
  mk_traffic_indicators(beta) %>%
  dplyr::mutate(type = "Route")

nbo_df <- nbo_df %>%
  mk_traffic_indicators(beta) %>%
  dplyr::mutate(type = "City")

estates_df <- estates_df %>%
  mk_traffic_indicators(beta)

# Trends figure ----------------------------------------------------------------
cong_df <- bind_rows(route_df,
                     nbo_df) %>%
  pivot_longer(cols = -c(uid, date, datetime, type)) %>%
  dplyr::filter(name %in% c("distance_km", "duration_in_traffic_min", "speed_in_traffic_kmh", "delay_factor", "delay_factor_od")) %>%
  group_by(date, type, name) %>%
  dplyr::summarise(value = mean(value)) %>%
  ungroup() %>%
  mutate(name_clean = case_when(
    name == "tl_prop_234" ~ "Traffic, Prop 2-4",
    name == "tl_prop_34" ~ "Traffic, Prop 3-4",
    name == "tl_prop_4" ~ "Traffic, Prop 4",
    name == "tl_mean" ~ "Traffic, Average",
    name == "tl_max" ~ "Traffic, Maximum",
    name == "duration_in_traffic_min" ~ "Duration (min)",
    name == "distance_km" ~ "Distance (km)",
    name == "speed_in_traffic_kmh" ~ "Speed (km/h)",
    name == "delay_factor" ~ "Delay Factor (Traffic Level Data)",
    name == "delay_factor_od" ~ "Delay Factor (O-D Data)"
  )) %>%
  mutate(name_clean = paste0(name_clean, "\n[", type , "]")) %>%
  mutate(name_clean = name_clean %>%
           factor(levels = c("Duration (min)\n[Route]",
                             "Speed (km/h)\n[Route]",
                             "Distance (km)\n[Route]",
                             
                             "Delay Factor (O-D Data)\n[Route]",
                             "Delay Factor (Traffic Level Data)\n[Route]",
                             "Delay Factor (Traffic Level Data)\n[City]"))) %>%
  dplyr::filter(!is.na(name_clean))

cong_df %>%
  dplyr::filter(date <= ymd("2022-09-01")) %>%
  ggplot() +
  geom_vline(xintercept = ymd("2022-08-09"),
             color = "red") + 
  geom_vline(xintercept = ymd("2022-08-15"),
             color = "forestgreen") + 
  geom_line(aes(x = date,
                y = value)) +
  facet_wrap(~name_clean,
             scales = "free_y") +
  labs(x = NULL,
       y = NULL) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(filename = file.path(figures_dir, "nbo_election_trends.png"),
       height = 4, width = 10)

# Regression -------------------------------------------------------------------
ymd("2022-08-09") + 7*4 - 1
length(ymd("2022-08-09"):ymd("2022-09-05"))

add_period <- function(df){
  
  df %>%
    dplyr::mutate(period = case_when(
      (datetime >= ymd("2022-08-09", tz = "Africa/Nairobi")) & 
        (datetime <= ymd("2022-08-15", tz = "Africa/Nairobi")) ~ 1,
      (datetime >= ymd("2022-08-16", tz = "Africa/Nairobi")) & 
        (datetime <= ymd("2022-09-05", tz = "Africa/Nairobi")) ~ 0
    )) %>%
    filter(!is.na(period))
  
}

route_df <- route_df %>% 
  add_period() %>%
  dplyr::mutate(dow = datetime %>% wday(),
                hour = datetime %>% hour())

nbo_df <- nbo_df %>% 
  add_period() %>%
  dplyr::mutate(dow = datetime %>% wday(),
                hour = datetime %>% hour())

lm1 <- feols(speed_in_traffic_kmh    ~ period | uid + dow + hour, data = route_df)
lm2 <- feols(duration_in_traffic_min ~ period | uid + dow + hour, data = route_df)
lm3 <- feols(distance_km             ~ period | uid + dow + hour, data = route_df)
lm4 <- feols(delay_factor_od         ~ period | uid + dow + hour, data = route_df)

lm5 <- feols(delay_factor             ~ period | uid + dow + hour, data = route_df)
lm6 <- feols(delay_factor             ~ period | dow + hour, data = nbo_df, vcov = "hetero")

#### Table Settings
my_style = style.tex(tpt = TRUE, 
                     notes.tpt.intro = "\\footnotesize")
setFixest_etable(style.tex = my_style)

dict = c(speed_in_traffic_kmh = "Speed (km)",
         duration_in_traffic_min = "Duration (min)",
         distance_km = "Distance (km)",
         delay_factor = "Delay Factor (TL Data)",
         delay_factor_od = "Delay Factor (OD Data)",
         tl_prop_234 = "Prop 2-4 Traffic",
         tl_prop_34 = "Prop 3-4 Traffic",
         tl_prop_4 = "Prop 4 Traffic",
         tl_prop_mean = "Avg Traffic",
         tl_prop_max = "Max Traffic",
         uid = "Route",
         period = "Election Week",
         dow = "Day of Week",
         hour = "Hour")
setFixest_dict(dict)

modelsummary_tab(list("Duration (min)"          = lm2,
                      "Speed (km)"              = lm1,
                      "Distance (km)"           = lm3,
                      "Delay Factor, OD Data"  = lm4,
                      "Delay Factor, TL Data"  = lm5,
                      "Delay Factor, TL Data"     = lm6),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("period" = "Election Week"),
                 gof_map = c("nobs", "adj.r.squared"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6,
                                    'Unit', "Route", "Route", "Route", "Route", "Route", "City",
                                    'Route FE', "Y", "Y", "Y", "Y", "Y", "N/A",
                                    'Hour FE',  "Y", "Y", "Y", "Y", "Y", "Y",
                                    'Day of Week FE', "Y", "Y", "Y", "Y", "Y", "Y"),
                 output = file.path(tables_dir,
                                    "nbo_elec.tex"))

# ADM3 trends ------------------------------------------------------------------
#### Prep data
estates_df <- estates_df %>% 
  add_period() %>%
  dplyr::mutate(dow = datetime %>% wday(),
                hour = datetime %>% hour())

#### Regression
clean_lm <- function(lm1, dv_i){
  lm1$coeftable %>%
    as.data.frame() %>%
    clean_names() %>%
    mutate(dv = dv_i)
}

lm_estates_df <- map_df(unique(estates_df$uid), function(uid_i){
  
  print(uid_i)
  
  estates_df_i <- estates_df[estates_df$uid %in% uid_i,]
  
  lm_df <- feols(delay_factor ~ period | dow + hour, data = estates_df_i, vcov = "hetero") %>% clean_lm("delay_factor")
  lm_df$uid <- uid_i
  return(lm_df)
  
})

#### Figure
estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))

estates_sf <- estates_sf %>%
  left_join(lm_estates_df, by = "uid") %>%
  mutate(sig = pr_t < 0.05)

title_i <- "Delay Factor"

estates_sf_var <- estates_sf %>%
  filter(dv %in% "delay_factor") 

max_v <- estates_sf_var$estimate %>% abs() %>% max()

estates_sf_var <- estates_sf_var %>%
  dplyr::mutate(estimate = case_when(
    sig %in% T ~ estimate,
    TRUE ~ NA
  ))

estates_sf_var %>%
  ggplot() +
  geom_sf(aes(fill = estimate,
              color = "p > 0.05")) +
  scale_fill_gradient2(low = "green2",
                       mid = "white",
                       high = "firebrick",
                       limits = c(-max_v, max_v),
                       na.value = "gray80") +
  scale_color_manual(values = "black") +
  labs(fill = "Coefficient",
       color = "",
       title = title_i) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5))

ggsave(filename = file.path(figures_dir, "nbo_elec_adm3.png"),
       height = 5, width = 6)

