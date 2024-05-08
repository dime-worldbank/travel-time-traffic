# Nairobi election

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))
nbo_df   <- readRDS(file.path(analysis_data_dir, "gadm1_wide.Rds"))

# Make daily data --------------------------------------------------------------
route_sum_df <- route_df %>%
  dplyr::mutate(datetime = datetime %>% floor_date(unit = "day")) %>%
  group_by(datetime) %>%
  dplyr::summarise(gg_tl_prop_234 = mean(gg_tl_prop_234, na.rm = T),
                   gg_tl_prop_34 = mean(gg_tl_prop_34, na.rm = T),
                   gg_tl_prop_4 = mean(gg_tl_prop_4, na.rm = T),
                   # gg_tl_mean = mean(gg_tl_mean, na.rm = T),
                   # gg_tl_max = mean(gg_tl_max, na.rm = T),
                   gg_duration_in_traffic_min = mean(gg_duration_in_traffic_min, na.rm = T),
                   gg_distance_km = mean(gg_distance_km, na.rm = T),
                   gg_speed_in_traffic_kmh = mean(gg_speed_in_traffic_kmh, na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = -datetime) %>%
  dplyr::mutate(type = "Route")

nbo_sum_df <- nbo_df %>%
  dplyr::mutate(datetime = datetime %>% floor_date(unit = "day")) %>%
  group_by(datetime) %>%
  dplyr::summarise(gg_tl_prop_234 = mean(gg_tl_prop_234, na.rm = T),
                   gg_tl_prop_34  = mean(gg_tl_prop_34, na.rm = T),
                   gg_tl_prop_4   = mean(gg_tl_prop_4, na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = -datetime) %>%
  dplyr::mutate(type = "City")

# gg_tl_mean     = mean(gg_tl_mean, na.rm = T),
# gg_tl_max      = mean(gg_tl_max, na.rm = T)

# Trends figure ----------------------------------------------------------------
cong_df <- bind_rows(route_sum_df,
                     nbo_sum_df) %>%
  mutate(name_clean = case_when(
    name == "gg_tl_prop_234" ~ "Traffic, Prop 2-4",
    name == "gg_tl_prop_34" ~ "Traffic, Prop 3-4",
    name == "gg_tl_prop_4" ~ "Traffic, Prop 4",
    name == "gg_tl_mean" ~ "Traffic, Average",
    name == "gg_tl_max" ~ "Traffic, Maximum",
    name == "gg_duration_in_traffic_min" ~ "Duration (min)",
    name == "gg_distance_km" ~ "Distance (km)",
    name == "gg_speed_in_traffic_kmh" ~ "Speed (km/h)"
  )) %>%
  mutate(name_clean = paste0(name_clean, "\n[", type , "]")) %>%
  mutate(name_clean = name_clean %>%
           factor(levels = c("Distance (km)\n[Route]",
                             "Duration (min)\n[Route]",
                             "Speed (km/h)\n[Route]",
                             
                             "Traffic, Prop 2-4\n[Route]",
                             "Traffic, Prop 3-4\n[Route]",
                             "Traffic, Prop 4\n[Route]",
                             # "Traffic, Average\n[Route]",
                             # "Traffic, Maximum\n[Route]",
                             
                             "Traffic, Prop 2-4\n[City]",
                             "Traffic, Prop 3-4\n[City]",
                             "Traffic, Prop 4\n[City]")))

# "Traffic, Average\n[City]",
# "Traffic, Maximum\n[City]"

cong_df %>%
  dplyr::filter(datetime <= ymd("2022-09-01")) %>%
  ggplot() +
  geom_vline(xintercept = ymd("2022-08-09", tz = "Africa/Nairobi"),
             color = "red") + 
  geom_vline(xintercept = ymd("2022-08-15", tz = "Africa/Nairobi"),
             color = "forestgreen") + 
  geom_line(aes(x = datetime,
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

lm1 <- feols(gg_speed_in_traffic_kmh    ~ period | uid + dow + hour, data = route_df)
lm2 <- feols(gg_duration_in_traffic_min ~ period | uid + dow + hour, data = route_df)
lm3 <- feols(gg_distance_km             ~ period | uid + dow + hour, data = route_df)

lm4 <- feols(gg_tl_prop_234             ~ period | uid + dow + hour, data = route_df)
lm5 <- feols(gg_tl_prop_234             ~ period | dow + hour, data = nbo_df, vcov = "hetero")

lm6 <- feols(gg_tl_prop_34              ~ period | uid + dow + hour, data = route_df)
lm7 <- feols(gg_tl_prop_34              ~ period | dow + hour, data = nbo_df, vcov = "hetero")

lm8 <- feols(gg_tl_prop_4               ~ period | uid + dow + hour, data = route_df)
lm9 <- feols(gg_tl_prop_4               ~ period | dow + hour, data = nbo_df, vcov = "hetero")

lm10 <- feols(gg_tl_mean               ~ period | uid + dow + hour, data = route_df)
lm11 <- feols(gg_tl_mean               ~ period | dow + hour, data = nbo_df, vcov = "hetero")

lm12 <- feols(gg_tl_max               ~ period | uid + dow + hour, data = route_df)
lm13 <- feols(gg_tl_max               ~ period | dow + hour, data = nbo_df, vcov = "hetero")

modelsummary_tab(list("Speed (km)" = lm1,
                      "Duration (min)" = lm2,
                      "Distance (km)" = lm3,
                      "2-4 Traffic" = lm4,
                      "2-4 Traffic" = lm5,
                      "3-4 Traffic" = lm6,
                      "3-4 Traffic" = lm7,
                      "4 Traffic" = lm8,
                      "4 Traffic" = lm9,
                      "Avg Traffic" = lm10,
                      "Avg Traffic" = lm11,
                      "Max Traffic" = lm12,
                      "Max Traffic" = lm13),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("period" = "Election Week"),
                 gof_map = c("nobs", "adj.r.squared"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6, ~V7, ~V8, ~V9, ~V10, ~V11, ~V12, ~V13,
                                    'Unit', "Route", "Route", "Route", "Route", "City", "Route", "City", "Route", "City", "Route", "City", "Route", "City", 
                                    'Route FE', "Y", "Y", "Y", "Y", "N/A", "Y", "N/A", "Y", "N/A", "Y", "N/A", "Y", "N/A",
                                    'Hour FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
                                    'Day of Week FE', "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
                 output = file.path(tables_dir,
                                    "nbo_elec.tex"))

# ADM1 trends ------------------------------------------------------------------


