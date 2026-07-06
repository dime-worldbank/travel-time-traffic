# Congestion in Nairobi 

hvline_color <- "black"

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))
gadm1_df <- readRDS(file.path(analysis_data_dir, "gadm1_wide.Rds"))

gadm3_df <- readRDS(file.path(analysis_data_dir, "estates_wide.Rds"))
gadm3_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))

osm_df <- readRDS(file.path(analysis_data_dir, "osm_10m_wide.Rds"))
gadm3_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))

# Cleanup ----------------------------------------------------------------------
route_df$gg_tl_prop_4[is.na(route_df$gg_tl_prop_4)] <- 0
gadm1_df$gg_tl_prop_4[is.na(gadm1_df$gg_tl_prop_4)] <- 0

route_df <- route_df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         date_week = datetime %>% floor_date(unit = "weeks"),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"),
         traffic_index = gg_tl_prop_2*1.086 + gg_tl_prop_3*3.946 + gg_tl_prop_4*5.979)

gadm1_df <- gadm1_df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         date_week = datetime %>% floor_date(unit = "weeks"),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"),
         traffic_index = gg_tl_prop_2*1.086 + gg_tl_prop_3*3.946 + gg_tl_prop_4*5.979)

# Congestion indicators --------------------------------------------------------
route_df <- route_df %>%
  dplyr::mutate(obs_modal = gg_distance_m_mode == gg_distance_m) %>%
  group_by(uid) %>%
  dplyr::mutate(speed_nocong = quantile(gg_speed_in_traffic_kmh[(obs_modal %in% T) & (hour %in% 3)], 0.5),
                #distance_nocong = quantile(gg_distance_km[(obs_modal %in% T) & (hour %in% 3)], 0.5),
                duration_nocong = quantile(gg_duration_in_traffic_min[(obs_modal %in% T) & (hour %in% 3)], 0.5)) %>%
  ungroup() %>%
  dplyr::mutate(speed_pc = (gg_speed_in_traffic_kmh - speed_nocong)/speed_nocong*100,
                distance_pc = (gg_distance_m - gg_distance_m_mode)/gg_distance_m_mode*100,
                duration_pc = (gg_duration_in_traffic_min - duration_nocong)/duration_nocong*100)

# Trends in Indicators ---------------------------------------------------------
route_week_df <- route_df %>%
  filter(uid %in% c(1,2,5:13)) %>%
  group_by(date_week, day_type) %>%
  dplyr::summarise(speed_pc = mean(speed_pc, na.rm = T),
                   duration_pc = mean(duration_pc, na.rm = T),
                   distance_pc = mean(distance_pc, na.rm = T),
                   traffic_index = mean(traffic_index, na.rm = T)) %>%
  ungroup()

route_hour_df <- route_df %>%
  filter(uid %in% c(1,2,5:13)) %>%
  group_by(hour, day_type) %>%
  dplyr::summarise(speed_pc = mean(speed_pc, na.rm = T),
                   duration_pc = mean(duration_pc, na.rm = T),
                   distance_pc = mean(distance_pc, na.rm = T),
                   traffic_index = mean(traffic_index, na.rm = T)) %>%
  ungroup()

gadm1_week_df <- gadm1_df %>%
  group_by(date_week, day_type) %>%
  dplyr::summarise(traffic_index_city = mean(traffic_index, na.rm = T)) %>%
  ungroup() 

gadm1_hour_df <- gadm1_df %>%
  group_by(hour, day_type) %>%
  dplyr::summarise(traffic_index_city = mean(traffic_index, na.rm = T)) %>%
  ungroup() 

week_df <- full_join(route_week_df,
                     gadm1_week_df,
                     by = c("date_week", "day_type")) %>%
  pivot_longer(cols = -c("date_week", "day_type")) %>%
  dplyr::mutate(name_clean = case_when(
    name == "speed_pc" ~ "Speed:\n% Change from Free Flow Conditions\n[11 Routes]",
    name == "duration_pc" ~ "Duration\n% Change from Free Flow Conditions\n[11 Routes]",
    name == "distance_pc" ~ "Distance\n% Change from Modal Route Distance\n[11 Routes]",
    name == "traffic_index" ~ "Traffic Index\n[11 Routes]",
    name == "traffic_index_city" ~ "Traffic Index\n[All Nairobi]"
  ))

hour_df <- full_join(route_hour_df,
                     gadm1_hour_df,
                     by = c("hour", "day_type")) %>%
  pivot_longer(cols = -c("hour", "day_type")) %>%
  dplyr::mutate(name_clean = case_when(
    name == "speed_pc" ~ "Speed:\n% Change from Free Flow Conditions\n[11 Routes]",
    name == "duration_pc" ~ "Duration\n% Change from Free Flow Conditions\n[11 Routes]",
    name == "distance_pc" ~ "Distance\n% Change from Modal Route Distance\n[11 Routes]",
    name == "traffic_index" ~ "Traffic Index\n[11 Routes]",
    name == "traffic_index_city" ~ "Traffic Index\n[All Nairobi]"
  ))

# Map prep ---------------------------------------------------------------------
gadm3_sum_df <- gadm3_df %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(label = T),
                day_type = ifelse(dow %in% c("Sat", "Sun"),
                       "Weekend",
                       "Weekday")) %>%
  dplyr::filter(datetime >= ymd("2022-08-01"),
                count_1 > 0,
                hour %in% 7:21) %>%
  dplyr::mutate(traffic_index = gg_tl_prop_2*1.086 + gg_tl_prop_3*3.946 + gg_tl_prop_4*5.979) %>%
  group_by(uid, day_type) %>%
  dplyr::summarise(traffic_index = mean(traffic_index)) %>%
  ungroup() 

gadm3_data_sf <- gadm3_sf %>%
  left_join(gadm3_sum_df, by = "uid")

# Figures ----------------------------------------------------------------------
p_1 <- week_df %>%
  dplyr::filter(date_week >= ymd("2022-08-01")) %>%
  ggplot() +
  geom_line(aes(x = date_week,
                y = value,
                color = day_type),
            linewidth = 1) +
  facet_wrap(~name_clean, scales = "free_y") +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue")) +
  labs(title = "A. Change in Indicators Over Time",
       color = NULL,
       x = NULL,
       y = NULL) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 7, color = "black")) 

p_2 <- hour_df %>%
  ggplot() +
  geom_line(aes(x = hour,
                y = value,
                color = day_type),
            linewidth = 1) +
  facet_wrap(~name_clean, scales = "free_y") +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue")) +
  labs(title = "B. Indicators by Time of Day",
       color = NULL,
       x = "Hour",
       y = NULL) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 7, color = "black")) 

quantile(gadm3_data_sf$traffic_index, 0.975)
gadm3_data_sf$traffic_index_2 <- gadm3_data_sf$traffic_index
gadm3_data_sf$traffic_index_2[gadm3_data_sf$traffic_index_2 >= 0.5] <- 0.5
p_3 <- ggplot() +
  geom_sf(
    data = gadm3_data_sf,
    aes(fill = traffic_index_2),
    color = "black"
  ) +
  scale_fill_distiller(
    palette = "Spectral",
    direction = -1, 
    na.value = "gray90",
    name = "Traffic\nIndex"
  ) +
  labs(title = "C. Traffic Over Space") +
  theme_void() +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~day_type)

p <- ggarrange(p_1, p_2, p_3, ncol = 1, heights = c(0.35, 0.35, 0.3))

ggsave(p, 
       filename = file.path(figures_dir, "casestudy1_indicators.png"),
       height = 11.5, width = 10.5)

