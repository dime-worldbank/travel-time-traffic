# Congestion in Nairobi 

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide_clean.Rds"))

# Cleanup ----------------------------------------------------------------------
df <- df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"))

# Time of Day ------------------------------------------------------------------
df %>%
  group_by(hour, day_type) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  
  pivot_longer(cols = -c(hour, day_type, uid)) %>%
  filter(name %in% c("mb_speed_in_traffic_kmh",
                     "gg_speed_in_traffic_kmh",
                     "mb_tl_prop_234",
                     "gg_tl_prop_234")) %>%
  mutate(name_clean = case_when(
    name == "gg_speed_in_traffic_kmh" ~ "Average Speed (km/h)\nGoogle",
    name == "mb_speed_in_traffic_kmh" ~ "Average Speed (km/h)\nMapbox",
    name == "gg_tl_prop_234" ~ "Proportion 2 - 4 Traffic\nGoogle",
    name == "mb_tl_prop_234" ~ "Proportion 2 - 4 Traffic\nMapbox"
  )) %>%
  
  ggplot() +
  geom_vline(aes(xintercept = 6), color = "gray90", size = 3) +
  geom_vline(aes(xintercept = 7), color = "gray90", size = 3) +
  geom_vline(aes(xintercept = 8), color = "gray90", size = 3) +
  geom_vline(aes(xintercept = 9), color = "gray90", size = 3) +
  
  geom_vline(aes(xintercept = 4 + 12), color = "gray90", size = 3) +
  geom_vline(aes(xintercept = 5 + 12), color = "gray90", size = 3) +
  geom_vline(aes(xintercept = 6 + 12), color = "gray90", size = 3) +
  geom_vline(aes(xintercept = 7 + 12), color = "gray90", size = 3) +
  geom_vline(aes(xintercept = 8 + 12), color = "gray90", size = 3) +
  geom_line(aes(x = hour,
                y = value,
                color = day_type)) +
  facet_wrap(~name_clean,
             scales = "free_y") +
  labs(x = "Hour of Day",
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue1")) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) 

ggsave(filename = file.path(figures_dir, "cong_timeofday.png"),
       height = 4, width = 6)

# Weekly Trends ----------------------------------------------------------------
df %>%
  mutate(week = datetime %>% floor_date(unit = "week")) %>%
  group_by(week, day_type) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  
  pivot_longer(cols = -c(week, day_type, uid)) %>%
  filter(name %in% c("mb_speed_in_traffic_kmh",
                     "gg_speed_in_traffic_kmh",
                     "mb_tl_prop_234",
                     "gg_tl_prop_234")) %>%
  mutate(name_clean = case_when(
    name == "gg_speed_in_traffic_kmh" ~ "Average Speed (km/h)\nGoogle",
    name == "mb_speed_in_traffic_kmh" ~ "Average Speed (km/h)\nMapbox",
    name == "gg_tl_prop_234" ~ "Proportion 2 - 4 Traffic\nGoogle",
    name == "mb_tl_prop_234" ~ "Proportion 2 - 4 Traffic\nMapbox"
  )) %>%
  
  ggplot(aes(x = week,
             y = value)) +
  geom_col() +
  facet_wrap(~name_clean,
             scales = "free_y") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) 

# Across Road ------------------------------------------------------------------

# Speed distribution -----------------------------------------------------------


