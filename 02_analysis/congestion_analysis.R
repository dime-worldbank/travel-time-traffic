# Congestion in Nairobi 

# TODO:
# 1. Congestion roads vs congestion whole city (do roads capture city level patterns?)
# 2. Speed dist -> modal route

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))
gadm_df <- readRDS(file.path(analysis_data_dir, "gadm2_wide.Rds"))
gadm_sf  <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_2_pk.rds"))

gadm1_df <- readRDS(file.path(analysis_data_dir, "gadm1_wide.Rds"))


# Cleanup ----------------------------------------------------------------------
route_df <- route_df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"))

gadm_df <- gadm_df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"))

# Time of Day ------------------------------------------------------------------
route_df %>%
  group_by(hour, day_type) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  
  pivot_longer(cols = -c(hour, day_type, uid)) %>%
  filter(name %in% c("gg_duration_in_traffic_min",
                     "gg_speed_in_traffic_kmh",
                     "gg_tl_prop_234")) %>%
  mutate(name_clean = case_when(
    name == "gg_speed_in_traffic_kmh" ~ "Average Speed (km/h)",
    name == "gg_duration_in_traffic_min" ~ "Average Duration (Mins)",
    name == "gg_tl_prop_234" ~ "Proportion 2 - 4 Traffic"
  )) %>%
  
  ggplot() +
  geom_vline(aes(xintercept = 6), color = "gray90", linewidth = 3) +
  geom_vline(aes(xintercept = 7), color = "gray90", linewidth = 3) +
  geom_vline(aes(xintercept = 8), color = "gray90", linewidth = 3) +
  geom_vline(aes(xintercept = 9), color = "gray90", linewidth = 3) +
  
  geom_vline(aes(xintercept = 4 + 12), color = "gray90", linewidth = 3) +
  geom_vline(aes(xintercept = 5 + 12), color = "gray90", linewidth = 3) +
  geom_vline(aes(xintercept = 6 + 12), color = "gray90", linewidth = 3) +
  geom_vline(aes(xintercept = 7 + 12), color = "gray90", linewidth = 3) +
  geom_vline(aes(xintercept = 8 + 12), color = "gray90", linewidth = 3) +
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
       height = 2.5, width = 8)

# Over time --------------------------------------------------------------------
# TODO: Compare against individual roads.
# gadm1_df %>%
#   ggplot() +
#   geom_line(aes(x = datetime,
#                 y = gg_tl_prop_234))
# 
# a <- tt_sf[!is.na(tt_sf$segment_id),]
# a <- a[a$segment_id %in% 3:4,]
# a <- a[!is.na(a$speed_in_traffic_kmh),]
# a$datetime %>% summary()
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolylines(data = a[a$segment_id %in% 3,][40801,])
# 
# route_df$road_name[route_df$uid %in% 3:4]
# 
# route_df %>%
#   filter(!is.na(gg_tl_prop_234),
#          !is.na(gg_speed_in_traffic_kmh)) %>%
#   mutate(week = datetime %>% floor_date(unit = "month")) %>%
#   
#   group_by(uid, week, day_type) %>%
#   dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
#   ungroup() %>%
#   filter(uid %in% 3:4) %>%
#   
#   ggplot() +
#   geom_line(aes(x = week,
#                 y = gg_distance_km,
#                 color = day_type)) +
#   facet_wrap(~uid)

# TODO: Add third row of traffic for all of Nairobi
route_df %>% 
  filter(!is.na(gg_tl_prop_234),
         !is.na(gg_speed_in_traffic_kmh)) %>%
  filter(uid %in% 1:13) %>% # constant sample
  mutate(week = datetime %>% floor_date(unit = "week")) %>%
  
  group_by(week, day_type) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  pivot_longer(cols = -c(week, day_type, uid)) %>%
  filter(name %in% c("gg_tl_prop_234",
                     "gg_tl_prop_34",
                     "gg_tl_prop_4",
                     "gg_speed_in_traffic_kmh",
                     "gg_duration_in_traffic_min",
                     "gg_distance_km")) %>%
  
  # group_by(name, day_type) %>%
  # dplyr::mutate(value = zoo::rollmean(value, k = 4, fill = NA, na.rm=T)) %>%
  # ungroup() %>%
  
  #rename_var("name") %>%
  dplyr::mutate(name = case_when(
    name == "gg_tl_prop_234" ~ "Traffic, Prop. 2-4",
    name == "gg_tl_prop_34" ~ "Traffic, Prop. 3-4",
    name == "gg_tl_prop_4" ~ "Traffic, Prop. 4",
    name == "gg_speed_in_traffic_kmh" ~ "Average Speed (km/)",
    name == "gg_duration_in_traffic_min" ~ "Average Duartion (mins)",
    name == "gg_distance_km" ~ "Average Distance (km)"
  )) %>%
  
  ggplot(aes(x = week,
             y = value,
             color = day_type)) +
  geom_line() +
  facet_wrap(~name,
             scales = "free_y") +
  labs(color = NULL,
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("dodgerblue",
                                "darkorange")) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size = 7, color = "black")) 

ggsave(filename = file.path(figures_dir, "indicators_over_time.png"),
       height = 3.5, width = 9.5)

# Map --------------------------------------------------------------------------
# TODO: Do hexagon instead?
gadm_wide_df <- gadm_df %>%
  group_by(NAME_2, day_type) %>%
  dplyr::summarise(gg_tl_prop_234 = mean(gg_tl_prop_234, na.rm = T),
                   gg_tl_prop_34  = mean(gg_tl_prop_34, na.rm = T),
                   gg_tl_prop_4   = mean(gg_tl_prop_4, na.rm = T)) 

gadm_all_sf <- gadm_sf %>%
  left_join(gadm_wide_df, by = "NAME_2") 

ggplot() +
  geom_sf(data = gadm_all_sf,
          aes(fill = gg_tl_prop_234)) +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~day_type) + 
  labs(fill = "% of roads with\ntraffic levels 2-4") +
  theme_void() + 
  theme(strip.text = element_text(face = "bold"))

ggsave(filename = file.path(figures_dir, "congestion_map.png"),
       height = 2, width = 8)

# Roads: speed and congestion --------------------------------------------------
p1 <- route_df %>%
  filter(!is.na(gg_speed_in_traffic_kmh),
         gg_diff_mode %in% F) %>%
  
  ggplot() +
  geom_boxplot(aes(x = gg_speed_in_traffic_kmh,
                   y = reorder(road_name,
                               gg_speed_in_traffic_kmh)),
               fill = "gray90") +
  labs(x = "Speed (km/h)",
       y = NULL,
       title = "A. Distribution of speeds across roads",
       subtitle = "Only considering speeds on modal route") +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 12)) 

p2 <- route_df %>%
  filter(!is.na(gg_tl_prop_234),
         gg_diff_mode %in% F) %>%
  
  ggplot() +
  geom_boxplot(aes(x = gg_tl_prop_234,
                   y = reorder(road_name,
                               -gg_tl_prop_234)),
               fill = "gray90") +
  labs(x = NULL,
       y = NULL,
       title = "B. Percent of road congested",
       subtitle = "Congestion refers to traffic level 2-4") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 12)) 

p <- ggarrange(p1, p2)

ggsave(p, filename = file.path(figures_dir, "dist_across_roads.png"),
       height = 4, width = 10)

