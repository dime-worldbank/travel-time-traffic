# Deviation Example 2: Permanent Change

# Deviation Example

# TRENDS IN INDICATORS =========================================================
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

p_trends <- route_df %>%
  dplyr::mutate(date = datetime %>% date()) %>%
  #dplyr::filter(date %in% ymd("2023-07-16")) %>%
  dplyr::filter(uid %in% 3) %>%
  dplyr::select(datetime, gg_speed_in_traffic_kmh, gg_duration_in_traffic_min, gg_distance_km, 
                gg_tl_prop_234, gg_tl_prop_34, gg_tl_prop_4, gg_tl_mean, gg_tl_max) %>%
  pivot_longer(cols = -c(datetime)) %>%
  
  mutate(datetime = floor_date(datetime, unit = "weeks")) %>%
  group_by(datetime, name) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(value)) %>%
  
  rename_var("name") %>%
  dplyr::mutate(name = case_when(
    name == "Average Speed" ~ "Average Speed (km/h)",
    name == "Duration" ~ "Duration (mins)",
    TRUE ~ name
  )) %>%
  dplyr::mutate(name = name %>%
                  factor(levels = c("Distance (km)",
                                    "Average Speed (km/h)",
                                    "Duration (mins)",
                                    "Traffic, Prop 2,3,4",
                                    "Traffic, Prop 3,4",
                                    "Traffic, Prop 4",
                                    "Traffic, Average",
                                    "Traffic, Maximum"))) %>%
  ggplot() +
  geom_vline(xintercept = ymd("2023-02-01", tz = "Africa/Nairobi"), color = "red") +
  geom_line(aes(x = datetime,
                y = value)) +
  facet_wrap(~name,
             scales = "free_y") +
  labs(x = NULL,
       y = NULL,
       title = "C. Trends in select indicators") +
  theme_classic2() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_blank(),
        axis.text = element_text(size = 6)) 

# ggsave(filename = file.path(figures_dir, "deviation_example_day.png"),
#        height = 4, width = 8)

# MAP ==========================================================================

# Load data --------------------------------------------------------------------
tt_sf <- readRDS(file.path(tt_dir,
                           "google_tt_data_geom.Rds"))

tt_sf <- tt_sf %>%
  dplyr::filter(!is.na(speed_in_traffic_kmh)) %>%
  group_by(segment_id) %>%
  mutate(distance_m_mode = Mode(distance_m, na.rm = T)) %>%
  ungroup() %>%
  mutate(diff_mode = abs(distance_m - distance_m_mode) > 100) %>%
  
  group_by(segment_id) %>%
  dplyr::mutate(distance_m_modal             = mean(distance_m[diff_mode == F]),
                duration_in_traffic_s_modal = mean(duration_in_traffic_s[diff_mode == F]),
                speed_in_traffic_kmh_modal    = mean(speed_in_traffic_kmh[diff_mode == F])) %>%
  ungroup() %>%
  mutate(distance_m_diff             = distance_m - distance_m_modal,
         duration_in_traffic_s_diff = duration_in_traffic_s - duration_in_traffic_s_modal,
         speed_in_traffic_kmh_diff    = speed_in_traffic_kmh - speed_in_traffic_kmh_modal)

tt_ex_sf_i <- tt_sf %>%
  filter(segment_id %in% 3,
         datetime %in% ymd_hms("2023-09-20 17:00:00", tz = "Africa/Nairobi"))

tt_modal_sf <- tt_sf[(tt_sf$segment_id %in% tt_ex_sf_i$segment_id) & (tt_sf$diff_mode %in% F),][1,]

tt_roads_sf <- bind_rows(tt_ex_sf_i, tt_modal_sf) %>%
  st_buffer(dist = 1 * 1000)

# Prep OSM ---------------------------------------------------------------------
nbo_sf <- readRDS(file.path(db_dir, "Data", "GADM", "RawData",
                            "gadm41_KEN_1_pk.rds"))
nbo_sf <- nbo_sf[nbo_sf$NAME_1 %in% "Nairobi",]

roads_sf <- readRDS(file.path(db_dir, "Data", "OSM", "FinalData",
                              "gis_osm_roads_free_1_nairobi.Rds"))

roads_1_sf <- roads_sf[str_detect(roads_sf$fclass, "motorway|trunk|primary"),] %>% 
  st_as_sf() %>%
  st_intersection(tt_roads_sf)

roads_2_sf <- roads_sf[str_detect(roads_sf$fclass, "secondary|tertiary"),] %>% 
  st_as_sf() %>%
  st_intersection(tt_roads_sf)

# Map --------------------------------------------------------------------------
route_df <- bind_rows(
  data.frame(type = "Original Route",
             distance_km             = round(tt_modal_sf$distance_m / 1000,1),
             duration_in_traffic_min = round(tt_modal_sf$duration_in_traffic_s / 60,1),
             speed_in_traffic_kmh    = round(tt_modal_sf$speed_in_traffic_kmh,1)),
  
  data.frame(type = "New Route",
             distance_km             = round(tt_ex_sf_i$distance_m / 1000,1),
             duration_in_traffic_min = round(tt_ex_sf_i$duration_in_traffic_s / 60,1),
             speed_in_traffic_kmh    = round(tt_ex_sf_i$speed_in_traffic_kmh,1))
)

p_table <- route_df %>%
  pivot_longer(cols = -type) %>%
  pivot_wider(names_from = type,
              id_cols = name,
              values_from = value) %>%
  mutate(name = case_when(
    name == "distance_km" ~ "Distance (km)",
    name == "duration_in_traffic_min" ~ "Duration (min)",
    name == "speed_in_traffic_kmh" ~ "Avg speed (km/h)"
  )) %>%
  dplyr::rename("Variable" = name) %>%
  ggtexttable(rows = NULL,
              theme = ttheme("blank")) %>%
  tab_add_hline(at.row = c(1, 2), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_hline(at.row = 4, row.side = "bottom", linewidth = 3, linetype = 1) %>%
  table_cell_bg(row = 2:4, column = 2, linewidth = 0, alpha = 0.3,
                fill="darkorange") %>%
  table_cell_bg(row = 2:4, column = 3, linewidth = 0, alpha = 0.3,
                fill="dodgerblue") %>%
  tab_add_title("B. Comparing modal and used route")

p_map <- ggplot() +
  geom_sf(data = roads_1_sf,
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = roads_2_sf,
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = tt_ex_sf_i,
          aes(color = "New Route"),
          linewidth = 1.75) +
  geom_sf(data = tt_modal_sf,
          aes(color = "Orignal Route"),
          linewidth = 1) +
  labs(color = NULL,
       title = "A. Original vs new route") +
  scale_color_manual(values = c("dodgerblue",
                                "darkorange")) +
  theme_void() +
  theme(legend.position = c(0.3, 0.15))


# ARRANGE/EXPORT ===============================================================
p_top <- ggarrange(p_map, p_table, nrow = 1)
p <- ggarrange(p_top, p_trends, ncol = 1, heights = c(0.5, 0.5))

ggsave(p, 
       filename = file.path(figures_dir, "deviation_example_2.png"),
       height = 6, width = 7)


