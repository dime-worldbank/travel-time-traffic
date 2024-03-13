# Deviation Example

# TRENDS IN INDICATORS =========================================================
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

p_trends <- route_df %>%
  dplyr::mutate(date = datetime %>% date()) %>%
  dplyr::filter(date %in% ymd("2023-07-16")) %>%
  dplyr::filter(uid %in% 25) %>%
  dplyr::select(datetime, gg_speed_in_traffic_kmh, gg_duration_in_traffic_min, gg_distance_km, 
                gg_tl_prop_234, gg_tl_prop_34, gg_tl_prop_4) %>%
  pivot_longer(cols = -c(datetime)) %>%
  rename_var("name") %>%
  dplyr::mutate(name = case_when(
    name == "Average Speed" ~ "Average Speed (km/h)",
    name == "Duration" ~ "Duration (mins)",
    TRUE ~ name
  )) %>%
  mutate(hour = datetime %>% hour()) %>%
  ggplot() +
  geom_vline(xintercept = 17, color = "red") +
  geom_line(aes(x = hour,
                y = value)) +
  facet_wrap(~name,
             scales = "free_y") +
  labs(x = "Hour",
       y = NULL,
       title = "C. Trends in indicators") +
  theme_classic2() +
  theme(strip.text = element_text(face = "bold"),
        axis.text = element_text(size = 8),
        strip.background = element_blank()) 

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
  filter(segment_id %in% 25,
         datetime %in% ymd_hms("2023-07-16 17:00:00", tz = "Africa/Nairobi"))

tt_modal_sf <- tt_sf[(tt_sf$segment_id %in% tt_ex_sf_i$segment_id) & (tt_sf$diff_mode %in% F),][1,]

tt_roads_sf <- bind_rows(tt_ex_sf_i, tt_modal_sf) %>%
  st_buffer(dist = 3 * 1000)

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
  data.frame(type = "Typical Route",
             distance_km             = round(tt_modal_sf$distance_m / 1000,1),
             duration_in_traffic_min = round(tt_modal_sf$duration_in_traffic_s / 60,1),
             speed_in_traffic_kmh    = round(tt_modal_sf$speed_in_traffic_kmh,1)),
  
  data.frame(type = "Route",
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
  geom_sf(data = tt_modal_sf,
          aes(color = "Typical Route"),
          linewidth = 1) +
  geom_sf(data = tt_ex_sf_i,
          aes(color = "Route"),
          linewidth = 1) +
  labs(color = NULL,
       title = "A. Typical vs used route") +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue")) +
  theme_void() +
  theme(legend.position = c(0.8, 0.15))

# ARRANGE/EXPORT ===============================================================
p_top <- ggarrange(p_map, p_table, nrow = 1)
p <- ggarrange(p_top, p_trends, ncol = 1, heights = c(0.5, 0.5))

ggsave(p, 
       filename = file.path(figures_dir, "deviation_example.png"),
       height = 4.5, width = 6)

