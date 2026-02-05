# Descriptive Summary

# Load data --------------------------------------------------------------------
route_df   <- readRDS(file.path(analysis_data_dir, "mapbox_routes.Rds"))
osm_df     <- readRDS(file.path(analysis_data_dir, "mapbox_osm_10m.Rds"))
gadm1_df   <- readRDS(file.path(analysis_data_dir, "mapbox_gadm1.Rds"))
estates_df <- readRDS(file.path(analysis_data_dir, "mapbox_estates.Rds"))

estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))
#osm_sf     <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
#osm_sf     <- osm_sf %>% st_intersection(estates_sf)

beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Restrict date/time -----------------------------------------------------------
route_df <- route_df %>%
  dplyr::filter(!is.na(speed_kmh),
                !is.na(tl_prop_2))

gadm1_df   <- gadm1_df[(gadm1_df$datetime     >= min(route_df$datetime)) & (gadm1_df$datetime   <= max(route_df$datetime)),]
osm_df     <- osm_df[(osm_df$datetime         >= min(route_df$datetime)) & (osm_df$datetime     <= max(route_df$datetime)),]
estates_df <- estates_df[(estates_df$datetime >= min(route_df$datetime)) & (estates_df$datetime <= max(route_df$datetime)),]

# Apply coefs ------------------------------------------------------------------
route_df   <- mk_traffic_indicators(route_df, beta)
gadm1_df   <- mk_traffic_indicators(gadm1_df, beta)
estates_df <- mk_traffic_indicators(estates_df, beta)
osm_df     <- mk_traffic_indicators(osm_df, beta)

# Analysis ---------------------------------------------------------------------
## Datetime
prep_datetime <- function(data){
  data %>%
    dplyr::mutate(hour = datetime %>% hour(),
                  dow = datetime %>% lubridate::wday(label = TRUE),
                  dow_group = case_when(
                    dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ "Mon - Fri",
                    dow %in% "Sat" ~ "Sat",
                    dow %in% "Sun" ~ "Sun"
                  ),
                  weekday_end = case_when(
                    dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ "Weekday",
                    dow %in% c("Sat", "Sun") ~ "Weekend"
                  )) 
}

estates_df <- estates_df %>% prep_datetime
route_df   <- route_df %>% prep_datetime
osm_df     <- osm_df %>% prep_datetime
gadm1_df   <- gadm1_df %>% prep_datetime

## Extra cleaning
route_df <- route_df %>%
  group_by(uid) %>%
  dplyr::mutate(duration_s_minimum = min(duration_s)) %>%
  ungroup() %>%
  dplyr::mutate(duration_pc = (duration_s - duration_s_minimum)/duration_s_minimum)

## To hour
route_hr_df <- route_df %>%
  group_by(hour, dow_group) %>%
  dplyr::summarise(delay_factor = mean(delay_factor),
                   duration_pc = mean(duration_pc)) %>%
  ungroup()

estates_hr_df <- estates_df %>%
  group_by(hour, dow_group) %>%
  dplyr::summarise(delay_factor = mean(delay_factor)) %>%
  ungroup()

gadm1_hr_df <- gadm1_df %>%
  group_by(hour, dow_group) %>%
  dplyr::summarise(delay_factor = mean(delay_factor)) %>%
  ungroup()

osm_hr_class_df <- osm_df %>%
  group_by(hour, dow_group, fclass) %>%
  dplyr::summarise(delay_factor = mean(delay_factor)) %>%
  ungroup()

# Time of Day: 26 Routes and city wide -----------------------------------------
p_route_tt <- route_hr_df %>%
  ggplot() +
  geom_line(aes(x = hour,
                y = duration_pc,
                color = dow_group),
            linewidth = 1) +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue",
                                "purple")) +
  labs(x = "Hour of day",
       y = "Duration (% change)",
       color = NULL,
       title = "A. Travel Duration\n% Change from Free-Flow\nRoutes [N=26]") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10))

p_route_tl <- route_hr_df %>%
  ggplot() +
  geom_line(aes(x = hour,
                y = delay_factor,
                color = dow_group),
            linewidth = 1) +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue",
                                "purple")) +
  labs(x = "Hour of day",
       y = "Delay factor",
       color = NULL,
       title = "B. Delay Factor\nRoutes [N=26]") +
  scale_y_continuous(limits = c(1, 1.15)) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10))

## City-wide
p_nbo <- gadm1_hr_df %>%
  ggplot() +
  geom_line(aes(x = hour,
                y = delay_factor,
                color = dow_group),
            linewidth = 1) +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue",
                                "purple")) +
  labs(x = "Hour of day",
       y = "Delay factor",
       color = NULL,
       title = "C. Delay Factor\n[All Nairobi]") +
  scale_y_continuous(limits = c(1, 1.15)) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10))

# Time of Day: Road Type -------------------------------------------------------
p_osm <- osm_hr_class_df %>%
  dplyr::mutate(fclass = fclass %>% as.character() %>% tools::toTitleCase(),
                fclass = fclass %>%
                  factor(levels = c("Trunk",
                                    "Primary",
                                    "Secondary",
                                    "Tertiary",
                                    "Unclassified",
                                    "Residential"))) %>%
  ggplot() +
  geom_line(aes(x = hour,
                y = delay_factor,
                color = dow_group),
            linewidth = 1) +
  facet_wrap(~fclass) +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue",
                                "purple")) +
  labs(x = "Hour of day",
       y = "Delay factor",
       color = NULL,
       title = "D. OpenStreetMap Road Classes",
       subtitle = "Average delay across roads by class, considering all roads in Nairobi with available data") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"))

# Time of Day: Arrange/Export --------------------------------------------------
p_top <- ggarrange(p_route_tt + theme(legend.position = "none"),
                   p_route_tl + theme(legend.position = "none"),
                   p_nbo + theme(legend.position = "none"),
                   nrow = 1,
                   common.legend = F)

p <- ggarrange(p_top,
               p_osm + theme(legend.position = "none"),
               ncol = 1,
               common.legend = T,
               legend = "right",
               heights = c(0.4, 0.65))

ggsave(p,
       filename = file.path(figures_dir, "delay_by_hour.png"),
       height = 8,
       width = 9)

# Map --------------------------------------------------------------------------
#estates_df <- osm_df
#estates_sf <- osm_sf

estates_uid_df <- estates_df %>%
  group_by(uid, weekday_end) %>%
  dplyr::summarise(delay_factor = mean(delay_factor)) %>%
  ungroup()

estates_uid_wide_df <- estates_uid_df %>%
  pivot_wider(id_cols = uid,
              names_from = weekday_end,
              values_from = delay_factor) %>%
  dplyr::mutate(weekday_m_weekend = Weekday - Weekend,
                weekday_m_weekend_pc = weekday_m_weekend / Weekend * 100,
                weekday_weekend_ratio = Weekday/Weekend)


estates_data_sf <- estates_sf %>%
  left_join(estates_uid_df, by = "uid") %>%
  dplyr::filter(!is.na(weekday_end)) %>%
  dplyr::mutate(weekday_end = case_when(
    weekday_end == "Weekday" ~ "A. Weekday",
    weekday_end == "Weekend" ~ "B. Weekend"
  ))

estates_data_diff_sf <- estates_sf %>%
  left_join(estates_uid_wide_df, by = "uid") %>%
  dplyr::filter(!is.na(weekday_m_weekend))

## Levels
estates_data_sf$delay_factor[estates_data_sf$delay_factor >= 1.15] <- 1.15

estates_data_sf <- estates_data_sf %>%
  arrange(delay_factor)

p_map_levels <- ggplot() +
  geom_sf(data = estates_data_sf,
          aes(fill = delay_factor)) +
  facet_wrap(~weekday_end) +
  scale_fill_distiller(
    palette = "RdYlGn",
    direction = -1   # optional: flip so high = red
  ) +
  labs(fill = "Delay\nFactor") +
  theme_void() +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 11))


estates_data_diff_sf$weekday_m_weekend_pc_adj <- estates_data_diff_sf$weekday_m_weekend_pc
estates_data_diff_sf$weekday_m_weekend_pc_adj[estates_data_diff_sf$weekday_m_weekend_pc_adj >= 2] <- 2
estates_data_diff_sf$weekday_m_weekend_pc_adj[estates_data_diff_sf$weekday_m_weekend_pc_adj <= -2] <- -2

estates_data_diff_sf$weekday_weekend_ratio_adj <- estates_data_diff_sf$weekday_weekend_ratio
estates_data_diff_sf$weekday_weekend_ratio_adj[estates_data_diff_sf$weekday_weekend_ratio >= 1.02] <- 1.02
estates_data_diff_sf$weekday_weekend_ratio_adj[estates_data_diff_sf$weekday_weekend_ratio <= -0.98] <- -0.98

## % Change
p_map_pc <- ggplot() +
  geom_sf(data = estates_data_diff_sf,
          aes(fill = weekday_m_weekend_pc_adj)) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1   # optional: flip so high = red
  ) +
  labs(title = "C. % Change in Delay Factor\nWeekday relative to Weekends",
       fill = "% Change") +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 11))

p_map <- ggarrange(p_map_levels,
          p_map_pc,
          ncol = 1,
          heights = c(0.5, 0.5))

ggsave(p_map,
       filename = file.path(figures_dir, "cong_maps_estates.png"),
       height = 5,
       width = 7)

# estates_data_sf %>%
#   st_drop_geometry() %>%
#   ggplot() +
#   geom_boxplot(aes(x = weekday_end,
#                    y = delay_factor))



