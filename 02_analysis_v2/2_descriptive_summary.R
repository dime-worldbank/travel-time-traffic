# Descriptive Summary

# Load data --------------------------------------------------------------------
route_df   <- readRDS(file.path(analysis_data_dir, "mapbox_routes.Rds"))
osm_df     <- readRDS(file.path(analysis_data_dir, "mapbox_osm_10m.Rds"))
gadm1_df   <- readRDS(file.path(analysis_data_dir, "mapbox_gadm1.Rds"))
estates_df <- readRDS(file.path(analysis_data_dir, "mapbox_estates.Rds"))

estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))

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

# Analysis ---------------------------------------------------------------------
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
       title = "C. OpenStreetMap Road Classes") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold"))

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
       title = "Routes [N=26]") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold"))

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
       title = "Routes [N=26]") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold"))

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
       title = "All Nairobi") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold"))



p_top <- ggarrange(p_route_tt,
                   p_route_tl,
                   p_nbo,
                   nrow = 1,
                   common.legend = T)
p <- ggarrange(p_top,
               p_osm + theme(legend.position = "none"),
               ncol = 1,
               common.legend = F,
               heights = c(0.4, 0.6))

p


# Map --------------------------------------------------------------------------
estates_uid_df <- estates_df %>%
  group_by(uid, weekday_end) %>%
  dplyr::summarise(delay_factor = mean(delay_factor)) %>%
  ungroup()

estates_data_sf <- estates_sf %>%
  left_join(estates_uid_df, by = "uid") %>%
  dplyr::filter(!is.na(weekday_end))

ggplot() +
  geom_sf(data = estates_data_sf,
          aes(fill = delay_factor)) +
  facet_wrap(~weekday_end) +
  scale_fill_distiller(
    palette = "Spectral",
    direction = -1   # optional: flip so high = red
  ) +
  theme_void()

estates_data_sf %>%
  st_drop_geometry() %>%
  ggplot() +
  geom_boxplot(aes(x = weekday_end,
                   y = delay_factor))



