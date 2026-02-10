# Descriptive Summary

# Load data --------------------------------------------------------------------
route_df   <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))
osm_df     <- readRDS(file.path(analysis_data_dir, "google_osm_10m.Rds"))
gadm1_df   <- readRDS(file.path(analysis_data_dir, "google_gadm1.Rds"))
estates_df <- readRDS(file.path(analysis_data_dir, "google_estates.Rds"))

estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))
osm_sf     <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
osm_sf     <- osm_sf %>% st_intersection(estates_sf)

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
# route_df <- route_df %>%
#   group_by(uid) %>%
#   #dplyr::mutate(duration_in_traffic_s_minimum = min(duration_in_traffic_s, na.rm = T)) %>%
#   dplyr::mutate(duration_in_traffic_s_minimum = duration_in_traffic_s %>%
#                   quantile(0.05, na.rm = T) %>%
#                   as.numeric()) %>%
#   ungroup() %>%
#   dplyr::mutate(duration_pc = (duration_in_traffic_s - duration_in_traffic_s_minimum)/duration_in_traffic_s_minimum)

## To hour
route_hr_df <- route_df %>%
  group_by(hour, dow_group) %>%
  dplyr::summarise(delay_factor = mean(delay_factor),
                   duration_pc = mean(duration_pc),
                   delay_factor_od = mean(delay_factor_od)) %>%
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
  dplyr::summarise(delay_factor = mean(delay_factor, na.rm = T)) %>%
  ungroup()

# Top roads --------------------------------------------------------------------
osm_length_df <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds")) %>%
  dplyr::mutate(length_km = as.numeric(st_length(geometry)/1000)) %>%
  st_drop_geometry() %>%
  dplyr::select(uid, length_km)

# osm_df %>%
#   dplyr::filter(dow_group %in% "Mon - Fri") %>%
#   group_by(hour) %>%
#   dplyr::summarise(delay_factor = mean(delay_factor, na.rm = T)) %>%
#   ungroup() %>%
#   arrange(-delay_factor)

osm_l <- osm_df %>%
  left_join(osm_length_df, by = "uid") %>%
  dplyr::filter(hour %in% 18,
                dow_group %in% "Mon - Fri") %>%
  group_by(name, length_km) %>%
  dplyr::summarise(delay_factor = mean(delay_factor, na.rm = T),
                   tl_prop_2 = mean(tl_prop_2, na.rm = T),
                   tl_prop_3 = mean(tl_prop_3, na.rm = T),
                   tl_prop_4 = mean(tl_prop_4, na.rm = T)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(delay_factor)) %>%
  dplyr::filter(length_km >= 0.5) %>%
  arrange(-delay_factor)

bin_width <- 0.25
bin_max   <- 8

binned_df <- osm_l %>%
  mutate(
    delay_bin = cut(
      delay_factor,
      breaks = seq(0, bin_max, by = bin_width),
      right  = FALSE,        # [a, b)
      include.lowest = TRUE
    )
  ) %>%
  count(delay_bin) %>%
  filter(!is.na(delay_bin))

ggplot(binned_df, aes(x = delay_bin, y = n)) +
  geom_col(fill = "grey70", color = "white") +
  geom_text(
    aes(label = n),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    x = "Delay factor",
    y = "N Roads",
    title = "Distribution of delay factors across roads",
    subtitle = "Figure uses all roads from OpenStreetMap that are over 500 meters in length"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave(filename = file.path(figures_dir, "delay_factor_roads.png"),
       height = 4,
       width = 7)

## Distribution of variables - - - - - 
osm_l <- osm_df %>%
  left_join(osm_length_df, by = "uid") %>%
  dplyr::filter(hour %in% 18,
                dow_group %in% "Mon - Fri") %>%
  dplyr::filter(!is.na(delay_factor)) %>%
  dplyr::filter(length_km >= 0.5) %>%
  dplyr::select(delay_factor, tl_prop_2, tl_prop_3, tl_prop_4)

osm_df %>%
  dplyr::filter(!is.na(delay_factor),
                !is.na(tl_prop_2)) %>%
  pull(name) %>%
  unique() %>%
  length()

osm_df$date %>% summary()

summ_all <- osm_l %>%
  #select(where(is.numeric), -length_km) %>%   # keep numeric, drop delay_factor
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    p01 = quantile(value, 0.01, na.rm = TRUE, names = FALSE),
    p10 = quantile(value, 0.10, na.rm = TRUE, names = FALSE),
    p25 = quantile(value, 0.25, na.rm = TRUE, names = FALSE),
    p50 = quantile(value, 0.50, na.rm = TRUE, names = FALSE),
    p75 = quantile(value, 0.75, na.rm = TRUE, names = FALSE),
    p90 = quantile(value, 0.90, na.rm = TRUE, names = FALSE),
    p99 = quantile(value, 0.99, na.rm = TRUE, names = FALSE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

dict <- c(
  delay_factor  = "Delay Factor",
  tl_prop_2  = "Prop. Traffic Level 2 (Medium)",
  tl_prop_3  = "Prop. Traffic Level 3 (High)",
  tl_prop_4  = "Prop. Traffic Level 4 (Severe)"
)

summ_all <- summ_all %>%
  mutate(variable = recode(variable, !!!dict))

digits <- 3

summ_tex <- summ_all %>%
  mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = digits))) %>%
  mutate(
    tex = paste(
      variable, min, p01, p10, p25, p50, p75, p90, p99, max,
      sep = " & "
    ) %>% paste0(" \\\\")
  )

sink(file.path(tables_dir, "osm_summary_stats.tex"))

cat("\\begin{tabular}{l | rrrrrrrrr} \n")
cat("\\hline \n")
cat("Variable & Min & P01 & P10 & P25 & P50 & P75 & P90 & P99 & Max \\\\ \n")
cat("\\hline \n")
cat(paste(summ_tex$tex, collapse = "\n"))
cat("\n\\hline \n")
cat("\\end{tabular}\n")

sink()


# Time of Day: 26 Routes and city wide -----------------------------------------
p_route_tt <- route_hr_df %>%
  ggplot() +
  geom_line(aes(x = hour,
                y = delay_factor_od,
                color = dow_group),
            linewidth = 1) +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue",
                                "purple")) +
  labs(x = "Hour of day",
       y = "Duration (% change)",
       color = NULL,
       title = "A. Delay Factor\n(O-D Data)\nRoutes [N=26]") +
  scale_y_continuous(limits = c(1, 2.1)) +
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
       title = "B. Delay Factor\n(Traffic Level Data)\nRoutes [N=26]") +
  scale_y_continuous(limits = c(1, 2.1)) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10))
p_route_tl

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
       title = "C. Delay Factor\n(Traffic Level Data)\n[All Nairobi]") +
  scale_y_continuous(limits = c(1, 2.1)) +
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
       subtitle = "Average delay across roads by class, considering all roads in Nairobi with available data, using traffic level data") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(face = "italic", size = 10))

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
estates_data_sf$delay_factor[estates_data_sf$delay_factor >= 1.5] <- 1.5

estates_data_sf <- estates_data_sf %>%
  arrange(delay_factor)

p_map_levels <- ggplot() +
  geom_sf(data = estates_data_sf,
          aes(fill = delay_factor)) +
  facet_wrap(~weekday_end) +
  scale_fill_distiller(
    palette = "RdYlGn",
    breaks = c(1.1, 1.2, 1.3, 1.4, 1.5),
    labels = c("1.1", "1.2", "1.3", "1.4", "> 1.5"),
    direction = -1   # optional: flip so high = red
  ) +
  labs(fill = "Delay\nFactor") +
  theme_void() +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 11))

estates_data_diff_sf$weekday_m_weekend_pc_adj <- estates_data_diff_sf$weekday_m_weekend_pc
estates_data_diff_sf$weekday_m_weekend_pc_adj[estates_data_diff_sf$weekday_m_weekend_pc_adj >= 10] <- 10
estates_data_diff_sf$weekday_m_weekend_pc_adj[estates_data_diff_sf$weekday_m_weekend_pc_adj <= -10] <- -10

estates_data_diff_sf$weekday_weekend_ratio_adj <- estates_data_diff_sf$weekday_weekend_ratio
estates_data_diff_sf$weekday_weekend_ratio_adj[estates_data_diff_sf$weekday_weekend_ratio >= 1.1] <- 1.1
estates_data_diff_sf$weekday_weekend_ratio_adj[estates_data_diff_sf$weekday_weekend_ratio <= -0.9] <- -0.9

## % Change
p_map_pc <- ggplot() +
  geom_sf(data = estates_data_diff_sf,
          aes(fill = weekday_m_weekend_pc_adj)) +
  scale_fill_distiller(
    palette = "RdBu",
    limits = c(-10, 10),
    breaks = c(-10, -5, 0, 5, 10),
    labels = c("< -10%", "-5%", "0%", "5%", "> 10%"),
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



