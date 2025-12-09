# Congestion in Nairobi 

hvline_color <- "black"

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))
gadm1_df <- readRDS(file.path(analysis_data_dir, "gadm1_wide.Rds"))

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
         traffic_index = gg_tl_prop_2*1.086 + gg_tl_prop_3*3.946 + gg_tl_prop_4*5.979) %>%
  dplyr::filter(count_1 > 0)

gadm1_df <- gadm1_df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         date_week = datetime %>% floor_date(unit = "weeks"),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"),
         traffic_index = gg_tl_prop_2*1.086 + gg_tl_prop_3*3.946 + gg_tl_prop_4*5.979)

# Trends in Indicators ---------------------------------------------------------
route_week_df <- route_df %>%
  filter(uid %in% c(1,2,5:13)) %>%
  group_by(date_week, day_type) %>%
  dplyr::summarise(gg_speed_in_traffic_kmh = mean(gg_speed_in_traffic_kmh, na.rm = T),
                   gg_duration_in_traffic_min = mean(gg_duration_in_traffic_min, na.rm = T),
                   gg_distance_km = mean(gg_distance_km, na.rm = T),
                   traffic_index = mean(traffic_index, na.rm = T)) %>%
  ungroup()

week_df <- full_join(route_week_df,
                     gadm1_week_df,
                     by = c("date_week", "day_type")) %>%
  pivot_longer(cols = -c("date_week", "day_type")) %>%
  dplyr::mutate(name_clean = case_when(
    name == "gg_speed_in_traffic_kmh" ~ "Speed (km/h)\n[11 Routes]",
    name == "gg_duration_in_traffic_min" ~ "Duration (min)\n[11 Routes]",
    name == "gg_distance_km" ~ "Distance (km)\n[11 Routes]",
    name == "traffic_index" ~ "Traffic Index\n[11 Routes]",
    name == "traffic_index_city" ~ "Traffic Index\n[All Nairobi]"
  ))

# Correlation figures ----------------------------------------------------------

#### Correlation figure
cor_weekday_df <- week_df %>%
  dplyr::filter(day_type == "Weekday") %>%
  dplyr::mutate(name_clean = name_clean %>% str_replace_all("from ", "from\n")) %>%
  pivot_wider(id_cols = date_week,
              names_from = name_clean,
              values_from = value) %>%
  dplyr::select(-date_week) %>%
  
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) #%>%

# mutate(name = name %>%
#          factor(levels = c("Distance (km)\n[11 Routes]",
#                            "Average Speed (km/h)\n[11 Routes]",
#                            "Duration (mins)\n[11 Routes]",
#                            "Traffic Index\n[11 Routes]",
#                            "Traffic Index\n[City Level]"))) %>%
# mutate(variable = variable %>%
#          factor(levels = c("Distance (km)\n[11 Routes]",
#                            "Average Speed (km/h)\n[11 Routes]",
#                            "Duration (mins)\n[11 Routes]",
#                            "Traffic Index\n[11 Routes]",
#                            "Traffic Index\n[City Level]")))

cor_weekend_df <- week_df %>%
  dplyr::filter(day_type == "Weekend") %>%
  dplyr::mutate(name_clean = name_clean %>% str_replace_all("from ", "from\n")) %>%
  pivot_wider(id_cols = date_week,
              names_from = name_clean,
              values_from = value) %>%
  dplyr::select(-date_week) %>%
  
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) # %>%

# mutate(name = name %>%
#          factor(levels = c("Distance (km)\n[11 Routes]",
#                            "Average Speed (km/h)\n[11 Routes]",
#                            "Duration (mins)\n[11 Routes]",
#                            "Traffic Index\n[11 Routes]",
#                            "Traffic Index\n[City Level]"))) %>%
# mutate(variable = variable %>%
#          factor(levels = c("Distance (km)\n[11 Routes]",
#                            "Average Speed (km/h)\n[11 Routes]",
#                            "Duration (mins)\n[11 Routes]",
#                            "Traffic Index\n[11 Routes]",
#                            "Traffic Index\n[City Level]")))


p_cor_weekday <- cor_weekday_df %>%
  ggplot(aes(x = variable,
             y = name,
             fill = value,
             label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black",
            size = 3) +
  
  geom_hline(yintercept = 3.5, color = hvline_color) +
  geom_vline(xintercept = 3.5, color = hvline_color) +
  
  geom_hline(yintercept = 6.5, color = hvline_color) +
  geom_vline(xintercept = 6.5, color = hvline_color) +
  
  scale_fill_distiller(palette = "RdBu",
                       na.value = "white",
                       direction = 0,
                       limits = c(-1, 1)) +
  labs(y = NULL,
       x = NULL,
       fill = "Correlation",
       title = "A. Correlation in indicators, weekday") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.position = "right")

p_cor_weekend <- cor_weekend_df %>%
  ggplot(aes(x = variable,
             y = name,
             fill = value,
             label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black",
            size = 3) +
  
  geom_hline(yintercept = 3.5, color = hvline_color) +
  geom_vline(xintercept = 3.5, color = hvline_color) +
  
  geom_hline(yintercept = 6.5, color = hvline_color) +
  geom_vline(xintercept = 6.5, color = hvline_color) +
  
  scale_fill_distiller(palette = "RdBu",
                       na.value = "white",
                       direction = 0,
                       limits = c(-1, 1)) +
  labs(y = NULL,
       x = NULL,
       fill = "Correlation",
       title = "B. Correlation in indicators, weekend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.position = "right")

p_cor <- ggarrange(p_cor_weekday,
                   p_cor_weekend,
                   common.legend = T,
                   legend = "right")

ggsave(p_cor, 
       filename = file.path(figures_dir, "cor_over_time.png"),
       height = 5, width = 10)











