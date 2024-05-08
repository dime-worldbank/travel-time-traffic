# Congestion in Nairobi 

hvline_color <- "black"

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))
gadm2_df <- readRDS(file.path(analysis_data_dir, "gadm2_wide.Rds"))
gadm2_sf  <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_2_pk.rds"))

gadm1_df <- readRDS(file.path(analysis_data_dir, "gadm1_wide.Rds"))

# Cleanup ----------------------------------------------------------------------
route_df <- route_df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"))

gadm1_df <- gadm1_df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"))

# Period with 11 Routes --------------------------------------------------------

# Time of Day ------------------------------------------------------------------
#### Prep data
route_long_df <- route_df %>%
  filter(uid %in% c(1,2,5:13)) %>% # Constant sample
  
  group_by(hour, day_type) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  
  pivot_longer(cols = -c(hour, day_type, uid)) %>%
  filter(name %in% c("gg_distance_km",
                     "gg_duration_in_traffic_min",
                     "gg_speed_in_traffic_kmh",
                     "gg_tl_prop_234",
                     "gg_tl_prop_34",
                     "gg_tl_prop_4")) %>%
  rename_var("name") %>%
  mutate(name = paste0(name, "\n[11 Routes]"))

gadm1_long_df <- gadm1_df %>%
  
  group_by(hour, day_type) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  
  pivot_longer(cols = -c(hour, day_type)) %>%
  filter(name %in% c("gg_tl_prop_234",
                     "gg_tl_prop_34",
                     "gg_tl_prop_4")) %>%
  rename_var("name") %>%
  mutate(name = paste0(name, "\n[City Level]"))

hour_long_df <- bind_rows(
  route_long_df,
  gadm1_long_df
) %>%
  mutate(name = name %>%
           factor(levels = c("Distance (km)\n[11 Routes]",
                             "Average Speed (km/h)\n[11 Routes]",
                             "Duration (mins)\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[11 Routes]",
                             "Traffic, Prop 3,4\n[11 Routes]",
                             "Traffic, Prop 4\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[City Level]",
                             "Traffic, Prop 3,4\n[City Level]",
                             "Traffic, Prop 4\n[City Level]")))

#### Correlation figure
hour_cor_df <- hour_long_df %>%
  pivot_wider(names_from = name,
              values_from = value) %>%
  dplyr::select(-c(hour, day_type, uid)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) %>%
  mutate(name = name %>%
           factor(levels = c("Distance (km)\n[11 Routes]",
                             "Average Speed (km/h)\n[11 Routes]",
                             "Duration (mins)\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[11 Routes]",
                             "Traffic, Prop 3,4\n[11 Routes]",
                             "Traffic, Prop 4\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[City Level]",
                             "Traffic, Prop 3,4\n[City Level]",
                             "Traffic, Prop 4\n[City Level]"))) %>%
  mutate(variable = variable %>%
           factor(levels = c("Distance (km)\n[11 Routes]",
                             "Average Speed (km/h)\n[11 Routes]",
                             "Duration (mins)\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[11 Routes]",
                             "Traffic, Prop 3,4\n[11 Routes]",
                             "Traffic, Prop 4\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[City Level]",
                             "Traffic, Prop 3,4\n[City Level]",
                             "Traffic, Prop 4\n[City Level]")))

#### Figure
LW = 3.1

p_trends <- hour_long_df %>%
  
  ggplot() +
  geom_vline(aes(xintercept = 6), color = "gray90", linewidth = LW) +
  geom_vline(aes(xintercept = 7), color = "gray90", linewidth = LW) +
  geom_vline(aes(xintercept = 8), color = "gray90", linewidth = LW) +
  geom_vline(aes(xintercept = 9), color = "gray90", linewidth = LW) +
  
  geom_vline(aes(xintercept = 4 + 12), color = "gray90", linewidth = LW) +
  geom_vline(aes(xintercept = 5 + 12), color = "gray90", linewidth = LW) +
  geom_vline(aes(xintercept = 6 + 12), color = "gray90", linewidth = LW) +
  geom_vline(aes(xintercept = 7 + 12), color = "gray90", linewidth = LW) +
  geom_vline(aes(xintercept = 8 + 12), color = "gray90", linewidth = LW) +
  geom_line(aes(x = hour,
                y = value,
                color = day_type)) +
  facet_wrap(~name,
             scales = "free_y") +
  labs(x = "Hour of Day",
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("darkorange",
                                "dodgerblue1")) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        plot.title = element_text(face = "bold"),
        legend.position = "right") 

ggsave(filename = file.path(figures_dir, "cong_timeofday.png"),
       height = 4.5, width = 9.5)

# Trends over time -------------------------------------------------------------
#### Prep data
route_df <- route_df %>%
  filter(uid %in% c(1,2,5:13)) %>% # Constant sample
  mutate(uid = uid %>% as.character())

cong_df <- bind_rows(
  gadm1_df %>% mutate(data_type = "[City Level]"),
  route_df %>% mutate(data_type = "[11 Routes]")
)

trends_df <- cong_df %>%
  filter(!is.na(gg_tl_prop_234),
         #!is.na(gg_speed_in_traffic_kmh),
         count_1 > 0,
         datetime >= ymd("2022-09-01", tz = "Africa/Nairobi")) %>%
  mutate(week = datetime %>% floor_date(unit = "week")) %>%
  
  #mutate(day_type = "all") %>%
  
  group_by(week, day_type, data_type) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  
  pivot_longer(cols = -c(week, day_type, data_type)) %>%
  filter(name %in% c("gg_tl_prop_234",
                     "gg_tl_prop_34",
                     "gg_tl_prop_4",
                     "gg_speed_in_traffic_kmh",
                     "gg_duration_in_traffic_min",
                     "gg_distance_km")) %>%
  
  #group_by(name, day_type, data_type) %>%
  #dplyr::mutate(value = zoo::rollmean(value, k = 4, fill = NA, na.rm=T)) %>%
  #ungroup() %>%
  
  rename_var("name") %>%
  mutate(name = paste0(name, "\n", data_type, "")) %>%
  
  mutate(name = name %>%
           factor(levels = c("Distance (km)\n[11 Routes]",
                             "Average Speed (km/h)\n[11 Routes]",
                             "Duration (mins)\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[11 Routes]",
                             "Traffic, Prop 3,4\n[11 Routes]",
                             "Traffic, Prop 4\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[City Level]",
                             "Traffic, Prop 3,4\n[City Level]",
                             "Traffic, Prop 4\n[City Level]"))) %>%
  
  ## Percent change from initial value (baseline)
  filter(!is.na(value)) %>%
  arrange(week) %>%
  
  group_by(name, day_type) %>%
  mutate(value_base = value[1]) %>%
  ungroup() %>%
  
  mutate(value_pc = (value - value_base) / value_base * 100) 

#### Correlation figure
cor_weekday_df <- trends_df %>%
  dplyr::filter(day_type == "Weekday") %>%
  pivot_wider(id_cols = week,
              names_from = name,
              values_from = value) %>%
  dplyr::select(-week) %>%
  
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) %>%
  
  mutate(name = name %>%
           factor(levels = c("Distance (km)\n[11 Routes]",
                             "Average Speed (km/h)\n[11 Routes]",
                             "Duration (mins)\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[11 Routes]",
                             "Traffic, Prop 3,4\n[11 Routes]",
                             "Traffic, Prop 4\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[City Level]",
                             "Traffic, Prop 3,4\n[City Level]",
                             "Traffic, Prop 4\n[City Level]"))) %>%
  mutate(variable = variable %>%
           factor(levels = c("Distance (km)\n[11 Routes]",
                             "Average Speed (km/h)\n[11 Routes]",
                             "Duration (mins)\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[11 Routes]",
                             "Traffic, Prop 3,4\n[11 Routes]",
                             "Traffic, Prop 4\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[City Level]",
                             "Traffic, Prop 3,4\n[City Level]",
                             "Traffic, Prop 4\n[City Level]")))

cor_weekend_df <- trends_df %>%
  dplyr::filter(day_type == "Weekend") %>%
  pivot_wider(id_cols = week,
              names_from = name,
              values_from = value) %>%
  dplyr::select(-week) %>%
  
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) %>%
  
  mutate(name = name %>%
           factor(levels = c("Distance (km)\n[11 Routes]",
                             "Average Speed (km/h)\n[11 Routes]",
                             "Duration (mins)\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[11 Routes]",
                             "Traffic, Prop 3,4\n[11 Routes]",
                             "Traffic, Prop 4\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[City Level]",
                             "Traffic, Prop 3,4\n[City Level]",
                             "Traffic, Prop 4\n[City Level]"))) %>%
  mutate(variable = variable %>%
           factor(levels = c("Distance (km)\n[11 Routes]",
                             "Average Speed (km/h)\n[11 Routes]",
                             "Duration (mins)\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[11 Routes]",
                             "Traffic, Prop 3,4\n[11 Routes]",
                             "Traffic, Prop 4\n[11 Routes]",
                             "Traffic, Prop 2,3,4\n[City Level]",
                             "Traffic, Prop 3,4\n[City Level]",
                             "Traffic, Prop 4\n[City Level]")))


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
       title = "B. Correlation in indicators, weekday") +
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
       title = "C. Correlation in indicators, weekend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.position = "right")

p_cor <- ggarrange(p_cor_weekday,
                   p_cor_weekend,
                   common.legend = T,
                   legend = "right")

#### Trends figure
p_trends <- trends_df %>%
  
  ggplot(aes(x = week,
             y = value,
             color = day_type)) +
  geom_line() +
  facet_wrap(~name,
             scales = "free_y") +
  labs(color = NULL,
       x = NULL,
       y = NULL,
       title = "A. Trends in indicators") +
  scale_color_manual(values = c("dodgerblue",
                                "darkorange")) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 7, color = "black")) 


p <- ggarrange(p_trends, p_cor, ncol = 1)

ggsave(p, 
       filename = file.path(figures_dir, "indicators_over_time_constant.png"),
       height = 9, width = 10.5)

