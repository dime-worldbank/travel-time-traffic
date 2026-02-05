# Congestion Road Type

osm_df <- readRDS(file.path(analysis_data_dir, "osm_10m_wide.Rds"))

osm_df <- osm_df %>%
  mutate(dow = datetime %>% lubridate::wday(label = T),
         hour = datetime %>% hour(),
         date_week = datetime %>% floor_date(unit = "weeks"),
         day_type = ifelse(dow %in% c("Sat", "Sun"),
                           "Weekend",
                           "Weekday"),
         fclass = fclass %>% as.character(),
         traffic_index = gg_tl_prop_2*1.086 + gg_tl_prop_3*3.946 + gg_tl_prop_4*5.979) %>%
  dplyr::filter(count_1 > 0)

osm_df %>%
  group_by(fclass, uid, day_type) %>%
  dplyr::summarise(traffic_index = mean(traffic_index)) %>%
  ungroup() %>%
  
  ggplot() +
  geom_boxplot(aes(x = traffic_index,
                   y = fclass,
                   fill = day_type))

osm_road_df <- osm_df %>%
  dplyr::filter(day_type == "Weekday") %>%
  dplyr::mutate(name = paste0(name, "\n[", fclass, "]")) %>%
  group_by(name) %>%
  dplyr::summarise(traffic_index = mean(traffic_index)) %>%
  ungroup() 

osm_road_df %>%
  slice_max(order_by = traffic_index, n = 20) %>%     # cleaner than arrange + head
  mutate(name = forcats::fct_reorder(name, traffic_index)) %>%  # order by traffic_index
  ggplot(aes(x = traffic_index, y = name)) +
  geom_col()

