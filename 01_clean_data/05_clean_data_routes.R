# Clean Data Routes

# Travel time
tt_df <- readRDS(file.path(tt_dir, "google_tt_data.Rds")) 

tt_df <- tt_df %>%
  dplyr::select(segment_id, datetime, 
                speed_kmh, speed_in_traffic_kmh,
                duration_s, duration_in_traffic_s,
                distance_m) %>%
  dplyr::rename(speed_kmh = speed_kmh,
                speed_in_traffic_kmh = speed_in_traffic_kmh,
                duration_s = duration_s,
                duration_in_traffic_s = duration_in_traffic_s,
                distance_m = distance_m) %>%
  dplyr::rename(uid = segment_id)

## Modal route
tt_df <- tt_df %>%
  group_by(uid) %>%
  dplyr::mutate(distance_mode = getmode(distance_m)) %>%
  ungroup()

## Make percent change duration
tt_df <- tt_df %>%
  dplyr::mutate(hour = datetime %>% hour()) %>%
  group_by(uid, distance_mode) %>%
  #dplyr::mutate(duration_in_traffic_s_minimum = min(duration_in_traffic_s, na.rm = T)) %>%
  dplyr::mutate(duration_in_traffic_s_minimum = duration_in_traffic_s %>%
                  quantile(0.05, na.rm = T) %>%
                  as.numeric()) %>%
  ungroup() %>%
  dplyr::mutate(duration_pc = (duration_in_traffic_s - duration_in_traffic_s_minimum)/duration_in_traffic_s_minimum,
                delay_factor_od = duration_pc + 1)


tt_df <- tt_df %>%
  group_by(uid) %>%
  dplyr::mutate(distance_m_mode = Mode(distance_m)) %>%
  ungroup() %>%
  dplyr::mutate(modal_route = distance_m_mode == distance_m)

# Append traffic data
tl_df <- file.path(extracted_data_dir, "google_typical_route_10m", "google_traffic_levels") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  map_df(readRDS) %>%
  dplyr::mutate(count_all = count_1 + count_2 + count_3 + count_4) %>%
  
  group_by(uid) %>%
  dplyr::mutate(count_all_max = max(count_all, na.rm = T)) %>%
  ungroup() %>%
  
  dplyr::mutate(tl_prop_234 = (count_2 + count_3 + count_4) / count_all_max,
                tl_prop_34  = (          count_3 + count_4) / count_all_max,
                tl_prop_4   = (                    count_4) / count_all_max,
                tl_prop_3   = (                    count_3) / count_all_max,
                tl_prop_2   = (                    count_2) / count_all_max) %>%
  
  dplyr::rename(tl_count_all_max = count_all_max) %>%
  dplyr::select(-c(count_0, count_1, count_2, count_3, count_4, count_all)) 

# Merge
route_df <- inner_join(tl_df, tt_df, by = c("uid", "datetime"))

# Cleanup
route_df <- route_df %>%
  dplyr::mutate(date = datetime %>% date(),
                hour = datetime %>% hour()) %>%
  dplyr::filter(!is.na(datetime)) %>%
  
  # Remove dates with missing data
  dplyr::filter(date < ymd("2023-08-17")) %>%
  dplyr::filter( ! ((date >= ymd("2023-02-23")) & (date <= ymd("2023-03-16"))) )

#### Add OSM
rt_typ_google_sf      <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))
rt_typ_google_10buff_sf <- st_buffer(rt_typ_google_sf, dist = 10)

length_df <- map_df(unique(rt_typ_google_10buff_sf$uid), function(uid_i){
  message(uid_i)
  
  rt_typ_google_10buff_sf_i <- rt_typ_google_10buff_sf[rt_typ_google_10buff_sf$uid %in% uid_i,]
  
  osm_sf_i <- st_intersection(osm_sf, rt_typ_google_10buff_sf_i) %>%
    st_drop_geometry() %>%
    group_by(fclass) %>%
    dplyr::summarise(length = sum(length, na.rm = T)) %>%
    ungroup() %>%
    dplyr::mutate(length_total = sum(length),
                  prop = length / length_total) %>%
    dplyr::select(fclass, prop) %>%
    pivot_wider(names_from = fclass,
                values_from = prop) %>%
    dplyr::mutate(uid = uid_i)
  
  return(osm_sf_i)
})

length_df <- length_df %>%
  mutate(across(everything(), ~ tidyr::replace_na(., 0))) %>%
  rename_with(~ paste0("prop_", .x), -uid)

route_df <- route_df %>%
  left_join(length_df, by = "uid")

# Export
saveRDS(route_df, file.path(analysis_data_dir, "google_routes.Rds"))






