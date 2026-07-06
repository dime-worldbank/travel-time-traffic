
# Traffic levels
tl_df <- file.path(extracted_data_dir, "estates", "google_traffic_levels") %>%
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
  dplyr::select(-c(count_0, count_1, count_2, count_3, count_4, count_all)) %>%
  
  dplyr::mutate(date = datetime %>% date()) %>%
  dplyr::filter(date < ymd("2023-08-17")) %>%
  dplyr::filter( ! ((date >= ymd("2023-02-23")) & (date <= ymd("2023-03-16"))) )

# Add roads
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds")) %>%
  dplyr::select(fclass) %>%
  dplyr::mutate(length = geometry %>% st_length() %>% as.numeric())
estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds")) %>%
  dplyr::select(uid)

length_df <- map_df(unique(estates_sf$uid), function(uid_i){
  message(uid_i)
  
  estates_sf_i <- estates_sf[estates_sf$uid %in% uid_i,]
  
  osm_sf_i <- st_intersection(osm_sf, estates_sf_i) %>%
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

tl_df <- tl_df %>%
  left_join(length_df, by = "uid")

# Export
saveRDS(tl_df, file.path(analysis_data_dir, "google_estates.Rds"))