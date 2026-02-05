
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

# Export
saveRDS(tl_df, file.path(analysis_data_dir, "google_estates.Rds"))