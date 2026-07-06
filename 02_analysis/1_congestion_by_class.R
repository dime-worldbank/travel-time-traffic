
osm_df <- readRDS(file.path(analysis_data_dir, "google_osm_10m.Rds"))

osm_df <- osm_df %>%
  dplyr::filter(!is.na(tl_prop_2)) 

osm_df %>%
  group_by(fclass) %>%
  dplyr::summarise(tl_prop_234 = mean(tl_prop_234),
                   tl_prop_34 = mean(tl_prop_34),
                   tl_prop_4 = mean(tl_prop_4)) %>%
  ungroup()
