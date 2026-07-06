# Clean OSM

# Load data --------------------------------------------------------------------
unit_df <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds")) %>%
  st_drop_geometry()

mb_tl_df <- file.path(data_dir, "extracted-data", "estates", "mapbox_traffic_levels") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS) %>%
  
  # No variation in data across hours after here; some issue with API, so remove
  dplyr::filter(datetime < ymd_hms("2024-01-12 00:00:00", tz = "Africa/Nairobi"))

# Merge ------------------------------------------------------------------------
unit_data_df <- mb_tl_df %>%
  left_join(unit_df, by = "uid")

# Cleanup ----------------------------------------------------------------------
unit_data_df <- unit_data_df %>%
  dplyr::mutate(length_mb_low = replace_na(length_mb_low, 0),
                length_mb_moderate = replace_na(length_mb_moderate, 0),
                length_mb_heavy = replace_na(length_mb_heavy, 0),
                length_mb_severe = replace_na(length_mb_severe, 0)) %>%
  dplyr::mutate(length_traffic_total = length_mb_low + length_mb_moderate + length_mb_heavy + length_mb_severe) %>%
  group_by(uid) %>%
  dplyr::mutate(length_traffic_total_max = max(length_traffic_total, na.rm = T)) %>%
  ungroup() %>%
  
  dplyr::mutate(tl_prop_2 = length_mb_moderate/length_traffic_total_max,
                tl_prop_3 = length_mb_heavy/length_traffic_total_max,
                tl_prop_4 = length_mb_severe/length_traffic_total_max)

# Export -----------------------------------------------------------------------
saveRDS(unit_data_df, file.path(analysis_data_dir, "mapbox_estates.Rds"))
