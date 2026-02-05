# Clean OSM

# Load data --------------------------------------------------------------------
osm_df <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_10m.Rds")) %>%
  st_drop_geometry()

osm_line_df <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds")) %>%
  st_as_sf() %>%
  dplyr::mutate(osm_road_length_m = geometry %>% st_length() %>% as.numeric()) %>%
  st_drop_geometry() %>%
  dplyr::select(uid, osm_road_length_m)

mb_tl_df <- file.path(data_dir, "extracted-data", "osm_10m", "mapbox_traffic_levels") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS) %>%
  
  # No variation in data across hours after here; some issue with API, so remove
  dplyr::filter(datetime < ymd_hms("2024-01-12 00:00:00", tz = "Africa/Nairobi"))

# Merge ------------------------------------------------------------------------
osm_data_df <- mb_tl_df %>%
  left_join(osm_df, by = "uid")

# Complete ---------------------------------------------------------------------
osm_data_df <- osm_data_df %>%
  complete(
    nesting(uid, name, fclass),
    datetime,
    fill = list(
      length_mb_low      = 0,
      length_mb_moderate = 0,
      length_mb_heavy    = 0,
      length_mb_severe   = 0
    )
  )

# Cleanup ----------------------------------------------------------------------
osm_data_df <- osm_data_df %>%
  dplyr::mutate(length_traffic_total = length_mb_low + length_mb_moderate + length_mb_heavy + length_mb_severe) %>%
  group_by(uid) %>%
  dplyr::mutate(length_traffic_total_max = max(length_traffic_total, na.rm = T)) %>%
  ungroup() %>%
  
  dplyr::mutate(tl_prop_2 = length_mb_moderate/length_traffic_total_max,
                tl_prop_3 = length_mb_heavy/length_traffic_total_max,
                tl_prop_4 = length_mb_severe/length_traffic_total_max) %>%
  left_join(osm_line_df, by = "uid")

# Export -----------------------------------------------------------------------
saveRDS(osm_data_df, file.path(analysis_data_dir, "mapbox_osm_10m.Rds"))
