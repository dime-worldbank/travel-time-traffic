# Travel time summary stats table

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# Select variables -------------------------------------------------------------
df <- df %>%
  dplyr::select(uid,
                gg_speed_in_traffic_kmh,
                gg_duration_in_traffic_s,
                gg_distance_m,
                
                mb_speed_in_traffic_kmh,
                mb_duration_in_traffic_s,
                mb_distance_mm,
                
                gg_tl_prop_234,
                gg_tl_prop_34,
                gg_tl_prop_4,
                
                mb_tl_prop_234,
                mb_tl_prop_34,
                mb_tl_prop_4) %>%
  na.omit()





