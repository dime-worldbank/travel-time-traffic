# Summary stats

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

route_df$datetime %>% min()
route_df$datetime %>% max()
