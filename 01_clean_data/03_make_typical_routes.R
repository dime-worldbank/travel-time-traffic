# Make Typical Routes

# Load data --------------------------------------------------------------------
tt_sf <- readRDS(file.path(tt_dir, "google_tt.Rds"))

tt_df <- tt_sf
tt_df$geometry <- NULL

# Common route -----------------------------------------------------------------
mode_df <- tt_df %>%
  group_by(segment_id) %>%
  dplyr::mutate(distance_mode = getmode(distance_m)) %>%
  ungroup() %>%
  dplyr::filter(distance_mode == distance_m) %>%
  group_by(segment_id) %>%
  slice_head(n = 1) %>%
  ungroup()

mode_sf <- tt_sf[tt_sf$uid %in% mode_df$uid,]
mode_sf <- mode_sf[mode_sf$segment_id %in% 1:26,]

# Export -----------------------------------------------------------------------
saveRDS(mode_sf, file.path(tt_dir, "google_typical_route.Rds"))






