# Append Maxbox TT Data

# Load Data --------------------------------------------------------------------
tt_sf <- list.files(file.path(tt_dir, 
                              "mapbox_daily_data_nairobi"),
                    full.names = T,
                    pattern = "*.Rds") %>%
  lapply(readRDS) %>%
  do.call(what = "rbind")

tt_od_df <- read_csv(file.path(tt_dir, "nairobi_tt_od.csv"))
tt_od_df <- tt_od_df %>%
  dplyr::select(segment_id, road) %>%
  dplyr::rename(road_name = road) %>%
  dplyr::mutate(road_name = road_name %>% tools::toTitleCase())

# Prep Variables ---------------------------------------------------------------
## Cleanup 
tt_sf <- tt_sf %>%
  dplyr::mutate(speed_kmh = (distance/1000) / (duration/60/60),
                uid = 1:n()) %>%
  dplyr::rename(distance_m = distance,
                duration_s = duration)

## Merge in road name
tt_sf <- tt_sf %>%
  left_join(tt_od_df, by = "segment_id")

# Export -----------------------------------------------------------------------
#### Full Data
saveRDS(tt_sf, file.path(tt_dir, "mapbox_tt.Rds"))

#### Just Data (Drop geometry)
tt_df <- tt_sf
tt_df$geometry <- NULL

saveRDS(tt_df, file.path(tt_dir,
                         "mapbox_data_tt.Rds"))

