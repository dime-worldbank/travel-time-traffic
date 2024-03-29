# Download Google Travel Time in Monthly Chunks

readRDS_constvar <- function(path){
  df <- readRDS(path)
  df$leg_id <- NULL
  df$departure_time <- NULL
  df$arrival_time <- NULL
  return(df)
}

# Load Data --------------------------------------------------------------------
tt_sf <- list.files(file.path(tt_dir, 
                              "google_daily_data_nairobi"),
                    full.names = T,
                    pattern = "*.Rds") %>%
  lapply(readRDS_constvar) %>%
  do.call(what = "rbind")

tt_od_df <- read_csv(file.path(tt_dir, "nairobi_tt_od.csv"))
tt_od_df <- tt_od_df %>%
  dplyr::select(segment_id, road) %>%
  dplyr::rename(road_name = road) %>%
  dplyr::mutate(road_name = road_name %>% tools::toTitleCase())

# Prep Variables ---------------------------------------------------------------
## Cleanup
tt_sf <- tt_sf %>%
  mutate(time = time %>% 
           as.character %>% 
           ymd_hms(tz = "UTC") %>% 
           with_tz(tzone = "Africa/Nairobi") %>% 
           floor_date(unit = "30 minutes"),
         speed_kmh = (distance_m/1000) / (duration_s/60/60),
         speed_in_traffic_kmh = (distance_m/1000) / (duration_in_traffic_s/60/60),
         uid = 1:n()) %>%
  dplyr::select(-c(alternative_id)) %>%
  dplyr::rename(segment_id = locations_segment_id,
                datetime   = time)

## Merge in road name
tt_sf <- tt_sf %>%
  left_join(tt_od_df, by = "segment_id")

## Subset to constant sample
tt_sf <- tt_sf %>%
  dplyr::filter(segment_id %in% 1:13)

# Export -----------------------------------------------------------------------

#### Full Data

#### Just Data (Drop geometry)
tt_df <- tt_sf %>%
  st_drop_geometry()

saveRDS(tt_df, file.path(tt_dir,
                         "google_tt_data.Rds"))

saveRDS(tt_sf, file.path(tt_dir,
                         "google_tt_data_geom.Rds"))

