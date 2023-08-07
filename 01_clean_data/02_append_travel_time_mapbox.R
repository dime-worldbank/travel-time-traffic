# Append Maxbox TT Data

# Load Data --------------------------------------------------------------------
tt_sf <- list.files(file.path(tt_dir, 
                              "mapbox_daily_data_nairobi"),
                    full.names = T,
                    pattern = "*.Rds") %>%
  lapply(readRDS) %>%
  do.call(what = "rbind")

# Prep Variables ---------------------------------------------------------------
tt_sf$speed_kmh <- (tt_sf$distance/1000) / (tt_sf$duration/60/60)

# Export -----------------------------------------------------------------------
#### Full Data
saveRDS(tt_sf, file.path(tt_dir,
                         "mapbox_tt.Rds"))

#### Just Data (Drop geometry)
tt_df <- tt_sf
tt_df$geometry <- NULL

var_label(tt_df$distance) <- "Distance (Meters)"
var_label(tt_df$duration) <- "Duration in traffic (Seconds)"
var_label(tt_df$speed_kmh) <- "Speed (Km/Hr)"
var_label(tt_df$time) <- "Date Time"
var_label(tt_df$road) <- "Road Name"
var_label(tt_df$segment_id) <- "Segment ID"

saveRDS(tt_df, file.path(tt_dir,
                         "mapbox_data_tt.Rds"))

