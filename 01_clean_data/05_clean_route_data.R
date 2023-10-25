# Append Google Route Data

# Load/prep google -------------------------------------------------------------
gg_df <- file.path(traffic_tt_dir, "google_individual_files_routes") %>%
  list.files(pattern = ".Rds",
             full.names = T) %>%
  map_df(readRDS)

gg_df <- gg_df %>%
  dplyr::mutate(count_ggtyp_all = count_ggtyp_0 + count_ggtyp_1 + count_ggtyp_2 + count_ggtyp_3 + count_ggtyp_4,
                count_mptyp_all = count_mptyp_0 + count_mptyp_1 + count_mptyp_2 + count_mptyp_3 + count_mptyp_4,
                count_ggrte_all = count_ggrte_0 + count_ggrte_1 + count_ggrte_2 + count_ggrte_3 + count_ggrte_4) %>%
  
  group_by(segment_id) %>%
  dplyr::mutate(count_ggtyp_all = max(count_ggtyp_all),
                count_mptyp_all = max(count_mptyp_all),
                count_ggrte_all = max(count_ggrte_all)) %>%
  ungroup() %>%
  
  dplyr::mutate(google_prop_ggtyp_234  = (count_ggtyp_2 + count_ggtyp_3 + count_ggtyp_4) / count_ggtyp_all,
                google_prop_ggtyp_34   = (count_ggtyp_3 + count_ggtyp_4) / count_ggtyp_all,
                google_prop_ggtyp_4    = (count_ggtyp_4) / count_ggtyp_all,
                
                google_prop_mptyp_234  = (count_mptyp_2 + count_mptyp_3 + count_mptyp_4) / count_mptyp_all,
                google_prop_mptyp_34   = (count_mptyp_3 + count_mptyp_4) / count_mptyp_all,
                google_prop_mptyp_4    = (count_mptyp_4) / count_mptyp_all,
                
                google_prop_ggrte_234  = (count_ggrte_2 + count_ggrte_3 + count_ggrte_4) / count_ggrte_all,
                google_prop_ggrte_34   = (count_ggrte_3 + count_ggrte_4) / count_ggrte_all,
                google_prop_ggrte_4    = (count_ggrte_4) / count_ggrte_all) %>%
  dplyr::rename(google_distance_m            = distance_m,
                google_duration_in_traffic_s = duration_in_traffic_s,
                google_speed_in_traffic_kmh  = speed_in_traffic_kmh) %>%
  dplyr::select(segment_id, datetime, contains("google_"))

# Load/prep mapbox -------------------------------------------------------------
mb_df <- file.path(traffic_tt_dir, "mapbox_individual_files_routes") %>%
  list.files(pattern = ".Rds",
             full.names = T) %>%
  map_df(readRDS)

# Change names so similar to Google
names(mb_df) <- names(mb_df) %>%
  str_replace_all("_low$",      "_1") %>%
  str_replace_all("_moderate$", "_2") %>%
  str_replace_all("_heavy$",    "_3") %>%
  str_replace_all("_severe$",  "_4")

mb_df <- mb_df %>%
  dplyr::mutate(length_ggtyp_all = length_ggtyp_1 + length_ggtyp_2 + length_ggtyp_3 + length_ggtyp_4,
                length_mptyp_all = length_mptyp_1 + length_mptyp_2 + length_mptyp_3 + length_mptyp_4,
                length_mprte_all = length_mprte_1 + length_mprte_2 + length_mprte_3 + length_mprte_4) %>%
  
  group_by(segment_id) %>%
  dplyr::mutate(length_ggtyp_all = max(length_ggtyp_all),
                length_mptyp_all = max(length_mptyp_all),
                length_mprte_all = max(length_mprte_all)) %>%
  ungroup() %>%
  
  dplyr::mutate(mapbox_prop_ggtyp_234 = (length_ggtyp_2 + length_ggtyp_3 + length_ggtyp_4) / length_ggtyp_all,
                mapbox_prop_ggtyp_34  = (length_ggtyp_3 + length_ggtyp_4) / length_ggtyp_all,
                mapbox_prop_ggtyp_4   = (length_ggtyp_4) / length_ggtyp_all,
                
                mapbox_prop_mptyp_234 = (length_mptyp_2 + length_mptyp_3 + length_mptyp_4) / length_mptyp_all,
                mapbox_prop_mptyp_34  = (length_mptyp_3 + length_mptyp_4) / length_mptyp_all,
                mapbox_prop_mptyp_4   = (length_mptyp_4) / length_mptyp_all,
                
                mapbox_prop_mprte_234 = (length_mprte_2 + length_mprte_3 + length_mprte_4) / length_mprte_all,
                mapbox_prop_mprte_34  = (length_mprte_3 + length_mprte_4) / length_mprte_all,
                mapbox_prop_mprte_4   = (length_mprte_4) / length_mprte_all) %>%
  dplyr::mutate(mapbox_speed_in_traffic_kmh = (distance_m/1000) / (duration_s/60/60)) %>%
  dplyr::rename(mapbox_distance_m            = distance_m,
                mapbox_duration_in_traffic_s = duration_s) %>%
  dplyr::select(segment_id, datetime, contains("mapbox_"))

# Only keep complete cases -----------------------------------------------------
gg_df <- gg_df %>%
  dplyr::filter(!is.na(google_duration_in_traffic_s),
                !is.na(google_prop_ggtyp_234))

mb_df <- mb_df %>%
  dplyr::filter(!is.na(mapbox_duration_in_traffic_s),
                !is.na(mapbox_prop_ggtyp_234))

# Merge: Make wide -------------------------------------------------------------
tt_wide_df <- gg_df %>%
  full_join(mb_df, by = c("segment_id", "datetime")) 

# Cleanup / add variables ------------------------------------------------------
tt_wide_df <- tt_wide_df %>%
  dplyr::mutate(date = datetime %>% date(),
                hour = datetime %>% hour()) %>%
  dplyr::select(segment_id, datetime, date, hour, everything())

# Make long --------------------------------------------------------------------
## Google
gg_sub_df <- tt_wide_df %>%
  dplyr::select(segment_id, datetime, date, hour, contains("google")) %>%
  mutate(source = "Google") 

names(gg_sub_df) <- names(gg_sub_df) %>%
  str_replace_all("google_", "")

## Mapbox
mb_sub_df <- tt_wide_df %>%
  dplyr::select(segment_id, datetime, date, hour, contains("mapbox")) %>%
  mutate(source = "Mapbox")

names(mb_sub_df) <- names(mb_sub_df) %>%
  str_replace_all("mapbox_", "") 

## Append
tt_long_df <- bind_rows(gg_sub_df,
                        mb_sub_df)

# Export -----------------------------------------------------------------------
saveRDS(tt_wide_df,   file.path(analysis_data_dir, "route_data_wide.Rds"))
write_dta(tt_wide_df, file.path(analysis_data_dir, "route_data_wide.dta"))
write_csv(tt_wide_df, file.path(analysis_data_dir, "route_data_wide.csv"))

saveRDS(tt_long_df,   file.path(analysis_data_dir, "route_data_long.Rds"))
write_dta(tt_long_df, file.path(analysis_data_dir, "route_data_long.dta"))
write_csv(tt_long_df, file.path(analysis_data_dir, "route_data_long.csv"))
