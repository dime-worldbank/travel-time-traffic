# Append Google Route Data

# Load/prep google -------------------------------------------------------------
gg_df <- file.path(traffic_tt_dir, "google_individual_files_routes") %>%
  list.files(pattern = ".Rds",
             full.names = T) %>%
  map_df(readRDS)

gg_df <- gg_df %>%
  dplyr::mutate(count_ggtyp_all = count_ggtyp_0 + count_ggtyp_1 + count_ggtyp_2 + count_ggtyp_3 + count_ggtyp_4,
                google_prop_ggtyp_234  = (count_ggtyp_2 + count_ggtyp_3 + count_ggtyp_4) / count_ggtyp_all,
                google_prop_ggtyp_34   = (count_ggtyp_3 + count_ggtyp_4) / count_ggtyp_all,
                google_prop_ggtyp_4    = (count_ggtyp_4) / count_ggtyp_all,
                
                count_mptyp_all = count_mptyp_0 + count_mptyp_1 + count_mptyp_2 + count_mptyp_3 + count_mptyp_4,
                google_prop_mptyp_234  = (count_mptyp_2 + count_mptyp_3 + count_mptyp_4) / count_mptyp_all,
                google_prop_mptyp_34   = (count_mptyp_3 + count_mptyp_4) / count_mptyp_all,
                google_prop_mptyp_4    = (count_mptyp_4) / count_mptyp_all,
                
                count_ggrte_all = count_ggrte_0 + count_ggrte_1 + count_ggrte_2 + count_ggrte_3 + count_ggrte_4,
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

mb_df <- mb_df %>%
  mutate(length_ggtyp_all = length_ggtyp_low + length_ggtyp_moderate + length_ggtyp_heavy + length_ggtyp_severe)

