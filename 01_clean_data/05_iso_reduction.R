# Reduction in route length

# Load data --------------------------------------------------------------------
#### Iso Routes
iso_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "appended_routes",
                            "iso_routes.Rds"))

## Add original length
iso_sf$length_m_orig <- iso_sf %>% st_length() %>% as.numeric()

#### Traffic
iso_traffic_df <- file.path(data_dir, "extracted-data", "h3_iso_routes", "google_traffic_levels") %>%
  list.files(full.names = T) %>%
  map_df(readRDS) %>%
  dplyr::rename(route_id = uid)

#### Beta Coefficients
beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Add OSM roads ----------------------------------------------------------------
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds")) %>%
  dplyr::select(fclass) %>%
  dplyr::mutate(length = geometry %>% st_length() %>% as.numeric())

iso_buff_sf <- st_buffer(iso_sf, dist = 10)

length_df <- map_df(unique(iso_buff_sf$route_id), function(route_id_i){
  message(route_id_i)
  
  iso_buff_sf_i <- iso_buff_sf[iso_buff_sf$route_id %in% route_id_i,]
  
  osm_sf_i <- st_intersection(osm_sf, iso_buff_sf_i) %>%
    st_drop_geometry() %>%
    group_by(fclass) %>%
    dplyr::summarise(length = sum(length, na.rm = T)) %>%
    ungroup() %>%
    dplyr::mutate(length_total = sum(length),
                  prop = length / length_total) %>%
    dplyr::select(fclass, prop) %>%
    pivot_wider(names_from = fclass,
                values_from = prop) %>%
    dplyr::mutate(route_id = route_id_i)
  
  return(osm_sf_i)
})

length_df <- length_df %>%
  mutate(across(everything(), ~ tidyr::replace_na(., 0))) %>%
  rename_with(~ paste0("prop_", .x), -route_id)

# -------------
weights <- c(
  prop_trunk = 4,
  prop_primary = 3,
  prop_secondary = 2,
  prop_tertiary = 1.5,
  prop_residential = 1,
  prop_unclassified = 0.5
)

length_v2_df <- length_df %>%
  dplyr::mutate(
    dplyr::across(
      names(weights),
      ~ .x * weights[dplyr::cur_column()]
    )
  ) %>%
  dplyr::mutate(
    total = rowSums(dplyr::select(., all_of(names(weights))))
  ) %>%
  dplyr::mutate(
    dplyr::across(
      all_of(names(weights)),
      ~ .x / total
    )
  ) %>%
  dplyr::select(-total)
# -------------

iso_traffic_df <- iso_traffic_df %>%
  left_join(length_df, by = "route_id")

if(F){
  iso_traffic_df <- iso_traffic_df %>%
    dplyr::mutate(prop_all = prop_trunk + prop_primary + prop_secondary + prop_tertiary + prop_residential + prop_unclassified)
  iso_traffic_df$prop_all %>% summary()
}

# Determine reduction ----------------------------------------------------------
iso_length_orig_df <- iso_sf %>%
  dplyr::select(route_id, length_m_orig) %>%
  st_drop_geometry()

iso_traffic_prepped_df <- iso_traffic_df %>%
  dplyr::mutate(dow = datetime %>% lubridate::wday(label = T),
                hour = datetime %>% hour()) %>%
  dplyr::mutate(dow_weekday = dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%
  dplyr::mutate(length_total = (count_1 + count_2 + count_3 + count_4),
                tl_prop_2 = count_2 / length_total,
                tl_prop_3 = count_3 / length_total,
                tl_prop_4 = count_4 / length_total)

for (agg_method in c("mean", "85th_percentile")) {

  if (agg_method == "mean") {
    iso_traffic_agg_df <- iso_traffic_prepped_df %>%
      group_by(route_id, hour, dow_weekday,
               prop_trunk_fast, prop_trunk, prop_primary, prop_secondary,
               prop_tertiary, prop_residential, prop_unclassified) %>%
      dplyr::summarise(tl_prop_2 = mean(tl_prop_2, na.rm = T),
                       tl_prop_3 = mean(tl_prop_3, na.rm = T),
                       tl_prop_4 = mean(tl_prop_4, na.rm = T),
                       .groups = "drop")
  } else {
    iso_traffic_agg_df <- iso_traffic_prepped_df %>%
      group_by(route_id, hour, dow_weekday,
               prop_trunk_fast, prop_trunk, prop_primary, prop_secondary,
               prop_tertiary, prop_residential, prop_unclassified) %>%
      dplyr::summarise(tl_prop_2 = quantile(tl_prop_2, 0.85, na.rm = T),
                       tl_prop_3 = quantile(tl_prop_3, 0.85, na.rm = T),
                       tl_prop_4 = quantile(tl_prop_4, 0.85, na.rm = T),
                       .groups = "drop")
  }

  iso_traffic_agg_df <- iso_traffic_agg_df %>%
    left_join(iso_length_orig_df, by = "route_id") %>%
    mk_traffic_indicators(beta) %>%
    dplyr::rename(prop_reduction = speed_multiplier)

  prop_reduc_df <- iso_traffic_agg_df %>%
    dplyr::select(route_id, prop_reduction, hour, dow_weekday)

  iso_sf_loop <- iso_sf %>%
    left_join(prop_reduc_df, by = "route_id")

  # Make shortened routes
  iso_m <- st_transform(iso_sf_loop, 32737)

  iso_m_trim <- iso_m %>%
    mutate(prop_reduction = pmin(pmax(prop_reduction, 0), 1)) %>%
    rowwise() %>%
    mutate(
      geometry = st_linesubstring(geometry, from = 0, to = prop_reduction)
    ) %>%
    ungroup() %>%
    st_as_sf()

  iso_trim_sf <- st_transform(iso_m_trim, st_crs(iso_sf))

  # Make polygons
  iso_trim_poly_sf <- iso_trim_sf %>%
    group_by(uid, hour, dow_weekday) %>%
    dplyr::summarise(geometry = geometry %>% st_union() %>% st_concave_hull(ratio = 0.75)) %>%
    ungroup()

  saveRDS(iso_trim_sf,      file.path(data_dir, "Isochrone Routes", paste0("iso_congestion_routes_", agg_method, ".Rds")))
  saveRDS(iso_trim_poly_sf, file.path(data_dir, "Isochrone Routes", paste0("iso_congestion_poly_",   agg_method, ".Rds")))

}

# Make polygons ----------------------------------------------------------------
iso_poly_sf <- iso_sf %>%
  group_by(uid) %>%
  dplyr::summarise(geometry = geometry %>% st_union() %>% st_concave_hull(ratio = 0.75)) %>%
  ungroup()

saveRDS(iso_poly_sf, 
        file.path(data_dir, "Isochrone Routes", "iso_poly.Rds"))

