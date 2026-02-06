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

# Determine reduction ----------------------------------------------------------
iso_length_orig_df <- iso_sf %>%
  dplyr::select(route_id, length_m_orig) %>%
  st_drop_geometry() 

iso_traffic_agg_df <- iso_traffic_df %>%
  dplyr::mutate(dow = datetime %>% lubridate::wday(label = T),
                hour = datetime %>% hour()) %>%
  dplyr::mutate(dow_weekday = dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%
  dplyr::mutate(length_total = (count_1 + count_2 + count_3 + count_4),
                tl_prop_2 = count_2 / length_total,
                tl_prop_3 = count_3 / length_total,
                tl_prop_4 = count_4 / length_total) %>%
  group_by(route_id, hour, dow_weekday) %>%
  dplyr::summarise(tl_prop_2 = mean(tl_prop_2),
                   tl_prop_3 = mean(tl_prop_3),
                   tl_prop_4 = mean(tl_prop_4)) %>%
  ungroup() %>%
  left_join(iso_length_orig_df, by = "route_id") %>%
  mutate(
    # Linear predictor: log(delay per km)
    CI = beta["tl_prop_2"] * tl_prop_2 +
      beta["tl_prop_3"] * tl_prop_3 +
      beta["tl_prop_4"] * tl_prop_4,
    
    # Delay factor relative to green
    delay_factor = exp(CI),
    
    # Speed as a fraction of green speed
    speed_multiplier = exp(-CI)
  ) %>%
  # ## Don't need to do this; speed_multiplier is the same as prop_distance
  # dplyr::mutate(
  #   travel_time_traffic = 30 * delay_factor
  # ) %>%
  # dplyr::mutate(speed_orig    = (length_m_orig/1000) / (30/60),
  #               speed_traffic = (length_m_orig/1000) / (travel_time_traffic/60),
  #               length_m_traffic = speed_traffic*0.5*1000,
  #               prop_distance = (length_m_traffic / length_m_orig))
  dplyr::rename(prop_reduction = speed_multiplier)

prop_reduc_df <- iso_traffic_agg_df %>%
  dplyr::select(route_id, prop_reduction, hour, dow_weekday)

iso_sf <- iso_sf %>%
  left_join(prop_reduc_df, by = "route_id")

# Make shortened routes --------------------------------------------------------
# 2) Project to meters (Kenya-ish example: UTM 37S)
iso_m <- st_transform(iso_sf, 32737)

# 3) Trim each line to the first prop_reduction fraction of its length
iso_m_trim <- iso_m %>%
  mutate(prop_reduction = pmin(pmax(prop_reduction, 0), 1)) %>%
  rowwise() %>%
  mutate(
    geometry = st_linesubstring(geometry, from = 0, to = prop_reduction) # REPLACE geometry
  ) %>%
  ungroup() %>%
  st_as_sf()

# 4) Transform back to the original CRS
iso_trim_sf <- st_transform(iso_m_trim, st_crs(iso_sf))

# 5) New end point / destination coordinates
# iso_trim_sf <- iso_trim_sf %>%
#   mutate(
#     .end_xy = lapply(st_geometry(.), \(g) {
#       xy <- st_coordinates(g)
#       xy[nrow(xy), c("X", "Y")]
#     }),
#     dst_longitude = vapply(.end_xy, `[[`, numeric(1), 1),
#     dst_latitude  = vapply(.end_xy, `[[`, numeric(1), 2)
#   ) %>%
#   select(-.end_xy)

# 6) Make polygons
iso_trim_poly_sf <- iso_trim_sf %>%
  group_by(uid, hour, dow_weekday) %>%
  dplyr::summarise(geometry = geometry %>% st_union() %>% st_concave_hull(ratio = 0.75)) %>%
  ungroup()

# iso_trim_sf_i <- iso_trim_sf[iso_trim_sf$uid %in% "887a6e5537fffff",] %>%
#   dplyr::filter(hour %in% 19,
#                 dow_weekday %in% T)
# 
# iso_trim_poly_sf_i <- iso_trim_poly_sf[iso_trim_poly_sf$uid %in% "887a6e5537fffff",] %>%
#   dplyr::filter(hour %in% 19,
#                 dow_weekday %in% T)
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolylines(data = iso_trim_sf_i) %>%
#   addPolygons(data = iso_trim_poly_sf_i)

saveRDS(iso_trim_sf, file.path(data_dir, "Isochrone Routes", "iso_congestion_routes.Rds"))
saveRDS(iso_trim_poly_sf, file.path(data_dir, "Isochrone Routes", "iso_congestion_poly.Rds"))

# Make polygons ----------------------------------------------------------------
iso_poly_sf <- iso_sf %>%
  group_by(uid) %>%
  dplyr::summarise(geometry = geometry %>% st_union() %>% st_concave_hull(ratio = 0.75)) %>%
  ungroup()

saveRDS(iso_poly_sf, 
        file.path(data_dir, "Isochrone Routes", "iso_poly.Rds"))
