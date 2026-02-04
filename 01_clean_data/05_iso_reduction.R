# Reduction in route length

# Load data --------------------------------------------------------------------
iso_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "appended_routes",
                            "iso_routes.Rds"))

## Add original length
iso_sf$length_m_orig <- iso_sf %>% st_length() %>% as.numeric()

# Determine reduction ----------------------------------------------------------
iso_sf$prop_reduction <- runif(1:nrow(iso_sf), 0.01, 0.99)

# Make shortened routes --------------------------------------------------------
# 0) Check geometry types (optional sanity check)
#iso_sf$geometry %>% st_geometry_type() %>% as.character() %>% table()

# 1) Merge MULTILINESTRING -> LINESTRING where possible (and keep only line features)
# iso_sf1 <- iso_sf %>%
#   mutate(geometry = st_line_merge(geometry)) %>%
#   filter(st_is(geometry, c("LINESTRING", "MULTILINESTRING")))

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

# 4) (Optional) Transform back to the original CRS
iso_trim_sf <- st_transform(iso_m_trim, st_crs(iso_sf))

# 5) New end point / destination coordinates
iso_trim_sf <- iso_trim_sf %>%
  mutate(
    .end_xy = lapply(st_geometry(.), \(g) {
      xy <- st_coordinates(g)
      xy[nrow(xy), c("X", "Y")]
    }),
    dst_longitude = vapply(.end_xy, `[[`, numeric(1), 1),
    dst_latitude  = vapply(.end_xy, `[[`, numeric(1), 2)
  ) %>%
  select(-.end_xy)

# Make polygons ----------------------------------------------------------------
iso_sf
iso_trim_sf

iso_poly_sf <- iso_sf %>%
  st_drop_geometry() %>%
  group_by(uid) %>%
  summarise(
    geometry = st_sfc({
      coords <- cbind(dst_longitude, dst_latitude)
      
      if (!all(coords[1, ] == coords[nrow(coords), ])) {
        coords <- rbind(coords, coords[1, ])
      }
      
      st_polygon(list(coords))
    }, crs = 4326),
    .groups = "drop"
  ) %>%
  st_as_sf()

iso_trim_poly_sf <- iso_trim_sf %>%
  st_drop_geometry() %>%
  group_by(uid) %>%
  summarise(
    geometry = st_sfc({
      coords <- cbind(dst_longitude, dst_latitude)
      
      if (!all(coords[1, ] == coords[nrow(coords), ])) {
        coords <- rbind(coords, coords[1, ])
      }
      
      st_polygon(list(coords))
    }, crs = 4326),
    .groups = "drop"
  ) %>%
  st_as_sf()

i = 20
leaflet() %>%
  addTiles() %>%
  addPolygons(data = iso_trim_poly_sf[i,], color = "red", opacity = 1) %>%
  addPolygons(data = iso_poly_sf[i,])










### CHECKS
iso_trim_sf$length_m_trim <- iso_trim_sf %>% st_length() %>% as.numeric()

iso_trim_sf <- iso_trim_sf %>%
  dplyr::mutate(prop_trim = length_m_trim/length_m_orig)

iso_trim_sf %>%
  st_drop_geometry() %>%
  ggplot() +
  geom_point(aes(x = prop_trim, y = prop_reduction))

i <- 350
iso_sf$prop_reduction[i]

leaflet() %>%
  addTiles() %>%
  addPolylines(data = iso_trim_sf[i,], color = "red", opacity = 1) %>%
  addPolylines(data = iso_sf[i,])





