# Create Routes

# Load / prep data -------------------------------------------------------------
roads_df <- readRDS(file.path(data_dir, "Travel Time Routes 2026",
                              "roads_traffic_agg.Rds"))

roads_df <- roads_df %>%
  group_by(fclass) %>%
  slice_head(n = 10) %>%
  ungroup()

osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds")) %>%
  st_as_sf()

# Sample origin/destination points per road ------------------------------------
# For each road, find the pair of vertices that is as far apart as possible
# while remaining <= 5km apart (straight-line / Haversine distance).

MAX_DIST_M <- 5000
CBD_LON <- 36.82352458060921
CBD_LAT <- -1.2833532424747365

sample_od_points <- function(geom, toward_cbd) {
  coords <- st_coordinates(geom)[, c("X", "Y")]  # matrix: lon, lat
  n <- nrow(coords)
  if (n < 2) return(NULL)
  
  pts <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = 4326)
  dist_mat <- units::drop_units(st_distance(pts))  # n x n matrix, meters
  diag(dist_mat) <- NA
  dist_mat[dist_mat > MAX_DIST_M] <- NA
  
  if (all(is.na(dist_mat))) {
    # Every pair exceeds 5 km — fall back to the two closest vertices
    dist_mat_full <- units::drop_units(st_distance(pts))
    diag(dist_mat_full) <- NA
    idx <- which(dist_mat_full == min(dist_mat_full, na.rm = TRUE), arr.ind = TRUE)[1, ]
  } else {
    idx <- which(dist_mat == max(dist_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
  }
  
  # Assign origin/dest based on direction relative to CBD
  cbd_pt <- st_sfc(st_point(c(CBD_LON, CBD_LAT)), crs = 4326)
  d_to_cbd <- units::drop_units(st_distance(pts[idx, ], cbd_pt))  # length-2 vector
  
  if (toward_cbd) {
    # Origin is farther from CBD (traveling toward it)
    origin_i <- idx[which.max(d_to_cbd)]
    dest_i   <- idx[which.min(d_to_cbd)]
  } else {
    # Origin is closer to CBD (traveling away from it)
    origin_i <- idx[which.min(d_to_cbd)]
    dest_i   <- idx[which.max(d_to_cbd)]
  }
  
  list(
    origin_lon = coords[origin_i, "X"], origin_lat = coords[origin_i, "Y"],
    dest_lon   = coords[dest_i,   "X"], dest_lat   = coords[dest_i,   "Y"]
  )
}

roads_routes_df <- roads_df %>%
  left_join(osm_sf %>% select(name, fclass, geometry), by = c("name", "fclass")) %>%
  st_as_sf() %>%
  rowwise() %>%
  mutate(od = list(sample_od_points(geometry, toward_cbd))) %>%
  filter(!is.null(od)) %>%
  mutate(
    origin_lon = od$origin_lon,
    origin_lat = od$origin_lat,
    dest_lon   = od$dest_lon,
    dest_lat   = od$dest_lat
  ) %>%
  dplyr::select(name, fclass, toward_cbd, origin_lon, origin_lat, dest_lon, dest_lat) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  dplyr::mutate(uid = paste(name, fclass))

# Manual fixes -----------------------------------------------------------------
manual_fixes <- tribble(
  ~name,                 ~fclass,          ~origin_lat,           ~origin_lon,          ~dest_lat,              ~dest_lon,
  "5th avenue ngong",    "unclassified",   -1.291852112236944, 36.80637291219083,       -1.2959579732292765, 36.80837074243227,
  "airport south road",  "primary",        -1.3466125996198655, 36.90982548384206,     -1.3338983696414057, 36.92737041166018,
  "magadi road",         "secondary",      -1.3888941,            36.7688671,           -1.3438885628273156,    36.76629199880162,
  "nairobi-nakuru road", "trunk",          -1.2458055938780823,   36.67967831873752,    -1.2601411903631143,    36.7222656276782,
  "mombasa road",        "trunk",          -1.3377148358526325, 36.89431271373084,      -1.3220057104421363,    36.84197857002372,
  "ngecha road",         "primary",        -1.2307204454952787, 36.77868040315766,    -1.2353199193139766, 36.76772028538408,
  "southern bypass",     "trunk",          -1.29497505286611,     36.68857339915386,    -1.3118919557101223,    36.73029619913267,
  "uhuru highway",       "trunk",          -1.3044646450463107,   36.82615347488513,    -1.2756927743984747,    36.812880984668446,
  "desai road",          "primary",        -1.2778695486941216,   36.830287095787874,   -1.272332814373133,     36.82768878720186,
  "harambee avenue",     "secondary",      -1.291163166926529,    36.81978102915129,    -1.2878070506738073,    36.826624903481544,
  "hinga road",          "secondary",      -1.2658399773125677,   36.7374864439027,     -1.2664026575898142,    36.74391533920725,
  "juja road",           "primary",        -1.2625905460870637,   36.87745794968857,    -1.2700758888365276,    36.83767512260488,
  "kibiku road",         "unclassified",   -1.2767943525230672,   36.958241916751184,   -1.282814474286799,     36.95985552836867,
  "kirinyaga road",      "tertiary",       -1.2822295614358443,   36.831700405341934,   -1.2793174837405163,    36.821981271988456,
  "kwaheri road",        "residential",    -1.2114077625101152,   36.8242315447232,     -1.2148328625254368,    36.83403499745209,
  "mbale road",          "residential",    -1.2891573,            36.854771899999996,   -1.2898101330121916,    36.85788303525734,
  "shimo la tewa road",  "unclassified",   -1.3046089504190594, 36.829608755044966,    -1.3080404376103487, 36.829542906202626,
  "thika road",          "trunk",          -1.2590721665354896,   36.84554600444856,    -1.2310006638148878,    36.877667771834616,
  "two rivers road",     "unclassified",   -1.2125789235343083,   36.79224617899866,    -1.2079971975881627,    36.79614800528328,
  "wakaba drive",        "residential",    -1.3502363476384782,   36.66170711346427,    -1.345345694414074,     36.665999227312284
) %>%
  mutate(uid = paste(name, fclass))

roads_routes_df <- roads_routes_df %>%
  rows_update(
    manual_fixes %>% select(uid, origin_lat, origin_lon, dest_lat, dest_lon),
    by = "uid"
  )

# Check roads ------------------------------------------------------------------
saveRDS(roads_routes_df, file.path(data_dir, "Travel Time Routes 2026",
                                   "od_pairs.Rds"))
write_csv(roads_routes_df %>%
            dplyr::select(uid, origin_lat, origin_lon, dest_lat, dest_lon), "~/Downloads/od_pairs.csv")


# leaflet() %>%
#   addTiles() %>%
#   addPolylines(data = osm_sf %>%
#                  dplyr::filter(name == "airport south road"))








