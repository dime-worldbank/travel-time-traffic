# Append Isochrones

iso_sf <- file.path(data_dir, "Isochrone Routes", "individual_routes") %>%
  list.files(full.names = T) %>%
  map_df(readRDS) %>%
  dplyr::mutate(route_id = 1:n()) %>%
  st_simplify()

uid_vec <- iso_sf$uid %>% unique()

iso_10m_sf <- iso_sf %>%
  st_buffer_chunks(dist = 10, chunk_size = 250) %>%
  st_simplify()

iso_20m_sf <- iso_sf %>%
  st_buffer_chunks(dist = 20, chunk_size = 250) %>%
  st_simplify()

saveRDS(iso_sf,
        file.path(data_dir, "Isochrone Routes", "appended_routes",
                  "iso_routes.Rds"))

saveRDS(iso_10m_sf,
        file.path(data_dir, "Isochrone Routes", "appended_routes",
                  "iso_10m_routes.Rds"))

saveRDS(iso_20m_sf,
        file.path(data_dir, "Isochrone Routes", "appended_routes",
                  "iso_20m_routes.Rds"))
