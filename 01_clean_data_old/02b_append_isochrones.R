# Append Isochrones

iso_sf <- file.path(data_dir, "Isochrone Routes", "individual_routes") %>%
  list.files(full.names = T) %>%
  map_df(readRDS) %>%
  dplyr::mutate(route_id = 1:n())

iso_10m_sf <- iso_sf %>%
  st_buffer_chunks(dist = 10, chunk_size = 250)

iso_20m_sf <- iso_sf %>%
  st_buffer_chunks(dist = 20, chunk_size = 250)

saveRDS(iso_sf,
        file.path(data_dir, "Isochrone Routes", "appended_routes",
                  "iso_routes.Rds"))

saveRDS(iso_10m_sf,
        file.path(data_dir, "Isochrone Routes", "appended_routes",
                  "iso_10m_routes.Rds"))

saveRDS(iso_20m_sf,
        file.path(data_dir, "Isochrone Routes", "appended_routes",
                  "iso_20m_routes.Rds"))

