
route_sf <- readRDS(file.path(data_dir, "Travel Time Routes 2026", "typical_routes.Rds"))

route_sf <- route_sf %>%
  dplyr::mutate(length = geometry %>% st_length() %>% as.numeric())

route_sf %>% pull(length) %>% summary()

