# Isochrone Changes

# Load data --------------------------------------------------------------------
nbo_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds")) 
h3_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "h3_polygon.Rds"))

osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
osm_sf <- st_intersection(osm_sf, h3_sf %>% st_union() %>% st_buffer(dist = 5500))

osm_main_sf <- osm_sf %>%
  dplyr::filter(fclass %in% c("trunk", "primary", "secondary"))

osm_other_sf <- osm_sf %>%
  dplyr::filter(!(fclass %in% c("trunk", "primary", "secondary")))


iso_poly_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "iso_poly.Rds"))

iso_cong_poly_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "iso_congestion_poly.Rds"))

# Prep data --------------------------------------------------------------------
iso_poly_sf <- iso_poly_sf %>% 
  dplyr::mutate(area_ff = geometry %>% st_area %>% as.numeric()) %>%
  st_drop_geometry()

iso_cong_poly_sf <- iso_cong_poly_sf %>% 
  dplyr::mutate(area_traffic = geometry %>% st_area %>% as.numeric()) %>%
  st_drop_geometry()

iso_cong_poly_sf <- iso_cong_poly_sf %>%
  left_join(iso_poly_sf, by = "uid") %>%
  dplyr::mutate(prop_area = area_traffic / area_ff)

iso_cong_poly_wdays_df <- iso_cong_poly_sf %>%
  dplyr::filter(hour %in% seq(0, 22, 2),
                dow_weekday %in% T)

h3_data_sf <- h3_sf %>%
  left_join(iso_cong_poly_wdays_df, by = "uid")

# Figure -----------------------------------------------------------------------
ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = h3_data_sf, aes(fill = prop_area)) +
  scale_fill_distiller(
    palette = "Spectral",
    direction = 1,
    limits = c(0, 1),
    oob = scales::squish
  ) +
  facet_wrap(~hour, ncol = 3) +
  labs(fill = "Proportion",
       title = "Proportion of free-flow 15-minute isochrone reachable under traffic conditions,\nby time of day and origin location") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        strip.text = element_text(size = 12)) 

ggsave(filename = file.path(figures_dir, "isochrone_hex_changes.png"),
       height = 10,
       width = 11)

