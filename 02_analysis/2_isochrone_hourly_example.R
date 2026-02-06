# Isochrone Example

# Load data --------------------------------------------------------------------
nbo_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds")) 

osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
osm_sf <- st_intersection(osm_sf, nbo_sf %>% st_union() %>% st_buffer(dist = 0))

osm_main_sf <- osm_sf %>%
  dplyr::filter(fclass %in% c("trunk", "primary", "secondary"))

osm_other_sf <- osm_sf %>%
  dplyr::filter(!(fclass %in% c("trunk", "primary", "secondary")))

iso_poly_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "iso_poly.Rds"))

iso_cong_poly_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "iso_congestion_poly.Rds"))

UID_I <- "887a6e5537fffff"
iso_poly_sf <- iso_poly_sf %>% dplyr::filter(uid %in% UID_I)
iso_cong_poly_sf <- iso_cong_poly_sf %>% 
  dplyr::filter(uid %in% UID_I) %>%
  dplyr::mutate(dow_weekday = case_when(
    dow_weekday %in% T ~ "Weekdays",
    dow_weekday %in% F ~ "Weekends"
  )) %>%
  dplyr::filter(hour %in% seq(0, 22, 2))

# Figure -----------------------------------------------------------------------
ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = iso_poly_sf, fill = NA, aes(color = "15-minute\nIsochrone:\nFree-Flow"), alpha = 1, linewidth = 1) +
  geom_sf(data = iso_cong_poly_sf, aes(fill = dow_weekday), alpha = 0.5) +
  scale_color_manual(values = "red") +
  labs(fill = "15-minute\nIsochrone:\nWith Traffic",
       color = NULL) +
  facet_wrap(~hour, ncol = 3) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")

ggsave(filename = file.path(figures_dir, "isochrone_hourly.png"),
       height = 10,
       width = 11)
