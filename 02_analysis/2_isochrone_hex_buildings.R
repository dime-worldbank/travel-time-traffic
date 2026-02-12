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


building_r <- rast(file.path(data_dir, "Google Building Volume", 
                                      "rawdata",
                                      "nairobi_building_heights_2023_10m.tif"))

# Prep data --------------------------------------------------------------------
iso_poly_sf$bv_ff      <- exact_extract(building_r, iso_poly_sf,      "sum") %>% as.vector()
iso_cong_poly_sf$bv_traffic <- exact_extract(building_r, iso_cong_poly_sf, "sum") %>% as.vector()

iso_poly_sf <- iso_poly_sf %>% 
  dplyr::mutate(area_ff = geometry %>% st_area %>% as.numeric()) %>%
  st_drop_geometry()

iso_cong_poly_sf <- iso_cong_poly_sf %>% 
  dplyr::mutate(area_traffic = geometry %>% st_area %>% as.numeric()) %>%
  st_drop_geometry()

iso_cong_poly_sf <- iso_cong_poly_sf %>%
  left_join(iso_poly_sf, by = "uid") %>%
  dplyr::mutate(prop_area = area_traffic / area_ff,
                prop_bv = bv_traffic / bv_ff,
                prop_bv_m_area = prop_bv - prop_area)

iso_cong_poly_wdays_df <- iso_cong_poly_sf %>%
  dplyr::filter(hour %in% seq(0, 21, 3),
                dow_weekday %in% T)

h3_data_sf <- h3_sf %>%
  left_join(iso_cong_poly_wdays_df, by = "uid")

h3_data_sf <- h3_data_sf %>%
  dplyr::mutate(hour = paste0(hour, ":00"),
                hour = hour %>%
                  factor(levels = seq(0, 21, 3) %>% paste0(":00")))

# Figure -----------------------------------------------------------------------
p1 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = h3_data_sf, 
          aes(fill = prop_area)) +
  scale_fill_distiller(
    palette = "Spectral",
    direction = 1,
    limits = c(0, 1),
    oob = scales::squish
  ) +
  facet_wrap(~hour, ncol = 4) +
  labs(fill = "Proportion",
       title = "A. Proportion of free-flow 15-minute isochrone reachable under traffic conditions,\nby time of day and origin location") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        strip.text = element_text(size = 12)) 

p2 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = h3_data_sf, aes(fill = prop_bv)) +
  scale_fill_distiller(
    palette = "Spectral",
    direction = 1,
    limits = c(0, 1),
    oob = scales::squish
  ) +
  facet_wrap(~hour, ncol = 4) +
  labs(fill = "Proportion",
       title = "B. Proportion of total building volume within free-flow 15-minute isochrone\nreachable under traffic conditions, by time of day and origin location") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        strip.text = element_text(size = 12)) 

p3 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = h3_data_sf, aes(fill = prop_bv_m_area)) +
  scale_fill_gradient2(
    low = "green2",        # Forest Green
    mid = "white",          # Zero point
    high = "#800080",       # Purple
    midpoint = 0,           # Anchor white at exactly 0
    limits = c(-0.05, 0.2), # Your requested range
    oob = scales::squish,   # Values outside the range get the edge colors
    name = "Proportion"
  ) + 
  facet_wrap(~hour, ncol = 4) +
  labs(fill = "Proportion",
       title = "C. Difference in proportion of buildings reachable compared to area reachable\n[panel B - panel A]",
       subtitle = "Positive values indicate proportion of buildings accessible is higher than proportion of area accessible when\ncomparing free flow to traffic conditions") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic"),
        strip.text = element_text(size = 12)) 

p_top <- ggarrange(p1, p2, ncol = 1, common.legend = T, legend = "right")
p <- ggarrange(p_top, p3, ncol = 1, heights = c(0.66, 0.37))

ggsave(p, filename = file.path(figures_dir, "isochrone_hex_changes.png"),
       height = 10,
       width = 9)


