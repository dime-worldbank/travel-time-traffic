# Isochrone Methods

# Load data -------------------------------------------------------------------
nbo_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds")) 
h3_sf  <- readRDS(file.path(data_dir, "Isochrone Routes", "h3_polygon.Rds"))
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
iso_route_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "appended_routes",
                                  "iso_routes.Rds"))
iso_poly_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "iso_poly.Rds"))

iso_cong_route_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "iso_congestion_routes.Rds")) %>%
  dplyr::filter(hour %in% 19,
                dow_weekday %in% T)
iso_cong_poly_sf <- readRDS(file.path(data_dir, "Isochrone Routes", "iso_congestion_poly.Rds")) %>%
  dplyr::filter(hour %in% 19,
                dow_weekday %in% T)

tiff_vec <- file.path(traffic_gg_raw_dir) %>%
  list.files(pattern = "*.tiff",
             full.names = T) 
traffic_r <- tiff_vec[1000] %>% raster()

# Prep data --------------------------------------------------------------------
UID_I <- "887a6e5537fffff"
#UID_I <- "887a6e423dfffff"

h3_sf_i        <- h3_sf        %>% dplyr::filter(uid == UID_I)
iso_route_sf_i <- iso_route_sf %>% dplyr::filter(uid == UID_I)
iso_poly_sf_i  <- iso_poly_sf %>% dplyr::filter(uid == UID_I)

iso_cong_route_sf_i <- iso_cong_route_sf %>% dplyr::filter(uid == UID_I)
iso_cong_poly_sf_i  <- iso_cong_poly_sf %>% dplyr::filter(uid == UID_I)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = iso_cong_route_sf_i) %>%
  addPolygons(data = iso_cong_poly_sf_i)

h3_sf_c_i <- h3_sf_i %>% st_centroid()

osm_sf <- st_intersection(osm_sf, nbo_sf %>% st_union() %>% st_buffer(dist = 0))
osm_sf <- st_intersection(osm_sf, iso_poly_sf_i %>% st_union() %>% st_buffer(dist = 5000))
traffic_r <- crop(traffic_r, osm_sf)

osm_main_sf <- osm_sf %>%
  dplyr::filter(fclass %in% c("trunk", "primary", "secondary"))

osm_other_sf <- osm_sf %>%
  dplyr::filter(!(fclass %in% c("trunk", "primary", "secondary")))

traffic_r <- aggregate(traffic_r, fact=18, fun=max)
traffic_df <- rasterToPoints(traffic_r, spatial = TRUE) %>% as.data.frame()
names(traffic_df) <- c("value", "x", "y")

# Figure A ---------------------------------------------------------------------
p1 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = h3_sf_i, fill = "red", alpha = 0.7) +
  geom_sf(data = h3_sf_c_i, color = "green2", size = 0.75) +
  labs(title = "Step 1. Make hexagon and centroid") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p1

# Figure B ---------------------------------------------------------------------
p2 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = iso_poly_sf_i, fill = "dodgerblue", alpha = 0.2) +
  geom_sf(data = h3_sf_i, fill = "red", alpha = 0.7) +
  geom_sf(data = h3_sf_c_i, color = "green2", size = 0.75) +
  labs(title = "Step 2. Construct 15-minute isochrone\nfrom centroid location") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p2

# Figure C ---------------------------------------------------------------------
p3 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = iso_poly_sf_i, fill = "dodgerblue", alpha = 0.2) +
  geom_sf(data = iso_route_sf_i, color = "dodgerblue", alpha = 1) +
  geom_sf(data = h3_sf_i, fill = "red", alpha = 0.7) +
  geom_sf(data = h3_sf_c_i, color = "green2", size = 0.75) +
  labs(title = "Step 3. Construct routes from\ncentroid to isochrone vertices") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p3

# Figure D ---------------------------------------------------------------------
p4 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  geom_sf(data = iso_poly_sf_i, fill = "dodgerblue", alpha = 0.2) +
  geom_sf(data = iso_route_sf_i, color = "dodgerblue", alpha = 1, linewidth = 1.5) +
  geom_raster(data = traffic_df, 
              aes(x = x, y = y, 
                  fill = as.factor(value))) +
  scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
  geom_sf(data = h3_sf_i, fill = "red", alpha = 0.7) +
  geom_sf(data = h3_sf_c_i, color = "green2", size = 0.75) +
  labs(title = "Step 4. Extract traffic information\nwithin routes",
       fill = "Traffic\nLevel") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")
p4

# Figure E ---------------------------------------------------------------------
p5 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  
  geom_sf(data = iso_poly_sf_i, fill = "dodgerblue", alpha = 0.2) +
  geom_sf(data = iso_route_sf_i, color = "dodgerblue", alpha = 1) +
  
  geom_sf(data = iso_cong_route_sf_i, color = "darkorange", alpha = 1, linewidth = 1.25) +
  
  geom_sf(data = h3_sf_i, fill = "red", alpha = 0.7) +
  geom_sf(data = h3_sf_c_i, color = "green2", size = 0.75) +
  labs(title = "Steps 5-7. Use traffic information to determine\nportion of original route can travel in 15 minutes",
       fill = "Traffic\nLevel") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p5

# Figure F ---------------------------------------------------------------------
p6 <- ggplot() +
  geom_sf(data = osm_main_sf, color = "gray40") +
  geom_sf(data = osm_other_sf, linewidth = 0.1, color = "gray40") +
  
  geom_sf(data = iso_poly_sf_i, fill = "dodgerblue", alpha = 0.2) +
  geom_sf(data = iso_route_sf_i, color = "dodgerblue", alpha = 1) +
  
  geom_sf(data = iso_cong_poly_sf_i, fill = "darkorange", alpha = 0.3) +
  geom_sf(data = iso_cong_route_sf_i, color = "darkorange", alpha = 1, linewidth = 1) +
  
  geom_sf(data = h3_sf_i, fill = "red", alpha = 0.7) +
  geom_sf(data = h3_sf_c_i, color = "green2", size = 0.75) +
  labs(title = "Step 8. Construction 15-minute isochrone\nunder traffic conditions",
       fill = "Traffic\nLevel") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p6

# Arrange/export ---------------------------------------------------------------
p <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
ggsave(p,
       filename = file.path(figures_dir, "isochrone_methods.png"),
       height = 11,
       width = 9.5)
