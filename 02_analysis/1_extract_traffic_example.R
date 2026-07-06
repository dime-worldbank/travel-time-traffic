# Extraction example

r <- file.path(traffic_gg_raw_dir) %>%
  list.files(pattern = "*.tiff",
             full.names = T) %>%
  head(1) %>%
  raster()

route_sf <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))
route_sf <- route_sf %>%
  filter(segment_id == 13)
route_buff_sf <- route_sf %>%
  st_buffer(dist = 10)

coord_df <- route_sf %>%
  st_coordinates() %>%
  as.data.frame()
coord_df <- coord_df[10,]
coord_sf <- st_as_sf(coord_df, coords = c("X", "Y"), crs=4326) %>%
  st_buffer(dist = 110)

r <- r %>% crop(coord_sf) %>% rast()
r[][r[] == 0] <- NA
r[] <- r[] %>% as.character()

r_bbox <- st_bbox(r)

p <- ggplot() + 
  geom_sf(data = route_buff_sf,
          fill = "gray70",
          aes(color = "Portion of\nO-D Route,\n10m Buffer")) +
  geom_spatraster(data = r) +
  xlim(c(r_bbox[1], r_bbox[3])) +
  ylim(c(r_bbox[2], r_bbox[4])) +
  theme_void() +
  scale_fill_manual(values = c("green2",
                               "orange",
                               "red",
                               "#660000"),
                    na.value = "transparent") +
  scale_color_manual(values = "black") +
  labs(fill = "Traffic\nLevel",
       color = NULL)

ggsave(p, 
       filename = file.path(figures_dir, 
                            "example_traffic_extraction.png"),
       height = 4, width = 4)