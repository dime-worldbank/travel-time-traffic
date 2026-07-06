
# Mapbox -----------------------------------------------------------------------
api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv")

mapbox_key <- api_keys %>%
  dplyr::filter(Service == "Mapbox",
                Account == "dimedanfo1@gmail.com") %>%
  pull(Key) %>%
  as.character()

## Grab shapefile of Manhattan
roi_sf <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm41_KEN_1_pk.rds")) %>%
  st_as_sf() %>%
  dplyr::filter(NAME_1 == "Nairobi")

## Query traffic data
roi_cong_poly <- get_vector_tiles(
  tileset_id = "mapbox.mapbox-traffic-v1",
  location = roi_sf,
  zoom = 14,
  access_token = mapbox_key
)$traffic

## Map
roi_cong_poly <- roi_cong_poly %>%
  mutate(congestion = congestion %>% 
           tools::toTitleCase() %>%
           factor(levels = c("Low", "Moderate", "Heavy", "Severe")))

ggplot() +
  geom_sf(data = roi_cong_poly, aes(color = congestion),
          size = 0.1) +
  scale_color_manual(values = c("green2", "orange", "red", "#660000")) +
  labs(color = "Congestion") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color="white"))



# Google -----------------------------------------------------------------------
nairobi <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm41_KEN_3_pk.rds"))

grid_df <- gt_make_grid(nairobi,
                        height = 10000, # 8000
                        width = 10000,
                        zoom = 17, # 17
                        reduce_hw = 75)

nrow(grid_df)

