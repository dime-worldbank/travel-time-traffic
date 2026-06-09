# Extract Data


r <- raster(file.path(data_dir, "Google Traffic 2026", "RawData", "gt_nairobi_utc1781037023.tif"))
routes_df <- readRDS(file.path(data_dir, "Travel Time Routes 2026", "Travel Time Data", "tt_2026-06-10_00-32-13.Rds"))
grid_df <- readRDS(file.path(data_dir, "Google Traffic 2026", "Grid", "grid_param.Rds"))

# Leaflet map: traffic raster + routes ----------------------------------------
library(leaflet)
library(sf)
library(googlePolylines)

pal <- colorNumeric(c("green", "yellow", "red"), values(r), na.color = "transparent")

# Decode polylines into a single sf object
routes_sf <- routes_df %>%
  filter(!is.na(encoded_polyline)) %>%
  rowwise() %>%
  mutate(geometry = st_sfc(
    st_linestring(as.matrix(googlePolylines::decode(encoded_polyline)[[1]][, c("lon", "lat")])),
    crs = 4326
  )) %>%
  ungroup() %>%
  st_as_sf()

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = grid_df) %>%
  addPolylines(data = routes_sf, color = "black", weight = 3, opacity = 0.8,
               label = ~name) %>%
  addCircleMarkers(data = routes_df,
                   lng = ~origin_lon, lat = ~origin_lat,
                   color = "darkgreen", fillColor = "green", fillOpacity = 1,
                   radius = 6, weight = 2, label = ~name) %>%
  addCircleMarkers(data = routes_df,
                   lng = ~dest_lon, lat = ~dest_lat,
                   color = "darkred", fillColor = "red", fillOpacity = 1,
                   radius = 6, weight = 2, label = ~name)