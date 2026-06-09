# Check Routes

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(googlePolylines)

# Routes to check --------------------------------------------------------------
tt_dir    <- file.path(data_dir, "Travel Time Routes 2026", "Travel Time Data")
first_file <- sort(list.files(tt_dir, pattern = "\\.Rds$", full.names = TRUE))[1]
tt_results <- readRDS(first_file) %>% select(uid, encoded_polyline)

routes_check <- readRDS(file.path(data_dir, "Travel Time Routes 2026", "od_pairs.Rds")) %>%
  left_join(tt_results, by = "uid")

# Output directory --------------------------------------------------------------
out_dir <- file.path(data_dir, "Travel Time Routes 2026", "Route Checks")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Function: make and save a leaflet map for one OD pair ------------------------
make_route_map <- function(row) {
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron)

  if (!is.na(row$encoded_polyline)) {
    decoded  <- googlePolylines::decode(row$encoded_polyline)[[1]]
    route_sf <- st_sfc(st_linestring(as.matrix(decoded[, c("lon", "lat")])), crs = 4326)
    m <- m %>% addPolylines(data = route_sf, color = "#2166ac", weight = 4, opacity = 0.9)
  }

  m <- m %>%
    addCircleMarkers(
      lng = row$origin_lon, lat = row$origin_lat,
      color = "darkgreen", fillColor = "green", fillOpacity = 1,
      radius = 8, stroke = TRUE, weight = 2,
      label = "Origin"
    ) %>%
    addCircleMarkers(
      lng = row$dest_lon, lat = row$dest_lat,
      color = "darkred", fillColor = "red", fillOpacity = 1,
      radius = 8, stroke = TRUE, weight = 2,
      label = "Destination"
    ) %>%
    addControl(
      html = paste0(
        "<div style='background:white;padding:6px;border-radius:4px;font-size:13px;'>",
        "<b>", row$name, "</b><br>",
        "fclass: ", row$fclass, "<br>",
        "toward_cbd: ", row$toward_cbd,
        "</div>"
      ),
      position = "topright"
    )

  safe_name <- gsub("[^a-zA-Z0-9_-]", "_", paste(row$name, row$fclass, sep = "_"))
  tmp_html  <- tempfile(fileext = ".html")
  saveWidget(m, tmp_html, selfcontained = TRUE)
  webshot(tmp_html, file = file.path(out_dir, paste0(safe_name, ".png")),
          vwidth = 900, vheight = 650)
  unlink(tmp_html)
}

# Apply to all rows -------------------------------------------------------------
walk(seq_len(nrow(routes_check)), function(i) {
  message("Saving map ", i, " / ", nrow(routes_check), ": ", routes_check$name[i])
  make_route_map(as.list(routes_check[i, ]))
})
