library(tidyverse)
library(sf)
library(googlePolylines)

# 1. Load all travel time files ------------------------------------------------
tt_dir <- file.path(data_dir, "Travel Time Routes 2026", "Travel Time Data")

all_tt <- list.files(tt_dir, pattern = "\\.Rds$", full.names = TRUE) %>%
  map(readRDS) %>%
  bind_rows()

# 2. For each uid, restrict to the modal distance ------------------------------
typical_routes <- all_tt %>%
  filter(!is.na(distance_m)) %>%
  group_by(uid) %>%
  mutate(modal_distance_m = as.numeric(names(which.max(table(distance_m))))) %>%
  filter(distance_m == modal_distance_m) %>%
  ungroup()

# 3. One row per uid, with decoded polyline geometry --------------------------
typical_routes_sf <- typical_routes %>%
  group_by(uid) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!is.na(encoded_polyline)) %>%
  rowwise() %>%
  mutate(geometry = st_sfc(
    st_linestring(as.matrix(googlePolylines::decode(encoded_polyline)[[1]][, c("lon", "lat")])),
    crs = 4326
  )) %>%
  ungroup() %>%
  st_as_sf()

#4. Export ---------------------------------------------------------------------
typical_routes_sf <- typical_routes_sf %>%
  dplyr::select(name, fclass, uid)

saveRDS(typical_routes_sf, file.path(data_dir, "Travel Time Routes 2026", "typical_routes.Rds"))



