library(tidyverse)
library(sf)
library(googlePolylines)

# 1. Load all travel time files ------------------------------------------------
tt_calib_dir <- file.path(data_dir, "Travel Time Routes 2026", "Travel Time Data")

all_tt <- list.files(tt_calib_dir, pattern = "\\.Rds$", full.names = TRUE) %>%
  map(readRDS) %>%
  bind_rows()

# Filter -----------------------------------------------------------------------
all_tt <- all_tt %>%
  dplyr::mutate(query_date_eat = query_datetime_eat %>% date(),
                query_hour_eat = query_datetime_eat %>% hour())

all_tt <- all_tt %>%
  dplyr::filter( ((query_date_eat >= ymd("2026-06-11")) & (query_date_eat <= ymd("2026-06-17"))) |
                   ((query_date_eat >= ymd("2026-07-08")) & (query_date_eat <= ymd("2026-07-14"))))

# CHECK
if(F){
  all_tt %>%
    dplyr::filter( ((query_date_eat >= ymd("2026-06-11")) & (query_date_eat <= ymd("2026-06-17"))) |
                     ((query_date_eat >= ymd("2026-07-08")) & (query_date_eat <= ymd("2026-07-14")))) %>%
    distinct(query_date_eat, query_hour_eat) %>%
    group_by(query_date_eat) %>%
    dplyr::summarise(n = n()) %>%
    ungroup()
}

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



