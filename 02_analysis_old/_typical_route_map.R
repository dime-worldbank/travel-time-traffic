# Map of typical routes

# Prep OSM ---------------------------------------------------------------------
nbo_sf <- readRDS(file.path(db_dir, "Data", "GADM", "RawData",
                            "gadm41_KEN_1_pk.rds"))
nbo_sf <- nbo_sf[nbo_sf$NAME_1 %in% "Nairobi",]

roads_sf <- readRDS(file.path(db_dir, "Data", "OSM", "FinalData",
                              "gis_osm_roads_free_1_nairobi.Rds"))

roads_1_sf <- roads_sf[str_detect(roads_sf$fclass, "motorway|trunk|primary"),] %>% 
  st_as_sf() %>%
  st_intersection(nbo_sf)

roads_2_sf <- roads_sf[str_detect(roads_sf$fclass, "secondary|tertiary"),] %>% 
  st_as_sf() %>%
  st_intersection(nbo_sf)

# Load data --------------------------------------------------------------------
route_sf <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))

# Map --------------------------------------------------------------------------

ggplot() +
  geom_sf(data = route_sf) 

