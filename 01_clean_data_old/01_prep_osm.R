# Prep OSM

source("https://raw.githubusercontent.com/ramarty/fast-functions/refs/heads/master/R/functions_in_chunks.R")

# Load data --------------------------------------------------------------------
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "gis_osm_roads_free_1_nairobi.Rds")) %>%
  st_as_sf()

#nbo_sf <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm41_KEN_1_pk.rds")) %>%
#  st_as_sf() %>%
#  dplyr::select(NAME_1)

osm_sf <- osm_sf %>%
  dplyr::filter(fclass %in% c("motorway", "motorway_link",
                              "trunk", "trunk_link",
                              "primary", "primary_link",
                              "secondary", "secondary_link",
                              "tertiary", "tertiary_link",
                              "residential",
                              "unclassified")) %>%
  dplyr::mutate(fclass = fclass %>%
                  str_replace_all("_link", "") %>%
                  as.character()) %>%
  dplyr::mutate(fclass = fclass %>%
                  factor(levels = c("motorway",
                                    "trunk",
                                    "primary",
                                    "secondary",
                                    "tertiary",
                                    "residential",
                                    "unclassified"),
                         ordered = T))

osm_sf <- osm_sf %>% st_simplify()

osm_sf <- osm_sf %>%
  dplyr::filter(!is.na(name)) %>%
  dplyr::mutate(name = name %>% tolower()) %>%
  group_by(name) %>%
  dplyr::summarise(geometry = st_union(geometry),
                   fclass = min(fclass)) %>% # min gives highest category
  ungroup()

osm_sf <- osm_sf %>%
  dplyr::mutate(uid = 1:n())

#osm_sf <- osm_sf %>% st_intersection(nbo_sf)

osm_10m_sf <- osm_sf %>% st_buffer_chunks(dist = 10, chunk_size = 50)

saveRDS(osm_10m_sf, file.path(data_dir, "OSM", "FinalData", "osm_nbo_10m.Rds"))
saveRDS(osm_sf, file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
