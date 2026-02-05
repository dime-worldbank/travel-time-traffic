# Create polyline of Nairobi expressway

q_sf <- opq(bbox = 'Nairobi, Kenya') %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf()

exp_sf <- q_sf$osm_lines %>%
  dplyr::filter(name == "Nairobi Expressway")

exp_sf <- exp_sf %>%
  group_by(name) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup()
 
saveRDS(exp_sf, file.path(nbo_exp_dir, "FinalData", "nairobi_expressway.Rds"))
