# Make Isochrone Polygons

if(T){
  file.path(data_dir, "Isochrone Routes", "individual_routes") %>%
    list.files(full.names = T) %>%
    file.remove()
  
  file.path(data_dir, "Isochrone Routes", "individual_routes") %>%
    list.files(full.names = T)
}

# Prep h3 cells ----------------------------------------------------------------
osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds")) %>%
  st_combine()

nbo_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds")) 

nbo_sub_sf <- nbo_sf %>%
  dplyr::filter(uid %in% c(124,
                           127,
                           131,
                           134,
                           136,
                           138,
                           10,
                           20,
                           23,
                           29,
                           18,
                           17,
                           142,
                           138,
                           128,
                           132,
                           133,
                           140,
                           151,
                           137,
                           27,
                           24,
                           26,
                           32,
                           41,
                           19,
                           147,
                           149,
                           141))

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = nbo_sub_sf,
#               popup = ~as.character(uid), color = "red") %>%
#   addPolygons(data = nbo_sf,
#               popup = ~as.character(uid))

h3_ids <- nbo_sub_sf %>%
  st_union() %>%
  polygon_to_cells(res = 8) 

h3_sf <- cell_to_polygon(h3_ids, simple = T) %>% st_as_sf()
h3_sf$uid <- h3_ids[[1]]

inter_tf <- st_intersects(h3_sf, osm_sf, sparse = F) %>% as.vector()

h3_sf <- h3_sf[inter_tf,]

# nrow(h3_sf)
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = h3_sf)

saveRDS(h3_sf, file.path(data_dir, "Isochrone Routes", "h3_polygon.Rds"))

# Make isochrones --------------------------------------------------------------
for(uid_i in unique(h3_sf$uid)){
  
  OUT_DIR <- file.path(data_dir, "Isochrone Routes", "individual_routes", paste0(uid_i, ".Rds"))
  
  if(!file.exists(OUT_DIR)){
    
    message(uid_i)
    
    h3_centroid_sf <- h3_sf[h3_sf$uid %in% uid_i,] %>% st_centroid()
    
    coord_i <- h3_centroid_sf %>%
      st_coordinates() %>%
      as.vector()
    
    iso_sf <- osrmIsochrone(loc = coord_i, breaks = 15, res = 20)
    
    # leaflet() %>%
    #   addTiles() %>%
    #   addPolygons(data = iso_sf)
    
    if(nrow(iso_sf) > 0){
      
      iso_coords_df <- iso_sf %>%
        st_coordinates() %>%
        as.data.frame() %>%
        dplyr::filter(L1 == 1) %>%
        dplyr::rename(longitude = X,
                      latitude = Y) %>%
        dplyr::mutate(order = 1:n())
      
      iso_coords_sf <- iso_coords_df %>%
        st_as_sf(coords = c("longitude", "latitude"),
                 remove = F,
                 crs = 4326)
      
      iso_routes_sf <- map_df(1:nrow(iso_coords_sf), function(dst_i){
        iso_coords_sf_i <- iso_coords_sf[dst_i,]
        
        route_sf <- osrmRoute(h3_centroid_sf, iso_coords_sf_i, overview = "full")
        
        route_sf$uid <- h3_centroid_sf$uid
        route_sf$dst_latitude = iso_coords_sf_i$latitude
        route_sf$dst_longitude = iso_coords_sf_i$longitude
        
        h3_centroid_df <- h3_centroid_sf %>%
          st_coordinates() %>%
          as.data.frame()
        
        route_sf$org_latitude  <- h3_centroid_df$Y
        route_sf$org_longitude <- h3_centroid_df$X
        
        return(route_sf)
      })
      
      
    } else{
      iso_routes_sf <-  NULL
    }
    
    saveRDS(iso_routes_sf, OUT_DIR)
  }
}






# coords <- as.matrix(
#   iso_routes_sf[, c("dst_longitude", "dst_latitude")] %>%
#     st_drop_geometry()
# )
# # make polygon
# poly_sf <- st_sf(
#   geometry = st_sfc(
#     st_polygon(list(coords)),
#     crs = 4326
#   )
# )


