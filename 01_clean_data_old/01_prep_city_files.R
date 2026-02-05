# Prep City Shapefiles

for(i in 1:3){
  gadm_sf <- gadm(country = "Kenya", 
       level=i, 
       path = tempdir(), 
       version="4.1", 
       resolution=1) %>%
    st_as_sf()
  
  gadm_sf <- gadm_sf[gadm_sf$NAME_1 %in% "Nairobi",]
  
  saveRDS(gadm_sf, file.path(gadm_dir, "RawData", paste0("gadm41_KEN_",i,"_pk.rds")))
}

nbo_estates_sf <- read_sf(file.path(data_dir, "Nairobi Estates", "RawData", "nairobi_estates.geojson"))
nbo_estates_sf <- nbo_estates_sf %>%
  dplyr::rename(uid = OBJECTID)
saveRDS(nbo_estates_sf,
        file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))
