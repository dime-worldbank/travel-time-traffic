# Route Summary

# Load data --------------------------------------------------------------------
mode_sf <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))

# Prep data --------------------------------------------------------------------
route_df <- mode_sf %>%
  mutate(distance_km = round(distance_m/1000, 2) ) %>%
  st_drop_geometry() %>%
  dplyr::select(road_name, distance_km) %>%
  arrange(road_name)

se_df <- map_df(1:nrow(mode_sf), function(i){
  
  coord_df <- mode_sf[i,] %>%
    st_coordinates() %>%
    as.data.frame()
  
  se_df <- bind_cols(
    coord_df %>%
      head(1) %>%
      dplyr::rename(start_lon = X,
                    start_lat = Y) %>%
      dplyr::select(start_lon, start_lat),
    
    coord_df %>%
      tail(1) %>%
      dplyr::rename(end_lon = X,
                    end_lat = Y) %>%
      dplyr::select(end_lon, end_lat)
  )
  
  return(se_df)
})

route_df <- bind_cols(route_df, se_df)

route_df <- route_df %>%
  mutate(tex = paste0(road_name, " & ",
                      distance_km, " & ",
                      start_lat, " & ",
                      start_lon, " & ",
                      end_lat, " & ",
                      end_lon, " \\\\ \n"))

# Table ------------------------------------------------------------------------
sink(file.path(tables_dir, "route_summary.tex"))
cat("\\begin{tabular}{ll | ll | ll} \n")
cat("\\hline \n")
cat("Road & Distance (Km) & Start Latitude & Start Longitude & End Latitude & End Longitude \\\\ \n")
cat("\\hline \n")
route_df %>%
  pull(tex) %>%
  cat()
cat("\\hline \n")
cat("\\end{tabular}")
sink()



