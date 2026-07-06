# Typical and Alternate Routes

for(data_source in c("Google", "Mapbox")){
  
  if(data_source == "Google"){
   tt_sf <- readRDS(file.path(tt_dir,
                                  "google_tt_data_geom.Rds"))
  } else{
    tt_sf <- readRDS(file.path(tt_dir, "mapbox_tt.Rds"))
  }
  
  tt_sf <- tt_sf %>%
    dplyr::mutate(row_id = 1:n())
  
  tt_df <- tt_sf %>%
    st_drop_geometry() %>%
    group_by(road_name, segment_id, distance_m) %>%
    dplyr::summarise(n = n(),
                     row_id = row_id[1]) %>%
    ungroup() %>%
    group_by(segment_id) %>%
    dplyr::mutate(n_prop = n / sum(n) * 100) %>%
    ungroup()
  
  tt_sf <- tt_sf[tt_sf$row_id %in% tt_df$row_id,]
  tt_sf <- tt_sf %>%
    left_join(tt_df, by = c("row_id", "road_name", "segment_id", "distance_m")) %>%
    arrange(n)
  
  p <- ggplot() +
    geom_sf(data = gg_tt_sf,
            aes(color = n_prop),
            linewidth = 0.5) +
    scale_color_distiller(palette = "Spectral") +
    labs(color = "Percent\nof Time\nRoute\nUsed",
         title = data_source) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    facet_wrap(~segment_id,
               ncol = 3)
  
  ggsave(p,
         filename = file.path(figures_dir, 
                              paste0(tolower(data_source),
                                     "_routes_alt_n.png")),
         height = 10, width = 7)
  
}


