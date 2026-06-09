

# Make grid --------------------------------------------------------------------
nairobi <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm41_KEN_3_pk.rds")) %>%
  dplyr::filter(!(NAME_3 %in% c("Kahawa West", "Mugumu-Ini", "Kahawa", "Zimmerman", "Karura", "Githurai",
                                "Ruai",
                                "Clay City", "Kasarani")))

grid_df <- gt_make_grid(nairobi,
                        height = 9000, # 8000
                        width = 9000,
                        zoom = 17, # 17
                        reduce_hw = 75)

grid_df <- grid_df %>%
  dplyr::filter(!(id %in% c(1)))

saveRDS(grid_df, file.path(data_dir, "Google Traffic 2026", "Grid", "grid_param.Rds"))


leaflet() %>%
  addTiles() %>%
  addPolygons(data = nairobi, color = "red") %>%
  addPolygons(data = grid_df, popup = ~as.character(id))

leaflet() %>%
  addTiles() %>%
  addPolygons(data = nairobi, color = "red", popup = ~NAME_3)