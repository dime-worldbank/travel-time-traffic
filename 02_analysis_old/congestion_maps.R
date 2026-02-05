# Congestion Map

adm_sf <- readRDS(file.path(gadm_dir, "RawData", paste0("gadm41_KEN_",3,"_pk.rds")))

cong_df <- readRDS(file.path(analysis_data_dir, "gadm3_wide.Rds"))

cong_sum_df <- cong_df %>%
  group_by(uid) %>%
  dplyr::summarise(gg_tl_prop_234 = mean(gg_tl_prop_234, na.rm = T),
                   gg_tl_prop_34 = mean(gg_tl_prop_34, na.rm = T),
                   gg_tl_prop_4 = mean(gg_tl_prop_4, na.rm = T)) %>%
  ungroup() %>%
  dplyr::rename(GID_3 = uid)

adm_sf <- adm_sf %>%
  left_join(cong_sum_df, by = "GID_3")


ggplot() +
  geom_sf(
    data = adm_sf,
    aes(fill = gg_tl_prop_234),
    color = "black"
  ) +
  scale_fill_distiller(
    palette = "Spectral",
    direction = -1,     # flips so low=blue, high=red (Spectral style)
    na.value = "gray90",
    name = "Prop TL 2–4"
  ) +
  theme_void()

