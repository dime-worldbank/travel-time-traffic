# Figure Showing All Data Sources

nbo_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds")) 
nbo_sf <- nbo_sf[nbo_sf$NAME_1 %in% "Nairobi",]

# Load / prep travel time data -------------------------------------------------
gg_tt_df <- readRDS(file.path(tt_dir, 
                              "google_daily_data_nairobi",
                              "google_tt_2023-09-19.Rds"))

gg_tt_df <- gg_tt_df %>%
  mutate(time = time %>% 
           as.character %>% 
           ymd_hms(tz = "UTC") %>% 
           with_tz(tzone = "Africa/Nairobi") %>% 
           floor_date(unit = "30 minutes"),
         speed_in_traffic_kmh = (distance_m/1000) / (duration_in_traffic_s/60/60)) %>%
  dplyr::filter(time %in% ymd_hms("2023-09-19 17:00:00", tz = "Africa/Nairobi"))

# Load / prep traffic level data -----------------------------------------------
#### Google
tiff_vec <- file.path(traffic_gg_raw_dir) %>%
  list.files(pattern = "*.tiff") 

tiff_datetime <- tiff_vec %>%
  str_replace_all("gt_nairobi_utc", "") %>%
  str_replace_all(".tiff", "") %>%
  as.numeric() %>%
  as_datetime(tz = "UTC") %>%
  round_date(unit = "30 minutes") %>%
  with_tz(tzone = "Africa/Nairobi")

google_tl_r <- raster(file.path(traffic_gg_raw_dir,
                                tiff_vec[tiff_datetime %in% ymd_hms("2022-09-08 17:00:00", tz = "Africa/Nairobi")]))

# Travel speed figure ----------------------------------------------------------
gg_tt_df <- gg_tt_df %>%
  arrange(speed_in_traffic_kmh)

p_tt <- ggplot() +
  geom_sf(data = nbo_sf, fill = "gray10") +
  geom_sf(data = gg_tt_df,
          color = "black",
          linewidth = 0.8) +
  geom_sf(data = gg_tt_df,
          aes(color = speed_in_traffic_kmh),
          linewidth = 0.6) +
  labs(color = "Traffic\nSpeed\n(km/h)",
       title = "A. Traffic speeds between select O-D pairs") +
  scale_color_distiller(palette = "Spectral") +
  theme_void() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", hjust = 0.5),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# Google Traffic figure --------------------------------------------------------
google_tl_r <- aggregate(google_tl_r, fact=9, fun=max)
google_tl_r <- google_tl_r %>% crop(nbo_sf) %>% mask(nbo_sf)

google_tl_df <- rasterToPoints(google_tl_r, spatial = TRUE) %>% as.data.frame()
names(google_tl_df) <- c("value", "x", "y")

p_tl <- ggplot() +
  geom_sf(data = nbo_sf, color = "black", fill = "gray10") +
  geom_raster(data = google_tl_df, 
              aes(x = x, y = y, 
                  fill = as.factor(value))) +
  labs(fill = "Traffic\nLevel",
       title = "B. Traffic levels") +
  scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
  coord_sf() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color="white"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# Arrange and export -----------------------------------------------------------
p <- ggarrange(p_tt,
               p_tl, 
               nrow = 1,
               common.legend = F)

ggsave(p,
       filename = file.path(figures_dir, "map_tt_tl.png"),
       height = 4, width = 10)


