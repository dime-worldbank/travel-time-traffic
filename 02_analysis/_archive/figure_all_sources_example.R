# Figure Showing All Data Sources

# ADD OSM BASEMAP
# ADD START/END

nbo_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds")) 
nbo_sf <- nbo_sf[nbo_sf$NAME_1 %in% "Nairobi",]

# Load / prep travel time data -------------------------------------------------
gg_tt_df <- readRDS(file.path(tt_dir, 
                              "google_daily_data_nairobi",
                              "google_tt_2023-09-19.Rds"))

mb_tt_df <- readRDS(file.path(tt_dir, 
                              "google_daily_data_nairobi",
                              "google_tt_2023-09-19.Rds"))

gg_tt_df <- gg_tt_df %>%
  mutate(time = time %>% 
           as.character %>% 
           ymd_hms(tz = "UTC") %>% 
           with_tz(tzone = "Africa/Nairobi") %>% 
           floor_date(unit = "30 minutes"),
         speed_in_traffic_kmh = (distance_m/1000) / (duration_in_traffic_s/60/60),
         source = "Google") %>%
  dplyr::filter(time %in% ymd_hms("2023-09-19 17:00:00", tz = "Africa/Nairobi"))

mb_tt_df <- mb_tt_df %>%
  dplyr::mutate(speed_kmh = (distance_m/1000) / (duration_s/60/60),
                time = time %>%
                  with_tz(tzone = "Africa/Nairobi") %>%
                  floor_date(unit = "30 minutes"),
                source = "Mapbox") %>%
  dplyr::filter(time %in% ymd_hms("2023-09-19 17:00:00", tz = "Africa/Nairobi")) %>%
  dplyr::rename(speed_in_traffic_kmh = speed_kmh) 

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

#### Mapbox
mp_tl_sf <- readRDS(file.path(traffic_mb_raw_dir, "mp_nairobi_2023_09_19.Rds"))

mp_tl_sf <- mp_tl_sf %>%
  dplyr::mutate(datetime_scrape = datetime_scrape %>%
                  round_date(unit = "30 minutes")) %>%
  dplyr::filter(datetime_scrape %in% ymd_hms("2023-09-19 17:00:00", tz = "Africa/Nairobi")) 

mp_tl_sf <- mp_tl_sf %>%
  st_intersection(nbo_sf)

# Individual figures -----------------------------------------------------------
tt_df <- bind_rows(gg_tt_df,
                   mb_tt_df) %>%
  arrange(speed_in_traffic_kmh)

p_tt <- ggplot() +
  geom_sf(data = tt_df,
          color = "black",
          linewidth = 1.2) +
  geom_sf(data = tt_df,
          aes(color = speed_in_traffic_kmh),
          linewidth = 1) +
  facet_wrap(~source) +
  labs(color = "Traffic Speed (km/h)") +
  scale_color_distiller(palette = "Spectral") +
  theme_void() +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold", hjust = 0.5))

#### Google Traffic
google_tl_r <- aggregate(google_tl_r, fact=7, fun=max)
google_tl_r <- google_tl_r %>% crop(nbo_sf) %>% mask(nbo_sf)

google_tl_df <- rasterToPoints(google_tl_r, spatial = TRUE) %>% as.data.frame()
names(google_tl_df) <- c("value", "x", "y")

p_gg_tl <- ggplot() +
  geom_sf(data = nbo_sf, color = "black", fill = NA) +
  geom_raster(data = google_tl_df, 
              aes(x = x, y = y, 
                  fill = as.factor(value))) +
  labs(fill = "Traffic\nLevel",
       title = "Google") +
  scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
  coord_sf() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color="white"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))

#### Mapbox Traffic
p_mp_tl <- mp_tl_sf %>%
  mutate(congestion = congestion %>% 
           as.character() %>%
           tools::toTitleCase() %>%
           factor(levels = c("Low", "Moderate", "Heavy", "Severe"))) %>%
  ggplot() +
  geom_sf(data = nbo_sf, color = "black", fill = NA) +
  geom_sf(aes(color = congestion)) +
  scale_color_manual(values = c("green2", "orange", "red", "#660000")) +
  labs(color = "Congestion",
       title = "Mapbox") +
  coord_sf() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color="white"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14))

# Arrange and export -----------------------------------------------------------
p_tl <- ggarrange(p_gg_tl + theme(legend.position = "none"), 
                  p_mp_tl, 
                  nrow = 1,
                  common.legend = T)

ggsave(p_tl,
       filename = file.path(figures_dir, "all_sources_example_tl.png"),
       height = 4*2, width = 10*2)

ggsave(p_tt,
       filename = file.path(figures_dir, "all_sources_example_tt.png"),
       height = 3, width =9)

