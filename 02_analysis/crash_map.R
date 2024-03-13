# Crash Map

# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))
rtc_sf <- readRDS(file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds"))
nbo_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds"))
rt_typ_google_sf <- readRDS(file.path(tt_dir, "google_typical_route.Rds"))
rt_typ_google_sf <- rt_typ_google_sf[rt_typ_google_sf$segment_id %in% 1:13,]

rtc_sf <- rtc_sf %>% st_intersection(nbo_sf)
rt_typ_google_sf <- rt_typ_google_sf %>% st_intersection(nbo_sf)

rtc_sf <- rtc_sf[rtc_sf$datetime <= max(route_df$datetime),]
rtc_sf <- rtc_sf[rtc_sf$datetime >= min(route_df$datetime),]

ggplot() +
  geom_sf(data = nbo_sf) +
  geom_sf(data = rt_typ_google_sf, 
          aes(color = "O-D Routes")) +
  geom_sf(data = rtc_sf, 
          pch = 21,
          fill = "black",
          color = "red", 
          size = 1) + 
  labs(color = NULL) +
  scale_color_manual(values = "green3") +
  theme_void()

ggsave(filename = file.path(figures_dir, "crash_map.png"),
       height = 3, width = 6)
