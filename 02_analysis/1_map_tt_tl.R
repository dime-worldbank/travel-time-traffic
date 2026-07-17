# Figure Showing All Data Sources

nbo_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm41_KEN_1_pk.rds")) 
nbo_sf <- nbo_sf[nbo_sf$NAME_1 %in% "Nairobi",]

osm_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_nbo_line.Rds"))
osm_main_sf <- osm_sf %>%
  dplyr::filter(fclass %in% c("trunk_fast", "trunk", "primary", "secondary"))

osm_other_sf <- osm_sf %>%
  dplyr::filter(!(fclass %in% c("trunk", "primary", "secondary")))

osm_main_sf <- st_intersection(osm_main_sf, nbo_sf)
osm_other_sf <- st_intersection(osm_other_sf, nbo_sf)

# Load / prep travel time data -------------------------------------------------
gg_tt_df <- readRDS(file.path(data_dir,
                              "Travel Time", 
                              "google_daily_data_nairobi",
                              "google_tt_2023-09-19.Rds"))

gg_tt_df <- gg_tt_df[gg_tt_df$locations_segment_id %in% 1:26,]

gg_tt_df <- gg_tt_df %>%
  mutate(time = time %>% 
           as.character %>% 
           ymd_hms(tz = "UTC") %>% 
           with_tz(tzone = "Africa/Nairobi") %>% 
           floor_date(unit = "30 minutes"),
         speed_in_traffic_kmh = (distance_m/1000) / (duration_in_traffic_s/60/60)) %>%
  dplyr::filter(time %in% ymd_hms("2023-09-19 17:00:00", tz = "Africa/Nairobi"))

# Load / prep travel time data: calibration ------------------------------------
gg_tt_calib_df <- readRDS(file.path(data_dir, 
                                    "Travel Time Routes 2026",
                                    "Travel Time Data",
                                    "tt_2026-06-16_14-00-10.Rds"))

gg_tt_calib_df <- gg_tt_calib_df %>%
  mutate(time = query_datetime_eat %>% 
           as.character %>% 
           ymd_hms(tz = "UTC") %>% 
           with_tz(tzone = "Africa/Nairobi") %>% 
           floor_date(unit = "30 minutes"),
         speed_in_traffic_kmh = (distance_m/1000) / (duration_in_traffic_s/60/60)) #%>%
#dplyr::filter(time %in% ymd_hms("2023-09-19 17:00:00", tz = "Africa/Nairobi"))

gg_tt_calib_df <- gg_tt_calib_df %>%
  filter(!is.na(encoded_polyline)) %>%
  mutate(
    geometry = map(encoded_polyline, ~ {
      coords <- googlePolylines::decode(.x)[[1]]
      
      coords <- coords %>%
        dplyr::select(lon, lat)
      
      st_linestring(as.matrix(coords))
    })
  ) %>%
  st_as_sf(sf_column_name = "geometry", crs = 4326)

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

se_sf <- map_df(1:nrow(gg_tt_df), function(i){
  
  gg_tt_df_i <- gg_tt_df[i,]
  
  start_df <- gg_tt_df_i %>%
    st_coordinates() %>%
    as.data.frame() %>%
    head(1) %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    mutate(type = "Start",
           route_id = i)
  
  end_df <- gg_tt_df_i %>%
    st_coordinates() %>%
    as.data.frame() %>%
    tail(1) %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    mutate(type = "End",
           route_id = i)
  
  out_df <- bind_rows(start_df,
                      end_df)
  
  # leaflet() %>%
  #   addTiles() %>%
  #   addCircles(data = out_df) %>%
  #   addPolylines(data = gg_tt_df_i, color = "red")
  
  return(out_df)
})

p_tt <- ggplot() +
  geom_sf(data = nbo_sf, color = "black", fill = "gray95") +
  geom_sf(data = osm_main_sf, color = "gray70", size = 0.5) +
  geom_sf(data = osm_other_sf, color = "gray70", size = 0.1) +
  geom_sf(data = gg_tt_df,
          color = "black",
          linewidth = 1.2) +
  geom_sf(data = gg_tt_df,
          aes(color = speed_in_traffic_kmh),
          linewidth = 1) +
  geom_sf(data = se_sf,
          aes(fill = "Origin/Destination\nLocation"),
          color = "gray90",
          pch = 21) +
  labs(color = "Traffic\nSpeed\n(km/h)",
       fill = NULL,
       title = "A. Traffic speeds between 26 O-D pairs",
       subtitle = "Data queried from July 2022 to August 2023") +
  #scale_color_distiller(palette = "Spectral") +
  scale_color_viridis_c(option = "plasma", begin = 0, end = 1) +
  scale_fill_manual(values = "dodgerblue") +
  theme_void() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", hjust = 0.5),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12))

p_tt

# Travel speed figure: Calibration ---------------------------------------------
gg_tt_calib_df <- gg_tt_calib_df %>%
  arrange(speed_in_traffic_kmh)

se_calib_sf <- map_df(1:nrow(gg_tt_calib_df), function(i){
  
  gg_tt_df_i <- gg_tt_calib_df[i,]
  
  start_df <- gg_tt_df_i %>%
    st_coordinates() %>%
    as.data.frame() %>%
    head(1) %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    mutate(type = "Start",
           route_id = i)
  
  end_df <- gg_tt_df_i %>%
    st_coordinates() %>%
    as.data.frame() %>%
    tail(1) %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    mutate(type = "End",
           route_id = i)
  
  out_df <- bind_rows(start_df,
                      end_df)
  
  # leaflet() %>%
  #   addTiles() %>%
  #   addCircles(data = out_df) %>%
  #   addPolylines(data = gg_tt_df_i, color = "red")
  
  return(out_df)
})

p_tt_calib <- ggplot() +
  geom_sf(data = nbo_sf, color = "black", fill = "gray95") +
  geom_sf(data = osm_main_sf, color = "gray70", size = 0.5) +
  geom_sf(data = osm_other_sf, color = "gray70", size = 0.1) +
  geom_sf(data = gg_tt_calib_df,
          color = "black",
          linewidth = 1.2) +
  geom_sf(data = gg_tt_calib_df,
          aes(color = speed_in_traffic_kmh),
          linewidth = 1) +
  geom_sf(data = se_calib_sf,
          aes(fill = "Origin/Destination\nLocation"),
          color = "gray90",
          pch = 21) +
  labs(color = "Traffic\nSpeed\n(km/h)",
       fill = NULL,
       title = "B. Traffic speeds between 60 O-D pairs",
       subtitle = "Data queried from\nJune 11 - June 17 and July 8 - July 14, 2026") +
  #scale_color_distiller(palette = "Spectral") +
  scale_color_viridis_c(option = "plasma", begin = 0, end = 1) +
  scale_fill_manual(values = "dodgerblue") +
  theme_void() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", hjust = 0.5),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12))

p_tt_calib

# Google Traffic figure --------------------------------------------------------
google_tl_r <- aggregate(google_tl_r, fact=12, fun=max)
google_tl_r <- google_tl_r %>% crop(nbo_sf) %>% mask(nbo_sf)

google_tl_df <- rasterToPoints(google_tl_r, spatial = TRUE) %>% as.data.frame()
names(google_tl_df) <- c("value", "x", "y")

p_tl <- ggplot() +
  geom_sf(data = nbo_sf, color = "black", fill = "gray10") +
  geom_raster(data = google_tl_df, 
              aes(x = x, y = y, 
                  fill = as.factor(value))) +
  labs(fill = "Traffic\nLevel",
       title = "C. Traffic levels") +
  scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
  coord_sf() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color="white"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.8,
    line_width = 0.8
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm")
  )




# Arrange and export -----------------------------------------------------------
p1 <- ggarrange(p_tt,
                p_tt_calib, 
                nrow = 1,
                common.legend = T,
                legend = "bottom")
p <- ggarrange(p1, p_tl, nrow = 2)

ggsave(p,
       filename = file.path(figures_dir, "map_tt_tl.png"),
       height = 8, width = 10)

ggsave(p,
       filename = file.path(figures_dir, "figure_1.png"),
       height = 8, width = 10)

# All routes -------------------------------------------------------------------
#### Roads
if(!file.exists(file.path(data_dir, "OSM", "FinalData", "osm_key_roads.Rds"))){
  roads_sf <- opq(st_bbox(nbo_sf), timeout = 999) %>%
    add_osm_feature(key = "highway", value = c("motorway",
                                               "trunk",
                                               "primary",
                                               "secondary",
                                               "tertiary",
                                               "unclassified")) %>%
    osmdata_sf()
  roads_sf <- roads_sf$osm_lines
  saveRDS(roads_sf, file.path(data_dir, "OSM", "FinalData", "osm_key_roads.Rds"))
} else{
  roads_sf <- readRDS(file.path(data_dir, "OSM", "FinalData", "osm_key_roads.Rds"))
}

roads_sf <- roads_sf %>%
  st_intersection(nbo_sf)

#### Data prep
gg_tt_df$route_id <- 1:nrow(gg_tt_df)

se_sf <- se_sf %>%
  dplyr::mutate(type = type %>%
                  fct_rev()) %>%
  dplyr::filter(type == "Start")
se_sf$type <- "Origin Location"

#### Figure
p <- ggplot() +
  geom_sf(data = nbo_sf,
          color = NA,
          fill = "gray80") +
  geom_sf(data = roads_sf, 
          linewidth = 0.1) +
  geom_sf(data = se_sf,
          aes(color = type)) +
  geom_sf(data = gg_tt_df,
          color = "red") +
  facet_wrap(~route_id,
             ncol = 4) +
  scale_color_manual(values = c("green")) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(color = NULL)

ggsave(p,
       filename = file.path(figures_dir, "map_tt_facet.png"),
       height = 11, width = 8)

