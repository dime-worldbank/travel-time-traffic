# Traffic Levels: Mapbox

mb_sf <- readRDS(file.path(data_dir, "Traffic", "mapbox_daily_data_nairobi", "mp_nairobi_2023_06_01.Rds"))
nbo_sf <- readRDS(file.path(data_dir, "GADM", "RawData", "gadm41_KEN_1_pk.rds"))

mb_sf <- mb_sf %>%
  mutate(congestion = congestion %>% 
           as.character() %>%
           tools::toTitleCase() %>%
           factor(levels = c("Low", "Moderate", "Heavy", "Severe")),
         hour = datetime_scrape %>% hour())

mb_hr_sf <- mb_sf %>%
  dplyr::filter(hour %in% seq(from = 0, to = 23, by = 4)) %>%
  dplyr::mutate(hour_str = paste0(hour, ":00"),
                hour_str = hour_str %>%
                  factor(levels = seq(from = 0, to = 23, by = 4) %>%
                           paste0(":00")))

inter_tf <- st_intersects(mb_hr_sf, nbo_sf, sparse = F)
mb_hr_sf <- mb_hr_sf[inter_tf,]

ggplot() +
  geom_sf(data = mb_hr_sf, aes(color = congestion),
          size = 0.1) +
  scale_color_manual(values = c("green2", "orange", "red", "#660000")) +
  labs(color = "Congestion",
       title = "Traffic level data from Mapbox: June 1, 2023") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color="white"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom") +
  facet_wrap(~hour_str, ncol = 2)

ggsave(filename = file.path(figures_dir,
                            "mapbox_traffic_example.png"),
       height = 8, width = 8)
