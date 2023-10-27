# Check Times

g_df <- readRDS(file.path(tt_dir, "google_tt_data.Rds"))
m_df <- readRDS(file.path(tt_dir, "mapbox_tt_data.Rds"))

g_hr_df <- g_df %>%
  group_by(datetime) %>%
  mutate(n_d = n()) %>%
  ungroup() %>%
  filter(n_d == 26) %>%
  
  mutate(hour = datetime %>% hour,
         dow = datetime %>% wday()) %>%
  group_by(hour, dow) %>%
  dplyr::summarise(speed_kmh = mean(speed_in_traffic_kmh)) %>%
  ungroup() %>%
  mutate(type = "Google")

m_hr_df <- m_df %>%
  group_by(datetime) %>%
  mutate(n_d = n()) %>%
  ungroup() %>%
  filter(n_d == 26) %>%
  
  mutate(hour = datetime %>% hour,
         dow = datetime %>% wday()) %>%
  group_by(hour, dow) %>%
  dplyr::summarise(speed_kmh = mean(speed_kmh)) %>%
  ungroup() %>%
  mutate(type = "Mapbox")

hr_df <- bind_rows(
  g_hr_df,
  m_hr_df
)

hr_df %>%
  ggplot(aes(x = hour, y = speed_kmh, color = type)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~dow)
