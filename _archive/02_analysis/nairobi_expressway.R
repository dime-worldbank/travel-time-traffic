# Nairobi Expressway Analysis

# Reg Coef
# 1. 0-30 // 30-60 // 60-90, etc [persistence]

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "gadm2_wide.Rds"))

df <- df %>%
  dplyr::mutate(date = datetime %>% date(),
                days_since_expr = difftime(date, ymd("2022-07-31"), units = "days") %>%
                  as.numeric())

df_sum <- df %>%
  group_by(NAME_2, date, days_since_expr) %>%
  dplyr::summarise(wz_delay_sum_min = sum(wz_delay_sum_min)) %>%
  ungroup()

df_sum %>%
  dplyr::filter(abs(days_since_expr) <= 120) %>%
  ggplot() +
  geom_vline(xintercept = ymd("2022-07-31"), color = "red") +
  geom_col(aes(x = date,
               y = wz_delay_sum_min)) +
  facet_wrap(~NAME_2, scales = "free_y")


df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  dplyr::mutate(date = datetime %>% date(),
                days_since_expr = difftime(date, ymd("2022-07-31"), units = "days") %>%
                  as.numeric())

df_sum <- df %>%
  group_by(uid, date, days_since_expr) %>%
  dplyr::summarise(wz_delay_sum_min = sum(wz_delay_sum_min),
                   gg_tl_prop_234 = mean(gg_tl_prop_234),
                   gg_tl_prop_34  = mean(gg_tl_prop_34),
                   gg_tl_prop_4   = mean(gg_tl_prop_4),
                   gg_speed_in_traffic_kmh = mean(gg_speed_in_traffic_kmh, na.rm = T),
                   gg_duration_in_traffic_s = mean(gg_duration_in_traffic_s, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(dow = date %>% wday())

df_sum %>%
  dplyr::filter(abs(days_since_expr) <= 180,
                dow %in% 4) %>%
  ggplot() +
  geom_vline(xintercept = ymd("2022-07-31"), color = "red") +
  geom_line(aes(x = date,
               y = gg_speed_in_traffic_kmh)) +
  facet_wrap(~uid, scales = "free_y")
