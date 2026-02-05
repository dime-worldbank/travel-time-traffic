

# Load data --------------------------------------------------------------------
# mroute_df <- readRDS(file.path(analysis_data_dir, "mapbox_routes.Rds"))
# 
# mroute_df <- mroute_df %>%
#   dplyr::select(uid, datetime, tl_prop_1, tl_prop_2, tl_prop_3, tl_prop_4, duration_s)

groute_df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide_clean.Rds"))

groute_df$mb_duration_in_traffic_min
groute_df$gg_duration_in_traffic_min

feols(gg_duration_in_traffic_min ~ mb_duration_in_traffic_min | uid, data = groute_df)

groute_df %>%
  ggplot() +
  geom_point(aes(x = gg_duration_in_traffic_min,
                 y = mb_duration_in_traffic_min))

groute_df %>%
  #dplyr::filter(uid == 18) %>%
  dplyr::select(datetime, uid, gg_duration_in_traffic_min, mb_duration_in_traffic_min) %>%
  pivot_longer(cols = -c(datetime, uid)) %>%
  ggplot() +
  geom_line(aes(x = datetime,
                y = value,
                color = name)) +
  facet_wrap(~uid)
