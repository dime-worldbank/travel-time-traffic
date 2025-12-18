# Event

# Load data --------------------------------------------------------------------
route_df   <- readRDS(file.path(analysis_data_dir, "mapbox_routes.Rds"))
gadm1_df   <- readRDS(file.path(analysis_data_dir, "mapbox_gadm1.Rds"))
estates_df <- readRDS(file.path(analysis_data_dir, "mapbox_estates.Rds"))

beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Apply coefs ------------------------------------------------------------------
route_df   <- mk_traffic_indicators(route_df, beta)
gadm1_df   <- mk_traffic_indicators(gadm1_df, beta)
estates_df <- mk_traffic_indicators(estates_df, beta)

# Cleanup ----------------------------------------------------------------------
## Route
route_df <- route_df %>%
  dplyr::mutate(date = datetime %>% date(),
                hour = datetime %>% hour(),
                dow = date %>% lubridate::wday(label = T),
                dow_weekday = dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%
  dplyr::filter(dow_weekday %in% T)

route_e1_df <- route_df %>%
  dplyr::mutate(event1_days = date %in% c(ymd("2023-07-19",
                                              "2023-07-20",
                                              "2023-07-21"))) %>%
  dplyr::filter((date >= (ymd("2023-07-19")-7*4)) & (date <= (ymd("2023-07-21")+7*4)))

## GADM
gadm1_df <- gadm1_df %>%
  dplyr::mutate(date = datetime %>% date(),
                hour = datetime %>% hour(),
                dow = date %>% lubridate::wday(label = T),
                dow_weekday = dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%
  dplyr::filter(dow_weekday %in% T)

gadm1_e1_df <- gadm1_df %>%
  dplyr::mutate(event1_days = date %in% c(ymd("2023-07-19",
                                              "2023-07-20",
                                              "2023-07-21"))) %>%
  dplyr::filter((date >= (ymd("2023-07-19")-7*4)) & (date <= (ymd("2023-07-21")+7*4)))

## Estates
estates_df <- estates_df %>%
  dplyr::mutate(date = datetime %>% date(),
                hour = datetime %>% hour(),
                dow = date %>% lubridate::wday(label = T),
                dow_weekday = dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%
  dplyr::filter(dow_weekday %in% T)

estates_e1_df <- estates_df %>%
  dplyr::mutate(event1_days = date %in% c(ymd("2023-07-19",
                                              "2023-07-20",
                                              "2023-07-21"))) %>%
  dplyr::filter((date >= (ymd("2023-07-19")-7*2)) & (date <= (ymd("2023-07-21")+7*2)))

feols(delay_factor ~ event1_days | hour + uid, data = route_e1_df)
feols(delay_factor ~ event1_days | hour, data = gadm1_e1_df)
feols(delay_factor ~ event1_days | hour + uid, data = estates_e1_df)




gadm1_e1_df$hour

gadm1_df %>%
  dplyr::filter(dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%
  dplyr::filter(date >= ymd("2023-07-17"),
                date <= ymd("2023-07-21")) %>%
  ggplot(aes(x = datetime,
             y = delay_factor)) +
  geom_vline(xintercept = ymd_hms("2023-07-19 00:00:00"), color = "red") +
  geom_line()



July 19–21, 2023
3-day anti-government protests (Azimio) with reported “less traffic” in Nairobi 
and businesses/schools disrupted.
https://www.standardmedia.co.ke/sports/national/article/2001477544/less-traffic-on-city-roads-activities-reduced-in-cbd


Late June 2023 (esp. June 30 onward): sharp fuel-price jump tied to Finance Act/VAT changes
https://www.rfi.fr/en/business-and-tech/20230630-kenya-s-president-to-get-pay-hike-as-economy-suffers?utm_source=chatgpt.com

