# Event

# https://www.sciencedirect.com/science/article/pii/S2950196225000225
# TODO: Do 2 weeks before and after break period (test differences in window)
# Test:
# - Location
# - Day of week

# osm_df %>%
#   dplyr::mutate(date = datetime %>% date(),
#                 hour = datetime %>% hour()) %>%
#   distinct(date, hour) %>%
#   group_by(date) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup()
# Load data --------------------------------------------------------------------
route_df <- readRDS(file.path(analysis_data_dir, "mapbox_routes.Rds"))
osm_df   <- readRDS(file.path(analysis_data_dir, "mapbox_osm_10m.Rds"))
gadm1_df   <- readRDS(file.path(analysis_data_dir, "mapbox_gadm1.Rds"))
estates_df <- readRDS(file.path(analysis_data_dir, "mapbox_estates.Rds"))

estates_sf <- readRDS(file.path(data_dir, "Nairobi Estates", "FinalData", "nairobi_estates.Rds"))

beta <- readRDS(file.path(data_dir, "Calibration Coefficients", "coefs.Rds"))

# Restrict date/time -----------------------------------------------------------
route_df <- route_df %>%
  dplyr::filter(!is.na(speed_kmh),
                !is.na(tl_prop_2))

gadm1_df   <- gadm1_df[(gadm1_df$datetime     >= min(route_df$datetime)) & (gadm1_df$datetime   <= max(route_df$datetime)),]
osm_df     <- osm_df[(osm_df$datetime         >= min(route_df$datetime)) & (osm_df$datetime     <= max(route_df$datetime)),]
estates_df <- estates_df[(estates_df$datetime >= min(route_df$datetime)) & (estates_df$datetime <= max(route_df$datetime)),]

route_df$datetime %>% summary()

# Apply coefs ------------------------------------------------------------------
route_df   <- mk_traffic_indicators(route_df, beta)
gadm1_df   <- mk_traffic_indicators(gadm1_df, beta)
estates_df <- mk_traffic_indicators(estates_df, beta)
osm_df <- mk_traffic_indicators(osm_df, beta)

osm_df %>%
  dplyr::mutate(hour = datetime %>% hour()) %>%
  group_by(hour) %>%
  dplyr::summarise(delay_factor = mean(delay_factor)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = hour,
                y = delay_factor))

# Clean data -------------------------------------------------------------------
# https://kicd.ac.ke/wp-content/uploads/2021/01/ACADEMIC-TERMS-CALENDAR-FOR-202020212022-2023.pdf
# https://www.kidsleaguekenya.org/2023/01/16/new-kenyan-school-term-dates/

clean_data <- function(data){
  data %>%
    dplyr::mutate(date = datetime %>% date(),
                  hour = datetime %>% hour(),
                  dow = date %>% lubridate::wday(label = T),
                  dow_weekday = dow %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%
    dplyr::filter(dow_weekday %in% T,
                  hour %in% 6:23) %>%
    dplyr::mutate(term_1_school = (date >= ymd("2023-01-23")) & (date <= ymd("2023-04-21")),
                  term_1_break = (date >= ymd("2023-03-23")) & (date <= ymd("2023-03-26")),
                  term_1_holiday = (date >= ymd("2023-04-22")) & (date <= ymd("2023-05-07")),
                  
                  term_2_school = (date >= ymd("2023-05-08")) & (date <= ymd("2023-08-11")),
                  term_2_break = (date >= ymd("2023-06-29")) & (date <= ymd("2023-07-02")),
                  term_2_holiday = (date >= ymd("2023-08-12")) & (date <= ymd("2023-08-27")),
                  
                  term_3_school = (date >= ymd("2023-08-28")) & (date <= ymd("2023-11-03")),
                  term_3_kcpe = (date >= ymd("2023-11-06")) & (date <= ymd("2023-11-09"))) %>%
    dplyr::mutate(
      term_1_school = case_when(
        term_1_break ~ FALSE, # if break is TRUE, then no school (FALSE)
        TRUE ~ term_1_school
      ),
      term_2_school = case_when(
        term_2_break ~ FALSE, # if break is TRUE, then no school (FALSE)
        TRUE ~ term_2_school
      )) %>%
    dplyr::mutate(term_school = as.numeric(term_1_school | term_2_school | term_3_school),
                  term_holiday = as.numeric(term_1_holiday | term_2_holiday),
                  term_break_holiday = as.numeric(term_1_holiday | term_2_holiday | term_1_break | term_2_break),
                  term_event = case_when(
                    term_1_school %in% T ~ "Term 1: School",
                    term_1_break %in% T ~ "Term 1: Break",
                    term_1_holiday %in% T ~ "Term 1: Holiday",
                    
                    term_2_school %in% T ~ "Term 2: School",
                    term_2_break %in% T ~ "Term 2: Break",
                    term_2_holiday %in% T ~ "Term 2: Holiday",
                    
                    term_3_school %in% T ~ "Term 3: School",
                    TRUE ~ "other"
                  )) %>%
    # Restrict before and after term 2 holiday
    dplyr::filter(date >= (ymd("2023-07-19")-14),
                  date <= (ymd("2023-07-21")+14)) %>%
    dplyr::mutate(protest_days = date %in% c(ymd("2023-07-19",
                                                 "2023-07-20",
                                                 "2023-07-21"))) %>%
    
    dplyr::mutate(morning = as.numeric(hour %in% 6:9),
                  afternoon = as.numeric(hour %in% 10:2)) #%>%
  #dplyr::filter(morning %in% 1 | afternoon %in% 1)
  
  # %>%
  #   dplyr::mutate(event1_days = date %in% c(ymd("2023-07-19",
  #                                               "2023-07-20",
  #                                               "2023-07-21"))) %>%
  #   dplyr::mutate(event1_days_4weeks = (date >= (ymd("2023-07-19")-7*4)) & (date <= (ymd("2023-07-21")+7*0)))
}

route_df   <- route_df   %>% clean_data()
gadm1_df   <- gadm1_df   %>% clean_data()
estates_df <- estates_df %>% clean_data()
osm_df     <- osm_df     %>% clean_data()

gadm1_df$term_2_school %>% table()
gadm1_df$term_2_break %>% table()
gadm1_df$term_2_holiday %>% table()

# Data availability ------------------------------------------------------------
osm_df %>%
  dplyr::mutate(date = datetime %>% date(),
                hour = datetime %>% hour()) %>%
  distinct(date, protest_days, hour) %>%
  group_by(date, protest_days) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = date,
             y = n,
             fill = protest_days,
             label = n)) +
  geom_col() +
  geom_text()

# Plot trends ------------------------------------------------------------------
gadm1_df %>%
  dplyr::filter(hour %in% 6:9) %>%
  group_by(date, term_event) %>%
  dplyr::summarise(delay_factor = mean(delay_factor)) %>%
  ungroup() %>%
  
  ggplot() +
  geom_line(aes(x = date,
                y = delay_factor,
                fill = term_event))

# Regressions ------------------------------------------------------------------
osm_df <- osm_df %>%
  dplyr::mutate(fclass = factor(fclass))

feols(log(duration_s) ~ protest_days | hour + uid, data = route_df %>% dplyr::filter(modal_route == T))
feols(delay_factor    ~ protest_days | hour + uid, data = route_df)
feols(delay_factor    ~ protest_days | hour + uid, data = estates_df)

estates_all_df <- map_df(unique(estates_df$uid), function(uid_i) {
  tryCatch({
    
    feols(
      delay_factor ~ protest_days | hour, data = estates_df %>% 
        dplyr::filter(
          #hour %in% c(6:9),
          uid %in% uid_i
        )
    )$coeftable %>%
      clean_names() %>%
      dplyr::mutate(uid = uid_i)
    
  }, error = function(e) {
    NULL
  })
})

estates_e1_sf <- estates_sf %>%
  left_join(estates_all_df, by = "uid")

ggplot() +
  geom_sf(
    data = estates_e1_sf
  ) +
  geom_sf(
    data = estates_e1_sf %>%
      dplyr::filter(pr_t <= 0.1),
    aes(fill = estimate)
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    low = "blue",
    mid = "white",
    high = "red"
  )









feols(log(duration_s) ~ term_school:morning | hour + uid, data = route_df)
feols(delay_factor    ~ term_school:morning | hour + uid, data = route_df)
feols(delay_factor    ~ term_school:morning | hour + uid, data = estates_df)
feols(delay_factor    ~ term_school:morning | hour + uid, weights = ~ osm_road_length_m, data = osm_df)

feols(delay_factor    ~ term_school:morning | hour + uid,  
      weights = ~ osm_road_length_m,
      data = osm_df %>% 
        dplyr::filter(fclass %in% "trunk"))

feols(delay_factor    ~ term_school:morning | hour + uid, 
      weights = ~ osm_road_length_m,
      data = osm_df %>% 
        dplyr::filter(fclass %in% "primary"))

feols(delay_factor    ~ term_school:morning | hour + uid,
      weights = ~ osm_road_length_m,
      data = osm_df %>% 
        dplyr::filter(fclass %in% "secondary"))

feols(delay_factor    ~ term_school:morning | hour + uid, 
      weights = ~ osm_road_length_m,
      data = osm_df %>% 
        dplyr::filter(fclass %in% "tertiary"))

feols(delay_factor    ~ term_school:morning | hour + uid, 
      weights = ~ osm_road_length_m,
      data = osm_df %>% 
        dplyr::filter(fclass %in% "residential"))

feols(delay_factor    ~ term_school:morning | hour + uid, 
      weights = ~ osm_road_length_m,
      data = osm_df %>% 
        dplyr::filter(fclass %in% "unclassified"))


lm_df <- map_df( unique(osm_df$uid), function(uid_i){
  message(uid_i)
  osm_df_i <- osm_df[osm_df$uid %in% uid_i,]
  if(sd(osm_df_i$delay_factor)>0){
    out <- feols(delay_factor    ~ term_school:morning | hour, 
                 data = osm_df[osm_df$uid %in% uid_i,])$coeftable %>%
      clean_names() %>%
      dplyr::mutate(uid = uid_i)
  } else{
    out <- NULL
  }
  return(out)
})

a <- lm_df[lm_df$pr_t <= 0.1,]



osm_df$uid %>% table() %>% table()



osm_df$fclass

route_df %>%
  dplyr::filter(hour %in% 6:9) %>%
  group_by(date, uid, term_event) %>%
  dplyr::summarise(duration_s = mean(duration_s),
                   delay_factor = mean(delay_factor)) %>%
  ungroup() %>%
  
  ggplot() +
  geom_line(aes(x = date,
                y = duration_s,
                color = term_event)) +
  facet_wrap(~uid, scales = "free_y")



hour_all_df <- map_df(0:23, function(hour_i) {
  tryCatch({
    
    feols(
      delay_factor ~ event_date_post | hour, data = estates_df %>% 
        dplyr::filter(
          abs(days_since_event) <= 14,
          hour %in% hour_i
        )
    ) %>%
      confint() %>%
      as.data.frame() %>%
      clean_names() %>%
      dplyr::mutate(hour = hour_i,
                    b = (x2_5_percent + x97_5_percent)/2) 
    
  }, error = function(e) {
    NULL
  })
})

hour_all_df %>%
  ggplot(aes(x = hour,
             y = b,
             ymin = x2_5_percent,
             ymax = x97_5_percent)) +
  geom_point() +
  geom_linerange()


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

