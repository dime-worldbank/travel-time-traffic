# Copy Files from smarTTrans Dropbox to Travel Time & Traffic Dropbox

# Paths ------------------------------------------------------------------------
smarttrans_data_dir <- "~/Dropbox/World Bank/IEs/CrashMap-Nairobi/Data"

# IN Files
nbo_mb_traffic_in <- file.path(smarttrans_data_dir, "Google Mapbox Traffic", "FinalData", "mapbox_daily_data")
nbo_gl_traffic_in <- file.path(smarttrans_data_dir, "Google Mapbox Traffic", "FinalData", "google_individual_rasters")

nbo_mb_tt_in <- file.path(smarttrans_data_dir, "Travel Time", "Mapbox Travel Time", "FinalData", "daily_data")
nbo_gl_tt_in <- file.path(smarttrans_data_dir, "Travel Time", "Google Travel Time", "FinalData", "daily_data")

# OUT Files
nbo_mb_traffic_out <- file.path(traffic_dir, "mapbox_daily_data_nairobi")
nbo_gl_traffic_out <- file.path(traffic_dir, "google_individual_rasters_nairobi")

nbo_mb_tt_out <- file.path(tt_dir, "mapbox_daily_data_nairobi")
nbo_gl_tt_out <- file.path(tt_dir, "google_daily_data_nairobi")

# Copy -------------------------------------------------------------------------
#### Traffic
# for(file_i in list.files(nbo_mb_traffic_in, pattern = "*.Rds")){
#   file.copy(from = file.path(nbo_mb_traffic_in, file_i),
#             to = file.path(nbo_mb_traffic_out, file_i))
# }
# 
# for(file_i in list.files(nbo_gl_traffic_in, pattern = "*.tiff")){
#   file.copy(from = file.path(nbo_gl_traffic_in, file_i),
#             to = file.path(nbo_gl_traffic_out, file_i))
# }

#### Travel Time
for(file_i in list.files(nbo_mb_tt_in, pattern = "*.Rds")){
  file.copy(from = file.path(nbo_mb_tt_in, file_i),
            to = file.path(nbo_mb_tt_out, file_i))
}

for(file_i in list.files(nbo_gl_tt_in, pattern = "*.Rds")){
  file.copy(from = file.path(nbo_gl_tt_in, file_i),
            to = file.path(nbo_gl_tt_out, file_i))
}

# Crashes: Sit Report ----------------------------------------------------------------------
rtc_df <- readRDS(file.path(sm_db_dir, "Data", "Police Data", "Crash Report Data", 
                            "All Reports", "sr_crashes.Rds"))

rtc_df <- rtc_df %>%
  dplyr::filter(latlon_type %in% "point",
                !is.na(latitude),
                !is.na(crash_time),
                !is.na(crash_date),
                rep_duplicate %in% 0) %>%
  dplyr::select(-contains("healthf")) %>%
  dplyr::select(-contains("victim_")) %>%
  dplyr::select(-contains("driver_")) %>%
  dplyr::select(-contains("ob_number_")) %>%
  dplyr::select(-c(crash_year, crash_month)) %>%
  dplyr::rename(crash_id = report_id) %>%
  dplyr::filter(crash_date >= ymd("2021-01-01")) 

rtc_sf <- st_as_sf(rtc_df, coords = c("longitude", "latitude"), crs = 4326)

saveRDS(rtc_sf, file.path(data_dir, "Police Crashes", "RawData", "crashes_sit_reports.Rds"))

# Crashes: Twitter -------------------------------------------------------------
twitter_df <- readRDS(file.path(sm_db_dir, "Data", "Twitter", "Microdata Catalogue - Crashes", 
                            "ma3route_crashes_algorithmcode.Rds"))

twitter_sf <- twitter_df %>%
  dplyr::filter(crash_date >= ymd("2022-07-28"),
                crash_date <= ymd("2023-08-17")) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::select(crash_id, crash_datetime)

ken_sf <- gadm("KEN", level=1, path = tempdir()) %>% st_as_sf()
nbo_sf <- ken_sf[ken_sf$NAME_1 %in% "Nairobi",]

inter_tf <- st_intersects(twitter_sf, nbo_sf, sparse = F) %>% as.vector()
twitter_sf <- twitter_sf[inter_tf,]

saveRDS(twitter_sf, file.path(data_dir, "Twitter Crashes", "RawData", "crashes_twitter.Rds"))

# Crashes: Fatal ----------------------------------------------------------------------
#### Load
geo_df <- readRDS(file.path(sm_db_dir, "Data", 
                            "NTSA Fatality Data",
                            "FinalData", "fatal_report_supervisor_geo.Rds"))

crashes_df <- readRDS(file.path(sm_db_dir, "Data", 
                                "NTSA Fatality Data",
                                "FinalData", "fatal_report.Rds"))

#### Merge
geo_df <- geo_df %>%
  dplyr::filter(can_geolocate %in% "Yes") %>%
  distinct(base, county, road, place, .keep_all = T) %>%
  dplyr::rename(base_sub_base = base) 

crashes_df <- crashes_df %>%
  left_join(geo_df, by = c("base_sub_base", "county", "road", "place"))

#### Cleanup
crashes_df <- crashes_df %>%
  dplyr::filter(county %in% "nairobi",
                loc_type %in% "Point",
                !is.na(latitude),
                !is.na(longitude),
                !is.na(time_minute),
                !is.na(time_hour),
                !is.na(date)) %>% 
  dplyr::mutate(date = date %>% as.character(),
                time_minute = time_minute %>% as.character(),
                time_hour = time_hour %>% as.character()) %>%
  dplyr::mutate(datetime = paste0(date, " ", time_hour, "-", time_minute) %>% ymd_hm(tz = "Africa/Nairobi"))

crashes_df <- crashes_df %>%
  dplyr::select(-c(brief_accident_details, comments, loc, county, day,
                   base_sub_base, place, gender, age, cause_code,
                   time_minute, time_hour, time_24_hours, can_geolocate,
                   date,
                   supervisor,
                   date_entered,
                   loc_type, can_geolocate, point_roadid_1, point_roadid_2)) %>%
  dplyr::select(-contains("area_")) %>%
  dplyr::rename(crash_id = uid)

crashes_sf <- st_as_sf(crashes_df, coords = c("longitude", "latitude"), crs = 4326)

saveRDS(crashes_sf, file.path(data_dir, "Police Crashes", "RawData", "crashes_fatal_ntsa.Rds"))








