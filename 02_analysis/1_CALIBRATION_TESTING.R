library(tidyverse)
library(fixest)
library(janitor)
library(lubridate)
library(broom)

# ==================================================================================
# Dataset 1: route_df (by-class, 57 routes)
# ==================================================================================
route_df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))

route_df <- route_df %>%
  dplyr::mutate(speed_kmh = speed_in_traffic_kmh,
                tt_hour_per_km = (duration_in_traffic_s/60/60) / (distance_m/1000),
                prop_ge_2 = tl_prop_2 + tl_prop_3 + tl_prop_4,
                prop_ge_3 = tl_prop_3 + tl_prop_4,
                prop_ge_4 = tl_prop_4) %>%
  dplyr::mutate(speed_kmh_ln = log(speed_kmh),
                tt_hour_per_km_ln = log(tt_hour_per_km)) %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% lubridate::wday(),
                date = datetime %>% date())

route_df <- route_df %>%
  filter(hour >= 6, hour <= 21)

route_df <- route_df %>%
  group_by(uid) %>%
  dplyr::mutate(speed_kmh_uid_max = quantile(speed_kmh, prob = 0.99, na.rm = T) %>% as.numeric(),
                tl_prop_3_max = quantile(tl_prop_3, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_4_max = quantile(tl_prop_4, prob = 0.95, na.rm = T) %>% as.numeric(),
                tl_prop_4_sd = sd(tl_prop_4, na.rm = T) %>% as.numeric()) %>%
  ungroup()

route_variation_df <- route_df %>%
  group_by(uid) %>%
  summarise(
    fclass = first(fclass),
    within_sd_prop3 = sd(tl_prop_3, na.rm = TRUE),
    within_sd_prop4 = sd(tl_prop_4, na.rm = TRUE),
    share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
    share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
    .groups = "drop"
  )

# REGRESSIONS ==================================================================
route_variation_df <- route_df %>%
  group_by(uid) %>%
  summarise(
    share_prop3_gt0 = mean(tl_prop_3 > 0, na.rm = TRUE),
    share_prop4_gt0 = mean(tl_prop_4 > 0, na.rm = TRUE),
    .groups = "drop"
  )

df_all  <- route_df
df_02   <- route_df %>% filter(uid %in% (route_variation_df %>% filter(share_prop3_gt0 >= 0.02, share_prop4_gt0 >= 0.02) %>% pull(uid)))

# 1. Pooled, no threshold
mod1 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, vcov = ~uid, data = df_all)


# 2. Pooled + speed interaction, no threshold
mod2 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 + tl_prop_2:speed_kmh_uid_max + tl_prop_3:speed_kmh_uid_max + tl_prop_4:speed_kmh_uid_max | uid, vcov = ~uid, data = df_all)

# 3. Pooled, threshold > 0.02
mod3 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, vcov = ~uid, data = df_02)

# 4. Pooled + speed interaction, threshold > 0.02
mod4 <- feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 + tl_prop_2:speed_kmh_uid_max + tl_prop_3:speed_kmh_uid_max + tl_prop_4:speed_kmh_uid_max | uid, vcov = ~uid, data = df_02)


feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, vcov = ~uid, data = df_all)
feols(tt_hour_per_km_ln ~ prop_ge_2 + prop_ge_3 + prop_ge_4 +
        + prop_ge_2:speed_kmh_uid_max + prop_ge_3:speed_kmh_uid_max + prop_ge_4:speed_kmh_uid_max| uid, vcov = ~uid, data = df_all)


feols(tt_hour_per_km_ln ~ tl_prop_2 + tl_prop_3 + tl_prop_4 | uid, vcov = ~uid, data = df_all %>%
        dplyr::filter(speed_kmh_uid_max >= 50))
