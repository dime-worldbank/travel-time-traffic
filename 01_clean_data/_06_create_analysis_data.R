# Clean Crash Data for Analysis

# Load data --------------------------------------------------------------------
for(dataset in c("gadm1_wide",
                 "gadm2_wide",
                 "google_typical_route_10m_wide",
                 "ntsa_crashes_100m_wide",
                 "ntsa_crashes_50m_wide")){
  
  print(dataset)
  
  df <- readRDS(file.path(analysis_data_dir, paste0(dataset, ".Rds")))
  
  names(df)
  df %>%
    filter(!is.na(mb_tl_prop_34)) %>%
    pull(datetime) %>%
    summary()
  
  dfa <- df[df$date %in% ymd("2023-06-01"),]
  
  df %>%
    group_by(date) %>%
    dplyr::summarise(gg_tl_prop_234 = sum(gg_tl_prop_234, na.rm = T)) %>%
    ungroup() %>%
    
    ggplot() +
    geom_col(aes(x = date,
                 y = gg_tl_prop_234))
  
  # Select variables -----------------------------------------------------------
  df <- df %>%
    dplyr::select(-contains("tmtm_")) %>%
    dplyr::select(-contains("wz_")) %>%
    dplyr::select(-c(gg_tl_count_all_max, mb_tl_length_all_max)) %>%
    na.omit() %>%
    mutate(date = datetime %>% date) %>%
    
    # No Google Traffic data beyond this point; all zeros
    dplyr::filter(date < ymd("2023-08-17"))
  
  # Create variables -----------------------------------------------------------
  if("gg_duration_in_traffic_s" %in% names(df)){
    df <- df %>%
      mutate(gg_duration_in_traffic_min = gg_duration_in_traffic_s / 60,
             mb_duration_in_traffic_min = mb_duration_in_traffic_s / 60) 
  }
  
  # Export data ------------------------------------------------------------------
  saveRDS(df, file.path(analysis_data_dir, paste0(dataset, "_clean.Rds")))
  
}
