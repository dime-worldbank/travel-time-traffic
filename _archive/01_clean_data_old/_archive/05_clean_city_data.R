# Clean City Data

unit <- "gadm1"

for(unit in c("gadm1", "gadm2", "gadm3")){
  
  if(unit == "gadm1") id_var <- "GID_1"
  if(unit == "gadm2") id_var <- "GID_2"
  if(unit == "gadm3") id_var <- "GID_3"
  
  # Load data --------------------------------------------------------------------
  gg_df <- file.path(city_traffic_dir, paste0(unit, "_individual_files_google")) %>%
    list.files(pattern = ".Rds",
               full.names = T) %>%
    map_df(readRDS)
  
  mb_df <- file.path(city_traffic_dir, paste0(unit, "_individual_files_mapbox")) %>%
    list.files(pattern = ".Rds",
               full.names = T) %>%
    map_df(readRDS)
  
  # Clean google -----------------------------------------------------------------
  gg_df$uid <- gg_df[[id_var]]
  
  gg_df <- gg_df %>%
    dplyr::mutate(count_all = count_0 + count_1 + count_2 + count_3 + count_4) %>%
    
    group_by(uid) %>%
    dplyr::mutate(count_all = max(count_all)) %>%
    ungroup() %>%
    
    dplyr::mutate(google_prop_234 = (count_2 + count_3 + count_4) / count_all,
                  google_prop_34  = (count_3 + count_4) / count_all,
                  google_prop_4   = (count_4) / count_all) %>%
    dplyr::select(uid, datetime, contains("google"))
  
  # Clean mapbox -----------------------------------------------------------------
  names(mb_df) <- names(mb_df) %>%
    str_replace_all("_low$",      "_1") %>%
    str_replace_all("_moderate$", "_2") %>%
    str_replace_all("_heavy$",    "_3") %>%
    str_replace_all("_severe$",   "_4")
  
  mb_df$uid <- mb_df[[id_var]]
  
  mb_df <- mb_df %>%
    dplyr::mutate(length_all = length_1 + length_2 + length_3 + length_4) %>%
    
    group_by(uid) %>%
    dplyr::mutate(length_all = max(length_all)) %>%
    ungroup() %>%
    
    dplyr::mutate(mapbox_prop_234 = (length_2 + length_3 + length_4) / length_all,
                  mapbox_prop_34  = (length_3 + length_4) / length_all,
                  mapbox_prop_4   = (length_4) / length_all) %>%
    dplyr::select(uid, datetime, contains("mapbox")) 
  
  # Merge: Make wide -------------------------------------------------------------
  tt_wide_df <- gg_df %>%
    full_join(mb_df, by = c("uid", "datetime")) 
  
  # Cleanup / add variables ------------------------------------------------------
  tt_wide_df <- tt_wide_df %>%
    dplyr::mutate(date = datetime %>% date(),
                  hour = datetime %>% hour()) %>%
    dplyr::select(uid, datetime, date, hour, everything())
  
  # Make long --------------------------------------------------------------------
  ## Google
  gg_sub_df <- tt_wide_df %>%
    dplyr::select(uid, datetime, date, hour, contains("google")) %>%
    mutate(source = "Google") 
  
  names(gg_sub_df) <- names(gg_sub_df) %>%
    str_replace_all("google_", "")
  
  ## Mapbox
  mb_sub_df <- tt_wide_df %>%
    dplyr::select(uid, datetime, date, hour, contains("mapbox")) %>%
    mutate(source = "Mapbox")
  
  names(mb_sub_df) <- names(mb_sub_df) %>%
    str_replace_all("mapbox_", "") 
  
  ## Append
  tt_long_df <- bind_rows(gg_sub_df,
                          mb_sub_df)
  
  # Cleanup IDs ------------------------------------------------------------------
  if(unit == "gadm1"){
    tt_wide_df <- tt_wide_df %>% dplyr::rename(GID_1 = uid)
    tt_long_df <- tt_long_df %>% dplyr::rename(GID_1 = uid)
  } 
  
  if(unit == "gadm2"){
    tt_wide_df <- tt_wide_df %>% dplyr::rename(GID_2 = uid)
    tt_long_df <- tt_long_df %>% dplyr::rename(GID_2 = uid)
  } 
  
  if(unit == "gadm3"){
    tt_wide_df <- tt_wide_df %>% dplyr::rename(GID_3 = uid)
    tt_long_df <- tt_long_df %>% dplyr::rename(GID_3 = uid)
  } 
  
  # Export -----------------------------------------------------------------------
  saveRDS(tt_wide_df,   file.path(analysis_data_dir, paste0(unit, "_data_wide.Rds")))
  write_dta(tt_wide_df, file.path(analysis_data_dir, paste0(unit, "_data_wide.dta")))
  write_csv(tt_wide_df, file.path(analysis_data_dir, paste0(unit, "_data_wide.csv")))
  
  saveRDS(tt_long_df,   file.path(analysis_data_dir, paste0(unit, "_data_long.Rds")))
  write_dta(tt_long_df, file.path(analysis_data_dir, paste0(unit, "_data_long.dta")))
  write_csv(tt_long_df, file.path(analysis_data_dir, paste0(unit, "_data_long.csv")))
  
}

