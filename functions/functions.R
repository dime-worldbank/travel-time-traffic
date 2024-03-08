# Functions --------------------------------------------------------------------
rename_var <- function(df, var){
  
  df$tmp_var <- df[[var]]
  
  df <- df %>%
    dplyr::mutate(tmp_var = case_when(
      tmp_var == "gg_speed_in_traffic_kmh"  ~ "Average Speed",
      tmp_var == "gg_speed_in_traffic_kmh_mean"  ~ "Average Speed",
      tmp_var == "gg_speed_in_traffic_kmh_wmean"  ~ "Average Speed",
      
      tmp_var == "gg_duration_in_traffic_s" ~ "Duration",
      tmp_var == "gg_duration_in_traffic_s_mean" ~ "Duration, Avg",
      tmp_var == "gg_duration_in_traffic_s_wmean" ~ "Duration, W Avg",
      
      tmp_var == "gg_duration_in_traffic_min" ~ "Duration",
      tmp_var == "gg_duration_in_traffic_min_mean" ~ "Duration",
      tmp_var == "gg_duration_in_traffic_min_wmean" ~ "Duration",
      
      tmp_var == "gg_distance_km" ~ "Distance (km)",
      tmp_var == "gg_distance_m_mean" ~ "Distance",
      tmp_var == "gg_distance_km_mean" ~ "Distance",
      
      tmp_var == "mb_speed_in_traffic_kmh"  ~ "[Mapbox TT]: Average Speed",
      tmp_var == "mb_speed_in_traffic_kmh_mean"  ~ "[Mapbox TT]: Average Speed, Avg",
      tmp_var == "mb_speed_in_traffic_kmh_wmean"  ~ "[Mapbox TT]: Average Speed, W Avg",
      
      tmp_var == "mb_duration_in_traffic_s" ~ "[Mapbox TT]: Duration",
      tmp_var == "mb_duration_in_traffic_s_mean" ~ "[Mapbox TT]: Duration, Avg",
      tmp_var == "mb_duration_in_traffic_s_wmean" ~ "[Mapbox TT]: Duration, W Avg",
      
      tmp_var == "mb_duration_in_traffic_min" ~ "[Mapbox TT]: Duration",
      tmp_var == "mb_duration_in_traffic_min_mean" ~ "[Mapbox TT]: Duration, Avg",
      tmp_var == "mb_duration_in_traffic_min_wmean" ~ "[Mapbox TT]: Duration, W Avg",
      
      tmp_var == "gg_tl_prop_234" ~ "Traffic, Prop 2,3,4",
      tmp_var == "gg_tl_prop_34"  ~ "Traffic, Prop 3,4",
      tmp_var == "gg_tl_prop_4"   ~ "Traffic, Prop 4",
      tmp_var == "mb_tl_prop_234" ~ "[Mapbox Traffic]: Prop 2,3,4",
      tmp_var == "mb_tl_prop_34"  ~ "[Mapbox Traffic]: Prop 3,4",
      tmp_var == "mb_tl_prop_4"   ~ "[Mapbox Traffic]: Prop 4",
      tmp_var == "tmtm_avgspeed_wmean" ~ "[TomTom]: Average Speed",
      tmp_var == "tmtm_q10_wmean" ~ "[TomTom]: 10th Perc. Speed",
      tmp_var == "tmtm_q20_wmean" ~ "[TomTom]: 20th Perc. Speed",
      tmp_var == "tmtm_q30_wmean" ~ "[TomTom]: 30th Perc. Speed",
      tmp_var == "tmtm_q40_wmean" ~ "[TomTom]: 40th Perc. Speed",
      tmp_var == "tmtm_q50_wmean" ~ "[TomTom]: 50th Perc. Speed",
      tmp_var == "tmtm_q60_wmean" ~ "[TomTom]: 60th Perc. Speed",
      tmp_var == "tmtm_q70_wmean" ~ "[TomTom]: 70th Perc. Speed",
      tmp_var == "tmtm_q80_wmean" ~ "[TomTom]: 80th Perc. Speed",
      tmp_var == "tmtm_q90_wmean" ~ "[TomTom]: 90th Perc. Speed",
      tmp_var == "tmtm_q95_wmean" ~ "[TomTom]: 95th Perc. Speed",
      tmp_var == "tmtm_sample_size_sum" ~ "[TomTom]: Sample Size",
      tmp_var == "wz_delay_sum_min" ~ "[Waze]: Delay Time",
      T ~ tmp_var
    ))
  
  df[[var]]  <- df$tmp_var
  df$tmp_var <- NULL 
  
  return(df)
}

extract_gt_to_poly <- function(r, locations_sf){
  
  count_0 <- function(x){
    sum(x == 0, na.rm = T)
  }
  
  count_1 <- function(x){
    sum(x == 1, na.rm = T)
  }
  
  count_2 <- function(x){
    sum(x==2, na.rm = T)
  }
  
  count_3 <- function(x){
    sum(x==3, na.rm = T)
  }
  
  count_4 <- function(x){
    sum(x==4, na.rm = T)
  }
  
  count_5 <- function(x){
    sum(x==5, na.rm = T)
  }
  
  ## Weird issue of counting "1s" -- numbers are way too large
  r[] <- r[] + 1
  
  locations_sf$count_0 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_1, ## count_1 because add 1 (so raw is 0)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$count_1 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_2, ## count_2 because add 1 (so raw is 1)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$count_2 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_3, ## count_3 because add 1 (so raw is 2)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$count_3 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_4, ## count_4 because add 1 (so raw is 3)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$count_4 <- exact_extract(x = r,
                                        y = locations_sf,
                                        fun = count_5, ## count_5 because add 1 (so raw is 4)
                                        summarize_df = T,
                                        max_cells_in_memory = 3e07)
  
  locations_sf$geometry <- NULL
  return(locations_sf)
}

calc_traffic_length <- function(polygon_sf, traffic_sf, add_by_class = F){
  
  length_all_df <- map_df(1:nrow(polygon_sf), function(i){
    
    road_i <- polygon_sf[i,]
    
    mp_sf_i_roadi <- st_intersection(road_i, traffic_sf)
    
    if(nrow(mp_sf_i_roadi) == 0){
      
      length_df <- data.frame(
        length_low = 0,
        length_moderate = 0,
        length_heavy = 0,
        length_severe = 0
      )
      
    } else{
      
      mp_sf_i_roadi$length_m <- mp_sf_i_roadi %>% st_length() %>% as.numeric()
      
      length_low      <- mp_sf_i_roadi$length_m[mp_sf_i_roadi$congestion %in% "low"] %>% sum()
      length_moderate <- mp_sf_i_roadi$length_m[mp_sf_i_roadi$congestion %in% "moderate"] %>% sum()
      length_heavy    <- mp_sf_i_roadi$length_m[mp_sf_i_roadi$congestion %in% "heavy"] %>% sum()
      length_severe   <- mp_sf_i_roadi$length_m[mp_sf_i_roadi$congestion %in% "severe"] %>% sum()
      
      road_i_df <- road_i
      road_i_df$geometry <- NULL 
      
      length_df <- data.frame(
        length_low = length_low,
        length_moderate = length_moderate,
        length_heavy = length_heavy,
        length_severe = length_severe
      )
      
      if(add_by_class){
        
        mp_sf_i_roadi_df <- mp_sf_i_roadi
        mp_sf_i_roadi_df$geometry <- NULL
        
        length_class_df <- mp_sf_i_roadi_wide_df <- mp_sf_i_roadi_df %>% 
          group_by(congestion, class) %>%
          dplyr::summarise(length_m = sum(length_m)) %>%
          ungroup() %>%
          pivot_wider(values_from = length_m,
                      names_from = c(congestion, class))
        
        length_df <- bind_cols(length_df, length_class_df)
        
      }
    }
    
    length_df <- bind_cols(length_df, road_i_df)
    
    return(length_df)
  })
  
  return(length_all_df)
  
}
