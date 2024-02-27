# Crash Analysis

## Three groups
# Google Traffic
# Google Speed
# No! Cases where have speed but not traffic --> Constant sample, of both!

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "ntsa_crashes_100m_wide.Rds"))

# Clean data -------------------------------------------------------------------
df <- df %>%
  dplyr::select(uid,
                datetime,
                crash_datetime,
                
                gg_speed_in_traffic_kmh_mean,
                gg_duration_in_traffic_s_mean,
                gg_distance_m_mean,
                
                gg_tl_prop_234,
                gg_tl_prop_34,
                gg_tl_prop_4) %>%
  
  # Remove 30 minute period
  dplyr::mutate(datetime_minute = datetime %>% minute()) %>%
  dplyr::filter(datetime_minute %in% 0) %>%
  dplyr::select(-datetime_minute) #%>%
  #pivot_longer(cols = -c(uid, datetime, crash_datetime))

# Cleanup ----------------------------------------------------------------------
df <- df %>%
  dplyr::mutate(crash_datetime = floor_date(crash_datetime, unit = "hours"),
                hours_since_crash = difftime(datetime, crash_datetime, units = "hours") %>% 
                  as.numeric()) %>%
  dplyr::mutate(hour = datetime %>% hour(),
                dow = datetime %>% wday())

# Take average value 2 months (8 weeks) before
df_typical <- df %>%
  dplyr::filter( (hours_since_crash < -24) & (hours_since_crash >= -24*7*8)) %>%
  pivot_longer(cols = c(contains("gg_"))) %>%
  group_by(uid, name, hour, dow) %>%
  dplyr::summarise(value_typical = mean(value, na.rm = T)) %>%
  ungroup()

df_sub <- df %>%
  dplyr::filter(abs(hours_since_crash) <= 10) %>%
  pivot_longer(cols = c(contains("gg_"))) 

df_sub <- df_sub %>%
  left_join(df_typical, by = c("uid", "name", "hour", "dow")) %>%
  dplyr::mutate(value_minus_typical = value - value_typical)

## Subset to variables where we have data
df_sub <- df_sub %>%
  
  dplyr::filter(!is.na(value)) %>%
  ungroup() %>%
  group_by(uid, name) %>%
  dplyr::mutate(n_var = n()) %>%
  ungroup() %>%
  dplyr::filter(n_var %in% 21) %>%
  
  dplyr::filter(abs(hours_since_crash) <= 10) %>%
  dplyr::mutate(post_crash = as.numeric(hours_since_crash >= 0),
                hours_since_crash_num = hours_since_crash) %>%
  dplyr::mutate(hours_since_crash = factor(hours_since_crash) %>%
                  relevel("-1")) 

## Define sample with both speed and traffic
uid_speed <- df_sub %>%
  dplyr::filter(name == "gg_speed_in_traffic_kmh_mean") %>%
  pull(uid) %>%
  unique()

uid_traffic <- df_sub %>%
  dplyr::filter(name == "gg_tl_prop_234") %>%
  pull(uid) %>%
  unique()

uid_both <- intersect(speed, traffic)

df_sub$both_sample <- df_sub$uid %in% uid_both

# Analysis: Hourly -------------------------------------------------------------
run_reg_hourly <- function(name_i, df_sub){
  print(name_i)
  
  ## Subset to variable
  df_sum_i <- df_sub[df_sub$name %in% name_i,]
  
  ## Regressions + grab coefficients
  lm_v_df <- feols(value ~ hours_since_crash | uid, df_sum_i) %>%
    confint() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::filter(variable %>% str_detect("hours_since_crash")) %>%
    dplyr::mutate(variable = variable %>% 
                    str_replace_all("hours_since_crash", "") %>%
                    as.numeric()) %>%
    clean_names() %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2) %>%
    dplyr::mutate(type = "Value")
  
  lm_vmt_df <- feols(value_minus_typical ~ hours_since_crash | uid, df_sum_i) %>%
    confint() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::filter(variable %>% str_detect("hours_since_crash")) %>%
    dplyr::mutate(variable = variable %>% 
                    str_replace_all("hours_since_crash", "") %>%
                    as.numeric()) %>%
    clean_names() %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2) %>%
    dplyr::mutate(type = "Value - Typical Value") 
  
  lm_df <- bind_rows(lm_v_df,
                     lm_vmt_df)
  
  ## Add variables
  lm_df$data_var <- name_i
  lm_df$n_locations <- df_sum_i$uid %>% unique() %>% length()
  
  return(lm_df)
}

lm_hr_coef_df <- bind_rows(
  map_df(unique(df_sub$name), run_reg_hourly, df_sub) %>%
    mutate(sample = "Full"),
  
  map_df(unique(df_sub$name), run_reg_hourly, df_sub[df_sub$both_sample %in% T,]) %>%
    mutate(sample = "Sub")
)

lm_hr_coef_df <- lm_hr_coef_df %>%
  dplyr::mutate(sig = ifelse(
    ((x2_5_percent > 0) & (x97_5_percent > 0)) | 
      ((x2_5_percent < 0) & (x97_5_percent < 0)),
    T,F
  )) %>%
  rename_var("data_var") %>%
  dplyr::mutate(data_var = paste0(data_var, "\nN = ", n_locations))

p <- lm_hr_coef_df %>%
  dplyr::filter(type %in% "Value - Typical Value",
                sample == "Full") %>%
  ggplot(aes(x = variable,
             y = b,
             ymin = x2_5_percent,
             ymax = x97_5_percent,
             color = sig)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_linerange(position = position_dodge(width = 0.9)) +
  scale_color_manual(values = c("black", "red")) +
  theme_minimal() +
  labs(x = "Hours Since Crash",
       y = "Coef (+/- 95% CI)",
       color = "p < 0.05") +
  facet_wrap(~data_var, scales = "free_y") 

ggsave(p, 
       filename = file.path(figures_dir, "lm_value_m_typical_hourly.png"),
       height = 7, width = 7)

p <- lm_hr_coef_df %>%
  dplyr::filter(type %in% "Value - Typical Value",
                sample == "Sub") %>%
  ggplot(aes(x = variable,
             y = b,
             ymin = x2_5_percent,
             ymax = x97_5_percent,
             color = sig)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_linerange(position = position_dodge(width = 0.9)) +
  scale_color_manual(values = c("black", "red")) +
  theme_minimal() +
  labs(x = "Hours Since Crash",
       y = "Coef (+/- 95% CI)",
       color = "p < 0.05") +
  facet_wrap(~data_var, scales = "free_y") 

ggsave(p, 
       filename = file.path(figures_dir, "lm_value_m_typical_hourly_sub.png"),
       height = 7, width = 7)

# Analysis: One Coef -----------------------------------------------------------
lm_one_coef_df <- map_df(unique(df_sub$name), function(name_i){
  
  print(name_i)
  
  ## Subset to variable
  df_sum_i <- df_sub[df_sub$name %in% name_i,]
  df_sum_i <- df_sum_i %>%
    dplyr::filter(hours_since_crash_num >= -3,
                  hours_since_crash_num <= 2)
  
  df_sum_i$value <- df_sum_i$value %>% scale() %>% as.numeric()
  df_sum_i$value_minus_typical <- df_sum_i$value_minus_typical %>% scale() %>% as.numeric()
  
  ## Regressions + grab coefficients
  lm_v_df <- feols(value ~ post_crash | uid, df_sum_i) %>%
    confint() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::filter(variable %>% str_detect("post_crash")) %>%
    clean_names() %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2) %>%
    dplyr::mutate(type = "Value")
  
  lm_vmt_df <- feols(value_minus_typical ~ post_crash | uid, df_sum_i) %>%
    confint() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    dplyr::filter(variable %>% str_detect("post_crash")) %>%
    clean_names() %>%
    dplyr::mutate(b = (x2_5_percent + x97_5_percent) / 2) %>%
    dplyr::mutate(type = "Value - Typical Value") 
  
  lm_df <- bind_rows(lm_v_df,
                     lm_vmt_df)
  
  ## Add variables
  lm_df$data_var <- name_i
  lm_df$n_locations <- df_sum_i$uid %>% unique() %>% length()
  
  return(lm_df)
})

lm_one_coef_df <- lm_one_coef_df %>%
  dplyr::mutate(sig = ifelse(
    ((x2_5_percent > 0) & (x97_5_percent > 0)) | 
      ((x2_5_percent < 0) & (x97_5_percent < 0)),
    T,F
  )) %>%
  rename_var("data_var") %>%
  dplyr::mutate(data_var = paste0(data_var, " N = ", n_locations))

p <- lm_one_coef_df %>%
  dplyr::filter(type %in% "Value - Typical Value") %>%
  ggplot(aes(y = data_var,
             x = b,
             xmin = x2_5_percent,
             xmax = x97_5_percent,
             color = sig)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_linerange(position = position_dodge(width = 0.9)) +
  geom_vline(xintercept = 0, color = "gray70") +
  scale_color_manual(values = c("black", "red")) +
  theme_minimal() +
  labs(x = "Post Crash",
       y = "Coef (+/- 95% CI)",
       color = "p < 0.05") 

ggsave(p, 
       filename = file.path(figures_dir, "lm_value_m_typical_post.png"),
       height = 7, width = 7)

