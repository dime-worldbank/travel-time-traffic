# Compare Data Sources

# NOTE:
# What if we want correlation over time? That's different!
# 1. Correlation over time for each, then average. [Time moves together?]
# 2. Correlation by unit for each, then average. [What is most congested?]

hvline_color <- "black"

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

df <- df %>%
  dplyr::filter(all_26_route %in% 1)

# Correlation Dataframes -------------------------------------------------------
cor_all_df <- df %>%
  dplyr::select(c(gg_duration_pc_diff,
                  gg_speed_pc_diff,
                  gg_duration_in_traffic_min,
                  gg_speed_in_traffic_kmh,
                  gg_tl_prop_234,
                  gg_tl_prop_34,
                  gg_tl_prop_4,
                  gg_tl_max,
                  gg_tl_mean)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) %>%
  rename_var("variable") %>%
  rename_var("name")

cor_over_time_pairs_df <- map_df(unique(df$uid), function(uid_i){
  
  cor_df <- df[df$uid %in% uid_i,] %>%
    dplyr::select(c(gg_duration_pc_diff,
                    gg_speed_pc_diff,
                    gg_duration_in_traffic_min,
                    gg_speed_in_traffic_kmh,
                    gg_tl_prop_234,
                    gg_tl_prop_34,
                    gg_tl_prop_4,
                    gg_tl_max,
                    gg_tl_mean)) %>%
    cor(use = "pairwise.complete.obs") %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    pivot_longer(cols = -variable) %>%
    rename_var("variable") %>%
    rename_var("name")
  
  cor_df$uid <- uid_i
  
  return(cor_df)
}) 

cor_over_time_df <- cor_over_time_pairs_df %>%
  group_by(variable, name) %>%
  dplyr::summarise(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  dplyr::filter(!(name %in% c("Duration, Diff. than Typical",
                              "Speed, Diff. than Typical"))) %>%
  dplyr::filter(!(variable %in% c("Duration, Diff. than Typical",
                              "Speed, Diff. than Typical")))  

cor_over_unit_df <- df %>%
  
  group_by(uid) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  ungroup() %>%
  
  dplyr::select(c(gg_duration_pc_diff,
                  gg_speed_pc_diff,
                  gg_tl_prop_234,
                  gg_tl_prop_34,
                  gg_tl_prop_4,
                  gg_tl_max,
                  gg_tl_mean)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) %>%
  rename_var("variable") %>%
  rename_var("name")

# Figure -----------------------------------------------------------------------
make_cor_fig <- function(cor_type, title){
  
  if(cor_type %in% "gg_route_cor_all"){
    cor_df <- cor_all_df
  }
  
  if(cor_type %in% "gg_route_cor_over_time"){
    cor_df <- cor_over_time_df
  }
  
  if(cor_type %in% "gg_route_cor_over_unit"){
    cor_df <- cor_over_unit_df
  }
  
  cor_df %>%
    ggplot(aes(x = variable,
               y = name,
               fill = value,
               label = round(value, 2))) +
    geom_tile(color = "white") +
    geom_text(color = "black",
              size = 3) +
    
    geom_hline(yintercept = 2.5, color = hvline_color) +
    geom_vline(xintercept = 2.5, color = hvline_color) +
    
    scale_fill_distiller(palette = "RdBu",
                         na.value = "white",
                         direction = 0,
                         limits = c(-1, 1)) +
    labs(y = NULL,
         x = NULL,
         fill = "Correlation",
         title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, color = "black"),
          axis.text.y = element_text(color = "black"),
          plot.title = element_text(face = "bold"),
          legend.position = "right")
  
}

p_cor_unit <- make_cor_fig("gg_route_cor_over_unit", 
                           "A. Correlation across average\nvalues within units")

pad_text <- rep("\n ", 10) %>% paste(collapse = "")
p_cor_time <- make_cor_fig("gg_route_cor_over_time", 
                           paste0(pad_text, "A. Average correlation over time"))


# Correlation examples: across units -------------------------------------------
p_cor_unit_ex <- df %>%
  
  group_by(uid) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  ungroup() %>%
  
  dplyr::select(c(gg_duration_pc_diff,
                  gg_tl_prop_234,
                  gg_tl_prop_34,
                  gg_tl_prop_4,
                  gg_tl_mean)) %>%
  
  pivot_longer(cols = -gg_duration_pc_diff) %>%
  rename_var("name") %>%
  
  ggplot(aes(x = gg_duration_pc_diff,
             y = value)) +
  geom_smooth(method = "lm",
              se = F,
              color = "orange") +
  geom_point() +
  facet_wrap(~name,
             scales = "free") +
  labs(x = "Duration, % Different than Typical", ## % difference??
       y = NULL,
       title = "B. Correlation of select traffic level indicators with\nhow much duration differs from typical duration",
       subtitle = "Each dot represents one O-D route") + 
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# Correlation examples: over time ----------------------------------------------
p_cor_time_ex <- df %>%
  dplyr::filter(!is.na(gg_duration_in_traffic_min),
                !is.na(gg_tl_prop_234)) %>%
  mutate(gg_duration_in_traffic_hr = gg_duration_in_traffic_min / 60) %>%
  
  group_by(uid) %>%
  mutate(cor = cor(gg_tl_prop_234, gg_duration_in_traffic_hr)) %>%
  ungroup() %>%
  
  mutate(title = paste0("Route ID: ", uid, "\nCor: ", round(cor, 2))) %>%
  
  ggplot(aes(x = gg_tl_prop_234,
             y = gg_duration_in_traffic_hr)) +
  geom_point(size = 0.5,
             alpha = 0.5) +
  geom_smooth(method = "lm",
              se = F,
              color = "orange") +
  # stat_cor(aes(label = ..r.label..),
  #          method = "pearson",
  #          color = "red",
  #          size = 3,
  #          label.x = 0.3) +
  labs(x = "Proportion traffic level 2-4",
       y = "Travel\nduration\n(hour)",
       title = "B. Travel duration vs traffic levels across routes",
       subtitle = "Each dot represents one point in time") +
  facet_wrap(~title, 
             scales = "free_y",
             ncol = 5) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 8),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold"))

# Arrange/export ---------------------------------------------------------------
#### Correlation across units
p_cor_unit_all <- ggarrange(p_cor_unit, p_cor_unit_ex, nrow = 1)

ggsave(p_cor_unit_all, 
       filename = file.path(figures_dir, "cor_across_vars_unit.png"),
       height = 4.5, width = 11)



#### Correlation over time
p_cor_time_all <- ggarrange(p_cor_time, p_cor_time_ex, nrow = 1,
                            heights = c(0.3, 1))

ggsave(p_cor_time_all, 
       filename = file.path(figures_dir, "cor_time.png"),
       height = 7, width = 12)




