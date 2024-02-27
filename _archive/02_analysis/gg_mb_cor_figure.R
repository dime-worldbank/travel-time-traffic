# Compare Data Sources

# NOTE:
# What if we want correlation over time? That's different!
# 1. Correlation over time for each, then average. [Time moves together?]
# 2. Correlation by unit for each, then average. [What is most congested?]

hvline_color <- "black"

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# Select variables -------------------------------------------------------------
df <- df %>%
  dplyr::select(uid,
                gg_speed_in_traffic_kmh,
                gg_duration_in_traffic_s,
                
                mb_speed_in_traffic_kmh,
                mb_duration_in_traffic_s,
                
                gg_tl_prop_234,
                gg_tl_prop_34,
                gg_tl_prop_4,
                
                mb_tl_prop_234,
                mb_tl_prop_34,
                mb_tl_prop_4) %>%
  na.omit()

# Correlation Dataframes -------------------------------------------------------
cor_over_time_df <- map_df(unique(df$uid), function(uid_i){
  
  cor_df <- df[df$uid %in% uid_i,] %>%
    cor(use = "pairwise.complete.obs") %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    pivot_longer(cols = -variable) %>%
    rename_var("variable") %>%
    rename_var("name")
  
  cor_df$uid <- uid_i
  
  return(cor_df)
}) %>%
  group_by(variable, name) %>%
  dplyr::summarise(value = mean(value, na.rm = T)) %>%
  ungroup()

# Figures ----------------------------------------------------------------------
p_gg <- cor_over_time_df %>%
  filter(variable %>% str_detect("Google"),
         name %>% str_detect("Google")) %>%
  ggplot(aes(x = variable,
             y = name,
             fill = value,
             label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black",
            size = 2.5) +

  geom_hline(yintercept = 3.5, color = hvline_color) +
  geom_vline(xintercept = 3.5, color = hvline_color) +
  
  scale_fill_distiller(palette = "RdBu",
                       na.value = "white",
                       direction = 0,
                       limits = c(-1, 1)) +
  labs(y = NULL,
       x = NULL,
       fill = "Correlation",
       title = "A. Google travel time and traffic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

p_mb <- cor_over_time_df %>%
  filter(variable %>% str_detect("Mapbox"),
         name %>% str_detect("Mapbox")) %>%
  ggplot(aes(x = variable,
             y = name,
             fill = value,
             label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black",
            size = 2.5) +
  
  geom_hline(yintercept = 3.5, color = hvline_color) +
  geom_vline(xintercept = 3.5, color = hvline_color) +
  
  scale_fill_distiller(palette = "RdBu",
                       na.value = "white",
                       direction = 0,
                       limits = c(-1, 1)) +
  labs(y = NULL,
       x = NULL,
       fill = "Correlation",
       title = "B. Mapbox travel time and traffic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

p <- ggarrange(p_gg, p_mb, nrow = 1,
               common.legend = T,
               legend = "right")

ggsave(p,
       filename = file.path(figures_dir, "google_map_cor.png"),
       height = 4, width = 9.25)

