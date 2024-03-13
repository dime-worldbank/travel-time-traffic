# Compare Data Sources

# NOTE:
# What if we want correlation over time? That's different!
# 1. Correlation over time for each, then average. [Time moves together?]
# 2. Correlation by unit for each, then average. [What is most congested?]

hvline_color <- "black"

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# Correlation Dataframes -------------------------------------------------------
cor_all_df <- df %>%
  dplyr::select(c(gg_duration_in_traffic_s,
                  gg_speed_in_traffic_kmh,
                  gg_tl_prop_234,
                  gg_tl_prop_34,
                  gg_tl_prop_4)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) %>%
  rename_var("variable") %>%
  rename_var("name")

cor_over_time_pairs_df <- map_df(unique(df$uid), function(uid_i){
  
  cor_df <- df[df$uid %in% uid_i,] %>%
    dplyr::select(c(gg_duration_in_traffic_s,
                    gg_speed_in_traffic_kmh,
                    gg_tl_prop_234,
                    gg_tl_prop_34,
                    gg_tl_prop_4)) %>%
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
  ungroup()

cor_over_unit_df <- df %>%
  
  group_by(uid) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  ungroup() %>%
  
  dplyr::select(c(gg_duration_in_traffic_s,
                  gg_speed_in_traffic_kmh,
                  gg_tl_prop_234,
                  gg_tl_prop_34,
                  gg_tl_prop_4)) %>%
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

p1 <- make_cor_fig("gg_route_cor_over_unit", "A. Correlation across average\nvalues within units")
p2 <- make_cor_fig("gg_route_cor_over_time", "B. Average correlation\nover time")

# Correlation distribution over time -------------------------------------------
## All correlations either all negative or positve, so fine
## to take absolute value
p3 <- cor_over_time_pairs_df %>%
  dplyr::filter(variable %in% c("Traffic, Prop 2,3,4",
                                "Traffic, Prop 3,4",
                                "Traffic, Prop 4"),
                name %in% c("Duration",
                            "Average Speed")) %>%
  dplyr::mutate(name = case_when(
    name == "Average Speed" ~ "Average\nSpeed",
    TRUE ~ name
  )) %>%
  dplyr::mutate(value = abs(value)) %>%
  ggplot() +
  geom_boxplot(aes(x = value,
                   y = variable,
                   fill = name)) +
  labs(x = "Correlation, absolute value",
       y = NULL,
       fill = "Google travel\ntime variable",
       title = "C. Correlation over time,\ndistribution across units") +
  scale_fill_manual(values = c("darkorange", "dodgerblue")) +
  scale_x_continuous(limits = c(0,1)) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom")

# Arrange/export ---------------------------------------------------------------

# p12 <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")
# p <- ggarrange(p12, p3, nrow = 1, widths = c(0.65, 0.35))
# 
# ggsave(p, 
#        filename = file.path(figures_dir, "cor_across_vars.png"),
#        height = 4.5, width = 12)

p12 <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "right")

ggsave(p12, 
       filename = file.path(figures_dir, "cor_across_vars.png"),
       height = 4.5, width = 10)





