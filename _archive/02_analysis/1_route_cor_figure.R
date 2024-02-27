# Compare Data Sources

# NOTE:
# What if we want correlation over time? That's different!
# 1. Correlation over time for each, then average. [Time moves together?]
# 2. Correlation by unit for each, then average. [What is most congested?]

hvline_color <- "black"

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide_clean.Rds"))

# Correlation Dataframes -------------------------------------------------------
cor_all_df <- df %>%
  dplyr::select(-uid) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) %>%
  rename_var("variable") %>%
  rename_var("name")

cor_over_time_df <- map_df(unique(df$uid), function(uid_i){
  
  cor_df <- df[df$uid %in% uid_i,] %>%
    dplyr::select(-uid) %>%
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

cor_over_unit_df <- df %>%
  
  group_by(uid) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  ungroup() %>%
  
  dplyr::select(-uid) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(cols = -variable) %>%
  rename_var("variable") %>%
  rename_var("name")

# Figure -----------------------------------------------------------------------
for(cor_type in c("gg_route_cor_all", 
                  "gg_route_cor_over_time", 
                  "gg_route_cor_over_unit")){
  
  if(cor_type %in% "gg_route_cor_all"){
    cor_df <- cor_all_df
  }
  
  if(cor_type %in% "gg_route_cor_over_time"){
    cor_df <- cor_over_time_df
  }
  
  if(cor_type %in% "gg_route_cor_over_unit"){
    cor_df <- cor_over_unit_df
  }
  
  p <- cor_df %>%
    ggplot(aes(x = variable,
               y = name,
               fill = value,
               label = round(value, 2))) +
    geom_tile(color = "white") +
    geom_text(color = "black",
              size = 2.5) +
    
    geom_hline(yintercept = 15.5, color = hvline_color) +
    geom_vline(xintercept = 15.5, color = hvline_color) +
    
    geom_hline(yintercept = 10.5, color = hvline_color) +
    geom_vline(xintercept = 10.5, color = hvline_color) +
    
    geom_hline(yintercept = 8.5, color = hvline_color) +
    geom_vline(xintercept = 8.5, color = hvline_color) +
    
    geom_hline(yintercept = 5.5, color = hvline_color) +
    geom_vline(xintercept = 5.5, color = hvline_color) +
    
    geom_hline(yintercept = 3.5, color = hvline_color) +
    geom_vline(xintercept = 3.5, color = hvline_color) +
    
    scale_fill_distiller(palette = "RdBu",
                         na.value = "white",
                         direction = 0,
                         limits = c(-1, 1)) +
    labs(y = NULL,
         x = NULL,
         fill = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, color = "black"),
          axis.text.y = element_text(color = "black"),
          legend.position = "top")
  
  ggsave(p, 
         filename = file.path(figures_dir, paste0(cor_type, ".png")),
         height = 7, width = 7)
  
}
