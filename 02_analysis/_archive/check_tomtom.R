# Check Tomtom

df <- readRDS(file.path(analysis_data_dir, 
                                paste0("gadm2", "_wide.Rds")))

df %>%
  dplyr::filter(!is.na(tmtm_q50_wmean)) %>%
  ggplot() +
  geom_col(aes(x = datetime, y = tmtm_q50_wmean)) +
  facet_wrap(~NAME_2)





