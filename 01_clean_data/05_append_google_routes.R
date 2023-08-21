# Append Google Route Data

gg_df <- file.path(traffic_tt_dir, "google_individual_files") %>%
  list.files(pattern = ".Rds",
             full.names = T) %>%
  map_df(readRDS)

gg_df <- gg_df %>%
  mutate(count_ggtyp_all = count_ggtyp_0 + count_ggtyp_1 + count_ggtyp_2 + count_ggtyp_3 + count_ggtyp_4)

