# Deviate Route

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide.Rds"))

# df <- df %>%
#   dplyr::filter(all_26_route %in% 1)

# Prep data --------------------------------------------------------------------
# df <- df %>%
#   dplyr::filter(!is.na(gg_distance_m)) %>%
#   group_by(uid) %>%
#   mutate(gg_distance_m_mode = Mode(gg_distance_m)) %>%
#   ungroup() %>%
#   mutate(gg_diff_mode = abs(gg_distance_m - gg_distance_m_mode) > 100)

# Prop time deviate ------------------------------------------------------------
df %>%
  group_by(uid) %>%
  summarise(gg_diff_mode_prop = mean(gg_diff_mode)) %>%
  ungroup() %>%
  mutate(gg_diff_mode_per = gg_diff_mode_prop * 100) %>%
  pull(gg_diff_mode_per) %>%
  summary()

df_sum <- df %>%
  group_by(uid) %>%
  summarise(gg_diff_mode_prop = mean(gg_diff_mode)) %>%
  ungroup() %>%
  mutate(gg_diff_mode_per = gg_diff_mode_prop * 100) %>%
  mutate(gg_diff_mode_round = case_when(
    gg_diff_mode_per == 0 ~ "0\\%",
    gg_diff_mode_per <= 1 ~ "0 - 1\\%",
    gg_diff_mode_per <= 10 ~ "1 - 10\\%",
    gg_diff_mode_per <= 20 ~ "10 - 20\\%",
    gg_diff_mode_per <= 30 ~ "20 - 30\\%",
    gg_diff_mode_per <= 40 ~ "30 - 40\\%",
    gg_diff_mode_per <= 50 ~ "40 - 50\\%",
    gg_diff_mode_per <= 60 ~ "50 - 60\\%",
    gg_diff_mode_per <= 70 ~ "60 - 70\\%",
    gg_diff_mode_per <= 80 ~ "70 - 80\\%"
  )) %>%
  group_by(gg_diff_mode_round) %>%
  summarise(n = n()) %>%
  ungroup() 

## Add missing categories
df_zeros <- data.frame(gg_diff_mode_round = c("0\\%",
                                              "0 - 1\\%",
                                              "1 - 10\\%",
                                              "10 - 20\\%",
                                              "20 - 30\\%",
                                              "30 - 40\\%",
                                              "40 - 50\\%"))
df_zeros$n <- 0

df_zeros <- df_zeros[!(df_zeros$gg_diff_mode_round %in% df_sum$gg_diff_mode_round),]

df_sum <- bind_rows(df_sum, df_zeros)

df_sum <- df_sum %>%
  mutate(gg_diff_mode_round = gg_diff_mode_round %>%
           factor(levels = c("0\\%",
                             "0 - 1\\%",
                             "1 - 10\\%",
                             "10 - 20\\%",
                             "20 - 30\\%",
                             "30 - 40\\%",
                             "40 - 50\\%"))) %>%
  arrange(gg_diff_mode_round)

## Make LaTeX variable
df_sum <- df_sum %>%
  mutate(tex = paste(gg_diff_mode_round, n, sep = " & ") %>% paste0(" \\\\ \n"))

# Make table -------------------------------------------------------------------
sink(file.path(tables_dir, "deviation_percent_time.tex"))

cat("\\begin{tabular}{c |c  c} \n")
cat("\\hline \n")
cat("Percent of time & & \\\\ \n ")
cat("route differs & N \\\\ \n ")
cat("from modal route & Routes \\\\ \n ")

cat("\\hline \n")
df_sum$tex %>% cat()
cat("\\hline \n ")
cat("\\end{tabular}")
sink()

