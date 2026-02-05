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
  dplyr::filter(!(uid %in% 3:4)) %>%
  group_by(uid) %>%
  summarise(gg_diff_mode_prop = mean(gg_diff_mode)) %>%
  ungroup() %>%
  mutate(gg_diff_mode_per = gg_diff_mode_prop * 100) %>%
  mutate(gg_diff_mode_round = case_when(
    gg_diff_mode_per == 0 ~ "0%",
    gg_diff_mode_per <= 1 ~ "0 - 1%",
    gg_diff_mode_per <= 2 ~ "1 - 2%",
    gg_diff_mode_per <= 3 ~ "2 - 3%",
    gg_diff_mode_per <= 4 ~ "3 - 4%",
    gg_diff_mode_per <= 5 ~ "4 - 5%",
    gg_diff_mode_per <= 6 ~ "5 - 6%",
    gg_diff_mode_per <= 7 ~ "6 - 7%",
    gg_diff_mode_per <= 8 ~ "7 - 8%",
  )) %>%
  group_by(gg_diff_mode_round) %>%
  summarise(n = n()) %>%
  ungroup() 

## Add missing categories
df_zeros <- data.frame(gg_diff_mode_round = c("0%",
                                              "0 - 1%",
                                              "1 - 2%",
                                              "2 - 3%",
                                              "3 - 4%",
                                              "4 - 5%",
                                              "5 - 6%",
                                              "6 - 7%",
                                              "7 - 8%"))
df_zeros$n <- 0

df_zeros <- df_zeros[!(df_zeros$gg_diff_mode_round %in% df_sum$gg_diff_mode_round),]

df_sum <- bind_rows(df_sum, df_zeros)

df_sum <- df_sum %>%
  mutate(gg_diff_mode_round = gg_diff_mode_round %>%
           factor(levels = c("0%",
                             "0 - 1%",
                             "1 - 2%",
                             "2 - 3%",
                             "3 - 4%",
                             "4 - 5%",
                             "5 - 6%",
                             "6 - 7%",
                             "7 - 8%"))) %>%
  arrange(gg_diff_mode_round)

p <- df_sum %>%
  mutate(prop = n / sum(n),
         per = round(prop*100, 1),
         per = paste0(per, "%"),
         label = paste0(n, " (", per, ")")) %>%
  ggplot() +
  geom_col(aes(y = n,
               x = gg_diff_mode_round)) +
  geom_text(aes(y = n + 1,
                x = gg_diff_mode_round,
                label = label)) +
  labs(x = "Percent of time route differs from model route",
       y = "N routes") +
  theme_classic() +
  theme(axis.text = element_text(color = "black"))

ggsave(p, 
       filename = file.path(figures_dir, "deviation_percent_time.png"),
       height = 2.5, width = 6)

# ## Make LaTeX variable
# df_sum <- df_sum %>%
#   mutate(tex = paste(gg_diff_mode_round, n, sep = " & ") %>% paste0(" \\\\ \n"))
# 
# df_sum <- df_sum[1:3,]

# # Make table -------------------------------------------------------------------
# sink(file.path(tables_dir, "deviation_percent_time.tex"))
# 
# cat("\\begin{tabular}{c |c  c} \n")
# cat("\\hline \n")
# cat("Percent of time & & \\\\ \n ")
# cat("route differs & N \\\\ \n ")
# cat("from modal route & Routes \\\\ \n ")
# 
# cat("\\hline \n")
# df_sum$tex %>% cat()
# cat("\\hline \n ")
# cat("\\end{tabular}")
# sink()

