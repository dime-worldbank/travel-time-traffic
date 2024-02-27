# Deviate Route

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(analysis_data_dir, "google_typical_route_10m_wide_clean.Rds"))

# Prep data --------------------------------------------------------------------
df <- df %>%
  group_by(uid) %>%
  mutate(gg_distance_m_mode = Mode(gg_distance_m),
         mb_distance_m_mode = Mode(mb_distance_m)) %>%
  ungroup() %>%
  mutate(gg_diff_mode = gg_distance_m != gg_distance_m_mode,
         mb_diff_mode = mb_distance_m != mb_distance_m_mode)

# Prop time deviate ------------------------------------------------------------
df_sum <- df %>%
  group_by(uid) %>%
  summarise(gg_diff_mode_prop = mean(gg_diff_mode),
            mb_diff_mode_prop = mean(mb_diff_mode)) %>%
  ungroup() %>%
  pivot_longer(cols = -uid) %>%
  mutate(value = value * 100) %>%
  mutate(value_round = case_when(
    value == 0 ~ "0\\%",
    value <= 1 ~ "0 - 1\\%",
    value <= 10 ~ "1 - 10\\%",
    value <= 20 ~ "10 - 20\\%",
    value <= 30 ~ "20 - 30\\%",
    value <= 40 ~ "30 - 40\\%",
    value <= 50 ~ "40 - 50\\%",
  )) %>%
  group_by(name, value_round) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(value_round = value_round %>%
           factor(levels = c("0\\%",
                             "0 - 1\\%",
                             "1 - 10\\%",
                             "10 - 20\\%",
                             "20 - 30\\%",
                             "30 - 40\\%",
                             "40 - 50\\%")),
         name = case_when(
           name == "gg_diff_mode_prop" ~ "Google",
           name == "mb_diff_mode_prop" ~ "Mapbox"
         ))

df_sum_tex <- df_sum %>%
  pivot_wider(id_cols = value_round,
              names_from = name,
              values_from = n) %>%
  mutate(Google = Google %>% replace_na(0),
         Mapbox = Mapbox %>% replace_na(0)) %>%
  arrange(value_round) %>%
  mutate(tex = paste(value_round, Google, Mapbox, sep = " & ") %>% paste0(" \\\\ \n"))

# p_diff <- df_sum %>%
#   ggplot() +
#   geom_col(aes(x = n,
#                y = value_round)) +
#   facet_wrap(~name) +
#   labs(x = "N routes",
#        y = "Percent of time\nroute differs\nfrom modal route",
#        title = "A. Percent of time route differs from modal route") +
#   theme_classic2() +
#   theme(strip.background = element_blank(),
#         strip.text = element_text(face = "bold"),
#         axis.title.y = element_text(angle = 0, vjust = 0.5),
#         axis.text = element_text(color = "black"),
#         plot.title = element_text(face = "bold", size = 10))

# Deviation amount -------------------------------------------------------------
df_sel <- df %>%
  mutate(mb_distance_m_pc = (mb_distance_m - mb_distance_m_mode)/mb_distance_m_mode*100,
         gg_distance_m_pc = (gg_distance_m - gg_distance_m_mode)/gg_distance_m_mode*100) %>%
  dplyr::select(uid,
                mb_distance_m_pc, gg_distance_m_pc, 
                mb_diff_mode, gg_diff_mode)

df_dev <- bind_rows(
  df_sel %>%
    dplyr::select(uid, starts_with("gg_")) %>%
    rename(distance_m_pc = gg_distance_m_pc,
           diff_mode = gg_diff_mode) %>%
    mutate(source = "Google"),
  
  df_sel %>%
    dplyr::select(uid, starts_with("mb_")) %>%
    rename(distance_m_pc = mb_distance_m_pc,
           diff_mode = mb_diff_mode) %>%
    mutate(source = "Mapbox")
)

dev_amount_tex <- df_dev %>%
  filter(diff_mode %in% T) %>%
  mutate(distance_m_pc_smpl = case_when(
    distance_m_pc < 1 ~ "\\< 1\\%",
    distance_m_pc < 5 ~ "1 - 5\\%",
    distance_m_pc < 10 ~ "5 - 10\\%",
    distance_m_pc < 50 ~ "10 - 50\\%",
    distance_m_pc < 100 ~ "50 - 100\\%",
    distance_m_pc < 200 ~ "100 - 200\\%"
  ) %>%
    factor(levels = c("\\< 1\\%",
                      "1 - 5\\%",
                      "5 - 10\\%",
                      "10 - 50\\%",
                      "50 - 100\\%",
                      "100 - 200\\%"))) %>%
  group_by(distance_m_pc_smpl, source) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  
  group_by(source) %>%
  mutate(n_total = sum(n)) %>%
  ungroup() %>%
  
  mutate(percent = round(n/n_total*100,2) %>% paste0("\\%")) %>%
  mutate(n_p = paste0(n, " (", percent, ")")) %>%
  
  pivot_wider(id_cols = distance_m_pc_smpl,
              names_from = source,
              values_from = n_p) %>%
  
  mutate(Mapbox = Mapbox %>% replace_na("0 (0\\%)")) %>%
  
  mutate(tex = paste(distance_m_pc_smpl, Google, Mapbox, sep = " & ") %>% paste0(" \\\\ \n"))

# Make table -------------------------------------------------------------------
sink(file.path(tables_dir, "deviation_gg_mb.tex"))

cat("\\begin{tabular}{c |c  c} \n")
cat("\\hline \n")
cat(" & Google & Mapbox \\\\ \n")

cat("\\hline \n ")
cat("\\hline \n ")
cat("Percent of time & & \\\\ \n ")
cat("route differs & \\multicolumn{2}{c}{N} \\\\ \n ")
cat("from modal route & \\multicolumn{2}{c}{Routes} \\\\ \n ")
cat("\\hline \n")
df_sum_tex$tex %>% cat()

cat("\\hline \n")
cat("\\hline \n")
cat("Percent change & & \\\\ \n ")
cat("in distance & \\multicolumn{2}{c}{N (\\%)} \\\\ \n ")
cat("from modal route & \\multicolumn{2}{c}{Observations} \\\\ \n ")
cat("\\hline \n")
dev_amount_tex$tex %>% cat()

cat("\\hline \n ")
cat("\\end{tabular}")

sink()



# p_pc <- df_dev %>%
#   filter(diff_mode %in% T) %>%
#   ggplot() +
#   geom_histogram(aes(y = distance_m_pc)) +
#   facet_wrap(~source,
#              scales = "free_y") +
#   labs(y = "Percent change\nin distance\nfrom modal route",
#        x = "N observations",
#        title = "B. Percent change in distance from modal route when route is different from modal route") +
#   theme_classic2() +
#   theme(strip.background = element_blank(),
#         strip.text = element_text(face = "bold"),
#         axis.title.y = element_text(angle = 0, vjust = 0.5),
#         axis.text = element_text(color = "black"),
#         plot.title = element_text(face = "bold", size = 10))
# 
# p <- ggarrange(p_diff, p_pc, ncol = 1)
# 
# ggsave(p, 
#        filename = file.path(figures_dir, "deviation_hists.png"),
#        height = 4, width = 8)

# Regressions: Binary ----------------------------------------------------------
df <- df %>%
  mutate(gg_distance_km = gg_distance_m / 1000,
         mb_distance_km = mb_distance_m / 1000,
         
         gg_duration_in_traffic_min = gg_duration_in_traffic_s / 60,
         mb_duration_in_traffic_min = mb_duration_in_traffic_s / 60)

lm1 <- feols(gg_distance_km            ~ gg_diff_mode | uid, data = df) 
lm2 <- feols(gg_duration_in_traffic_min ~ gg_diff_mode | uid, data = df) 
lm3 <- feols(gg_speed_in_traffic_kmh  ~ gg_diff_mode | uid, data = df) 

lm4 <- feols(mb_distance_km            ~ mb_diff_mode | uid, data = df) 
lm5 <- feols(mb_duration_in_traffic_min ~ mb_diff_mode | uid, data = df) 
lm6 <- feols(mb_speed_in_traffic_kmh  ~ mb_diff_mode | uid, data = df) 

modelsummary_tab(list("Distance (km)" = lm1,
                      "Duration (min)" = lm2,
                      "Speed (km/h)" = lm3,
                      "Distance (km)" = lm4,
                      "Duration (min)" = lm5,
                      "Speed (km/h)" = lm6),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("gg_diff_modeTRUE" = "Route Different",
                              "mb_diff_modeTRUE" = "Route Different"),
                 gof_map = c("nobs", "adj.r.squared"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4, ~V5, ~V6,
                                    'Route FE', "Y", "Y", "Y", "Y", "Y", "Y",
                                    "Source", "Google", "Google", "Google", "Mapbox", "Mapbox", "Mapbox"),
                 output = file.path(tables_dir,
                                    "reg_route_dev.tex"))

# Regressions: Continuous ------------------------------------------------------
df <- df %>%
  mutate(diff_from_mode_km = (gg_distance_m - gg_distance_m_mode) / 1000)

lm1 <- feols(gg_speed_in_traffic_kmh    ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,]) 
lm2 <- feols(gg_duration_in_traffic_min ~ diff_from_mode_km | uid, data = df[df$gg_diff_mode %in% T,]) 
lm3 <- feols(gg_speed_in_traffic_kmh    ~ diff_from_mode_km | uid, data = df) 
lm4 <- feols(gg_duration_in_traffic_min ~ diff_from_mode_km | uid, data = df) 

modelsummary_tab(list("Speed (km/h)" = lm1,
                      "Duration (min)" = lm2,
                      "Speed (km/h)" = lm3,
                      "Duration (min)" = lm4),
                 stars = c('*' = .1, '**' = .05, "***" = 0.01),
                 coef_map = c("diff_from_mode_km" = "Distance difference from modal route (km)"),
                 gof_map = c("nobs", "adj.r.squared"),
                 escape = FALSE,
                 add_rows = tribble(~term, ~V1, ~V2, ~V3, ~V4,
                                    'Route FE', "Y", "Y", "Y", "Y", 
                                    "Include Modal Route Obs", "N", "N", "Y", "Y",
                                    "Source", "Google", "Google", "Google", "Google"),
                 output = file.path(tables_dir,
                                    "reg_route_continuous_dev.tex"))

# Speed check ------------------------------------------------------------------

p_theme <- theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = 8),
                 axis.title.x = element_text(size = 8),
                 plot.title = element_text(face = "bold", size = 10),
                 axis.text = element_text(color = "black"))

p_distance <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_distance_km = mean(gg_distance_km)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_distance_km) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = true - false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  labs(title = "A. Difference in average distance",
       x = "Avg distance modal route - Avg distance non-modal route",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

p_duration <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_duration_in_traffic_min = mean(gg_duration_in_traffic_min)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_duration_in_traffic_min) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = true - false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  labs(title = "B. Difference in average duration",
       x = "Avg duration modal route - Avg duration non-modal route",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

p_speed <- df %>%
  group_by(uid, gg_diff_mode) %>%
  dplyr::summarise(gg_speed_in_traffic_kmh = mean(gg_speed_in_traffic_kmh)) %>%
  ungroup() %>%
  pivot_wider(id_cols = uid,
              names_from = gg_diff_mode,
              values_from = gg_speed_in_traffic_kmh) %>%
  clean_names() %>%
  filter(!is.na(true)) %>%
  mutate(speed_diff = true - false) %>%
  
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_histogram(aes(x = speed_diff),
                 fill = "dodgerblue",
                 color = "black") +
  labs(title = "C. Difference in average speed",
       x = "Avg speed modal route - Avg speed non-modal route",
       y = "O-D\npair") +
  theme_classic2() +
  p_theme

p <- ggarrange(p_distance, p_duration, p_speed, nrow = 1)

p_a <- annotate_figure(p, 
                top = text_grob("Difference in indicators between typical and other routes, across O-D pairs", 
                                color = "black", face = "bold", size = 14))

ggsave(p_a, 
       filename = file.path(figures_dir, "dev_modal_nonmodal_diff.png"),
       height = 3, width = 10)

# Example ----------------------------------------------------------------------
# Longer duration but faster speed. Speed similar.



