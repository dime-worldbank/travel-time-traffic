# Route Distance

mb_df <- readRDS(file.path(analysis_data_dir, "mapbox_routes.Rds"))

mb_df <- mb_df %>%
  dplyr::filter(!is.na(distance_m))

# Persistent change ------------------------------------------------------------
p_dist_trend <- mb_df %>%
  dplyr::filter(!is.na(distance_m)) %>%
  dplyr::mutate(dist_diff = (distance_m != distance_m_mode),
                uid = paste0("Route ID: ", uid),
                uid = uid %>%
                  factor(labels = c("Route ID: 1",
                                    "Route ID: 2",
                                    "Route ID: 3",
                                    "Route ID: 4",
                                    "Route ID: 5",
                                    "Route ID: 6",
                                    "Route ID: 7",
                                    "Route ID: 8",
                                    "Route ID: 9",
                                    "Route ID: 10",
                                    "Route ID: 11",
                                    "Route ID: 12",
                                    "Route ID: 13",
                                    "Route ID: 14",
                                    "Route ID: 15",
                                    "Route ID: 16",
                                    "Route ID: 17",
                                    "Route ID: 18",
                                    "Route ID: 19",
                                    "Route ID: 20",
                                    "Route ID: 21",
                                    "Route ID: 22",
                                    "Route ID: 23",
                                    "Route ID: 24",
                                    "Route ID: 25",
                                    "Route ID: 26"))) %>%
  ggplot() +
  geom_col(aes(x = datetime,
               y = distance_m/1000,
               fill = dist_diff)) +
  facet_wrap(~uid) +
  scale_fill_manual(values = c("gray20", "red")) +
  labs(x = NULL,
       y = "Distance (km)",
       fill = "Different\nfrom Modal\nRoute") +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        strip.background = element_blank())

ggsave(p_dist_trend, 
       filename = file.path(figures_dir, "mb_route_dist_trend.png"),
       height = 8, width = 15.3)

# Route level indicators -------------------------------------------------------

mb_agg_df <- mb_df %>%
  dplyr::filter(!(uid %in% c(18, 25))) %>%
  dplyr::mutate(dist_diff = (distance_m != distance_m_mode),
                dist_pc = (distance_m - distance_m_mode)/distance_m_mode*100,
                dist_pc_abs = abs(dist_pc)) %>%
  group_by(uid, distance_m_mode) %>%
  dplyr::summarise(n_obs = n(),
                   dist_pc_abs_mean = mean(dist_pc_abs),
                   dist_pc_abs_when_diff_mean = mean(dist_pc_abs[dist_diff == T]),
                   n_diff = sum(dist_diff),
                   dist_pc_max = max(dist_pc),
                   distance_m_min = min(distance_m),
                   distance_m_max = max(distance_m)) %>%
  ungroup() %>%
  dplyr::mutate(prop_diff = n_diff / n_obs,
                distance_m_min_pc = (distance_m_min - distance_m_mode)/distance_m_mode * 100,
                distance_m_max_pc = (distance_m_max - distance_m_mode)/distance_m_mode * 100) 

# Figure: Percent distances that are different ---------------------------------
per_diff_levels <- c(
  "0%",
  ">0% - 1%",
  "1 - 5%",
  "5 - 10%",
  "10 - 15%",
  "15 - 20%",
  "20 - 25%",
  "25 - 30%",
  "30 - 35%",
  "35 - 40%",
  "40 - 45%"
)

p1 <- mb_agg_df %>%
  mutate(per_diff = prop_diff * 100) %>%
  mutate(
    per_diff_cat = case_when(
      per_diff == 0  ~ "0%",
      per_diff <= 1  ~ ">0% - 1%",
      per_diff <= 5  ~ "1 - 5%",
      per_diff <= 10 ~ "5 - 10%",
      per_diff <= 15 ~ "10 - 15%",
      per_diff <= 20 ~ "15 - 20%",
      per_diff <= 25 ~ "20 - 25%",
      per_diff <= 30 ~ "25 - 30%",
      per_diff <= 35 ~ "30 - 35%",
      per_diff <= 40 ~ "35 - 40%",
      per_diff <= 45 ~ "40 - 45%"
    )
  ) %>%
  group_by(per_diff_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(
    per_diff_cat = per_diff_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    per_diff_cat = factor(per_diff_cat, levels = per_diff_levels),
    per = n / sum(n) * 100,
    per_str = paste0(round(per, 1), "%"),
    label = ifelse(n == 0, "", paste0(n, "\n(", per_str, ")"))
  ) %>%
  ggplot(aes(x = per_diff_cat, y = n)) +
  geom_col() +
  geom_text(aes(label = label), nudge_y = 0.6, size = 3.5) +
  labs(
    x = "Percent of trips differing from modal route",
    y = "Number of Routes",
    title = "A. Percent of times route differs\nfrom modal route",
  ) +
  theme_classic2() +
  theme(axis.text.x = element_text(color = "black", size = 10, angle = 45, vjust = 0.5),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"))

# Figure: When different, average percent change -------------------------------
# Define bins
dist_levels <- c(
  "<1%",
  "1–10%",
  "10–20%",
  "20–30%",
  "30–40%",
  "40–50%",
  "50–60%",
  "60–70%",
  "70–80%",
  "80–90%",
  "90–100%"
)

p2 <- mb_agg_df %>%
  filter(!is.na(dist_pc_abs_when_diff_mean)) %>%
  mutate(
    dist_pc = dist_pc_abs_when_diff_mean,
    dist_cat = case_when(
      dist_pc < 1   ~ "<1%",
      dist_pc <= 10 ~ "1–10%",
      dist_pc <= 20 ~ "10–20%",
      dist_pc <= 30 ~ "20–30%",
      dist_pc <= 40 ~ "30–40%",
      dist_pc <= 50 ~ "40–50%",
      dist_pc <= 60 ~ "50–60%",
      dist_pc <= 70 ~ "60–70%",
      dist_pc <= 80 ~ "70–80%",
      dist_pc <= 90 ~ "80–90%",
      dist_pc <= 100 ~ "90–100%"
    )
  ) %>%
  group_by(dist_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(
    dist_cat = dist_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    dist_cat = factor(dist_cat, levels = dist_levels),
    per = n / sum(n) * 100,
    per_str = paste0(round(per, 1), "%"),
    label = ifelse(n == 0, "", paste0(n, "\n(", per_str, ")"))
  ) %>%
  ggplot(aes(x = dist_cat, y = n)) +
  geom_col() +
  geom_text(aes(label = label), nudge_y = 0.65, size = 3.5) +
  labs(
    x = "% Route Length Difference",
    y = "Number of Routes",
    title = "B. Average percent difference in route distance",
    subtitle = "Among instances where route differs from modal route"
  ) +
  theme_classic2() +
  theme(axis.text.x = element_text(color = "black", size = 10, angle = 45, vjust = 0.5),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"))

# Figure: When different, average percent change -------------------------------
# Define bins
dist_levels <- c(
  "<1%",
  "1–10%",
  "10–20%",
  "20–30%",
  "30–40%",
  "40–50%",
  "50–60%",
  "60–70%",
  "70–80%",
  "80–90%",
  "90–100%"
)

p3 <- mb_agg_df %>%
  dplyr::filter(dist_pc_max != 0) %>%
  mutate(
    dist_pc = dist_pc_max,
    dist_cat = case_when(
      dist_pc < 1   ~ "<1%",
      dist_pc <= 10 ~ "1–10%",
      dist_pc <= 20 ~ "10–20%",
      dist_pc <= 30 ~ "20–30%",
      dist_pc <= 40 ~ "30–40%",
      dist_pc <= 50 ~ "40–50%",
      dist_pc <= 60 ~ "50–60%",
      dist_pc <= 70 ~ "60–70%",
      dist_pc <= 80 ~ "70–80%",
      dist_pc <= 90 ~ "80–90%",
      dist_pc <= 100 ~ "90–100%"
    )
  ) %>%
  group_by(dist_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(
    dist_cat = dist_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    dist_cat = factor(dist_cat, levels = dist_levels),
    per = n / sum(n) * 100,
    per_str = paste0(round(per, 1), "%"),
    label = ifelse(n == 0, "", paste0(n, "\n(", per_str, ")"))
  ) %>%
  ggplot(aes(x = dist_cat, y = n)) +
  geom_col() +
  geom_text(aes(label = label), nudge_y = 0.5, size = 3.5) +
  labs(
    x = "% Route Length Difference",
    y = "Number of Routes",
    title = "C. Maximum percent difference in route distance",
    subtitle = "Among instances where route differs from modal route"
  ) +
  theme_classic2() +
  theme(axis.text.x = element_text(color = "black", size = 10, angle = 45, vjust = 0.5),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"))

p <- ggarrange(p1, p2, p3, nrow = 1)
ggsave(p, 
       filename = file.path(figures_dir, "mb_route_change_hist.png"),
       height = 5, width = 15.3)

