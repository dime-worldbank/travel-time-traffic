# Delay Factor Exceedance Thresholds
#
# Share of observations exceeding a range of observed delay factor
# thresholds, separately for the calibration sample and the long-panel
# sample. Complements the discussion of Figure
# \ref{fig:observed_vs_predicted_delay_main} (main text), which notes that
# prediction error grows with congestion severity but that only a small
# share of observations reach the most severe congestion levels.

# Load data --------------------------------------------------------------------
route_cal_df <- readRDS(file.path(extracted_data_dir, "data_for_calibration", "google_traffic_tt.Rds"))
route_26_df  <- readRDS(file.path(analysis_data_dir, "google_routes.Rds"))

## Observed delay factor: calibration sample -------------------------------------
route_cal_df <- route_cal_df %>%
  group_by(uid) %>%
  dplyr::mutate(duration_in_traffic_s_minimum = duration_in_traffic_s[hour %in% 1:4] %>%
                  quantile(0.01, na.rm = T) %>%
                  as.numeric()) %>%
  ungroup() %>%
  dplyr::mutate(delay_factor_od = (duration_in_traffic_s - duration_in_traffic_s_minimum) / duration_in_traffic_s_minimum + 1)

## Observed delay factor: long-panel sample --------------------------------------
# Already computed upstream (01_clean_data/05_clean_data_routes.R), using the
# same definition (1st percentile of duration between 1am and 4am as free-flow).

# Exceedance table ---------------------------------------------------------------
thresholds <- c(1.05, 1.10, 1.15, 1.20, seq(1.25, 5, by = 0.25))

exceedance_df <- tibble(threshold = thresholds) %>%
  dplyr::mutate(
    n_calibration   = sum(!is.na(route_cal_df$delay_factor_od)),
    n_longpanel     = sum(!is.na(route_26_df$delay_factor_od)),
    pct_calibration = purrr::map_dbl(threshold, ~ mean(route_cal_df$delay_factor_od > .x, na.rm = TRUE)) * 100,
    pct_longpanel   = purrr::map_dbl(threshold, ~ mean(route_26_df$delay_factor_od > .x, na.rm = TRUE)) * 100
  )

print(exceedance_df, n = Inf)

# Export ---------------------------------------------------------------------------
sink(file.path(tables_dir, "delay_factor_exceedance_thresholds.tex"))
cat("\\begin{tabular}{l | rr} \n")
cat("\\hline \n")
cat("\\shortstack[l]{Observed Delay\\\\Factor Threshold} & \\shortstack{Calibration Sample\\\\(\\% Exceeding)} & \\shortstack{Long-Panel Sample\\\\(\\% Exceeding)} \\\\ \n")
cat("\\hline \n")
exceedance_df %>%
  dplyr::mutate(tex = paste0("$>$", sprintf("%.2f", threshold), " & ",
                              sprintf("%.2f", pct_calibration), "\\% & ",
                              sprintf("%.2f", pct_longpanel), "\\% \\\\ \n")) %>%
  dplyr::pull(tex) %>%
  cat()
cat("\\hline \n")
cat("\\end{tabular}")
sink()
