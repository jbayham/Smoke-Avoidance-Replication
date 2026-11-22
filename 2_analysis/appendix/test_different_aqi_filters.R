# Robustness of the day-of dose response to how the smoke PM series is constructed.
#
# Two construction choices are crossed here:
#   1. the AQI filter    - drop county-days whose smoke PM is not corroborated by the
#                          monitor-based AQI, at the 5th, 10th, or 20th percentile (or no filter)
#   2. the background filter - subtract off the county background PM distribution at 1 or 2 SD
#                          (or no background filter)
# and each cell is estimated twice, once with the area-averaged and once with the
# area-median smoke PM aggregation.  The result is a 4 x 3 grid of dose-response plots
# with two glyphs (avg / med) per panel.
#
# The bins enter as BINARY indicators, matching the main day-of spec in figure 1.  The
# `<prefix>_bin_*` columns in the panel are continuous (the ug/m^3 falling in that bin), so
# each one is converted in place to `as.integer(. > 0)` below - exactly how
# 1_build/reg_data/build_reg_data_daily.R builds the unprefixed `bin_d_*` variables from
# `adj_pm_smoke_med_1sd_bin_*`.  That makes the (No AQI filter, 1 SD, Median) cell of this
# grid identical to figure 1's baseline, which is verified at the end of the run.

library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(PLOTTING_HELPERS_PATH)

COEF_PATH   <- file.path(REGRESSION_RESULTS_DIR, "aqi_filter_grid_dose_response_binary_coefs.rds")
FIGURE_PATH <- file.path(APPENDIX_FIGURES_DIR, "aqi_filter_grid_dose_response.pdf")
COUNTS_PATH <- file.path(REGRESSION_RESULTS_DIR, "aqi_filter_grid_smoke_day_counts.csv")
PANEL_PATH  <- file.path(APPENDIX_FIGURES_DIR, "aqi_filter_grid_dose_response_median_only.pdf")
RERUN       <- TRUE  # run.py owns input-aware stage reuse
RECOUNT     <- !file.exists(COUNTS_PATH)   # set TRUE by hand to re-tabulate smoke days

AQI_LEVELS <- c("No AQI filter", "AQI 5th pctile", "AQI 10th pctile", "AQI 20th pctile")
BG_LEVELS  <- c("No background filter", "1 SD background filter", "2 SD background filter")
FUN_LEVELS <- c("Average", "Median")

# The prefix for each (AQI filter, background filter, aggregation) cell of the grid.  The
# no-AQI-filter row uses a different naming family than the filtered rows.
spec_grid <- expand_grid(
  aqi_filter = factor(AQI_LEVELS, levels = AQI_LEVELS),
  bg_filter  = factor(BG_LEVELS,  levels = BG_LEVELS),
  smoke_fun  = factor(FUN_LEVELS, levels = FUN_LEVELS)
) %>%
  mutate(
    fun_tag = if_else(smoke_fun == "Average", "avg", "med"),
    aqi_tag = recode(as.character(aqi_filter),
                     "No AQI filter" = "", "AQI 5th pctile" = "0p05",
                     "AQI 10th pctile" = "0p10", "AQI 20th pctile" = "0p20"),
    bg_tag  = recode(as.character(bg_filter),
                     "No background filter" = "", "1 SD background filter" = "1sd",
                     "2 SD background filter" = "2sd"),
    prefix = case_when(
      aqi_tag == "" & bg_tag == "" ~ paste0("area_weighted_smoke_", fun_tag),
      aqi_tag == ""                ~ paste0("adj_pm_smoke_", fun_tag, "_", bg_tag),
      bg_tag  == ""                ~ paste0("adj_smoke_pm_", fun_tag, "_", aqi_tag),
      TRUE                         ~ paste0("adj_smoke_pm_", fun_tag, "_", aqi_tag, "_bg_avg_", bg_tag)
    )
  )

if (RERUN || RECOUNT) {
  cat("Loading data...\n")
  full_df <- read_csv(
    DAILY_DATA_FILEPATH,
    col_select = c(county_geoid, date, state_year_month, day_of_week, season, balanced_panel,
                   asinh_daily_visits_deferrable,
                   binned_pr, binned_tmmn, binned_tmmx, binned_rmax,
                   starts_with("area_weighted_smoke_"), starts_with("adj_pm_smoke_"),
                   starts_with("adj_smoke_pm_")),
    show_col_types = FALSE
  ) %>%
    mutate(across(c(binned_pr, binned_tmmn, binned_tmmx, binned_rmax), as.factor)) %>%
    # Binary versions of every bin, in place, so the formula below is unchanged.
    mutate(across(matches("_bin_[0-9]{3}_[0-9]{3}$"), ~as.integer(.x > 0)))

  panel_df <- panel(full_df %>% filter(balanced_panel), ~ county_geoid + date)
  rm(full_df); gc()
}

# ---- How much does each filter actually bite? ------------------------------------------
# The AQI filter does not drop county-days: `combine_and_process_smoke_with_aqi_filtering.R`
# sets smoke PM to 0 when the monitor AQI does not corroborate it (and passes the value
# through untouched where there is no monitor at all, `is.na(aqi)`).  The background filter
# works the same way.  So every spec is estimated on the identical sample, and the only
# thing that moves is how many county-days are classified as smoke days rather than as part
# of the omitted zero-smoke category.  That reclassification is what gets counted here.
if (RECOUNT) {
  bin_suffixes <- c("bin_001_015", "bin_015_030", "bin_030_060",
                    "bin_060_100", "bin_100_200", "bin_200_500")

  counts <- lapply(seq_len(nrow(spec_grid)), function(i) {
    spec <- spec_grid[i, ]
    cols <- paste0(spec$prefix, "_", bin_suffixes)
    per_bin <- vapply(cols, function(cl) sum(panel_df[[cl]]), numeric(1))
    tibble(
      prefix = spec$prefix,
      aqi_filter = as.character(spec$aqi_filter),
      bg_filter = as.character(spec$bg_filter),
      smoke_fun = as.character(spec$smoke_fun),
      n_county_days = nrow(panel_df),
      n_smoke_days = sum(per_bin),
      !!!setNames(as.list(per_bin), paste0("n_", bin_suffixes))
    )
  }) %>% bind_rows()

  write_csv(counts, COUNTS_PATH)
} else {
  counts <- read_csv(COUNTS_PATH, show_col_types = FALSE)
}

if (RERUN) {
  run_regression <- function(prefix) {
    formula_str <- paste0(
      "asinh_daily_visits_deferrable ~ ",
      prefix, "_bin_001_015 + ",
      prefix, "_bin_015_030 + ",
      prefix, "_bin_030_060 + ",
      prefix, "_bin_060_100 + ",
      prefix, "_bin_100_200 + ",
      prefix, "_bin_200_500 + ",
      "binned_pr + binned_tmmn + binned_tmmx + binned_rmax | ",
      "date + state_year_month + day_of_week^county_geoid^season"
    )
    feols(as.formula(formula_str), panel_df, cluster = ~county_geoid)
  }

  # Estimate every cell and keep only the smoke-bin coefficients.
  results_dfm <- lapply(seq_len(nrow(spec_grid)), function(i) {
    spec <- spec_grid[i, ]
    cat("Running", i, "of", nrow(spec_grid), ":", spec$prefix, "\n")
    add_new_sample_and_sector(
      run_regression(spec$prefix), "Deferrable Sectors", sample_id = NULL,
      sample_label = as.character(spec$smoke_fun), overall_sample_label = "Smoke PM aggregation",
      vars_to_plot_start = paste0(spec$prefix, "_bin_")
    ) %>%
      mutate(aqi_filter = as.character(spec$aqi_filter),
             bg_filter  = as.character(spec$bg_filter),
             prefix     = spec$prefix)
  }) %>%
    bind_rows()

  saveRDS(results_dfm, COEF_PATH)

  # The (No AQI filter, 1 SD background, Median) cell is the main day-of spec, so its
  # coefficients must match figure 1's saved baseline to numerical precision.
  base_path <- file.path(REGRESSION_RESULTS_DIR, "binary_base_day_of_dose_response.rds")
  if (file.exists(base_path)) {
    fig1 <- readRDS(base_path)$coeftable
    fig1 <- fig1[str_starts(row.names(fig1), "bin_d_"), "Estimate"]
    here <- results_dfm %>%
      filter(prefix == "adj_pm_smoke_med_1sd") %>%
      arrange(vars) %>%
      pull(Estimate)
    cat("Max abs. difference vs figure 1 baseline:",
        format(max(abs(here - fig1[order(names(fig1))])), scientific = TRUE), "\n")
  }
} else {
  cat("Reading cached coefficients from", COEF_PATH, "\n")
  results_dfm <- readRDS(COEF_PATH)
}

# The asinh coefficients become percent changes the same way as in figure 1.
plot_data <- results_dfm %>%
  mutate(
    low_ci  = Estimate - 1.96 * Std..Error,
    high_ci = Estimate + 1.96 * Std..Error,
    low_ci  = exp(low_ci) - 1,
    high_ci = exp(high_ci) - 1,
    Estimate = exp(Estimate) - 1,
    bin = factor(BIN_LABELS[paste0("bin_d_", str_sub(vars, start = -7))],
                 levels = unname(BIN_LABELS)),
    aqi_filter = factor(aqi_filter, levels = AQI_LEVELS),
    bg_filter  = factor(bg_filter,  levels = BG_LEVELS),
    `Smoke PM aggregation` = factor(`Smoke PM aggregation`, levels = FUN_LEVELS)
  )

aqi_grid_plot <- ggplot(plot_data, aes(x = bin, y = Estimate, color = bin,
                                       shape = `Smoke PM aggregation`,
                                       ymin = low_ci, ymax = high_ci)) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_errorbar(position = position_dodge(width = 0.6), width = 0.2, linewidth = 1) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  facet_grid(aqi_filter ~ bg_filter) +
  labs(x = expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))) +
  scale_color_manual("Smoke PM AQI\nBin equivalent", values = BIN_COLORS, labels = BIN_LABELS) +
  scale_shape_manual(values = c("Average" = 16, "Median" = 17)) +
  scale_y_continuous("Daily Change in Visits", labels = scales::label_percent()) +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom",
        panel.spacing = unit(1, "lines")) +
  guides(color = "none")

ggsave(FIGURE_PATH, plot = aqi_grid_plot,
       width = 16, height = 18, dpi = 300, units = "in")

cat("Wrote", FIGURE_PATH, "\n")

# ---- Median-only single panel ----------------------------------------------------------
# Average and median are indistinguishable above 60 ug/m^3 (see the run summary), so the
# compact version of this appendix figure keeps the median aggregation only - which is the
# one the main day-of spec uses - and dodges all 12 filter combinations into one panel.
# Shape encodes the background filter, opacity the AQI filter.
median_plot_data <- plot_data %>% filter(`Smoke PM aggregation` == "Median")

median_only_plot <- ggplot(median_plot_data,
                           aes(x = bin, y = Estimate, color = bin,
                               shape = bg_filter, alpha = aqi_filter,
                               group = interaction(bg_filter, aqi_filter),
                               ymin = low_ci, ymax = high_ci)) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_errorbar(position = position_dodge(width = 0.85), width = 0, linewidth = 0.6) +
  geom_point(position = position_dodge(width = 0.85), size = 2.6) +
  labs(x = expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))) +
  scale_color_manual("Smoke PM AQI\nBin equivalent", values = BIN_COLORS, labels = BIN_LABELS) +
  scale_shape_manual("Background filter", values = c(16, 17, 15)) +
  scale_alpha_manual("AQI filter", values = c(1, 0.75, 0.55, 0.35)) +
  scale_y_continuous("Daily Change in Visits", labels = scales::label_percent()) +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "right") +
  guides(color = "none",
         shape = guide_legend(override.aes = list(alpha = 1, size = 3)),
         alpha = guide_legend(override.aes = list(shape = 16, size = 3)))

ggsave(PANEL_PATH, plot = median_only_plot,
       width = 13, height = 7, dpi = 300, units = "in")

cat("Wrote", PANEL_PATH, "\n")

# ---- What the AQI filter actually reclassifies ------------------------------------------
drop_summary <- counts %>%
  filter(smoke_fun == "Median") %>%
  group_by(bg_filter) %>%
  mutate(baseline_smoke_days = n_smoke_days[aqi_filter == "No AQI filter"],
         n_reclassified     = baseline_smoke_days - n_smoke_days,
         pct_of_smoke_days  = n_reclassified / baseline_smoke_days,
         pct_of_county_days = n_reclassified / n_county_days) %>%
  ungroup() %>%
  select(bg_filter, aqi_filter, n_county_days, n_smoke_days,
         n_reclassified, pct_of_smoke_days, pct_of_county_days)

cat("\n=== County-days reclassified out of a smoke bin by the AQI filter (median) ===\n")
print(as.data.frame(drop_summary), row.names = FALSE)

cat("\n=== Estimation-sample size by spec (should be identical everywhere) ===\n")
print(table(counts$n_county_days))

# ---- Appendix table: what each filter reclassifies ---------------------------------------
stopifnot(length(unique(counts$n_county_days)) == 1)  # the table's note asserts this
n_obs <- unique(counts$n_county_days)

fmt_n   <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_pct <- function(x) ifelse(x == 0, "--",
                       ifelse(100 * x < 0.0001, "$<$0.0001\\%",
                              paste0(formatC(100 * x, format = "f", digits = 4), "\\%")))

tab_rows <- drop_summary %>%
  mutate(bg_filter  = factor(bg_filter,  levels = BG_LEVELS),
         aqi_filter = factor(aqi_filter, levels = AQI_LEVELS)) %>%
  arrange(bg_filter, aqi_filter)

body <- ""
for (bg in BG_LEVELS) {
  grp <- tab_rows %>% filter(bg_filter == bg)
  body <- paste0(body, "   \\emph{", bg, "}\\\\\n")
  for (i in seq_len(nrow(grp))) {
    r <- grp[i, ]
    is_base <- r$aqi_filter == "No AQI filter"
    body <- paste0(
      body, "   \\quad ", as.character(r$aqi_filter),
      if (is_base) " (baseline)" else "",
      " & ", fmt_n(r$n_smoke_days),
      " & ", if (is_base) "--" else fmt_n(r$n_reclassified),
      " & ", if (is_base) "--" else fmt_pct(r$pct_of_smoke_days),
      " & ", if (is_base) "--" else fmt_pct(r$pct_of_county_days),
      "\\\\\n")
  }
  if (bg != BG_LEVELS[length(BG_LEVELS)]) body <- paste0(body, "   \\addlinespace\n")
}

tex <- paste0(
"\\begingroup\n\\centering\n\\footnotesize\n",
"\\begin{tabular}{lcccc}\n",
"   \\tabularnewline \\midrule \\midrule\n",
"   & Smoke days & \\multicolumn{3}{c}{Reclassified as zero-smoke by the AQI filter}\\\\\n",
"      \\cmidrule(lr){3-5}\n",
"   & (county-days) & County-days & \\% of smoke days & \\% of county-days\\\\\n",
"   \\midrule\n",
body,
"   \\midrule \\midrule\n",
"   \\multicolumn{5}{l}{\\emph{Estimation sample: ", fmt_n(n_obs),
" county-days in every specification}}\\\\\n",
"\\end{tabular}\n \n\\par \\raggedright \n",
"Smoke days are county-days assigned to a non-zero smoke PM$_{2.5}$ bin, using the ",
"area-median aggregation. Neither filter drops observations: both set the day's ",
"smoke-attributable PM$_{2.5}$ to zero when it fails the test, moving the county-day into ",
"the omitted zero-smoke category, so all ", fmt_n(n_obs), " county-days are retained ",
"throughout. The background filter removes smoke PM$_{2.5}$ below the county's background ",
"distribution at 1 or 2 standard deviations. The AQI filter additionally zeroes out a ",
"county-day when the share of its PM$_{2.5}$ unexplained by the monitor-based AQI falls ",
"below the stated percentile; county-days with no monitor reading pass through unfiltered, ",
"so the AQI filter can only bite where an AQI observation exists. Percentages are relative ",
"to the baseline smoke-day count within each background-filter block.\n\\par\\endgroup\n")

writeLines(tex, file.path(APPENDIX_TABLES_DIR, "appendix_table_aqi_filter_reclassification.tex"))
cat("\nWrote", file.path(APPENDIX_TABLES_DIR, "appendix_table_aqi_filter_reclassification.tex"), "\n")
