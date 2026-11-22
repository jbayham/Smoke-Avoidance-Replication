library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(PLOTTING_HELPERS_PATH)

CONSTANT_TO_ENSURE_NO_ZERO_OR_NEGATIVE_VALUES <- 15

# The two tails compared below. The table reports the move from the 5th to the
# 95th percentile of the within-county smoke SD among the county-days that
# actually have smoke variation.
LOWER_PCTILE <- 0.05
UPPER_PCTILE <- 0.95
LOWER_LABEL  <- sprintf("%.0f%%", 100 * LOWER_PCTILE)
UPPER_LABEL  <- sprintf("%.0f%%", 100 * UPPER_PCTILE)

full_df <- read_csv(
  DAILY_DATA_FILEPATH,
  col_select = c(county_geoid, date, state_year_month, day_of_week, season,
                 balanced_panel, asinh_daily_visits_deferrable,
                 bin_d_001_015, bin_d_015_030, bin_d_030_060,
                 bin_d_060_100, bin_d_100_200, bin_d_200_500,
                 binned_pr, binned_tmmn, binned_tmmx, binned_rmax,
                 within_daily_smoke_var_med, within_daily_smoke_var_avg),
  show_col_types = FALSE
) %>%
  mutate(
    across(c(binned_pr, binned_tmmn, binned_tmmx, binned_rmax), ~as.factor(.), .names = "{.col}"),
    within_daily_smoke_sd_med_log =if_else(within_daily_smoke_var_med ==0, NA, log(sqrt(within_daily_smoke_var_med))),
    within_daily_smoke_sd_avg_log =if_else(within_daily_smoke_var_avg ==0, NA, log(sqrt(within_daily_smoke_var_avg))),
    within_daily_smoke_sd_med_log_adj =if_else(within_daily_smoke_var_med ==0, 0, within_daily_smoke_sd_med_log + CONSTANT_TO_ENSURE_NO_ZERO_OR_NEGATIVE_VALUES),
    within_daily_smoke_sd_avg_log_adj =if_else(within_daily_smoke_var_avg ==0, 0, within_daily_smoke_sd_avg_log + CONSTANT_TO_ENSURE_NO_ZERO_OR_NEGATIVE_VALUES)
  )
# adding 15 to the logged values makes sure that the observations with variance 
# are seperated from the smoke free-observations when we take the log
# > min(log(sqrt(full_df$within_daily_smoke_var_avg[full_df$within_daily_smoke_var_avg > 0])), na.rm=T)
# [1] -12.27019
# > min(log(sqrt(full_df$within_daily_smoke_var_med[full_df$within_daily_smoke_var_med > 0])), na.rm=T)
# [1] -12.22363


# Balanced panel only, so that these columns sit on the same estimation sample as
# the main day-of dose response (2_analysis/figure_1_2_day_of_analysis_run.R) that
# they are tabled against.
panel_df <- panel(full_df %>% filter(balanced_panel), ~ county_geoid + date)

# Re-run with binary versions of the bin variables on the base dataframe

est_bin_base_variance_median <- feols(asinh_daily_visits_deferrable ~
                                        bin_d_001_015 + 
                                        bin_d_015_030 +
                                        bin_d_030_060 + 
                                        bin_d_060_100 +
                                        bin_d_100_200 + 
                                        bin_d_200_500 +
                                        within_daily_smoke_sd_med_log_adj +
                                        binned_pr + binned_tmmn + binned_tmmx + binned_rmax |
                                        date + state_year_month + day_of_week^county_geoid^season,
                                      panel_df, cluster = ~county_geoid)


est_bin_base_variance_avg <- feols(asinh_daily_visits_deferrable ~
                                     bin_d_001_015 + 
                                     bin_d_015_030 +
                                     bin_d_030_060 + 
                                     bin_d_060_100 +
                                     bin_d_100_200 + 
                                     bin_d_200_500 +
                                     within_daily_smoke_sd_avg_log_adj + 
                                     binned_pr + binned_tmmn + binned_tmmx + binned_rmax |
                                     date + state_year_month + day_of_week^county_geoid^season,
                                   panel_df, cluster = ~county_geoid)


# Quantiles are taken over the estimation sample, and only over the county-days
# with non-zero within-county smoke variance (the `_log` column is NA exactly
# where the variance is zero, i.e. the smoke-free days that get the 0 floor).
est_sample <- panel_df
results_avg <- quantile(est_sample$within_daily_smoke_sd_avg_log_adj[!is.na(est_sample$within_daily_smoke_sd_avg_log)],
                        c(LOWER_PCTILE, 0.25, 0.75, UPPER_PCTILE), na.rm=T)
results_med <- quantile(est_sample$within_daily_smoke_sd_med_log_adj[!is.na(est_sample$within_daily_smoke_sd_med_log)],
                        c(LOWER_PCTILE, 0.25, 0.75, UPPER_PCTILE), na.rm=T)

coefficient_on_within_sd_avg <- est_bin_base_variance_avg$coefficients[['within_daily_smoke_sd_avg_log_adj']]
se_on_within_sd_avg <- est_bin_base_variance_avg$coeftable['within_daily_smoke_sd_avg_log_adj', 'Std. Error']
change_from_05_to_95_avg <- (results_avg[[UPPER_LABEL]] - results_avg[[LOWER_LABEL]])
#95p in PM units 
sd_95_percentile_in_pm_units_avg <- exp(results_avg[[UPPER_LABEL]] - CONSTANT_TO_ENSURE_NO_ZERO_OR_NEGATIVE_VALUES)

#5p in PM units 
sd_05_percentile_in_pm_units_avg <- exp(results_avg[[LOWER_LABEL]] - CONSTANT_TO_ENSURE_NO_ZERO_OR_NEGATIVE_VALUES)

change_from_05_to_95_in_variance_effect_on_visits_avg <- change_from_05_to_95_avg*coefficient_on_within_sd_avg


coefficient_on_within_sd_med <- est_bin_base_variance_median$coefficients[['within_daily_smoke_sd_med_log_adj']]
se_on_within_sd_med <- est_bin_base_variance_median$coeftable['within_daily_smoke_sd_med_log_adj', 'Std. Error']
#95p in PM units 
sd_95_percentile_in_pm_units_med <- exp(results_med[[UPPER_LABEL]] - CONSTANT_TO_ENSURE_NO_ZERO_OR_NEGATIVE_VALUES)

#5p in PM units 
sd_05_percentile_in_pm_units_med <- exp(results_med[[LOWER_LABEL]] - CONSTANT_TO_ENSURE_NO_ZERO_OR_NEGATIVE_VALUES)
change_from_05_to_95_med <- (results_med[[UPPER_LABEL]] - results_med[[LOWER_LABEL]])
change_from_05_to_95_in_variance_effect_on_visits_med <- change_from_05_to_95_med*coefficient_on_within_sd_med

results_csv <- tribble(
  ~background_pm_smoke_threshold_estimate_fn, ~coefficient,                 ~standard_error,     ~sd_95_percentile_in_pm_units,    ~sd_05_percentile_in_pm_units,    ~change_from_05_to_95_reg_units, ~effect_on_visits_of_05_to_95_change,
  "median",                                   coefficient_on_within_sd_med, se_on_within_sd_med, sd_95_percentile_in_pm_units_med, sd_05_percentile_in_pm_units_med, change_from_05_to_95_med,        change_from_05_to_95_in_variance_effect_on_visits_med,
  "average",                                  coefficient_on_within_sd_avg, se_on_within_sd_avg, sd_95_percentile_in_pm_units_avg, sd_05_percentile_in_pm_units_avg, change_from_05_to_95_avg,        change_from_05_to_95_in_variance_effect_on_visits_avg
)

write_csv(results_csv, file.path(APPENDIX_TABLES_DIR, 'within_county_smoke_variance_results.csv'))


# ==============================================================================
# LaTeX table for the appendix
#
# Three columns: the main day-of dose response, and the same specification with
# the within-county log SD of smoke PM added, once with the background-PM
# threshold set by the county median and once by the county average. Both SD
# variables carry the same dict label so etable collapses them onto one row.
# ==============================================================================

est_bin_base <- readRDS(file.path(REGRESSION_RESULTS_DIR, "binary_base_day_of_dose_response.rds"))

etable_dict <- c(
  bin_d_001_015 = "Smoke Day (0, 15] \\textmu $g$ PM$_{2.5}$",
  bin_d_015_030 = "Smoke Day (15, 30] \\textmu $g$ PM$_{2.5}$",
  bin_d_030_060 = "Smoke Day (30, 60] \\textmu $g$ PM$_{2.5}$",
  bin_d_060_100 = "Smoke Day (60, 100] \\textmu $g$ PM$_{2.5}$",
  bin_d_100_200 = "Smoke Day (100, 200] \\textmu $g$ PM$_{2.5}$",
  bin_d_200_500 = "Smoke Day (200, 500] \\textmu $g$ PM$_{2.5}$",
  within_daily_smoke_sd_med_log_adj = "Within-County log SD of Smoke PM$_{2.5}$",
  within_daily_smoke_sd_avg_log_adj = "Within-County log SD of Smoke PM$_{2.5}$",
  asinh_daily_visits_deferrable = "asinh(Deferrable Daily Visits)",
  county_geoid = "County",
  date = "Day",
  state_year_month = "State-Year-Month",
  day_of_week = "Day of Week",
  season = "Season",
  notes = "Clustered (County) standard-errors in parentheses"
)

variance_models  <- list(est_bin_base, est_bin_base_variance_median, est_bin_base_variance_avg)
variance_headers <- c("Main Specification",
                      "SD, County-Median Background PM",
                      "SD, County-Average Background PM")

fmt_pm  <- function(x) sprintf("%.3f", x)
fmt_pct <- function(x) sprintf("%+.3f\\%%", 100 * x)

# list() cannot take a computed expression as a name, so the labels are attached
# after the fact.
variance_extralines <- list(
  c("--", fmt_pm(sd_05_percentile_in_pm_units_med), fmt_pm(sd_05_percentile_in_pm_units_avg)),
  c("--", fmt_pm(sd_95_percentile_in_pm_units_med), fmt_pm(sd_95_percentile_in_pm_units_avg)),
  c("--", fmt_pm(change_from_05_to_95_med), fmt_pm(change_from_05_to_95_avg)),
  c("--", fmt_pct(change_from_05_to_95_in_variance_effect_on_visits_med),
    fmt_pct(change_from_05_to_95_in_variance_effect_on_visits_avg))
)
names(variance_extralines) <- c(
  paste0("SD at ", LOWER_LABEL, " (\\textmu $g$ PM$_{2.5}$)"),
  paste0("SD at ", UPPER_LABEL, " (\\textmu $g$ PM$_{2.5}$)"),
  paste0(LOWER_LABEL, " $\\to$ ", UPPER_LABEL, " change (log SD units)"),
  paste0("Effect on visits of ", LOWER_LABEL, " $\\to$ ", UPPER_LABEL)
)

# Counties that sit entirely inside one smoke grid cell have a within-county
# variance of NA (var() of a single cell), so columns 2-3 drop them for the whole
# panel. These are all small Virginia independent cities.
n_single_cell_counties <- n_distinct(panel_df$county_geoid[is.na(panel_df$within_daily_smoke_var_med)])
n_dropped_county_days  <- nobs(est_bin_base) - nobs(est_bin_base_variance_median)

variance_notes <- paste(
  "The within-county SD of smoke PM$_{2.5}$ enters as $\\log(\\mathrm{SD})$ shifted by",
  paste0(CONSTANT_TO_ENSURE_NO_ZERO_OR_NEGATIVE_VALUES, ","),
  "which keeps every county-day with smoke variation strictly positive; smoke-free county-days (zero variance) are set to zero and so sit below all of them.",
  "Percentiles are taken over the estimation sample, among county-days with non-zero smoke variance.",
  "The effect on visits is the coefficient times the change in log SD units, in asinh (approximately log) points.",
  "Columns 2 and 3 have fewer observations than column 1 because the within-county SD is undefined for counties that fall entirely within a single smoke PM$_{2.5}$ grid cell;",
  sprintf("these %d counties, all small Virginia independent cities, account for the %s dropped county-days.",
          n_single_cell_counties, format(n_dropped_county_days, big.mark = ","))
)

etable(variance_models,
       dict    = etable_dict,
       headers = variance_headers,
       keep    = "Smoke Day|Within-County",
       group   = list("Flexible Weather Controls" = "binned_"),
       extralines = variance_extralines)

etable(variance_models,
       dict    = etable_dict,
       headers = variance_headers,
       keep    = "Smoke Day|Within-County",
       group   = list("Flexible Weather Controls" = "binned_"),
       extralines = variance_extralines,
       notes   = variance_notes,
       file    = file.path(APPENDIX_TABLES_DIR, "appendix_table_within_county_smoke_variance.tex"),
       tex     = TRUE,
       replace = TRUE)

cat("\nWrote", file.path(APPENDIX_TABLES_DIR, "appendix_table_within_county_smoke_variance.tex"), "\n")
