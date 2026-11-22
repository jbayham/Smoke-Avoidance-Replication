library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# Main specification plus county-by-month-of-sample and state-by-day-of-sample
# fixed effects added together. State-by-day nests both
# the day and state-year-month fixed effects of the main spec; they are kept in
# the formula so every column shares the main spec's FE list, and fixest drops
# the redundant ones.

full_df <- read_csv(
  DAILY_DATA_FILEPATH,
  col_select = c(county_geoid, date, state_year_month,
                 day_of_week, season,
                 asinh_daily_visits_deferrable, balanced_panel,
                 bin_d_001_015, bin_d_015_030, bin_d_030_060,
                 bin_d_060_100, bin_d_100_200, bin_d_200_500,
                 binned_pr, binned_tmmn, binned_tmmx, binned_rmax),
  show_col_types = FALSE
) %>%
mutate(
  # Approved reproduction-plan correction: full calendar date, not day of month.
  # Build these keys here so the historical daily panel and baseline stay intact.
  year_month = format(date, '%Y-%m'),
  state_day = paste(substr(county_geoid, 1, 2), format(date, '%Y-%m-%d'), sep = ':'),
  across(c(binned_pr, binned_tmmn, binned_tmmx, binned_rmax), ~as.factor(.), .names = "{.col}")
) %>%
filter(balanced_panel)

stopifnot(n_distinct(full_df$state_day) ==
            nrow(distinct(full_df, state = substr(county_geoid, 1, 2), date)))

panel_df <- panel(full_df, ~ county_geoid + date)

base_formula <- paste(
  "asinh_daily_visits_deferrable ~ bin_d_001_015 + bin_d_015_030 + bin_d_030_060 +",
  "bin_d_060_100 + bin_d_100_200 + bin_d_200_500 +",
  "binned_pr + binned_tmmn + binned_tmmx + binned_rmax"
)
main_fe <- "date + state_year_month + day_of_week^county_geoid^season"

fe_list <- c(
  main = main_fe,
  both = paste(main_fe, "+ county_geoid^year_month + state_day")
)

fe_models <- map(fe_list, function(fe) {
  est <- feols(as.formula(paste(base_formula, "|", fe)), panel_df, cluster = ~county_geoid)
  print(summary(est))
  est
})

saveRDS(fe_models, file.path(REGRESSION_RESULTS_DIR, "robustness_additional_fixed_effects.rds"))


# ==============================================================================
# LaTeX table for the appendix
# ==============================================================================

etable_dict <- c(
  bin_d_001_015 = "Smoke Day (0, 15]",
  bin_d_015_030 = "Smoke Day (15, 30]",
  bin_d_030_060 = "Smoke Day (30, 60]",
  bin_d_060_100 = "Smoke Day (60, 100]",
  bin_d_100_200 = "Smoke Day (100, 200]",
  bin_d_200_500 = "Smoke Day (200, 500]",
  asinh_daily_visits_deferrable = "asinh(Deferrable Daily Visits)",
  county_geoid = "County",
  date = "Day",
  state_year_month = "State-Year-Month",
  state_day = "State-Day",
  year_month = "Month of Sample",
  day_of_week = "Day of Week",
  season = "Season",
  notes = "Clustered (County) standard-errors in parentheses"
)


etable(unname(fe_models),
       dict  = etable_dict,
       keep  = "%^bin_d_",
       group = list("Flexible Weather Controls" = "binned_"))

etable(unname(fe_models),
       dict    = etable_dict,
       keep    = "%^bin_d_",
       group   = list("Flexible Weather Controls" = "binned_"),
       file    = file.path(APPENDIX_TABLES_DIR, "appendix_table_additional_fixed_effects.tex"),
       tex     = TRUE,
       replace = TRUE)

cat("\nWrote", file.path(APPENDIX_TABLES_DIR, "appendix_table_additional_fixed_effects.tex"), "\n")
