################################################################################
# Is the weekly spend-on-visits slope (table_2_with_visits_and_deflated_spend_
# run_and_output.R) attenuated? Two checks, each run on all places and on the
# places in both panels ("matched"), from the output of
# 1_build/daily_consumer_data/add_daily_matched_poi_spend_and_visits_by_county.R:
#
#   1. Matched places. If the slope rises when spend and visits cover the same
#      places, the gap between the panels' coverage was flattening it.
#   2. Split-sample IV. Places are split into two fixed random halves. Spend of
#      half k is regressed on visits of half k, instrumented with visits of the
#      other half, and the two stacks are pooled (ORIV, Gillen, Snowberg & Yariv
#      2019). Sampling noise in visits is independent across halves, so this
#      removes the attenuation it causes. Coverage gaps are shared by both
#      halves, so this does not remove them; comparing IV on all vs matched
#      places separates the two. Spend and visits come from the same half so
#      real place-level co-movement in the other half doesn't inflate the IV.
#
# A coverage gap need not flatten the slope: if visits-only places (parks) move
# with spend-generating activity, all-places visits understate that activity's
# swings and the all-places slope can sit above the matched one.
#
# Weather controls are counts of days in each weather bin (see CONTROLS below),
# replacing the levels and squares used in the table 2 script.
#
# The weekly matched-POI spend and visits read here are built in build_reg_data_weekly.R and
# merged into the weekly reg data (WEEKLY_DATA_FILEPATH) as the mpoi_* columns: all-places halves
# mpoi_spend_h1/h2 and mpoi_visits_h1/h2, and the matched-places halves mpoi_matched_*. Spend is
# already in 2017 dollars, Nov 30 / 2023-01-01 (no visits in the foot-traffic data) are already
# dropped, and the weather-bin controls, state-month, season and week-of-month sit in the same
# file, keyed to the Tuesday-Monday weeks of that build. Here we unstack those columns back into
# the two samples ("all places", "matched places") the estimators run on.
#
# Every estimator runs under the table 2 fixed effects: county, week, state-month,
# and county x season x week-of-month, on whole weeks, as in the table 2 script.
################################################################################
library(conflicted)
library(tidyverse)
library(data.table)
library(fixest)
library(arrow)
library(glue)
conflicts_prefer(dplyr::filter, dplyr::between, lubridate::isoweek, lubridate::isoyear, lubridate::mday)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# all-places OLS and split-sample IV, with first-stage statistics
TABLE_PATH   <- file.path(APPENDIX_TABLES_DIR, 'appendix_table_spend_on_visits_ols_and_split_sample_iv_asinh.tex')

# Weather controls: days in each quintile bin of precipitation, min and max
# temperature, and max humidity, built in build_reg_data_weekly.R. Bin 0 of
# precipitation (no rain) is the omitted category; the other four sets sum to the
# number of days, so fixest drops one count per set.
CONTROLS <- as.vector(outer(1:5, c('pr', 'tmmn', 'tmmx', 'rmax'), \(k, v) paste0('n_days_weather_', k, '_', v)))

SPECS <- list(
  'table 2 FEs' = list(fes = c('week_and_year', 'state_year_month', 'season^county_geoid^week_of_month'))
)

# ---- weekly reg data: the mpoi_* matched-POI columns plus the FE keys and weather controls,
# built in build_reg_data_weekly.R (already in 2017 dollars, missing-visit days dropped).
# County FIPS are identifiers. Preserve leading zeros and the documented
# literal invalid key; the existing complete-case sample rules apply below.
weekly_df <- open_csv_dataset(WEEKLY_DATA_FILEPATH,
                              col_types = arrow::schema(
                                county_geoid = arrow::string(),
                                state_year_month = arrow::string(),
                                week_and_year = arrow::string())) %>%
  select(county_geoid, week_and_year, state_year_month, season, week_of_month, all_of(CONTROLS),
         starts_with('mpoi_')) %>%
  collect() %>%
  as.data.table()
weekly_df[, `:=`(county_geoid  = str_pad(as.character(county_geoid), 5, pad = '0'),
                 week_and_year = str_pad(as.character(week_and_year), 6, pad = '0'))]

# unstack the wide mpoi_* columns into the two samples, rebuilding the half columns the
# estimators expect (spend_h1/h2, visits_h1/h2). Keep only county-weeks with complete weather
# and matched-POI coverage (the FE keys and controls come from the weather side of the build).
KEEP_COLS <- c('county_geoid', 'week_and_year', 'state_year_month', 'season', 'week_of_month', CONTROLS)
make_sample <- function(sample_name, prefix) {
  cols <- paste0(prefix, c('spend_h1', 'spend_h2', 'visits_h1', 'visits_h2'))
  out  <- weekly_df[complete.cases(weekly_df[, c(KEEP_COLS, cols), with = FALSE]),
                    c(KEEP_COLS, cols), with = FALSE]
  setnames(out, cols, c('spend_h1', 'spend_h2', 'visits_h1', 'visits_h2'))
  out[, `:=`(sample = sample_name, spend = spend_h1 + spend_h2, visits = visits_h1 + visits_h2)][]
}
weekly <- rbind(make_sample('all places', 'mpoi_'), make_sample('matched places', 'mpoi_matched_'))
weekly_by_spec <- lapply(SPECS, \(spec) weekly)

message(glue('County-weeks with zero visits in a half (few places per half make asinh(x) noisy):'))
print(weekly_by_spec[['table 2 FEs']][, .(county_weeks = .N, pct_zero_visits_h1 = 100 * mean(visits_h1 == 0),
                                          pct_zero_visits_h2 = 100 * mean(visits_h2 == 0)), by = sample])

# stack the halves: in stack k the outcome and regressor come from half k and
# the instrument from the other half.
stack_halves <- function(wide) {
  keep <- intersect(c('county_geoid', 'week_and_year', 'ym', 'week_ym', 'state_year_month', 'season', 'week_of_month', CONTROLS), names(wide))
  rbind(wide[, c(.(stack = 1L, spend_half = spend_h1, visits_half = visits_h1, visits_other = visits_h2), .SD), .SDcols = keep],
        wide[, c(.(stack = 2L, spend_half = spend_h2, visits_half = visits_h2, visits_other = visits_h1), .SD), .SDcols = keep])
}

# The fixed effects are interacted with the stack indicator, so everything but the
# slope may differ by stack. The weather controls are not: both stacks cover the
# same counties and weeks, so stack-specific weather slopes add little, and 20
# controls interacted would double the regressors.

formulas_for <- function(fes) {
  fe   <- paste(fes, collapse = ' + ')
  sfe  <- paste(glue('{fes}^stack'), collapse = ' + ')
  ctrl <- paste(CONTROLS, collapse = ' + ')
  list(
    total    = as.formula(glue('asinh(spend) ~ asinh(visits) + {ctrl} | {fe}')),
    half_ols = as.formula(glue('asinh(spend_half) ~ asinh(visits_half) + {ctrl} | {sfe}')),
    half_iv  = as.formula(glue('asinh(spend_half) ~ {ctrl} | {sfe} | asinh(visits_half) ~ asinh(visits_other)'))
  )
}
formulas_by_spec <- lapply(SPECS, \(spec) formulas_for(spec$fes))

row_for <- function(fit, spec_name, sample_name, estimator, coef_name = 'asinh(visits)', se_override = NULL) {
  tibble(fixed_effects = spec_name, sample = sample_name, estimator = as.character(estimator), estimate = unname(coef(fit)[coef_name]),
         se = if (is.null(se_override)) unname(se(fit)[coef_name]) else se_override, n_obs = nobs(fit))
}

timed <- function(label, expr) {
  t0  <- Sys.time()
  out <- force(expr)
  message(glue('  {label}: {round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)} min'))
  out
}

# writes the results to CSV
save_results <- function(results) {
  print(results %>% mutate(across(c(estimate, se), ~ round(.x, 4))), n = Inf, width = Inf)
  write_csv(results, SPEND_ON_VISITS_SPLIT_SAMPLE_RESULTS_PATH)
  message(glue('Wrote {SPEND_ON_VISITS_SPLIT_SAMPLE_RESULTS_PATH}'))
}

# ---- table: all-places OLS on county totals and split-sample IV, under both FE sets
# First-stage statistics of the split-sample IV, blank for OLS. The F is the cluster-robust
# Wald statistic on the other half's visits.
first_stage_stat <- function(fit, what) {
  if (!isTRUE(fit$iv)) return(NA_real_)
  if (what == 'f') return(fitstat(fit, 'ivwald1')[[1]]$stat)
  # stored at estimation with the clustered SEs; summary(stage = 1) would refetch the data by name
  unname(fit$iv_first_stage[[1]]$coeftable['asinh(visits_other)', if (what == 'coef') 'Estimate' else 'Std. Error'])
}
fitstat_register('fs_f',    \(fit) first_stage_stat(fit, 'f'),    'First-stage F (cluster-robust Wald)')
fitstat_register('fs_coef', \(fit) first_stage_stat(fit, 'coef'), 'First-stage coef., other-half visits')
fitstat_register('fs_se',   \(fit) first_stage_stat(fit, 'se'),   'First-stage SE')
# from the fit itself: fixest's 'my' refetches the data by name, which can be a later object
fitstat_register('y_mean',  \(fit) mean(fitted(fit) + resid(fit)), 'Dependent variable mean')

# models: named list 'OLS|<sample>' and 'IV|<sample>', all under the table 2 FEs spec;
# n_counties: counties in each model's sample, same order
write_spend_on_visits_table <- function(models, n_counties) {
  model_samples <- sub('^[^|]+\\|', '', names(models))
  is_iv         <- startsWith(names(models), 'IV')
  sample_label  <- if_else(model_samples == 'matched places', 'Matched places', 'All places')
  yes_no        <- \(x) if_else(x, 'Yes', 'No')
  dict <- setNames(c(rep('asinh(Weekly visits)', 2), rep('asinh(Weekly spend)', 2), 'County'),
                   c('asinh(visits)', 'fit_asinh(visits_half)', 'asinh(spend)', 'asinh(spend_half)', 'county_geoid'))
  args <- list(
    unname(models), dict = dict, keep = '%visits', drop.section = 'fixef',
    fitstat = ~ n + r2 + wr2 + fs_f + fs_coef + fs_se + y_mean,
    digits = 'r4', digits.stats = 4,
    headers = list('Estimator' = if_else(is_iv, 'Split-sample IV', 'OLS'), 'Sample' = sample_label),
    group = list('Weather bin controls' = '^n_days_weather_'),
    extralines = list(
      '^_Week FEs'                               = rep('Yes', length(models)),
      '^_County $\\times$ season $\\times$ week-of-month FEs' = rep('Yes', length(models)),
      '^_State $\\times$ year-month FEs'         = rep('Yes', length(models)),
      '^_FEs interacted with half'               = yes_no(is_iv),
      '^_Unit of observation'                    = paste0('County-week', if_else(is_iv, ' $\\times$ half', '')),
      '_Counties'                                = format(n_counties, big.mark = ',')
    ),
    notes = paste('\\footnotesize \\emph{Spend is in 2017 dollars. Matched places are those covered by both the spend',
                  'and visits panels. Split-sample IV stacks the two random halves of places: spend and visits of one',
                  'half, instrumented with visits of the other (ORIV), so it has twice the observations. All specifications',
                  'use the table 2 fixed effects on whole Tuesday-Monday weeks.}')
  )
  print(do.call(etable, args))
  do.call(etable, c(args, list(tex = TRUE, fontsize = 'footnotesize', file = TABLE_PATH, replace = TRUE)))
  message(glue('Wrote {TABLE_PATH}'))
}
table_models <- list()
table_n_counties <- c()

samples <- c('all places', 'matched places')
stacked_by_spec <- lapply(weekly_by_spec, \(weekly) {
  setNames(lapply(samples, \(sample_name) stack_halves(weekly[sample == sample_name])), samples)
})

# ---- linear: OLS on county totals, OLS on each half, and split-sample IV
results <- map_dfr(names(SPECS), \(spec_name) {
  f <- formulas_by_spec[[spec_name]]
  map_dfr(samples, \(sample_name) {
    message(glue('---- linear, {spec_name}, {sample_name}'))
    wide    <- weekly_by_spec[[spec_name]][sample == sample_name]
    stacked <- stacked_by_spec[[spec_name]][[sample_name]]
    total    <- timed('OLS, county total', feols(f$total, wide, cluster = ~county_geoid))
    half_ols <- timed('OLS, same half',    feols(f$half_ols, stacked, cluster = ~county_geoid))
    half_iv  <- timed('split-sample IV',   feols(f$half_iv, stacked, cluster = ~county_geoid))
    message(glue('  first-stage F {round(fitstat(half_iv, "ivwald1")[[1]]$stat, 1)}'))
    # the table shows only the table 2 FEs spec, for all places and matched places
    if (spec_name == 'table 2 FEs') {
      table_models[[glue('OLS|{sample_name}')]] <<- total
      table_models[[glue('IV|{sample_name}')]]  <<- half_iv
      table_n_counties[glue('OLS|{sample_name}')] <<- uniqueN(wide$county_geoid)
      table_n_counties[glue('IV|{sample_name}')]  <<- uniqueN(stacked$county_geoid)
    }
    bind_rows(
      row_for(total,    spec_name, sample_name, 'OLS, county total'),
      row_for(half_ols, spec_name, sample_name, 'OLS, same half (stacked)', 'asinh(visits_half)'),
      row_for(half_iv,  spec_name, sample_name, 'Split-sample IV (ORIV)',   'fit_asinh(visits_half)')
    )
  })
})
save_results(results)
ordered <- c(glue('OLS|{samples}'), glue('IV|{samples}'))
write_spend_on_visits_table(table_models[ordered], table_n_counties[ordered])
rm(table_models)
