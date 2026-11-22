# Run from the repository root: Rscript --vanilla tests/test_lpdid_thresholds.R
suppressPackageStartupMessages({library(tidyverse); library(data.table); library(fixest)})
Sys.setenv(SMOKE_AVOIDANCE_PROJECT_DIR = normalizePath('.'))
source('2_analysis/lpdid_build_dataset.R')
data.table::setDTthreads(2)
fixest::setFixest_nthreads(1)
set.seed(814)

# Four county series across the November 30 splice, including exact thresholds,
# a renewed event, a missing exposure, and a control's later moderate smoke.
dates <- seq(as.Date('2019-11-01'), as.Date('2020-01-20'), by = 'day')
raw <- CJ(county_geoid = c('01001','01003','01005','01007'), date = dates)
raw[, k := seq_len(.N), by = county_geoid]
raw[, `:=`(adj_pm_smoke_med_1sd = runif(.N,0,5), balanced_panel = format(date,'%m-%d')!='11-30',
  state_year_month = paste0('01', format(date,'%Y%m')), week = 1L,
  day_of_week = lubridate::wday(date), season = 1L,
  binned_pr = sample(0:2,.N,TRUE), binned_tmmn = sample(1:3,.N,TRUE),
  binned_tmmx = sample(1:3,.N,TRUE), binned_rmax = sample(1:3,.N,TRUE))]
raw[county_geoid=='01001' & k %in% c(20,25), adj_pm_smoke_med_1sd := 60]
raw[county_geoid=='01003' & k==25, adj_pm_smoke_med_1sd := 30]
raw[county_geoid=='01003' & k %in% c(40,41), adj_pm_smoke_med_1sd := 60]
raw[county_geoid=='01005' & k==42, adj_pm_smoke_med_1sd := 40]
raw[county_geoid=='01007' & k==12, adj_pm_smoke_med_1sd := NA_real_]
raw[, asinh_daily_visits_deferrable := 3 + as.numeric(factor(county_geoid)) +
      .01*k - .2*(adj_pm_smoke_med_1sd>=60) + rnorm(.N,sd=.1)]
fixture <- tempfile(fileext='.csv')
fwrite(raw, fixture)
test_panel_path <- Sys.getenv('LPDID_TEST_PANEL', unset = '')
if (nzchar(test_panel_path)) {
  dir.create(dirname(test_panel_path), recursive=TRUE, showWarnings=FALSE)
  file.copy(fixture, test_panel_path, overwrite=TRUE)
}
DAILY_DATA_FILEPATH <- fixture
dt <- build_lpdid_panel(save_to=NULL)
unlink(fixture)
stopifnot(!any(format(dt$date,'%m-%d')=='11-30'))
stopifnot(dt[county_geoid=='01001' & date==as.IDate('2019-11-20'), is_onset])
stopifnot(!dt[county_geoid=='01001' & date==as.IDate('2019-11-25'), is_onset])
# A later event does not retrospectively exclude the first eligible onset.
stopifnot(sum(dt[county_geoid=='01001', is_onset])==1)

# Independent window enumeration checks BOTH thresholds and all boundary cases.
manual_controls <- function(threshold,h) {
  mask <- rep(FALSE,nrow(dt))
  for (cty in unique(dt$county_geoid)) {
    rows <- which(dt$county_geoid==cty)
    for (j in seq_along(rows)) {
      lo <- j-H; hi <- j+max(h,0)
      if(lo>=1 && hi<=length(rows)) {
        values <- dt$smoke[rows[lo:hi]]
        mask[rows[j]] <- !anyNA(values) && all(values<threshold)
      }
    }
  }
  mask
}
for (h in c(-7L,0L,1L,14L)) for (threshold in c(60,30)) {
  stopifnot(identical(as.logical(lpdid_control_mask(dt,h,threshold)),manual_controls(threshold,h)))
}
legacy <- lpdid_treated_mask(dt,treated_cap=30)
strict <- lpdid_treated_mask(dt,treated_cap=30,treated_strict=TRUE)
at30 <- dt$county_geoid=='01003' & dt$date==as.IDate('2019-12-10')
stopifnot(sum(at30)==1,legacy[at30],!strict[at30], all(!strict | legacy))
# A future 40-exposure control day qualifies at h=0 but fails <30 by h=14.
control_row <- dt$county_geoid=='01005' & dt$date==as.IDate('2019-12-05')
stopifnot(lpdid_control_mask(dt,0,30)[control_row],
          !lpdid_control_mask(dt,14,30)[control_row],
          lpdid_control_mask(dt,14,60)[control_row])
cat('PASS: exact 30/60 boundaries, treated-only versus both-side masks, horizon windows, missing values, repeat events, and November splice.\n')

# Check sample accounting for the main and treated-only robustness specifications.
for (cap in c(NA_real_,30)) {
  fit <- estimate_lpdid(copy(dt),horizons=c(0L,1L,14L),
    treated_cap=cap,fe_spec='| county_geoid + date',verbose=FALSE)
  used <- fit %>% filter(h!=-1)
  stopifnot(all(used$n_obs==used$n_treated_used+used$n_control_used),
            all(used$n_treated_with_control_state_month_used<=used$n_treated_used))
}
strict_fit <- estimate_lpdid(copy(dt),horizons=c(0L,1L,14L),treated_cap=30,
  treated_strict=TRUE,control_threshold=30,fe_spec='| county_geoid + date',verbose=FALSE)
stopifnot(all(is.finite(strict_fit$estimate)))
cat('PASS: both-side strict <30 specification estimates successfully on the fixture.\n')
