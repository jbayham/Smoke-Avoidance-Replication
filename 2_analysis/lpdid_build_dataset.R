# ==============================================================================
# lpdid_build_dataset.R
#
# Single source of truth for the LP-DiD analysis panel and the estimation
# routine that runs on it. Previously this code was duplicated (and had drifted)
# across:
#   2_analysis/daily_visits_lpdid.R
#   2_analysis/appendix/prior_smoke_filter_lpdid_run_and_plot.R
#   2_analysis/appendix/prior_smoke_filter_lpdid_control_filter_run_and_plot.R
#
# Those scripts now do:
#   source(file.path(CODE_DIR, "2_analysis", "lpdid_build_dataset.R"))
#   dt <- load_lpdid_panel()
#
# To force a rebuild of the cached panel:
#   Rscript 2_analysis/lpdid_build_dataset.R          # from the shell
#   load_lpdid_panel(rebuild = TRUE)                  # from an R session
#
# ==============================================================================
# LP-DiD SPECIFICATION (Dube, Girardi, Jorda & Taylor 2023, eq. 13, adapted to
# non-absorbing treatment):
#
#   For each horizon h in {K_LOW, ..., K_HIGH} \ {-1}:
#
#     y_{i,t+h} - y_{i,t-1} = beta^h * Delta D_{it}
#                             + X_{it}' gamma^h        ... binned weather
#                             + delta_t^h              ... date FE
#                             + mu_{s,y,m}^h           ... state x year-month FE
#                             + lambda_{dow,i,season}^h ... county x dow x season FE
#                             + e_{it}^h
#
#   y            = asinh(daily deferrable visits)
#   D_{it}       = 1{adj_pm_smoke_med_1sd >= OVER_THRESHOLD}
#   Delta D_{it} = 1{D_{it} = 1 AND D_{i,t-1} = 0}      (smoke onset)
#   h = -1 is the reference period (beta^{-1} := 0)
#
# SAMPLE (eq. 15, adapted):
#   (a) Newly treated: Delta D_{it} = 1 and D_{i,t-j} = 0 for j = 1..H
#   (b) Clean control: D_{i,t+j} = 0 for j = -H..max(h, 0)
#
# ==============================================================================
# PRE-PERIOD SMOKE CAP (appendix robustness only)
#
# The clean-control rule above is a threshold rule: a day qualifies as a clean
# control as long as its max smoke over [t-H, t+max(h,0)] stays below
# OVER_THRESHOLD (= 60 ug/m3). The same is true on the treated side -- the
# median onset is preceded by a 14-day window whose max smoke is ~32 ug/m3, so
# y_{t-1} is already depressed at baseline and beta^h is attenuated.
#
# The MAIN specification applies no additional cap: treated = any clean
# onset, control = the standard clean-control rule. The legacy appendix uses
# this treated-only robustness cut:
#
#   TREATED cap : an onset counts as treated only if its pre-window max smoke
#                 over [t-H, t-1] is <= APPENDIX_TREATED_CAP (30 ug/m3).
#                 Pre-window only -- smoke at t and after IS the treatment.
#
# The legacy comparison leaves controls unchanged. The additional both_lt30
# specification requires smoke strictly below 30 on BOTH sides, over the
# treated pre-window and the full horizon-specific control window. It records
# eligibility, estimation-sample attrition, and state-month comparison support.
# Historical rationale for the earlier treated-only choice: capping controls
# over a window that
# extends past t strips out precisely the county-days that share a state-month
# with an onset (a county in a smoky state-month rarely has a long quiet
# stretch), which silently removes treated onsets from identification: under a
# 15 ug/m3 control cap on [t-14, t+14], 579 of 1,252 onsets landed in a
# state-year-month cell with zero clean controls and were absorbed whole by
# that fixed effect.
#
# A day whose pre-window is not fully observed yields NA and is dropped (the
# cap cannot be verified).
#
# ==============================================================================
# BALANCED PANEL
#
# `balanced_panel` is FALSE for two different reasons:
#
#   (a) 10 counties with incomplete visits coverage (the Vermont block).
#       These are dropped outright.
#
#   (b) November 30 in every year, which is missing from the visits data
#       everywhere. These rows are DROPPED and the series is spliced: within a
#       county, December 1 is treated as the day after November 29, so every
#       lag, lead and rolling window steps over the gap rather than being
#       invalidated by it.
#
# The splice is a deliberate choice to preserve sample. The cost is that a
# window spanning the seam covers one extra calendar day -- a 14-day washout
# ending December 1 reaches back to November 16, not November 17. That only
# matters for events near the seam, and >= 60 ug/m3 smoke onsets are
# overwhelmingly summer and early-autumn events; the build reports how many
# onsets sit within a window's reach of a November 30 so the count stays
# visible.
#
# The alternative -- keeping the rows as NA placeholders so any window touching
# them is dropped -- was used earlier and is strictly more conservative, but it
# discards observations around the seam for no gain given how few onsets are
# involved.
#
# What is NOT acceptable is filtering the rows out without thinking about it:
# that is the same splice, but silent. The build asserts that every county has
# exactly the same spliced date sequence, so the arithmetic is at least uniform
# across counties.
# ==============================================================================

library(tidyverse)
library(data.table)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# ------------------------------------------------------------------------------
# Design constants (shared by every LP-DiD script)
# ------------------------------------------------------------------------------

H              <- 14L    # washout: days without smoke required before an onset
K_LOW          <- -7L    # earliest pre-treatment horizon (placebo)
K_HIGH         <- 14L    # latest post-treatment horizon
OVER_THRESHOLD <- 60     # ug/m3 defining D_{it} = 1

HORIZONS <- setdiff(K_LOW:K_HIGH, -1L)   # h = -1 is the normalized reference

SMOKE_VAR   <- "adj_pm_smoke_med_1sd"
OUTCOME_VAR <- "asinh_daily_visits_deferrable"

# The main specification is uncapped. This is the appendix robustness cut only
# (see the block comment above).
APPENDIX_TREATED_CAP <- 30   # ug/m3 over [t-14, t-1], treated side only

LPDID_FE_SPEC   <- "| date + state_year_month + day_of_week^county_geoid^season"
LPDID_CTRL_SPEC <- "+ binned_pr + binned_tmmn + binned_tmmx + binned_rmax"

LPDID_PREPPED_PATH <- file.path(INTERMEDIATE_REG_DATA_DIR, "lpdid_prepped_panel.rds")

# Column-name suffix for a horizon: h = -3 -> "neg_3", h = 5 -> "5"
horizon_str <- function(h) if (h < 0) paste0("neg_", abs(h)) else as.character(h)


# ------------------------------------------------------------------------------
# Panel construction
# ------------------------------------------------------------------------------

build_lpdid_panel <- function(save_to = LPDID_PREPPED_PATH) {

  keep <- c("county_geoid", "date", "state_year_month",
            "week", "day_of_week", "season", "balanced_panel",
            OUTCOME_VAR, SMOKE_VAR,
            "binned_pr", "binned_tmmn", "binned_tmmx", "binned_rmax")

  cat("Step 1: reading the daily panel...\n")
  dt <- fread(DAILY_DATA_FILEPATH, select = keep,
              colClasses = list(character = "county_geoid"), showProgress = FALSE)
  dt[, date := as.IDate(date)]
  cat("  raw rows:", format(nrow(dt), big.mark = ","),
      "| counties:", uniqueN(dt$county_geoid), "\n")

  cat("Step 2: applying the balanced-panel restriction...\n")
  # (a) drop counties that are never in the balanced panel (incomplete coverage)
  ok_counties <- dt[, .(any_ok = any(balanced_panel)), by = county_geoid][any_ok == TRUE, county_geoid]
  n_drop <- uniqueN(dt$county_geoid) - length(ok_counties)
  dt <- dt[county_geoid %chin% ok_counties]
  cat("  dropped", n_drop, "counties with incomplete visits coverage\n")

  # (b) drop the unbalanced rows (Nov 30) and splice: after this, consecutive
  #     rows within a county are consecutive OBSERVATIONS, and Dec 1 follows
  #     Nov 29 directly. Every shift()/frollsum() below is positional, so they
  #     all step over the seam.
  all_dates <- seq(min(dt$date), max(dt$date), by = "day")
  n_before  <- nrow(dt)
  dt <- dt[balanced_panel == TRUE]
  setorder(dt, county_geoid, date)
  cat("  dropped", format(n_before - nrow(dt), big.mark = ","),
      "unbalanced rows (Nov 30); Dec 1 now follows Nov 29\n")

  # (c) every county must carry the identical spliced date sequence, otherwise
  #     "one row back" means different things in different counties
  kept_dates <- sort(unique(dt$date))
  stopifnot(nrow(dt) == length(ok_counties) * length(kept_dates))
  stopifnot(dt[, .N, by = county_geoid][, all(N == length(kept_dates))])
  seam <- as.integer(diff(kept_dates))
  cat("  dates per county:", length(kept_dates), "of", length(all_dates),
      "calendar days |", sum(seam == 2), "spliced seams\n")
  stopifnot(all(seam %in% c(1L, 2L)))

  cat("Step 3: treatment, onset, and spell length...\n")
  setnames(dt, c(OUTCOME_VAR, SMOKE_VAR), c("y", "smoke"), skip_absent = TRUE)

  dt[, over_60   := as.integer(smoke >= OVER_THRESHOLD)]
  dt[, d_over_60 := over_60 - shift(over_60, 1L), by = county_geoid]

  # D = 0 on every one of the H days before t, all H of them observed
  dt[, sum_over_lb := frollsum(shift(over_60, 1L), H, algo = "exact"), by = county_geoid]
  dt[, is_onset := !is.na(d_over_60) & d_over_60 == 1L &
                   !is.na(sum_over_lb) & sum_over_lb == 0]

  # Spell length, top-coded at 3+: every onset lands in exactly one bucket.
  dt[, over_60_lead1 := shift(over_60, 1L, type = "lead"), by = county_geoid]
  dt[, over_60_lead2 := shift(over_60, 2L, type = "lead"), by = county_geoid]
  dt[, is_onset_1day  := is_onset & !is.na(over_60_lead1) & over_60_lead1 == 0L]
  dt[, is_onset_2day  := is_onset & !is.na(over_60_lead1) & over_60_lead1 == 1L &
                                    !is.na(over_60_lead2) & over_60_lead2 == 0L]
  dt[, is_onset_3plus := is_onset & !is.na(over_60_lead1) & over_60_lead1 == 1L &
                                    !is.na(over_60_lead2) & over_60_lead2 == 1L]
  dt[, c("over_60_lead1", "over_60_lead2", "sum_over_lb") := NULL]

  cat("  Any onset (clean):", format(sum(dt$is_onset), big.mark = ","), "\n")
  cat("    1-day spells:   ", format(sum(dt$is_onset_1day),  big.mark = ","), "\n")
  cat("    2-day spells:   ", format(sum(dt$is_onset_2day),  big.mark = ","), "\n")
  cat("    3+ day spells:  ", format(sum(dt$is_onset_3plus), big.mark = ","), "\n")
  stopifnot(sum(dt$is_onset_1day) + sum(dt$is_onset_2day) +
            sum(dt$is_onset_3plus) <= sum(dt$is_onset))

  # How much does the splice actually touch? An onset within H days after a
  # seam, or K_HIGH days before one, has a window that steps over it.
  seam_dates <- kept_dates[c(FALSE, diff(kept_dates) == 2)]   # the Dec 1s
  near_seam  <- vapply(dt$date[dt$is_onset], function(d)
                       any(abs(as.integer(d - seam_dates)) <= max(H, K_HIGH)),
                       logical(1))
  cat("  onsets whose window spans a spliced seam:", sum(near_seam),
      "of", sum(dt$is_onset), "\n")

  cat("Step 4: pre-window smoke maxima...\n")
  # Max smoke over [t-H, t-1]: the treated-side cap window. data.table 1.17 has
  # no frollmax and frollapply would mean ~6M R callbacks per window; pmax over
  # H lagged copies is vectorised and identical. NA propagates, so a partially
  # observed pre-window gives NA and is excluded.
  dt[, pre_max_smoke := do.call(pmax, shift(smoke, 1:H)), by = county_geoid]
  # Max over [t-H, t], i.e. including the day itself. Not used by any current
  # specification; kept because it is what the clean-control consistency check
  # below tests against and it costs one pmax.
  dt[, bwd_max_smoke := pmax(pre_max_smoke, smoke)]

  cat("Step 5: horizon-specific clean-control flags, outcome diffs, smoke leads...\n")
  dt[, y_lag1 := shift(y, 1L), by = county_geoid]

  for (h in HORIZONS) {
    h_str <- horizon_str(h)
    aw    <- max(h, 0L)
    w     <- H + aw + 1L               # clean-control window [t-H, t+max(h,0)]

    cc_col <- paste0("cc_", h_str)
    dy_col <- paste0("dy_", h_str)
    sm_col <- paste0("smoke_h_", h_str)

    # frollsum is right-aligned; shift back by `aw` to re-centre it on t.
    dt[, cc_tmp := shift(frollsum(over_60, w, algo = "exact"), -aw), by = county_geoid]
    # NA (window runs off the panel, or contains an unobserved day) -> not a
    # clean control. Stored as 0 rather than NA so the sample filter never
    # sees NA.
    dt[, (cc_col) := as.numeric(!is.na(cc_tmp) & cc_tmp == 0)]

    if (h >= 0) {
      dt[, (dy_col) := shift(y,     h,      type = "lead") - y_lag1, by = county_geoid]
      dt[, (sm_col) := shift(smoke, h,      type = "lead"),          by = county_geoid]
    } else {
      dt[, (dy_col) := shift(y,     abs(h)) - y_lag1, by = county_geoid]
      dt[, (sm_col) := shift(smoke, abs(h)),          by = county_geoid]
    }
  }
  dt[, cc_tmp := NULL]

  # Weather controls enter as factors
  for (v in c("binned_pr", "binned_tmmn", "binned_tmmx", "binned_rmax")) {
    set(dt, j = v, value = as.factor(dt[[v]]))
  }

  cat("Step 6: internal consistency checks...\n")
  # An onset is by construction untreated on all H preceding days, so its
  # pre-window max must be strictly below the treatment threshold.
  stopifnot(all(dt$pre_max_smoke[dt$is_onset] < OVER_THRESHOLD, na.rm = TRUE))
  stopifnot(!any(is.na(dt$pre_max_smoke[dt$is_onset])))
  # A clean control at h has D = 0 over the whole window, so its window max
  # must also be below the threshold.
  stopifnot(all(dt$bwd_max_smoke[dt$cc_0 == 1] < OVER_THRESHOLD, na.rm = TRUE))
  # An onset day itself is treated, so it is never a clean control.
  stopifnot(!any(dt$is_onset & dt$cc_0 == 1))

  if (!is.null(save_to)) {
    cat("  saving prepped panel to", save_to, "\n")
    saveRDS(dt, save_to)
  }
  cat("Done.\n")
  dt
}


#' Load the LP-DiD panel, building and caching it if needed.
load_lpdid_panel <- function(rebuild = FALSE, path = LPDID_PREPPED_PATH) {
  if (!rebuild && file.exists(path)) {
    cat("Loading cached LP-DiD panel from", path, "\n")
    dt <- readRDS(path)
    setDT(dt)
    setorder(dt, county_geoid, date)
    return(dt)
  }
  build_lpdid_panel(save_to = path)
}


# ------------------------------------------------------------------------------
# Sample masks
# ------------------------------------------------------------------------------

#' Treated rows: an onset of the requested spell type, optionally required to
#' have a quiet pre-window (max smoke over [t-H, t-1] <= treated_cap).
lpdid_treated_mask <- function(dt, onset_var = "is_onset", treated_cap = NA_real_,
                               treated_strict = FALSE) {
  ok <- dt[[onset_var]]
  ok[is.na(ok)] <- FALSE
  if (!is.na(treated_cap)) {
    below_cap <- if (treated_strict) dt$pre_max_smoke < treated_cap else dt$pre_max_smoke <= treated_cap
    ok <- ok & !is.na(dt$pre_max_smoke) & below_cap
  }
  ok
}

#' Clean controls at horizon h: D = 0 across [t-H, t+max(h,0)], fully observed.
#' Optional strict threshold applies over the entire control window, including t.
#' The default uses the original cached flags without changing the main sample.
lpdid_control_mask <- function(dt, h, control_threshold = OVER_THRESHOLD) {
  stopifnot(length(control_threshold) == 1L, is.finite(control_threshold),
            control_threshold > 0, control_threshold <= OVER_THRESHOLD)
  if (control_threshold == OVER_THRESHOLD) {
    return(dt[[paste0("cc_", horizon_str(h))]] == 1)
  }
  # Every window uses the same sorted, spliced observation sequence as baseline.
  # Missing exposure makes the whole window ineligible; no NA-to-zero imputation.
  key <- paste0("cc_lt", control_threshold, "_", horizon_str(h))
  if (!key %in% names(dt)) {
    aw <- max(h, 0L)
    dt[, (key) := {
      above <- as.integer(smoke >= control_threshold)
      total <- shift(frollsum(above, H + aw + 1L, algo = "exact"), -aw)
      !is.na(total) & total == 0
    }, by = county_geoid]
  }
  dt[[key]] == 1
}


# ------------------------------------------------------------------------------
# Estimation
# ------------------------------------------------------------------------------

#' Run the LP-DiD over every horizon.
#'
#' @param dv_prefix "dy_" for the visits long difference, "smoke_h_" for the
#'   descriptive smoke-PM "first stage" (levels).
#' @param treated_cap  ug/m3 cap on the treated pre-window; NA for no cap.
#'
#' Standard errors: the reported `se` (and the CI built from it) is clustered on
#' county, matching the rest of the paper. Onsets are heavily clustered in time,
#' so county-only clustering treats hundreds of counties hit by one plume as
#' independent; the two-way alternatives are computed from the same fit and
#' returned as `se_cty_smth` (county + state-year-month) and `se_cty_date`
#' (county + calendar date) for the robustness table, but they are not what the
#' CIs use.
estimate_lpdid <- function(dt, horizons = HORIZONS,
                           onset_var     = "is_onset",
                           treated_cap   = NA_real_,
                           dv_prefix     = "dy_",
                           treatment_var = "d_over_60",
                           fe_spec       = LPDID_FE_SPEC,
                           ctrl_spec     = LPDID_CTRL_SPEC,
                           verbose       = TRUE,
                           treated_strict = FALSE,
                           control_threshold = OVER_THRESHOLD) {

  treated_mask <- lpdid_treated_mask(dt, onset_var, treated_cap, treated_strict)
  n_treated    <- sum(treated_mask)

  results <- map_dfr(horizons, function(h) {
    h_str  <- horizon_str(h)
    dy_var <- paste0(dv_prefix, h_str)

    control_mask <- lpdid_control_mask(dt, h, control_threshold)
    sample_df    <- dt[treated_mask | control_mask]

    # A treated row sitting in a state-year-month cell that contains no clean
    # control is fit perfectly by that FE and contributes nothing to beta.
    # Uncapped this is zero, but it is cheap to verify and it is what caught
    # the control-cap problem described in the header.
    smth_ok         <- unique(dt$state_year_month[control_mask])
    n_treated_ident <- sum(treated_mask & dt$state_year_month %in% smth_ok)

    fml <- as.formula(paste0(dy_var, " ~ ", treatment_var, " ", ctrl_spec, " ", fe_spec))
    est <- tryCatch(
      feols(fml, data = sample_df, cluster = ~county_geoid),
      error = function(e) { warning("feols failed at h = ", h, ": ", e$message); NULL }
    )

    if (is.null(est)) {
      return(tibble(h = h, estimate = NA_real_, se = NA_real_, ci_low = NA_real_,
                    ci_high = NA_real_, se_cty_smth = NA_real_,
                    se_cty_date = NA_real_, n_obs = NA_integer_,
                    n_treated = n_treated, n_treated_ident = n_treated_ident,
                    n_control = sum(control_mask), n_treated_used = NA_integer_,
                    n_control_used = NA_integer_,
                    n_treated_with_control_state_month_used = NA_integer_))
    }

    # A support diagnostic is not an identified-onset count. Retain the legacy
    # n_treated_ident field for historical comparisons, but also report actual
    # used observations and whether those rows share a state-month with controls.
    used <- sample_df[fixest::obs(est), c(treatment_var, 'state_year_month'), with = FALSE]
    is_treated_used <- used[[treatment_var]] == 1
    control_cells <- unique(used$state_year_month[!is_treated_used])
    n_treated_used <- sum(is_treated_used)
    n_control_used <- sum(!is_treated_used)
    n_treated_supported_used <- sum(is_treated_used & used$state_year_month %in% control_cells)
    # A collinear treatment is an unavailable estimate, never a zero effect.
    coef_val <- unname(est$coefficients[treatment_var])
    tv       <- treatment_var
    se_at    <- function(cl) tryCatch(sqrt(vcov(est, cluster = cl)[tv, tv]),
                                      error = function(e) NA_real_)
    se_val      <- se_at(~county_geoid)                        # reported
    se_cty_smth <- se_at(~county_geoid + state_year_month)     # reference only
    se_cty_date <- se_at(~county_geoid + date)                 # reference only

    if (verbose) {
      cat(sprintf("    h = %3d | beta = %9.4f | se = %7.4f (2-way %7.4f) | N = %s | treated %d (%d identified)\n",
                  h, coef_val, se_val, se_cty_smth, format(est$nobs, big.mark = ","),
                  n_treated, n_treated_ident))
    }

    tibble(h = h, estimate = coef_val, se = se_val,
           ci_low = coef_val - 1.96 * se_val, ci_high = coef_val + 1.96 * se_val,
           se_cty_smth = se_cty_smth, se_cty_date = se_cty_date,
           n_obs = est$nobs, n_treated = n_treated,
           n_treated_ident = n_treated_ident, n_control = sum(control_mask),
           n_treated_used = n_treated_used, n_control_used = n_control_used,
           n_treated_with_control_state_month_used = n_treated_supported_used)
  })

  bind_rows(results,
            tibble(h = -1L, estimate = 0, se = 0, ci_low = 0, ci_high = 0,
                   se_cty_smth = 0, se_cty_date = 0, n_obs = NA_integer_,
                   n_treated = n_treated, n_treated_ident = NA_integer_,
                   n_control = NA_integer_)) %>%
    arrange(h)
}


# Rebuild the cache when this file is run directly (`Rscript lpdid_build_dataset.R`).
# When it is source()d from another script, sys.nframe() > 0 and nothing runs.
if (sys.nframe() == 0L) {
  invisible(build_lpdid_panel())
}
