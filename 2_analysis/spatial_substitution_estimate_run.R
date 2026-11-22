# ============================================================================
# spatial_substitution_estimate_run.R
#
# THE spatial-substitution SDEM estimation. Run it after
# build_spatial_weight_matrix.R has written sdem_prepared_panel.rds. It sources
# spatial_substitution_estimation_helpers.R for the sparse ML engine and the
# design, fits the model on the full panel, and saves the results to
# spatial_sdem_v4_dcs_units_results.rds. The figure is a separate step --
# spatial_substitution_estimate_plot.R, run against those saved results.
#
#     y = X b + WX t + C g + u,   u = lambda W u + e
#
# State x year-month, the four weather bin factors and the day effects are
# dummy columns in X (C), filtered by B = I - lambda W like every other
# regressor. The cross-sectional unit is day-of-week x county x season. Within a
# (dow, season) cell the unit effect is constant over the cell's dates while B
# acts only ACROSS counties within a date, so those unit effects concentrate
# out of the likelihood EXACTLY and the county-level W is all the engine needs:
# log|I - lambda W_tiled| = n_cell * log|I - lambda W|, the identity section 1
# checks numerically before the fit.
# ============================================================================

library(tidyverse)
library(Matrix)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(file.path(CODE_DIR, "2_analysis", "spatial_substitution_estimation_helpers.R"))

say <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

OUT_PATH <- file.path(REGRESSION_RESULTS_DIR,
                      "spatial_sdem_v4_dcs_units_results.rds")

# ============================================================================
# 1. Sample, design, and the tiled-W Jacobian check
# ============================================================================

dat <- sdem_v4_load_panel(say = say)
des <- sdem_v4_build_design(dat, say = say)

# the (dow, season) cell each date belongs to
cell_of_date <- des$panel %>%
  distinct(spend_date_range_start, day_of_week, season) %>%
  arrange(spend_date_range_start) %>%
  mutate(cell = interaction(day_of_week, season, drop = TRUE))
cell_lev       <- levels(cell_of_date$cell)
n_cell         <- length(cell_lev)
dates_per_cell <- as.integer(table(cell_of_date$cell))
n_units        <- dat$N * n_cell

say("(dow, season) cells: ", n_cell, "; dates per cell range ",
    min(dates_per_cell), " to ", max(dates_per_cell),
    "; units (dow x county x season): ", n_units)

# The tiled weights W_tiled = I_n_cell (x) W, and the Jacobian identity that
# lets the county-level W stand in for them:
#     log|I - lambda W_tiled| = n_cell * log|I - lambda W|
say("Checking the tiled-W Jacobian identity")
W_tiled   <- Matrix::bdiag(rep(list(dat$W), n_cell))
lam_probe <- 0.5
ld_tiled  <- as.numeric(Matrix::determinant(
  Diagonal(nrow(W_tiled)) - lam_probe * W_tiled, logarithm = TRUE)$modulus)
ld_county <- as.numeric(Matrix::determinant(
  Diagonal(dat$N) - lam_probe * dat$W, logarithm = TRUE)$modulus)
say("log|I - .5 W_tiled| = ", round(ld_tiled, 6),
    "   vs  ", n_cell, " x log|I - .5 W| = ", round(n_cell * ld_county, 6),
    "   (diff ", signif(abs(ld_tiled - n_cell * ld_county), 3), ")")
stopifnot(abs(ld_tiled - n_cell * ld_county) < 1e-6 * abs(ld_tiled))
rm(W_tiled); gc()

# ============================================================================
# 2. Fit the full sample
# ============================================================================

t0  <- Sys.time()
mom <- sdem_moments(des$X, des$WX, des$y, des$Wy, des$dcs)
say("Moments built in ",
    round(as.numeric(Sys.time() - t0, units = "mins"), 2), " min")

keep <- sdem_full_rank_cols(mom$A0)
if (length(keep) < ncol(des$X))
  say("Dropping ", ncol(des$X) - length(keep),
      " structurally collinear columns")
mom <- sdem_moments_subset(mom, keep)

sdem_ml <- sdem_sparse_fit(mom, dat$W, dat$N, dat$TT, colnames(des$X)[keep])

bins <- SMOKE_VARS[SMOKE_VARS %in% names(coef(sdem_ml)) &
                   W_SMOKE_VARS %in% names(coef(sdem_ml))]
say("lambda = ", round(sdem_ml$lambda, 5), ",  sigma^2 = ",
    signif(sdem_ml$sigma2, 5))
print(summary(sdem_ml, which = c(bins, paste0("w_", bins))))

# ============================================================================
# 3. Impacts, typical episode, joint tests, and save
# ============================================================================

results <- list(
  n_counties = dat$N, n_dates = dat$TT, bins = bins, sdem_ml = sdem_ml,
  beta_names = bins, theta_names = paste0("w_", bins),
  n_units = n_units, n_cells = n_cell, dates_per_cell = dates_per_cell,
  spec = paste("dow x county x season units: W tiled over the (dow, season)",
               "cells; unit effects concentrated out exactly"))

results <- sdem_v4_report(results, sdem_ml, bins, des$panel, des$X_wsmoke, say)

saveRDS(results, OUT_PATH)
say("Saved results to ", OUT_PATH)
say("DONE")
