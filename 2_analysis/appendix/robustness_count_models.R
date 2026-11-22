library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

full_df <- read_csv(
  DAILY_DATA_FILEPATH,
  col_select = c(county_geoid, date, state_year_month, week, balanced_panel,
                 day_of_week, season, daily_visits_deferrable,
                 asinh_daily_visits_deferrable,
                 bin_d_001_015, bin_d_015_030, bin_d_030_060,
                 bin_d_060_100, bin_d_100_200, bin_d_200_500,
                 binned_pr, binned_tmmn, binned_tmmx, binned_rmax),
  show_col_types = FALSE
) %>%
  mutate(
    across(c(binned_pr, binned_tmmn, binned_tmmx, binned_rmax), ~as.factor(.), .names = "{.col}"),
    # +1 so zero-visit county-days keep a positive weight in the weighted asinh OLS
    visit_weight = daily_visits_deferrable + 1
  )

panel_df <- panel(full_df %>% filter(balanced_panel), ~ county_geoid + date)


# ==============================================================================
# 1. Diagnostics: check whether Poisson or NB is appropriate
#    Dispersion is computed WITHIN counties (the primary FE group), since
#    between-county variation is absorbed by fixed effects.
# ==============================================================================

cat("\n=== Overall dependent variable diagnostics ===\n")
dv <- panel_df$daily_visits_deferrable[!is.na(panel_df$daily_visits_deferrable)]
cat("N obs:       ", length(dv), "\n")
cat("Mean:        ", round(mean(dv), 2), "\n")
cat("Overall Var/Mean:", round(var(dv) / mean(dv), 0),
    "(inflated by between-county variation)\n")
cat("% zeros:     ", round(100 * mean(dv == 0), 2), "%\n\n")

# Within county only
cat("=== Within-county dispersion ===\n")
within_county <- panel_df %>%
  filter(!is.na(daily_visits_deferrable)) %>%
  group_by(county_geoid) %>%
  summarise(mu = mean(daily_visits_deferrable),
            v  = var(daily_visits_deferrable),
            ratio = v / mu, .groups = "drop")
cat("Median within-county var/mean:", round(median(within_county$ratio, na.rm = TRUE), 0), "\n")

# Within county x day_of_week x season (the interaction FE in the model)
cat("\n=== Within county x day_of_week x season cell dispersion ===\n")
within_cell <- panel_df %>%
  filter(!is.na(daily_visits_deferrable)) %>%
  group_by(county_geoid, day_of_week, season) %>%
  summarise(mu = mean(daily_visits_deferrable),
            v  = var(daily_visits_deferrable),
            ratio = v / mu, .groups = "drop") %>%
  filter(!is.na(ratio) & is.finite(ratio))
cat("N cells:              ", nrow(within_cell), "\n")
cat("Median cell var/mean: ", round(median(within_cell$ratio), 1), "\n")
cat("P25 cell var/mean:    ", round(quantile(within_cell$ratio, 0.25), 1), "\n")
cat("P75 cell var/mean:    ", round(quantile(within_cell$ratio, 0.75), 1), "\n")
cat("(Poisson requires var/mean = 1; values >> 1 indicate overdispersion)\n")
cat("Note: date and state-year-month FEs absorb additional variation,\n")
cat("so residual dispersion is lower than these cell-level numbers.\n\n")

# Variance decomposition
total_var   <- var(dv)
between_var <- var(within_county$mu)
cat("=== Variance decomposition for county ===\n")
cat("Between-county share:", round(100 * between_var / total_var, 1), "%\n")
cat("Within-county share: ", round(100 * (1 - between_var / total_var), 1), "%\n\n")

between_var_cell <- var(within_county$mu)
cat("=== Variance decomposition for county-day of week-season ===\n")
cat("Between-county share:", round(100 * between_var_cell / total_var, 1), "%\n")
cat("Within-county share: ", round(100 * (1 - between_var_cell / total_var), 1), "%\n\n")


# ==============================================================================
# 2. Poisson pseudo-maximum-likelihood (PPML) — fixest::fepois
# ==============================================================================

cat("Estimating Poisson (PPML) model...\n")
est_poisson <- fepois(
  daily_visits_deferrable ~ bin_d_001_015 +
    bin_d_015_030 +
    bin_d_030_060 +
    bin_d_060_100 +
    bin_d_100_200 +
    bin_d_200_500 +
    binned_pr + binned_tmmn + binned_tmmx + binned_rmax |
    date + state_year_month + day_of_week^county_geoid^season,
  data = panel_df,
  cluster = ~county_geoid
)
cat("\n=== Poisson (PPML) results ===\n")
print(summary(est_poisson))

saveRDS(est_poisson, file.path(REGRESSION_RESULTS_DIR, "robustness_poisson_day_of.rds"))


# ==============================================================================
# 3. Poisson dispersion test
# ==============================================================================

# Cameron-Trivedi (1990) style overdispersion test:
# Regress (y_i - mu_i)^2 - y_i on mu_i. If alpha > 0, overdispersion present.
cat("\n=== Overdispersion test (Cameron-Trivedi style) ===\n")
mu_hat <- fitted(est_poisson)
# Recover y on exactly the estimation sample: fixest's response residual is
# y - mu_hat, so each count is paired with its own fitted value. Subsetting
# panel_df by hand and trimming with head() misaligns the two vectors, since
# the rows fixest drops (NAs in the other RHS vars, in the FEs, and separated
# observations) are scattered through the panel rather than sitting at the end.
y      <- mu_hat + resid(est_poisson, type = "response")
cat("Using", length(mu_hat), "observations for dispersion test\n")
aux_y <- (y - mu_hat)^2 - y
aux_reg <- lm(aux_y ~ mu_hat)
cat("Dispersion parameter alpha:\n")
print(summary(aux_reg)$coefficients["mu_hat", ])
cat("\nIf alpha > 0 and significant, NB is preferred over Poisson.\n")


# ==============================================================================
# 4. Visit-weighted asinh OLS
#     The main asinh spec weights every county-day equally, so it estimates the
#     average percent change across counties. Weighting by visits instead
#     targets the percent change in aggregate visits -- the same estimand PPML
#     recovers, since its score equations weight by the fitted mean. Weights
#     are visits + 1 so zero-visit days stay in the sample.
# ==============================================================================

cat("\nEstimating visit-weighted asinh OLS...\n")
est_asinh_weighted <- feols(
  asinh_daily_visits_deferrable ~ bin_d_001_015 +
    bin_d_015_030 +
    bin_d_030_060 +
    bin_d_060_100 +
    bin_d_100_200 +
    bin_d_200_500 +
    binned_pr + binned_tmmn + binned_tmmx + binned_rmax |
    date + state_year_month + day_of_week^county_geoid^season,
  data = panel_df,
  cluster = ~county_geoid,
  weights = ~visit_weight
)
cat("\n=== Visit-weighted asinh OLS results ===\n")
print(summary(est_asinh_weighted))

saveRDS(est_asinh_weighted, file.path(REGRESSION_RESULTS_DIR, "robustness_asinh_weighted_day_of.rds"))


# ==============================================================================
# 5. Compare coefficient estimates across models
# ==============================================================================

cat("\n=== Comparison of smoke PM coefficients across models ===\n")

# Load the asinh model estimated on the same daily bin indicators
est_original <- readRDS(file.path(REGRESSION_RESULTS_DIR, "binary_base_day_of_dose_response.rds"))

smoke_vars <- c("bin_d_001_015", "bin_d_015_030",
                "bin_d_030_060", "bin_d_060_100",
                "bin_d_100_200", "bin_d_200_500")

comparison <- data.frame(
  bin = c("(0, 15]", "(15, 30]", "(30, 60]", "(60, 100]", "(100, 200]", "(200, 500]"),
  asinh_coef   = coef(est_original)[smoke_vars],
  asinh_wt_coef = coef(est_asinh_weighted)[smoke_vars],
  poisson_coef = coef(est_poisson)[smoke_vars]
)
# Poisson uses a log link, so exp(b) - 1 is an exact percent change.
comparison$poisson_pct <- sprintf("%+.2f%%", 100 * (exp(comparison$poisson_coef) - 1))

cat("\nCoefficient comparison (all in semi-elasticity / log-point scale):\n")
print(comparison, row.names = FALSE)

cat("asinh coefficients approximate log for large values.\n")


# ==============================================================================
# 6. LaTeX table for the appendix
# ==============================================================================

etable_dict <- c(
  bin_d_001_015 = "Smoke Day (0, 15]",
  bin_d_015_030 = "Smoke Day (15, 30]",
  bin_d_030_060 = "Smoke Day (30, 60]",
  bin_d_060_100 = "Smoke Day (60, 100]",
  bin_d_100_200 = "Smoke Day (100, 200]",
  bin_d_200_500 = "Smoke Day (200, 500]",
  daily_visits_deferrable = "Deferrable Daily Visits",
  county_geoid = "County",
  date = "Day",
  state_year_month = "State-Year-Month",
  week = "Week of Year",
  day_of_week = "Day of Week",
  season = "Season",
  notes = "Clustered (County) standard-errors in parentheses"
)

count_models  <- list(est_asinh_weighted, est_poisson)
count_headers <- c("asinh OLS (visit-weighted)", "Poisson (PPML)")

cat("\n=== Console view (etable) ===\n")
etable(count_models,
       dict    = etable_dict,
       headers = count_headers,
       keep    = "Smoke Day",
       group   = list("Flexible Weather Controls" = "binned_"),
       family  = FALSE,
       fitstat = ~ n + sq.cor)

# ------------------------------------------------------------------------------
# Paired Coeff. / % change table.
#
# etable cannot emit a derived column beside its own output, so the LaTeX is
# built directly here. Each model gets two columns: the log-link coefficient,
# and the exact percent change in visits, 100 * (exp(b) - 1). Because both
# models use a log link that transform is exact, not an approximation -- and it
# matters at the top bin, where the naive 100*b reading overstates the drop by
# ~1.5 percentage points. Standard errors are carried into percent terms by the
# delta method: se(100*(exp(b)-1)) = 100 * exp(b) * se(b).
#
# Significance thresholds match fixest's TeX default (***: 0.01, **: 0.05,
# *: 0.1), which is what the other tables in 2_analysis/tables use.
# ------------------------------------------------------------------------------

tex_stars <- function(p) ifelse(p < .01, "$^{***}$",
                        ifelse(p < .05, "$^{**}$",
                        ifelse(p < .1,  "$^{*}$", "")))

# pct_exact marks models where 100*(exp(b)-1) is the exact percent change. That
# holds for any log-link model (Poisson). For the asinh main specification it
# is an approximation: asinh(y) ~ log(2y) only for large y. Deferrable visits are
# large here (median fitted mean ~985), so the approximation is close, but the
# distinction is flagged in the table notes rather than papered over.
build_coef_pct_table <- function(models, headers, dep_labels, vars, dict,
                                 pct_exact = rep(TRUE, length(models))) {
  n_m <- length(models)

  body <- ""
  for (v in vars) {
    row_est <- dict[[v]]
    row_se  <- ""
    for (m in models) {
      b  <- coef(m)[v]
      se <- summary(m)$se[v]
      p  <- summary(m)$coeftable[v, 4]
      st <- tex_stars(p)
      row_est <- paste0(row_est,
                        sprintf(" & %.4f%s & %+.2f\\%%%s", b, st,
                                100 * (exp(b) - 1), st))
      row_se  <- paste0(row_se,
                        sprintf(" & (%.4f) & (%.2f)", se, 100 * exp(b) * se))
    }
    body <- paste0(body, "   ", row_est, "\\\\\n   ", row_se, "\\\\\n")
  }

  pct_hd <- ifelse(pct_exact, "\\% Change", "\\% Change$^{\\dagger}$")

  foot_row <- function(txt)
    sprintf("   \\multicolumn{%d}{l}{%s}\\\\\n", 2 * n_m + 1, txt)

  paste0(
    "\n\\begingroup\n\\centering\n",
    "\\begin{tabular}{l", paste(rep("cc", n_m), collapse = ""), "}\n",
    "   \\tabularnewline \\midrule \\midrule\n",
    "   Dependent Variable: & ",
    paste(sprintf("\\multicolumn{2}{c}{%s}", dep_labels), collapse = " & "), "\\\\\n",
    "   & ", paste(sprintf("\\multicolumn{2}{c}{%s}", headers), collapse = " & "),
    "\\\\\n",
    "   Model: & ",
    paste(sprintf("\\multicolumn{2}{c}{(%d)}", seq_len(n_m)), collapse = " & "),
    "\\\\\n",
    "   & ", paste(sprintf("Coeff. & %s", pct_hd), collapse = " & "), "\\\\\n",
    "   \\midrule\n   \\emph{Variables}\\\\\n",
    body,
    "   \\midrule\n   \\emph{Fit statistics}\\\\\n",
    "   Observations & ",
    paste(sapply(models, function(m) sprintf("\\multicolumn{2}{c}{%s}",
                 formatC(m$nobs, big.mark = ",", format = "d"))),
          collapse = " & "), "\\\\\n",
    "   Squared Correlation & ",
    paste(sapply(models, function(m) sprintf("\\multicolumn{2}{c}{%.5f}",
                 fitstat(m, "sq.cor")$sq.cor)), collapse = " & "), "\\\\\n",
    "   \\midrule \\midrule\n",
    if (any(!pct_exact))
      foot_row(paste("\\emph{$\\dagger$ \\% Change is approximate for the asinh",
                     "specifications; exact for the log-link Poisson model}"))
    else "",
    foot_row("\\emph{Signif. Codes: ***: 0.01, **: 0.05, *: 0.1}"),
    "\\end{tabular}\n\\par\\endgroup\n"
  )
}

# est_original is the main asinh specification (same bins, same FE structure),
# loaded in section 6. It leads the table so the others read as robustness.
table_models <- list(est_original, est_asinh_weighted, est_poisson)
table_headers <- c("asinh OLS (main)", "asinh OLS (visit-weighted)", "Poisson (PPML)")
table_deps <- c("asinh(Deferrable Daily Visits)",
                "asinh(Deferrable Daily Visits)", "Deferrable Daily Visits")

count_tex <- build_coef_pct_table(table_models, table_headers, table_deps,
                                  smoke_vars, etable_dict,
                                  pct_exact = c(FALSE, FALSE, TRUE))
tex_path <- file.path(APPENDIX_TABLES_DIR, "appendix_table_count_models.tex")
writeLines(count_tex, tex_path)
cat("\nWrote", tex_path, "\n")
cat(count_tex)

cat("\nDone.\n")
