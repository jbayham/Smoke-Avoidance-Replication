# Reproduce the legacy treated-only check, then estimate the manuscript's
# strict <30 restriction on BOTH sides. Run with an isolated CODE_DIR/DATA_DIR.
library(tidyverse)
library(data.table)
library(fixest)
source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()),
                 'project_helpers_and_constants.R'))
source(file.path(CODE_DIR, '2_analysis/lpdid_build_dataset.R'))

out_dir <- file.path(REGRESSION_RESULTS_DIR, 'lpdid_threshold_robustness')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(APPENDIX_FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(APPENDIX_TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(INTERMEDIATE_REG_DATA_DIR, recursive = TRUE, showWarnings = FALSE)

# Reject stale checkpoints when panel, implementation, or R/package versions change.
signature <- digest::digest(list(
  daily_input = digest::digest(file = DAILY_DATA_FILEPATH, algo = 'sha256'),
  helper = digest::digest(file = file.path(CODE_DIR, '2_analysis/lpdid_build_dataset.R'), algo = 'sha256'),
  runner = digest::digest(file = file.path(CODE_DIR, '2_analysis/appendix/lpdid_threshold_robustness.R'), algo = 'sha256'),
  versions = sapply(c('fixest', 'data.table', 'dplyr'), function(p) as.character(packageVersion(p))),
  R = R.version.string, H = H, horizons = HORIZONS))
signature_path <- file.path(out_dir, 'panel_signature.txt')
same_input <- file.exists(signature_path) && identical(readLines(signature_path), signature)
dt <- load_lpdid_panel(rebuild = !same_input)
writeLines(signature, signature_path)

cells <- list(
  base = list(tcap = NA_real_, strict = FALSE, ccap = 60,
              label = 'Main: both <60'),
  legacy_t30 = list(tcap = 30, strict = FALSE, ccap = 60,
                    label = 'Legacy: treated <=30; controls <60'),
  both_lt30 = list(tcap = 30, strict = TRUE, ccap = 30,
                   label = 'Robustness: both <30'))
saveRDS(list(signature = signature, cells = cells, H = H, horizons = HORIZONS,
             onset_threshold = OVER_THRESHOLD, splice_nov30 = TRUE),
        file.path(out_dir, 'specification.rds'))

# Counts are candidate onset/control rows, not unique counties or the final
# complete-case estimation sample. Also report comparison support by horizon.
eligibility <- bind_rows(lapply(names(cells), function(key) {
  ce <- cells[[key]]
  treated <- lpdid_treated_mask(dt, treated_cap = ce$tcap, treated_strict = ce$strict)
  bind_rows(lapply(HORIZONS, function(h) {
    control <- lpdid_control_mask(dt, h, ce$ccap)
    tibble(cell = key, h = h, n_treated = sum(treated), n_control = sum(control),
      n_treated_counties = uniqueN(dt$county_geoid[treated]),
      n_control_counties = uniqueN(dt$county_geoid[control]),
      n_treated_with_control_state_month = sum(treated & dt$state_year_month %in% unique(dt$state_year_month[control])))
  }))
}))
baseline_counts <- eligibility %>% filter(cell == 'base') %>%
  select(h, baseline_treated = n_treated, baseline_control = n_control,
         baseline_treated_supported = n_treated_with_control_state_month)
eligibility <- eligibility %>% left_join(baseline_counts, by = 'h') %>% mutate(
  treated_removed = baseline_treated - n_treated,
  treated_reduction_pct = 100 * treated_removed / baseline_treated,
  controls_removed = baseline_control - n_control,
  control_reduction_pct = 100 * controls_removed / baseline_control,
  treated_without_control_state_month = n_treated - n_treated_with_control_state_month)
write_csv(eligibility, file.path(out_dir, 'sample_reductions_by_horizon.csv'))

# One checkpoint per horizon/outcome/cell; failed fits are saved but retried on
# restart. The reference row h=-1 is normalized by the original estimator.
fits <- list()
for (key in names(cells)) {
  ce <- cells[[key]]
  for (outcome in c('over_60', 'smoke_as_dv')) {
    dv <- if (outcome == 'over_60') 'dy_' else 'smoke_h_'
    for (h in HORIZONS) {
      path <- file.path(out_dir, sprintf('%s_%s_h%s.rds', key, outcome, horizon_str(h)))
      old <- if (file.exists(path)) readRDS(path) else NULL
      if (!is.null(old) && identical(old$signature, signature) &&
          all(is.finite(old$result$estimate)) && all(is.finite(old$result$se))) {
        result <- old$result
      } else {
        cat('Estimating', key, outcome, 'h =', h, '\n')
        result <- estimate_lpdid(dt, horizons = h, treated_cap = ce$tcap,
          treated_strict = ce$strict, control_threshold = ce$ccap, dv_prefix = dv)
        saveRDS(list(signature = signature, result = result), paste0(path, '.tmp'))
        stopifnot(file.rename(paste0(path, '.tmp'), path))
      }
      fits[[length(fits) + 1L]] <- result %>% mutate(cell = key, outcome = outcome)
    }
  }
}
results <- bind_rows(fits) %>% distinct(cell, outcome, h, .keep_all = TRUE) %>%
  arrange(outcome, cell, h)
write_csv(results, file.path(out_dir, 'estimates_and_used_samples.csv'))
saveRDS(results, file.path(out_dir, 'estimates_and_used_samples.rds'))
used_counts <- results %>% filter(h != -1) %>% left_join(eligibility, by = c('cell', 'h'), suffix = c('', '_eligible'))
used_base <- used_counts %>% filter(cell == 'base') %>% select(outcome, h,
  baseline_treated_used = n_treated_used, baseline_control_used = n_control_used)
used_counts <- used_counts %>% left_join(used_base, by = c('outcome', 'h')) %>% mutate(
  treated_used_reduction_pct = 100 * (1 - n_treated_used / baseline_treated_used),
  control_used_reduction_pct = 100 * (1 - n_control_used / baseline_control_used))
write_csv(used_counts, file.path(out_dir, 'sample_reductions_and_estimation_attrition.csv'))

summary <- results %>% filter(h != -1) %>% group_by(outcome, cell) %>% summarize(
  n_treated = max(n_treated), n_treated_ident_min = min(n_treated_ident),
  n_control = max(n_control), h0 = estimate[h == 0], h1 = estimate[h == 1],
  h1_se = se[h == 1], cum_0_2 = sum(estimate[h %in% 0:2]),
  pre_mean = mean(estimate[h < -1]), .groups = 'drop')
write_csv(summary, file.path(out_dir, 'summary.csv'))

# Compare to archived original output, never to an output just overwritten.
reference_path <- Sys.getenv('LPDID_REFERENCE_SUMMARY', unset = '')
if (nzchar(reference_path)) {
  reference <- read_csv(reference_path, show_col_types = FALSE) %>%
    mutate(cell = recode(cell, t30 = 'legacy_t30')) %>%
    pivot_longer(-c(outcome, cell), names_to = 'statistic', values_to = 'reference')
  comparison <- summary %>% filter(cell != 'both_lt30') %>%
    pivot_longer(-c(outcome, cell), names_to = 'statistic', values_to = 'reproduced') %>%
    full_join(reference, by = c('outcome', 'cell', 'statistic')) %>%
    mutate(difference = reproduced - reference,
      tolerance = if_else(startsWith(statistic, 'n_'), 0, 1e-6),
      agrees = !is.na(difference) & abs(difference) <= tolerance)
  write_csv(comparison, file.path(out_dir, 'legacy_reference_comparison.csv'))
}

# New figure names keep the legacy manuscript PDFs available for comparison.
for (outcome in c('over_60', 'smoke_as_dv')) {
  plotted <- results %>% filter(.data$outcome == .env$outcome) %>%
    mutate(cell = factor(cell, levels = names(cells), labels = sapply(cells, `[[`, 'label')))
  if (outcome == 'over_60') {
    plotted <- plotted %>% mutate(across(c(estimate, ci_low, ci_high), ~100 * expm1(.)))
  }
  p <- ggplot(plotted, aes(h, estimate, color = cell, fill = cell)) +
    geom_hline(yintercept = 0, color = 'grey60') +
    geom_vline(xintercept = -0.5, linetype = 'dashed', color = 'grey60') +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.10, color = NA) +
    geom_line() + geom_point(size = 1.5) + theme_minimal(base_size = 13) +
    theme(legend.position = 'bottom') +
    labs(x = 'Observations since smoke onset',
      y = if (outcome == 'over_60') 'Change in visits (%)' else 'Smoke PM2.5 (ug/m3)',
      color = NULL, fill = NULL,
      caption = '14-observation pre-window; November 30 omitted. County-clustered 95% intervals.')
  ggsave(file.path(APPENDIX_FIGURES_DIR, paste0('lpdid_threshold_robustness_', outcome, '.pdf')),
         p, width = 10, height = 6)
}
print(summary, width = Inf)
if (any(!is.finite(results$estimate[results$h != -1])) ||
    any(!is.finite(results$se[results$h != -1]))) {
  stop('Some horizon estimates are unavailable; inspect the saved diagnostics before interpreting robustness.')
}
