library(tidyverse)
library(data.table)
library(fixest)
library(scales)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(file.path(CODE_DIR, "2_analysis", "lpdid_build_dataset.R"))
source(PLOTTING_HELPERS_PATH)

# ==============================================================================
# APPENDIX: does a quiet pre-period on the TREATED side change the result?
#
# The main LP-DiD (2_analysis/daily_visits_lpdid.R) requires only that a treated
# onset have no OTHER >= 60 ug/m3 day in the preceding 14 days. It does not
# require those days to be clean: the median onset is preceded by a 14-day
# window whose max smoke is ~32 ug/m3. If counties are already curtailing trips
# under that moderate smoke then y_{t-1} is depressed at baseline and beta^h
# understates the full response.
#
# This appendix reports the main results alongside one cut:
#
#   base   every clean onset                                (= main spec)
#   t30    onset counts as treated only if its pre-window
#          max smoke over [t-14, t-1] is <= 30 ug/m3
#
# The cap is on the TREATED side only, over the pre-window only -- smoke from t
# onward IS the treatment. The clean-control group is identical in both cells,
# which follows the convention already used for the spell-length cuts in the
# main script: only the treated sample is subset.
#
# Everything else -- panel, horizons, controls, fixed effects, county
# clustering -- comes from lpdid_build_dataset.R and is identical across cells.
#
# Both outcomes are estimated for each cell:
#   (i)  asinh(daily deferrable visits), long difference y_{t+h} - y_{t-1}
#   (ii) smoke PM (adj_pm_smoke_med_1sd) in LEVELS at t+h -- the descriptive
#        "first stage" showing how much smoke each treated sample actually sees
# ==============================================================================

# Default TRUE: a plain run refits every cell, so results can never go stale
# against a changed panel. Set LPDID_REFIT <- FALSE before sourcing to reuse
# cells already on disk -- only safe when nothing upstream has changed.
REFIT <- if (exists("LPDID_REFIT")) LPDID_REFIT else TRUE

dt <- load_lpdid_panel()

cells <- list(
  list(key = "base", tcap = NA_real_,
       lab = "No pre-period cap (main spec)"),
  list(key = "t30",  tcap = APPENDIX_TREATED_CAP,
       lab = sprintf("Pre-period max <= %g", APPENDIX_TREATED_CAP))
)

cat("\n=== Treated sample by cell ===\n")
for (ce in cells) {
  cat(sprintf("  %-5s %-32s treated onsets = %6s\n", ce$key, ce$lab,
              format(sum(lpdid_treated_mask(dt, "is_onset", ce$tcap)),
                     big.mark = ",")))
}
cat(sprintf("  clean controls @ h = 0: %s | @ h = %d: %s (identical in both cells)\n",
            format(sum(lpdid_control_mask(dt, 0L)), big.mark = ","), K_HIGH,
            format(sum(lpdid_control_mask(dt, K_HIGH)), big.mark = ",")))

outcomes <- list(
  list(tag = "over_60",     dv = "dy_"),        # visits, long difference
  list(tag = "smoke_as_dv", dv = "smoke_h_")    # smoke PM, levels
)

res_path <- function(tag, key)
  file.path(REGRESSION_RESULTS_DIR,
            sprintf("lpdid_cc_%s_prior_smoke_%s.rds", tag, key))


# ==============================================================================
# Estimate
# ==============================================================================

for (ce in cells) {
  for (oc in outcomes) {
    if (!REFIT && file.exists(res_path(oc$tag, ce$key))) {
      cat(sprintf("\n--- LP-DiD [%s] %s: reusing cached fit ---\n", oc$tag, ce$key))
      next
    }
    cat(sprintf("\n--- LP-DiD [%s] %s: %s ---\n", oc$tag, ce$key, ce$lab))
    res <- estimate_lpdid(dt, HORIZONS,
                          onset_var   = "is_onset",
                          treated_cap = ce$tcap,
                          dv_prefix   = oc$dv)
    saveRDS(res, res_path(oc$tag, ce$key))
    cat("Done.\n")
  }
}


# ==============================================================================
# Plot
# ==============================================================================

res_all <- map_dfr(outcomes, function(oc) {
  map_dfr(cells, function(ce) {
    p <- res_path(oc$tag, ce$key)
    if (!file.exists(p)) { warning("missing: ", p); return(NULL) }
    readRDS(p) %>% mutate(outcome = oc$tag, cell = ce$key)
  })
})

saveRDS(res_all, file.path(REGRESSION_RESULTS_DIR,
                           "lpdid_prior_smoke_treated_cap.rds"))

# Level names are plain ASCII (the default pdf device font has no "<=" glyph);
# the legend renders them as plotmath via `labels =` below.
cell_colors <- c(base = "#455A64", t30 = "#C62828")
cell_labels <- c(
  expression("No pre-period cap"),
  bquote("Pre-period max" <= .(APPENDIX_TREATED_CAP) ~ mu * "g/m"^3)
)

plot_df <- res_all %>% mutate(cell = factor(cell, levels = names(cell_colors)))

make_plot <- function(tag, ylab, pct) {
  p <- ggplot(plot_df %>% filter(outcome == tag),
              aes(x = h, y = estimate, color = cell, fill = cell)) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "darkgrey", linetype = "dashed") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2, position = position_dodge(width = 0.4)) +
    scale_color_manual(values = cell_colors, labels = cell_labels) +
    scale_fill_manual(values  = cell_colors, labels = cell_labels) +
    labs(x = "Days Since Smoke Onset", y = ylab, color = NULL, fill = NULL) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom")
  if (pct) p <- p + scale_y_continuous(labels = label_percent())
  p
}

ggsave(file.path(APPENDIX_FIGURES_DIR, "lpdid_prior_smoke_treated_cap_visits.pdf"),
       plot  = make_plot("over_60", "% Change in Daily Visits", TRUE),
       width = 9, height = 6, dpi = 300, units = "in")

ggsave(file.path(APPENDIX_FIGURES_DIR, "lpdid_prior_smoke_treated_cap_smoke.pdf"),
       plot  = make_plot("smoke_as_dv",
                         expression("Smoke PM"[2.5] ~ "(" * mu * "g/m"^3 * ")"),
                         FALSE),
       width = 9, height = 6, dpi = 300, units = "in")


# ==============================================================================
# Comparison table
# ==============================================================================

summ <- res_all %>%
  filter(h != -1) %>%
  group_by(outcome, cell) %>%
  summarise(n_treated = max(n_treated, na.rm = TRUE),
            # min across horizons of the treated rows that actually identify beta;
            # equals n_treated unless a fixed effect is absorbing treated rows
            n_treated_ident_min = min(n_treated_ident, na.rm = TRUE),
            n_control = max(n_control, na.rm = TRUE),
            h0        = estimate[h == 0],
            h1        = estimate[h == 1],
            h1_se     = se[h == 1],
            cum_0_2   = sum(estimate[h %in% 0:2]),
            pre_mean  = mean(estimate[h < -1]),
            .groups   = "drop") %>%
  arrange(outcome, cell)

cat("\n=== TREATED PRE-PERIOD CAP SUMMARY ===\n")
print(as.data.frame(summ), digits = 3)
write_csv(summ, file.path(APPENDIX_TABLES_DIR, "lpdid_prior_smoke_treated_cap.csv"))

cat("\n=== Treated pre-period cap appendix done. ===\n")
