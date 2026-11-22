library(tidyverse)
library(data.table)
library(fixest)
library(scales)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(file.path(CODE_DIR, "2_analysis", "lpdid_build_dataset.R"))
source(PLOTTING_HELPERS_PATH)

# ==============================================================================
# MAIN LP-DiD RESULTS
#
# The specification, the panel, the horizons, and estimate_lpdid() all live in
# 2_analysis/lpdid_build_dataset.R -- see the header there for the estimating
# equation, the sample rule, and how the balanced panel is handled.
#
# This is the plain specification: no smoke caps on either side.
#
#   TREATED  any onset with a clean 14-day washout (D = 0 on t-14 .. t-1)
#   CONTROL  the standard clean-control rule, D = 0 across [t-14, t+max(h,0)]
#
# The treated-side pre-period robustness cut lives in
# 2_analysis/appendix/prior_smoke_filter_lpdid_run_and_plot.R.
#
# Standard errors are clustered on county. estimate_lpdid() also returns
# two-way county + state-year-month and county + date SEs as reference columns,
# but the reported se and the plotted CIs are county-clustered.
#
# SPELL-LENGTH HETEROGENEITY
#
#   Does the dynamic response differ for short vs. persistent smoke events?
#     - 1-day spell:  onset at t, D_{t+1} = 0
#     - 2-day spell:  onset at t, D_{t+1} = 1, D_{t+2} = 0
#     - 3+ day spell: onset at t, D_{t+1} = 1, D_{t+2} = 1
#
#   The longest bucket is top-coded at 3 days rather than split into exact-3 and
#   4+, so every onset lands in exactly one bucket and the persistent-smoke
#   group is not thinned to the point of being uninformative.
#
#   Only the treated sample is subset by spell duration; the clean control group
#   is the same for all three.
#
# OUTPUT
#   figures/lpdid_cc_over_60.pdf                 main event study
#   figures/lpdid_cc_smoke_as_dv.pdf             smoke PM "first stage"
#   figures/lpdid_cc_spell_progressive_{1,2,3}.pdf
#                                                progressive reveal of the three
#                                                spell curves on shared axes,
#                                                for presentations
#   figures/lpdid_cc_spell_progressive_3_ci_bars.pdf
#                                                final frame with per-horizon CI
#                                                error bars over the bands
# ==============================================================================

dt <- load_lpdid_panel()

cat("\n=== Sample (no smoke caps) ===\n")
for (ov in c("is_onset", "is_onset_1day", "is_onset_2day", "is_onset_3plus")) {
  cat(sprintf("  %-15s onsets: %6s\n", ov,
              format(sum(lpdid_treated_mask(dt, ov)), big.mark = ",")))
}
for (h in c(0L, K_HIGH)) {
  cat(sprintf("  clean controls @ h = %2d: %s\n", h,
              format(sum(lpdid_control_mask(dt, h)), big.mark = ",")))
}


# ==============================================================================
# Step 1: Estimate
# ==============================================================================

specs <- list(
  list(file = "lpdid_cc_over_60",      onset = "is_onset",       dv = "dy_",
       lab  = "Any onset >60 ug/m3"),
  list(file = "lpdid_cc_spell_1day",   onset = "is_onset_1day",  dv = "dy_",
       lab  = "1-day spells"),
  list(file = "lpdid_cc_spell_2day",   onset = "is_onset_2day",  dv = "dy_",
       lab  = "2-day spells"),
  list(file = "lpdid_cc_spell_3plus",  onset = "is_onset_3plus", dv = "dy_",
       lab  = "3+ day spells"),
  # Descriptive "first stage": smoke PM in levels at t+h on the same sample.
  list(file = "lpdid_cc_smoke_as_dv",  onset = "is_onset",       dv = "smoke_h_",
       lab  = "Smoke PM as DV")
)

for (sp in specs) {
  cat("\n--- Estimating LP-DiD:", sp$lab, "---\n")
  res <- estimate_lpdid(dt, HORIZONS, onset_var = sp$onset, dv_prefix = sp$dv)
  saveRDS(res, file.path(REGRESSION_RESULTS_DIR, paste0(sp$file, ".rds")))
  cat("Done.\n")
}


# ==============================================================================
# Step 2: Main event-study plot
# ==============================================================================

read_res <- function(f) readRDS(file.path(REGRESSION_RESULTS_DIR, paste0(f, ".rds")))

results_any <- read_res("lpdid_cc_over_60")
print(results_any, n = Inf)

plot_lpdid <- function(results, ylab, color = "red", pct = TRUE) {
  p <- ggplot(results, aes(x = h, y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "darkgrey", linetype = "dashed") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, fill = color) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                  width = 0.3, color = color, linewidth = 0.8) +
    geom_point(color = color, size = 2.5) +
    labs(x = "Days Since Smoke Event", y = ylab) +
    theme_minimal(base_size = 16)
  if (pct) p <- p + scale_y_continuous(labels = label_percent())
  p
}

ggsave(file.path(FIGURES_DIR, "lpdid_cc_over_60.pdf"),
       plot  = plot_lpdid(results_any, "Daily Change in Visits"),
       width = 7, height = 5, dpi = 300, units = "in")


# ==============================================================================
# Step 3: Smoke PM as dependent variable (descriptive "first stage")
#
# For each horizon h, on the same sample:
#   smoke_{i,t+h} = beta^h * onset_{it} + X_{it} gamma + FEs + e_{it}
# ==============================================================================

results_smoke_dv <- read_res("lpdid_cc_smoke_as_dv")
print(results_smoke_dv, n = Inf)

ggsave(file.path(FIGURES_DIR, "lpdid_cc_smoke_as_dv.pdf"),
       plot  = plot_lpdid(results_smoke_dv,
                          bquote("Smoke PM"[2.5] ~ " (" * mu * "g/m"^3 * ")"),
                          color = "darkblue", pct = FALSE),
       width = 7, height = 5, dpi = 300, units = "in")


# ==============================================================================
# Step 4: Spell length -- progressive reveal on shared axes
#
# One figure per cumulative subset (1-day; 1+2-day; all three), so the curves
# can be layered in one at a time in a talk. Axes are fixed across all three.
# ==============================================================================

spell_colors <- c("1-day spell"  = "#2196F3",
                  "2-day spell"  = "#FF9800",
                  "3+ day spell" = "#D32F2F")

spell_combined <- bind_rows(
  read_res("lpdid_cc_spell_1day")  %>% mutate(spell = "1-day spell"),
  read_res("lpdid_cc_spell_2day")  %>% mutate(spell = "2-day spell"),
  read_res("lpdid_cc_spell_3plus") %>% mutate(spell = "3+ day spell")
) %>%
  mutate(spell = factor(spell, levels = names(spell_colors)))

y_range      <- range(c(spell_combined$ci_low, spell_combined$ci_high), na.rm = TRUE)
x_range      <- range(spell_combined$h, na.rm = TRUE)
spell_levels <- names(spell_colors)

# Points and error bars share one dodge so each horizon's bar sits under its
# own point; the ribbon is not dodged.
spell_dodge <- position_dodge(width = 0.4)

# ci_bars = TRUE draws per-horizon CI error bars on top of the shaded band, the
# way the single-series plots in plot_lpdid() do.
plot_spell <- function(dat, ci_bars = FALSE) {
  p <- ggplot(dat, aes(x = h, y = estimate, color = spell, fill = spell)) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "darkgrey", linetype = "dashed") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.6)
  if (ci_bars) {
    p <- p + geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                           width = 0.3, linewidth = 0.5, alpha = 0.8,
                           position = spell_dodge)
  }
  p +
    geom_point(size = 2, position = spell_dodge) +
    # Keep every level in the legend from the first frame on, so the legend box
    # does not resize as curves are added.
    scale_color_manual(values = spell_colors, limits = spell_levels, drop = FALSE) +
    scale_fill_manual(values  = spell_colors, limits = spell_levels, drop = FALSE) +
    scale_y_continuous(labels = label_percent(), limits = y_range) +
    scale_x_continuous(limits = x_range) +
    labs(x = "Days Since Smoke Onset", y = "Daily Change in Visits",
         color = "Spell Duration", fill = "Spell Duration") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "inside", legend.position.inside = c(0.8, 0.2))
}

for (i in seq_along(spell_levels)) {
  dat_i <- spell_combined %>%
    filter(spell %in% spell_levels[1:i]) %>%
    mutate(spell = droplevels(spell))

  ggsave(file.path(FIGURES_DIR, paste0("lpdid_cc_spell_progressive_", i, ".pdf")),
         plot = plot_spell(dat_i), width = 9, height = 6, dpi = 300, units = "in")
}

# Same final frame, with CI error bars drawn in addition to the shaded band.
ggsave(file.path(FIGURES_DIR, "lpdid_cc_spell_progressive_3_ci_bars.pdf"),
       plot  = plot_spell(spell_combined, ci_bars = TRUE),
       width = 9, height = 6, dpi = 300, units = "in")

cat("\n=== Main LP-DiD done. ===\n")
print(spell_combined %>% select(spell, h, estimate, se, n_obs, n_treated) %>%
      arrange(spell, h), n = Inf)
