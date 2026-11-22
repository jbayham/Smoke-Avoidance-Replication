# ============================================================================
# spatial_substitution_estimate_plot.R
#
# Three-panel SDEM impacts figure for the fit saved by
# spatial_substitution_estimate_run.R: (a) direct, (b) indirect,
# (c) typical episode. The total-effect endpoint is left out, and there is no
# title or subtitle so the figure can be captioned in the paper.
#
# Run standalone after spatial_substitution_estimate_run.R has saved its
# results.
# ============================================================================

library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

results <- readRDS(file.path(REGRESSION_RESULTS_DIR,
                             "spatial_sdem_v4_dcs_units_results.rds"))

EFFECT_LEVELS <- c("direct", "indirect", "typical_episode")
EFFECT_LABELS <- c("(a) Direct\n(own-county smoke)",
                   "(b) Indirect\n(neighbors' smoke)",
                   "(c) Typical Episode\n(average neighbors' smoke)")

plot_data <- results$impacts$ml %>%
  bind_rows(results$typical_episode) %>%
  filter(effect_type %in% EFFECT_LEVELS) %>%
  mutate(
    effect_type = factor(effect_type, levels = EFFECT_LEVELS, labels = EFFECT_LABELS),
    variable = factor(BIN_LABELS[variable], levels = unname(BIN_LABELS)),
    # exp(b) - 1 puts the coefficient on a proportional-change scale. exp() is
    # monotone, so transforming the CI ENDPOINTS carries the coverage over
    # exactly.
    ci_low   = exp(ci_low) - 1,
    ci_high  = exp(ci_high) - 1,
    estimate = exp(estimate) - 1
  )
stopifnot(nlevels(droplevels(plot_data$effect_type)) == 3)

sdem_three_panel_plot <- ggplot(plot_data, aes(x = variable, y = estimate, color = variable,
                                               ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_errorbar(width = 0.2, linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~effect_type, nrow = 1) +
  scale_x_discrete(expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")")), labels = BIN_LABELS) +
  scale_color_manual(values = BIN_COLORS, labels = BIN_LABELS) +
  scale_y_continuous("Daily Change in Visits", labels = scales::label_percent()) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(color = "none")

fig_path <- file.path(FIGURES_DIR,
                      "spatial_sdem_direct_indirect_typical_episode_v4_dcs_units.pdf")
ggsave(fig_path, plot = sdem_three_panel_plot, width = 12, height = 5.5, dpi = 300, units = "in")
cat("Saved plot to", fig_path, "\n")

print(as.data.frame(plot_data %>% select(effect_type, variable, estimate, ci_low, ci_high)))
