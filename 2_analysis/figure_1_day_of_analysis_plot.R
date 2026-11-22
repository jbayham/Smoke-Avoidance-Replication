library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(PLOTTING_HELPERS_PATH)

est_bin_base <- readRDS(file.path(REGRESSION_RESULTS_DIR, "binary_base_day_of_dose_response.rds"))


baseline_daily_df <- add_new_sample_and_sector(
  reg_results = est_bin_base,
  sector_name = "Deferrable Sectors",
  sample_label = "Overall Deferrable Foot Traffic",
  overall_sample_label = "Sample",
  vars_to_plot_start = "bin_d_"
)

daily_base_plot_data <- baseline_daily_df %>%
  mutate(
    # The translation between % change and the estimate is approximate.
    # This makes it officially a percent change:
    low_ci = Estimate - 1.96 * Std..Error,
    high_ci = Estimate + 1.96 * Std..Error,
    Estimate = exp(Estimate) - 1,
    low_ci   = exp(low_ci) - 1,
    high_ci  = exp(high_ci) - 1,
    bin = factor(BIN_LABELS[vars], levels = unname(BIN_LABELS)),
  )


daily_base_plot <- ggplot(daily_base_plot_data, aes(x = bin, y = Estimate, color = bin, ymin = low_ci, ymax = high_ci)) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_errorbar(width = 0.2, linewidth = 1) +
  geom_point(size = 3) +
  labs(x = expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))) +
  scale_color_manual(
    "Smoke PM AQI\nBin equivalent",
    values = BIN_COLORS,
    labels = BIN_LABELS
  ) +
  scale_y_continuous("Daily Change in Visits", labels = scales::label_percent()) +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(color = "none")


ggsave("~/Documents/Research/Avoidance Paper/2_analysis/figures/figure_1a_dose_response_daily_effect.pdf",
       plot = daily_base_plot,
       width = 7, height = 5, dpi = 300, units = "in")
