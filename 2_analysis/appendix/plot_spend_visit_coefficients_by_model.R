# Plots the spend-on-visits coefficients from matched_poi_and_split_sample_spend_on_visits.R
# as one untitled figure per results file and fixed-effects set with estimators on the x-axis
# and all vs. matched places distinguished by shape. Error bars are +/- 1.96 SE;
#   Monthly-matched run: table 2 FEs
library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

ESTIMATOR_ORDER <- c(
  "OLS, county total",
  "OLS, same half (stacked)",
  "Split-sample IV (ORIV)"
)

read_results <- function(path) {
  out <- read_csv(path, show_col_types = FALSE) %>%
    #Filtering out the unused estimators
    filter(str_sub(estimator, start=-7)!= "tercile") %>% 
    filter(str_sub(estimator, end=4)!= "PPML") %>% 
    mutate(
      estimator = factor(estimator, levels = ESTIMATOR_ORDER),
      sample = factor(sample, levels = c("all places", "matched places")),
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )
  stopifnot(!anyNA(out$estimator))
  out
}

monthly_matched <- read_results(SPEND_ON_VISITS_SPLIT_SAMPLE_RESULTS_PATH)

DODGE_WIDTH   <- 0.5
WHISKER_WIDTH <- 0.1

# x positions are dodged by hand so all vs. matched places sit side by side at each estimator.
plot_coefficients <- function(df) {
  df <- df %>%
    group_by(estimator) %>%
    mutate(x = as.numeric(estimator) + (n() > 1) * (as.numeric(sample) - 1.5) * DODGE_WIDTH / 2) %>%
    ungroup()

  ggplot(df, aes(x = x, y = estimate, shape = sample)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_errorbar(data = filter(df, !is.na(se)),
                  aes(ymin = ci_low, ymax = ci_high), width = WHISKER_WIDTH) +
    geom_point(size = 2.5, fill = "white") +
    scale_shape_manual(values = c("all places" = 16, "matched places" = 21), name = NULL) +
    scale_x_continuous(breaks = seq_along(levels(df$estimator)),
                       labels = str_wrap(levels(df$estimator), width = 13),
                       limits = c(0.5, length(levels(df$estimator)) + 0.5), expand = c(0, 0)) +
    labs(x = NULL, y = "Coefficient on asinh(visits)") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 8)
    )
}

ggsave(file.path(APPENDIX_FIGURES_DIR,
                 paste0("spend_visit_coefficients_by_model_table_2_fes.pdf")),
       plot_coefficients(filter(monthly_matched, fixed_effects == "table 2 FEs")), width = 8, height = 5)

