# R_LIBS_USER=<run>/library Rscript --vanilla tests/test_weekend_figure_extraction.R
suppressPackageStartupMessages(library(tidyverse))
# Load only the extraction functions, without reading the large saved models.
needed <- c("interaction_coefficient", "extract_weekend_estimates",
            "extract_group_levels", "extract_group_differences", "estimated_quartiles",
            "regression_key", "collect_group_estimates")
for (expr in parse("2_analysis/figure_2_day_of_analysis_heterogeneities_plot.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      as.character(expr[[2]])[1] %in% needed) eval(expr)
}
set.seed(126)
d <- expand.grid(county = 1:20, date = 1:40)
d$is_weekend <- d$date %% 7 %in% c(0, 6)
d$exposure <- sample(0:2, nrow(d), replace = TRUE)
d$bin_a <- as.numeric(d$exposure == 1)
d$bin_b <- as.numeric(d$exposure == 2)
d$y <- rnorm(nrow(d)) + d$bin_a * (-0.2 - 0.3 * d$is_weekend) +
  d$bin_b * (-0.5 + 0.1 * d$is_weekend) + sin(d$date)
direct <- lm(y ~ bin_a:is_weekend + bin_b:is_weekend + factor(date), d)
baseline <- lm(y ~ bin_a + bin_b + bin_a:I(as.numeric(is_weekend)) +
                 bin_b:I(as.numeric(is_weekend)) + factor(date), d)
stopifnot(max(abs(fitted(direct) - fitted(baseline))) < 1e-10)
smoke_bins <- c("bin_a", "bin_b")
fit <- list(betas = coef(direct), vcov = vcov(direct))
actual <- extract_weekend_estimates(fit)
for (bin in smoke_bins) {
  term <- paste0(bin, ":I(as.numeric(is_weekend))")
  b <- coef(baseline); V <- vcov(baseline)
  expected <- c(b[[bin]], b[[bin]] + b[[term]])
  expected_se <- sqrt(c(V[bin, bin], V[bin, bin] + V[term, term] + 2*V[bin, term]))
  rows <- actual$levels[actual$levels$bin == bin, ]
  contrast <- actual$differences[actual$differences$bin == bin, ]
  stopifnot(max(abs(rows$Estimate - expected)) < 1e-10,
            max(abs(rows$Std..Error - expected_se)) < 1e-10,
            abs(contrast$Estimate - b[[term]]) < 1e-10,
            abs(contrast$Std..Error - sqrt(V[term, term])) < 1e-10)
}
# Interaction order is arbitrary; unavailable slopes must not become zero effects.
renamed <- sub("^(.*):(.*)$", "\\2:\\1", names(fit$betas))
names(fit$betas) <- renamed
dimnames(fit$vcov) <- list(renamed, renamed)
stopifnot(isTRUE(all.equal(actual, extract_weekend_estimates(fit))))
missing <- interaction_coefficient(names(fit$betas), "bin_a", "is_weekendTRUE")
fit$betas <- fit$betas[names(fit$betas) != missing]
fit$vcov <- fit$vcov[names(fit$betas), names(fit$betas)]
partial <- extract_weekend_estimates(fit)
stopifnot(is.na(partial$levels$Estimate[partial$levels$bin == "bin_a" &
                                       partial$levels$Group == "Weekend"]),
          is.na(partial$differences$Estimate[partial$differences$bin == "bin_a"]))
# Each date has exactly one weekend status: interacting date with it adds no groups.
date_group <- interaction(d$date, d$is_weekend, drop = TRUE)
stopifnot(nlevels(date_group) == length(unique(d$date)))
# The appendix omits weekend only; county panels retain their specified models.
county_betas <- c(bin_a = -0.2, bin_b = -0.5,
                  "bin_a:income_Q2" = 0.1, "bin_b:income_Q2" = 0.2)
county_vcov <- diag(0.01, 4)
dimnames(county_vcov) <- list(names(county_betas), names(county_betas))
heterogeneity_specs <- list(list(label = "income", sample = "Median Household Income"))
REGRESSIONS <- list(weekend = list(betas = coef(direct), vcov = vcov(direct)),
                    income = list(betas = county_betas, vcov = county_vcov),
                    income_appendix = list(betas = county_betas, vcov = county_vcov))
main <- collect_group_estimates(list(output_suffix = "", appendix_only = FALSE))
appendix <- collect_group_estimates(list(output_suffix = "_appendix", appendix_only = TRUE))
stopifnot("weekend" %in% names(main$levels), "weekend" %in% names(main$differences),
          !"weekend" %in% names(appendix$levels), !"weekend" %in% names(appendix$differences),
          identical(main$levels$income, appendix$levels$income),
          identical(main$differences$income, appendix$differences$income))
cat("PASS: levels and contrasts match equivalent fitted parameterization; covariance,\n",
    "interaction ordering, missing slopes, date/weekend grouping, and appendix omission checked.\n")
