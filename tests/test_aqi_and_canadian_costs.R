# Exercise the actual expressions without rebuilding inputs or refitting models.
suppressPackageStartupMessages(library(tidyverse))
root <- Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd())
assignment <- function(expressions, name) {
  matches <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('<-')) &&
    identical(x[[2]], as.name(name)), expressions)
  stopifnot(length(matches) == 1L)
  matches[[1]]
}

aqi <- parse(file.path(root, '1_build/smoke_pm/combine_and_process_smoke_with_aqi_filtering.R'))
eval(assignment(aqi, 'lerp'))
eval(assignment(aqi, 'convert_pm25_to_aqi'))
pm <- c(-1, 0, 9, 9.09, 9.1, 12, 35.4, 35.5, 55.4, 55.5,
        150.4, 150.5, 250.4, 250.5, 350.4, 350.5, 500.4, 500.5, NA)
expected <- c(0, 0, 50, 50, 51, 56, 100, 101, 150, 151,
              200, 201, 300, 301, 400, 401, 500, NA, NA)
stopifnot(identical(convert_pm25_to_aqi(pm), expected))
# The collaborator changed only the first two intervals. Preserve the supplied
# upper intervals and missing-value behavior rather than changing the convention.
stopifnot(all(diff(convert_pm25_to_aqi(seq(8, 13, by = 0.1))) >= 0))
cat('PASS: corrected AQI lower breakpoints, truncation, upper intervals and missing values.\n')

costs <- parse(file.path(root, '2_analysis/estimate_welfare_losses_from_spending.R'))
# Deliberately scramble bin order and include a low bin: counts must match names.
welfare_per_bin <- tibble(bin = c('200_500', '001_015', '060_100', '100_200'),
  change_in_spend_from_smoke = c(-30, -10000, -10, -20), welfare = c(3, 10000, 1, 2))
eval(assignment(costs, 'canada_estimate'))
stopifnot(nrow(canada_estimate) == 1L,
  identical(unname(unlist(select(canada_estimate, starts_with('days_')))), c(1218, 645, 6)),
  identical(unname(unlist(select(canada_estimate, starts_with('spend_')))), c(-12180, -12900, -180)),
  identical(unname(unlist(select(canada_estimate, starts_with('welfare_')))), c(-1218, -1290, -18)))
messages <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('print')) &&
  any(grepl('Canada Smoke', deparse(x), fixed = TRUE)), costs)
stopifnot(length(messages) == 2L)
printed <- capture.output(for (x in messages) eval(x))
stopifnot(any(grepl('Canada Smoke Spend: -25260', printed, fixed = TRUE)),
          any(grepl('Canada Smoke Welfare: -2526', printed, fixed = TRUE)))
cat('PASS: Canadian-smoke county-day counts, named bin matching, cost totals and reported signs.\n')
