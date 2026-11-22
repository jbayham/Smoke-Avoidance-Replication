# Execute the appendix script's actual key-building mutate on dates that would
# collide under the old state x day-of-month definition.
suppressPackageStartupMessages(library(dplyr))
expressions <- parse('2_analysis/appendix/robustness_additional_fixed_effects.R')
find_mutate <- function(x) {
  if (!is.call(x)) return(NULL)
  if (identical(x[[1]], quote(mutate))) return(x)
  for (item in as.list(x)[-1]) {
    result <- find_mutate(item)
    if (!is.null(result)) return(result)
  }
  NULL
}
calls <- Filter(Negate(is.null), lapply(expressions, find_mutate))
stopifnot(length(calls) == 1)
fixture <- tibble(county_geoid = c('01001','01003','01001','02013'),
                  date = as.Date(c('2020-01-01','2020-01-01','2020-02-01','2020-01-01')),
                  binned_pr = 1, binned_tmmn = 1, binned_tmmx = 1, binned_rmax = 1)
result <- eval(as.call(c(list(quote(dplyr::mutate), quote(fixture)), as.list(calls[[1]])[-1])))
stopifnot(identical(result$year_month,c('2020-01','2020-01','2020-02','2020-01')),
          result$state_day[1] == result$state_day[2],
          result$state_day[1] != result$state_day[3],
          result$state_day[1] != result$state_day[4],
          n_distinct(result$state_day) == 3,
          n_distinct(paste(result$county_geoid,result$year_month)) == 4)
cat('PASS: full-date state keys and county-year-month groups.\n')
