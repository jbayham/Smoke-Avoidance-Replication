# Calendar checks against the production builder, without loading research data.
suppressPackageStartupMessages({library(dplyr); library(lubridate)})
exprs <- parse('1_build/reg_data/build_reg_data_weekly.R')
definition <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('<-')) &&
                       identical(x[[2]], as.name('add_tue_mon_week')), as.list(exprs))
stopifnot(length(definition) == 1L)
eval(definition[[1]])
days <- tibble(date = seq(as.Date('2018-12-25'), as.Date('2024-01-08'), by = 'day'))
x <- add_tue_mon_week(days, date)
stopifnot(all(wday(x$week_start) == 3L),
          all(as.integer(x$date - x$week_start) %in% 0:6),
          all(x$week_and_year == sprintf('%02d%d', isoweek(x$date - 1), isoyear(x$date - 1))))
# One county-week must not be split by the calendar year at New Year.
new_year <- filter(x, date >= as.Date('2019-12-31'), date <= as.Date('2020-01-06'))
stopifnot(n_distinct(new_year$week_and_year) == 1L, all(new_year$week_and_year == '012020'))
# The fourth day always identifies the month containing most of the full week.
starts <- unique(x$week_start)
majority_month <- function(start) {
  counts <- table(format(as.Date(start, origin = '1970-01-01') + 0:6, '%Y-%m'))
  names(counts)[which.max(counts)]
}
stopifnot(identical(format(starts + 3, '%Y-%m'), vapply(starts, majority_month, character(1))))
set.seed(28)
shuffled <- add_tue_mon_week(days[sample(nrow(days)), , drop = FALSE], date) %>% arrange(date)
stopifnot(isTRUE(all.equal(x, shuffled, tolerance = 0)))
cat('PASS: Tuesday-Monday weeks, New Year, majority month and row-order invariance.\n')
