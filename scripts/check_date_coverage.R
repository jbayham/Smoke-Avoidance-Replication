# Compare calendar values, independent of integer/double Date storage.
# The only accepted absent input dates are the documented November 30 visits.
check_date_coverage <- function(date, month, kind) {
  stopifnot(kind %in% c('spend', 'visits'), length(month) == 1L)
  bounds <- seq(as.Date(paste0(month, '-01')), by='month', length.out=2)
  expected <- as.character(seq(bounds[1], bounds[2]-1, by='day'))
  observed <- sort(unique(as.character(as.Date(date))))
  missing <- setdiff(expected, observed)
  unexpected <- setdiff(observed, expected)
  allowed <- if (kind == 'visits') expected[substr(expected, 6, 10) == '11-30'] else character()
  list(ok = length(date) > 0L && !anyNA(date) &&
         length(setdiff(missing, allowed)) == 0L && length(unexpected) == 0L,
       missing = missing, unexpected = unexpected,
       allowed_missing = intersect(missing, allowed))
}
