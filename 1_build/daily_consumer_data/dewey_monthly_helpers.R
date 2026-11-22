# Shared readers for current Dewey monthly deliveries. These originate in the
# matched-POI builder and are also used to rebuild the main county totals.
open_lowercase <- function(path) {
  ds <- arrow::open_dataset(path)
  dplyr::select(ds, dplyr::all_of(setNames(names(ds), tolower(names(ds)))))
}

list_monthly_files <- function(dir, first_month = FIRST_MONTH, last_month = LAST_MONTH) {
  if (!dir.exists(dir)) stop('Missing delivery directory: ', dir)
  paths <- sort(list.files(dir, pattern = '^\\d{4}-\\d{2}--.*\\.parquet$', full.names = TRUE))
  files <- data.table::data.table(path = paths, month = as.Date(paste0(substr(basename(paths), 1, 7), '-01')))
  files[month >= first_month & month <= last_month]
}

require_columns <- function(path, needed) {
  have <- names(open_lowercase(path))
  missing <- setdiff(needed, have)
  if (length(missing)) stop(basename(path), ' lacks ', paste(missing, collapse = ', '))
}

to_start_date <- function(x) {
  if (inherits(x, 'POSIXct')) as.Date(x, tz = 'UTC')
  else if (inherits(x, 'Date')) x
  else as.Date(substr(as.character(x), 1, 10))
}

cbg_to_county <- function(poi_cbg) {
  cbg <- as.character(poi_cbg)
  cbg <- paste0(strrep('0', pmax(12 - nchar(cbg), 0)), cbg)
  ifelse(is.na(poi_cbg), NA_character_, substr(cbg, 1, 5))
}

# Daily arrays may be JSON strings wrapped in extra literal quotes. Null days
# count as zero, as in the newer matched-POI builder. Invalid tokens stop a build.
sum_daily_arrays <- function(places, array_col, value_name,
                             group_cols = c('county_geoid', 'half', 'is_matched'),
                             first_cols = character(), chunk_rows = CHUNK_ROWS) {
  places <- data.table::copy(places)
  places[, cell := .GRP, by = group_cols]
  cells <- unique(places[, c('cell', group_cols), with = FALSE])
  empty <- cells[0, ..group_cols]
  empty[, `:=`(date = integer(), value = numeric(), n_active = integer())]
  for (col in first_cols) empty[[col]] <- places[[col]][0]
  data.table::setnames(empty, c('value', 'n_active'), c(value_name, paste0('n_places_with_', value_name)))
  if (!nrow(places)) return(empty)
  if (anyNA(places$start_date)) stop('Missing or invalid array start date.')
  chunks <- lapply(seq.int(1L, nrow(places), by = chunk_rows), function(first_row) {
    rows <- first_row:min(first_row + chunk_rows - 1L, nrow(places))
    strings <- places[[array_col]][rows]
    if (anyNA(strings)) stop('Missing daily array in ', array_col)
    arrays <- strsplit(gsub('[\\[\\]\\s"]', '', strings, perl = TRUE), ',', fixed = TRUE)
    n_days <- lengths(arrays)
    tokens <- unlist(arrays, use.names = FALSE)
    values <- suppressWarnings(as.numeric(tokens))
    if (any(is.na(values) & tolower(tokens) != 'null') || any(!is.finite(values[!is.na(values)]))) {
      stop('Invalid daily-array value in ', array_col)
    }
    values[is.na(values)] <- 0
    expanded <- data.table::data.table(
      cell = rep(places$cell[rows], n_days),
      date = rep(as.integer(places$start_date[rows]), n_days) + sequence(n_days) - 1L,
      value = values, source_row = rep(rows, n_days))
    if (!nrow(expanded)) return(data.table::data.table(
      cell = integer(), date = integer(), value = numeric(), n_active = integer(), source_row = integer()))
    expanded[, .(value = sum(value), n_active = sum(value > 0), source_row = min(source_row)),
             by = .(cell, date)]
  })
  sums <- data.table::rbindlist(chunks)
  if (!nrow(sums)) return(empty)
  sums <- sums[, .(value = sum(value), n_active = sum(n_active), source_row = min(source_row)), by = .(cell, date)]
  for (col in first_cols) sums[[col]] <- places[[col]][sums$source_row]
  # Carry the row order through later county aggregation for the panel-scaling
  # fields, which use the first observed POI, as in the original Python builder.
  sums <- cells[sums, on = 'cell'][, cell := NULL]
  data.table::setnames(sums, c('value', 'n_active'), c(value_name, paste0('n_places_with_', value_name)))
  if (!length(first_cols)) sums[, source_row := NULL]
  sums[]
}
