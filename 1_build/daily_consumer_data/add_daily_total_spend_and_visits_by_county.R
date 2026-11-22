# Rebuild the main county totals from the supplied monthly deliveries.
# Uses the newer matched-POI reader's casing, filename, date and array handling.
# Replaces the two legacy Python total builders, retaining their output columns.
suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(arrow)
})

script_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
project_root <- Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = if (length(script_arg)) {
  normalizePath(file.path(dirname(sub('^--file=', '', script_arg[1])), '../..'))
} else getwd())
source(file.path(project_root, 'project_helpers_and_constants.R'))
source(file.path(CODE_DIR, '1_build/daily_consumer_data/dewey_monthly_helpers.R'))

FIRST_MONTH <- as.Date(Sys.getenv('DEWEY_FIRST_MONTH', unset = '2019-01-01'))
LAST_MONTH <- as.Date(Sys.getenv('DEWEY_LAST_MONTH', unset = '2023-12-01'))
CHUNK_ROWS <- daily_array_chunk_rows()

read_total_places <- function() {
  places <- open_lowercase(PLACE_DIR) %>%
    dplyr::filter(n_months_with_spend > 0) %>%
    dplyr::select(placekey, statefp, countyfp, naics_code) %>%
    collect() %>% as.data.table()
  places[, `:=`(county_geoid = paste0(sprintf('%02d', as.integer(statefp)), sprintf('%03d', as.integer(countyfp))),
                 naics_code = as.character(naics_code))]
  places <- unique(places[, .(placekey, county_geoid, naics_code)])
  if (anyDuplicated(places$placekey)) stop('Places dimension has conflicting county/NAICS assignments.')
  places
}

read_total_spend_month <- function(paths, places) {
  rows <- rbindlist(lapply(paths, function(path) {
    require_columns(path, c('placekey', 'raw_total_spend', 'spend_by_day', 'spend_date_range_start'))
    open_lowercase(path) %>% dplyr::filter(raw_total_spend > 0) %>%
      dplyr::select(placekey, spend_by_day, spend_date_range_start) %>% collect()
  }))
  rows <- merge(rows, places, by = 'placekey', all = FALSE, sort = FALSE)
  rows[, `:=`(start_date = to_start_date(spend_date_range_start),
               naics6 = substr(naics_code, 1, 6), deferrable = substr(naics_code, 1, 2) %in% c('71', '72'))]
  sum_daily_arrays(rows, 'spend_by_day', 'spend', group_cols = c('county_geoid', 'naics6', 'deferrable'))
}

read_total_visits_month <- function(paths) {
  rows <- rbindlist(lapply(paths, function(path) {
    require_columns(path, c('placekey', 'visits_by_day', 'raw_visit_counts',
                            'normalized_visits_by_state_scaling', 'poi_cbg', 'naics_code', 'date_range_start'))
    open_lowercase(path) %>% dplyr::filter(raw_visit_counts > 0) %>%
      dplyr::select(placekey, visits_by_day, raw_visit_counts, normalized_visits_by_state_scaling,
                    poi_cbg, naics_code, date_range_start) %>% collect()
  }))
  rows[, `:=`(county_geoid = cbg_to_county(poi_cbg), start_date = to_start_date(date_range_start),
               naics6 = substr(as.character(naics_code), 1, 6),
               deferrable = substr(as.character(naics_code), 1, 2) %in% c('71', '72'))]
  rows <- rows[!is.na(county_geoid)]
  sum_daily_arrays(rows, 'visits_by_day', 'visits', group_cols = c('county_geoid', 'naics6', 'deferrable'),
                   first_cols = c('raw_visit_counts', 'normalized_visits_by_state_scaling'))
}

county_totals <- function(sums, value_name) {
  sums <- copy(sums)
  if (!nrow(sums)) stop('No usable daily ', value_name, ' rows in a delivery month.')
  if ('source_row' %in% names(sums)) setorder(sums, source_row)
  totals <- sums[, .(total = sum(get(value_name)),
                     deferrable = sum(get(value_name)[deferrable])), by = .(county_geoid, date)]
  sectors <- sums[deferrable & !is.na(naics6), .(value = sum(get(value_name))), by = .(county_geoid, date, naics6)]
  if (nrow(sectors)) {
    sectors[, naics6 := paste0('naics_', naics6)]
    wide <- dcast(sectors, county_geoid + date ~ naics6, value.var = 'value', fill = 0)
    totals <- merge(totals, wide, by = c('county_geoid', 'date'), all.x = TRUE, sort = FALSE)
    setnafill(totals, fill = 0, cols = grep('^naics_', names(totals), value = TRUE))
  }
  if (value_name == 'visits') {
    metadata <- sums[, .(raw_visit_counts = raw_visit_counts[1],
                         normalized_visits_by_state_scaling = normalized_visits_by_state_scaling[1],
                         first_source_row = source_row[1]), by = .(county_geoid, date)]
    totals <- merge(totals, metadata, by = c('county_geoid', 'date'), sort = FALSE)
  }
  totals[, date := as.Date(date, origin = '1970-01-01')]
  totals
}

write_county_months <- function(parts_dir, destination, value_name, months) {
  # Read the staged summaries by actual day rather than delivery month. This
  # combines visits whose arrays spill into another month without double counting.
  ds <- open_dataset(parts_dir, unify_schemas = TRUE)
  sum_cols <- c('total', 'deferrable', grep('^naics_', names(ds), value = TRUE))
  for (i in seq_along(months)) {
    first <- months[i]
    last <- seq(first, by = 'month', length.out = 2)[2] - 1
    rows <- ds %>% dplyr::filter(date >= first, date <= last) %>% collect() %>% as.data.table()
    if (!nrow(rows)) stop('No county ', value_name, ' observations for ', first)
    setnafill(rows, fill = 0, cols = sum_cols)
    if (value_name == 'visits') setorder(rows, delivery_month, first_source_row)
    combined <- rows[, lapply(.SD, sum), by = .(county_geoid, date), .SDcols = sum_cols]
    if (value_name == 'visits') {
      metadata <- rows[, .(raw_visit_counts = raw_visit_counts[1],
                           normalized_visits_by_state_scaling = normalized_visits_by_state_scaling[1]),
                       by = .(county_geoid, date)]
      combined <- merge(combined, metadata, by = c('county_geoid', 'date'))
      setnames(combined, c('date', 'total', 'deferrable'),
               c('date_range_start', 'daily_visits', 'daily_visits_deferrable'))
    } else setnames(combined, 'date', 'spend_date_range_start')
    setindex(combined, NULL)
    write_parquet(combined, file.path(destination, paste0(format(first, '%Y-%m'), '.parquet')))
  }
}

build_county_daily_totals <- function() {
  months <- seq(FIRST_MONTH, LAST_MONTH, by = 'month')
  spend_files <- list_monthly_files(DEWEY_SPEND_FILES_DIR)
  visits_files <- list_monthly_files(DEWEY_MONTHLY_PATTERNS_FILES_DIR)
  for (files in list(spend_files, visits_files)) {
    missing <- setdiff(as.character(months), as.character(files$month))
    if (length(missing)) stop('Missing delivery months: ', paste(missing, collapse = ', '))
  }
  destinations <- c(sub('/$', '', COUNTY_DAILY_SPEND_DIR), sub('/$', '', COUNTY_DAILY_VISITS_DIR))
  if (any(file.exists(destinations))) {
    stop('County totals already exist; use a fresh SMOKE_AVOIDANCE_DATA_DIR to rebuild and compare vintages.')
  }
  for (d in unique(dirname(destinations))) dir.create(d, recursive=TRUE, showWarnings=FALSE)
  parent <- file.path(DATA_DIR, 'intermediate/consumer')
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile('.monthly_build_', tmpdir = parent)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  for (subdir in c('spend_parts', 'visits_parts', 'daily_spend', 'daily_visits')) dir.create(file.path(stage, subdir))
  places <- read_total_places()
  for (i in seq_along(months)) {
    month <- months[i]
    message('Building county totals: ', format(month, '%Y-%m'))
    for (value_name in c('spend', 'visits')) {
      sums <- if (value_name == 'spend') {
        read_total_spend_month(spend_files[month == months[i], path], places)
      } else read_total_visits_month(visits_files[month == months[i], path])
      totals <- county_totals(sums, value_name)
      totals[, delivery_month := month]
      setindex(totals, NULL)
      write_parquet(totals, file.path(stage, paste0(value_name, '_parts'), paste0(format(month, '%Y-%m'), '.parquet')))
      rm(sums, totals)
      gc(verbose = FALSE)
    }
  }
  write_county_months(file.path(stage, 'spend_parts'), file.path(stage, 'daily_spend'), 'spend', months)
  write_county_months(file.path(stage, 'visits_parts'), file.path(stage, 'daily_visits'), 'visits', months)
  for (i in seq_along(destinations)) {
    if (file.exists(destinations[i]) || !file.rename(file.path(stage, basename(destinations[i])), destinations[i])) {
      stop('Cannot publish county totals: ', destinations[i])
    }
  }
  message('Wrote current-delivery county totals to ', parent)
}

if (sys.nframe() == 0L) build_county_daily_totals()
