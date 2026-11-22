# Small Parquet fixtures exercise the current delivery format end to end.
# Run from the repository root: Rscript --vanilla tests/test_dewey_monthly_build.R
suppressPackageStartupMessages({library(arrow); library(data.table); library(dplyr)})
project <- normalizePath('.')
fixture <- tempfile('dewey_build_test_')
dir.create(fixture)
Sys.setenv(SMOKE_AVOIDANCE_PROJECT_DIR = project,
           SMOKE_AVOIDANCE_DATA_DIR = file.path(fixture, 'outputs'),
           PLACE_DIR = file.path(fixture, 'places'),
           DEWEY_SPEND_FILES_DIR = file.path(fixture, 'spend'),
           DEWEY_MONTHLY_PATTERNS_FILES_DIR = file.path(fixture, 'visits'),
           DEWEY_FIRST_MONTH = '2019-01-01', DEWEY_LAST_MONTH = '2019-02-01')
dir.create(Sys.getenv('DEWEY_SPEND_FILES_DIR'))
dir.create(Sys.getenv('DEWEY_MONTHLY_PATTERNS_FILES_DIR'))
places <- data.frame(placekey = c('a', 'b', 'c', 'no_spend'),
                     statefp = c('08', '06', '08', '08'), countyfp = '001',
                     naics_code = c('722511', '713940', '441110', '722511'),
                     n_months_with_spend = c(2, 2, 2, 0))
write_dataset(places, Sys.getenv('PLACE_DIR'), partitioning = 'naics_code')
spend <- data.frame(placekey = c('a', 'c', 'b', 'unknown', 'no_spend'),
                    raw_total_spend = c(30, 15, 5, 1000, 1000),
                    spend_by_day = c('[10,20]', '[7,8]', '[5]', '[1000]', '[1000]'),
                    spend_date_range_start = as.POSIXct(c('2019-01-31', '2019-01-31', '2019-01-01', '2019-01-01', '2019-01-01'), tz = 'UTC'),
                    naics_code = c('722511','441110','713940','722511','722511'))
names(spend) <- toupper(names(spend))
write_parquet(spend, file.path(Sys.getenv('DEWEY_SPEND_FILES_DIR'), '2019-01--data_0.snappy.parquet'))
spend <- data.frame(placekey = 'a', raw_total_spend = 30, spend_by_day = '[30]',
                    spend_date_range_start = as.POSIXct('2019-02-01', tz = 'UTC'), naics_code = '722511')
write_parquet(spend, file.path(Sys.getenv('DEWEY_SPEND_FILES_DIR'), '2019-02--data_0.parquet'))
visits <- data.frame(placekey = c('a', 'c'), raw_visit_counts = c(3,4),
                     normalized_visits_by_state_scaling = c(30,40),
                     visits_by_day = c('"[1,2]"','[4,0]'), poi_cbg = '080010000001',
                     naics_code = c('722511','441110'), date_range_start = as.POSIXct('2019-01-31', tz = 'UTC'))
write_parquet(visits, file.path(Sys.getenv('DEWEY_MONTHLY_PATTERNS_FILES_DIR'), '2019-01--data_0.parquet'))
visits <- data.frame(placekey = 'b', raw_visit_counts = 5, normalized_visits_by_state_scaling = 50,
                     visits_by_day = '[5]', poi_cbg = '060010000001', naics_code = '713940',
                     date_range_start = as.POSIXct('2019-01-01', tz = 'UTC'))
names(visits) <- toupper(names(visits))
write_parquet(visits, file.path(Sys.getenv('DEWEY_MONTHLY_PATTERNS_FILES_DIR'), '2019-01--data_1.parquet'))
visits <- data.frame(placekey = c('a','b','excluded'), raw_visit_counts = c(3,4,0),
                     normalized_visits_by_state_scaling = c(90,40,0),
                     visits_by_day = c('[3]', '"[4,null]"', '[invalid]'),
                     poi_cbg = c('080010000001','060010000001','080010000001'), naics_code = c('722511','713940','722511'),
                     date_range_start = as.POSIXct('2019-02-01', tz = 'UTC'))
write_parquet(visits, file.path(Sys.getenv('DEWEY_MONTHLY_PATTERNS_FILES_DIR'), '2019-02--data_0.parquet'))

source('1_build/daily_consumer_data/add_daily_total_spend_and_visits_by_county.R')
old_chunk_env <- Sys.getenv('SMOKE_ARRAY_CHUNK_ROWS', unset = NA_character_)
Sys.setenv(SMOKE_ARRAY_CHUNK_ROWS = '25000')
stopifnot(daily_array_chunk_rows() == 25000L)
Sys.setenv(SMOKE_ARRAY_CHUNK_ROWS = '1.5')
stopifnot(inherits(try(daily_array_chunk_rows(), silent = TRUE), 'try-error'))
if (is.na(old_chunk_env)) Sys.unsetenv('SMOKE_ARRAY_CHUNK_ROWS') else Sys.setenv(SMOKE_ARRAY_CHUNK_ROWS = old_chunk_env)
chunk_fixture <- data.table(county_geoid = c('08001','08001','06001'), half = 1L,
  is_matched = TRUE, start_date = as.Date(c('2019-01-31','2019-02-01','2019-01-31')),
  visits_by_day = c('[1,2]', '[3,null]', '[4,5]'))
small <- sum_daily_arrays(chunk_fixture, 'visits_by_day', 'visits', chunk_rows = 1L)
large <- sum_daily_arrays(chunk_fixture, 'visits_by_day', 'visits', chunk_rows = 500000L)
setorderv(small, c('county_geoid','date')); setorderv(large, c('county_geoid','date'))
stopifnot(isTRUE(all.equal(as.data.frame(small), as.data.frame(large))))
CHUNK_ROWS <- 1L  # also exercise aggregation across small chunks
inputs <- list.files(fixture, recursive = TRUE, full.names = TRUE)
input_hashes <- tools::md5sum(inputs)
build_county_daily_totals()
spend <- as.data.table(open_dataset(COUNTY_DAILY_SPEND_DIR) %>% collect())
visits <- as.data.table(open_dataset(COUNTY_DAILY_VISITS_DIR) %>% collect())
stopifnot(!anyDuplicated(spend[, .(county_geoid, spend_date_range_start)]),
           !anyDuplicated(visits[, .(county_geoid, date_range_start)]))
stopifnot(spend[county_geoid == '08001' & spend_date_range_start == as.Date('2019-01-31'), total] == 17,
           spend[county_geoid == '08001' & spend_date_range_start == as.Date('2019-02-01'), total] == 58,
           spend[county_geoid == '08001' & spend_date_range_start == as.Date('2019-02-01'), deferrable] == 50,
           sum(spend$naics_722511) == 60, sum(spend$naics_713940) == 5)
stopifnot(visits[county_geoid == '08001' & date_range_start == as.Date('2019-01-31'), daily_visits] == 5,
           visits[county_geoid == '08001' & date_range_start == as.Date('2019-02-01'), daily_visits_deferrable] == 5,
           visits[county_geoid == '08001' & date_range_start == as.Date('2019-02-01'), normalized_visits_by_state_scaling] == 30,
           visits[county_geoid == '06001' & date_range_start == as.Date('2019-02-02'), daily_visits] == 0)
stopifnot(inherits(spend$spend_date_range_start, 'Date'), inherits(visits$date_range_start, 'Date'))
output_files <- list.files(COUNTY_DAILY_SPEND_DIR, full.names = TRUE)
before <- tools::md5sum(output_files)
stopifnot(inherits(try(build_county_daily_totals(), silent = TRUE), 'try-error'))
stopifnot(identical(before, tools::md5sum(output_files)))

# Exercise the weekly builder's actual spending loader and week assignment.
suppressPackageStartupMessages({library(lubridate); library(stringr)})
weekly_expr <- parse('1_build/reg_data/build_reg_data_weekly.R')
for (expr in weekly_expr) {
  if (is.call(expr) && identical(expr[[1]], as.name('<-')) &&
      as.character(expr[[2]]) %in% c('add_tue_mon_week', 'new_daily_spend')) eval(expr)
}
stopifnot(sum(new_daily_spend$daily_spend) == sum(spend$total),
           sum(new_daily_spend$daily_spend_deferrable) == sum(spend$deferrable),
           inherits(new_daily_spend$date, 'Date'), !anyNA(new_daily_spend$week_and_year))

# The existing matched-POI builder uses the same helpers and delivery fixtures.
source('1_build/daily_consumer_data/add_daily_matched_poi_spend_and_visits_by_county.R')
matched <- as.data.table(read_parquet(COUNTY_DAILY_MATCHED_POI_SPEND_VISITS_PATH))
stopifnot(sum(matched$spend) == sum(spend$deferrable),
           sum(matched$visits) == sum(visits$daily_visits_deferrable),
           all(matched$half %in% 1:2), !anyNA(matched))

# Reject malformed arrays and preserve empty-array handling without fake days.
bad <- data.table(county_geoid = '08001', half = 1L, is_matched = TRUE,
                   start_date = as.Date('2019-01-01'), visits_by_day = '[1,bad]')
stopifnot(inherits(try(sum_daily_arrays(bad, 'visits_by_day', 'visits'), silent = TRUE), 'try-error'))
bad[, visits_by_day := '[]']
stopifnot(nrow(sum_daily_arrays(bad, 'visits_by_day', 'visits')) == 0)
stopifnot(identical(input_hashes, tools::md5sum(inputs)))

# Missing delivery months stop before creating any output directories.
Sys.setenv(SMOKE_AVOIDANCE_DATA_DIR = file.path(fixture, 'missing_month_outputs'))
source(file.path(project, 'project_helpers_and_constants.R'))
LAST_MONTH <- as.Date('2019-03-01')
stopifnot(inherits(try(build_county_daily_totals(), silent = TRUE), 'try-error'), !dir.exists(DATA_DIR))
unlink(fixture, recursive = TRUE)
cat('PASS: current-delivery totals and matched-POI build; case variants, multiple files, quoted/null arrays, month spillovers, NAICS, county assignment, scaling fields and overwrite protection.\n')
