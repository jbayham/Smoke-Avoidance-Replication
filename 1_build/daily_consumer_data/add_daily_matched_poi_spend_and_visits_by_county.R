################################################################################
# County x day spend and visits at deferrable places (NAICS 71 and 72), split two
# ways to check whether the spend-on-visits slope is attenuated because the two
# panels cover different places:
#
#   is_matched  TRUE if the placekey is in both panels that month: positive spend
#               in the spend panel and positive visits in the patterns panel.
#               Summing only matched rows gives spend and visits over the same places.
#   half        fixed random split of placekeys (a hash of the placekey), so a place
#               is in the same half every month and in both panels. Visits from
#               one half instrument visits from the other in the split-sample IV.
#
# Filters, deferrable definition, and county assignment (places file for spend,
# poi_cbg for visits) follow add_daily_total_spend_and_visits_by_county.R; both read the RSTOR re-delivery
# of both panels (flat YYYY-MM--data_*.parquet files, several per month). Summed
# over half and is_matched, the output is compared with the production county
# totals at the end. A matched place whose two county codes disagree lands in
# different counties on the two sides; the log reports how often.
#
# The RSTOR foot-traffic delivery stores visits_by_day wrapped in literal quotes
# ("[16,34,...,46]"), so the quotes are stripped along with the brackets; left in,
# they made the first and last day of every month unparseable. Each parsed array
# sums to raw_visit_counts. Mid-month visits match production (ratio 0.997).
#
# Visits are the raw visits_by_day counts, as in production. The state scaling
# factor is constant within a state-month, so state_year_month FEs absorb it.
#
# Output: one row per county x date x half x is_matched, with spend, visits, and
# the number of places with positive spend / visits that day. A visits file's
# arrays can run past its month, so rows are summed across months at the end.
#
# Downstream: 1_build/reg_data/build_reg_data_weekly.R aggregates this file to
# county x week -- for all deferrable places and for the matched subset, each split
# into the two halves -- and merges those as the mpoi_* columns of the weekly reg
# data (WEEKLY_DATA_FILEPATH). The split-sample IV / matched-places checks in
# 2_analysis/matched_poi_and_split_sample_spend_on_visits.R and the coverage maps in
# 2_analysis/appendix/spend_visits_coverage_summary.R read those columns, not this
# file directly.
#
# Server only: reads the dewey spend, patterns, and places data.
################################################################################
library(conflicted)
library(dplyr)
library(arrow)
library(data.table)
library(glue)
conflicts_prefer(dplyr::filter, data.table::between)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(file.path(CODE_DIR, '1_build/daily_consumer_data/dewey_monthly_helpers.R'))

FIRST_MONTH <- as.Date(Sys.getenv('DEWEY_FIRST_MONTH', unset = '2019-01-01'))
LAST_MONTH  <- as.Date(Sys.getenv('DEWEY_LAST_MONTH', unset = '2023-12-01'))

# same deferrable definition as the production county builds
DEFERRABLE_NAICS2 <- c('71', '72')

# rows of place-months exploded to place-days at a time; bounds peak memory
CHUNK_ROWS <- 500000L

# digest2int is a deterministic string hash, so the split does not depend on
# which places are in a given file or on the order they are read
poi_half <- function(placekey) digest::digest2int(placekey) %% 2L + 1L

# ---- places: county and NAICS for spend placekeys (as in the production spend build)
deferrable_places <- open_dataset(PLACE_DIR) %>%
  filter(n_months_with_spend > 0,
         substr(as.character(naics_code), 1, 2) %in% DEFERRABLE_NAICS2) %>%
  select(placekey, statefp, countyfp) %>%
  collect() %>%
  as.data.table()
deferrable_places[, county_geoid := paste0(formatC(as.integer(statefp), width = 2, flag = '0'),
                                           formatC(as.integer(countyfp), width = 3, flag = '0'))]
deferrable_places <- unique(deferrable_places[, .(placekey, spend_county_geoid = county_geoid)], by = 'placekey')
message(glue('{nrow(deferrable_places)} deferrable places with spend in the places file'))

# ---- monthly files
spend_files <- list_monthly_files(DEWEY_SPEND_FILES_DIR)
visit_files <- list_monthly_files(DEWEY_MONTHLY_PATTERNS_FILES_DIR)
stopifnot(nrow(spend_files) > 0, nrow(visit_files) > 0)
require_columns(spend_files$path[1], c('placekey', 'naics_code', 'raw_total_spend', 'spend_by_day', 'spend_date_range_start'))
require_columns(visit_files$path[1], c('placekey', 'naics_code', 'raw_visit_counts', 'visits_by_day', 'poi_cbg', 'date_range_start'))

months <- sort(unique(spend_files$month[spend_files$month %in% visit_files$month]))
stopifnot(length(months) > 0)
missing_months <- setdiff(seq(FIRST_MONTH, LAST_MONTH, by = 'month'), months)
if (length(missing_months) > 0) warning(glue('No spend or no visits files for: {toString(as.Date(missing_months))}'))

read_spend_month <- function(month) {
  paths <- spend_files$path[spend_files$month == month]
  spend <- rbindlist(lapply(paths, \(path) {
    open_lowercase(path) %>%
      # deferrable in the places file, as in production, or by this file's own
      # NAICS, so the log can say how much spend the places file misses
      filter(raw_total_spend > 0,
             placekey %in% deferrable_places$placekey | substr(as.character(naics_code), 1, 2) %in% DEFERRABLE_NAICS2) %>%
      select(placekey, raw_total_spend, spend_by_day, spend_date_range_start) %>%
      collect() %>%
      mutate(spend_date_range_start = to_start_date(spend_date_range_start))
  }))
  spend <- deferrable_places[spend, on = 'placekey']
  not_in_places <- is.na(spend$spend_county_geoid)
  if (any(not_in_places)) {
    message(glue('    dropped {sum(not_in_places)} deferrable-NAICS spend places with no deferrable entry in the places file ',
                 '({round(100 * sum(spend$raw_total_spend[not_in_places]) / sum(spend$raw_total_spend), 1)}% of spend)'))
  }
  spend <- spend[!not_in_places]
  n_off_month <- sum(spend$spend_date_range_start != month)
  if (n_off_month > 0) message(glue('    {n_off_month} spend rows start on a day other than {month}'))
  setnames(spend, c('spend_county_geoid', 'spend_date_range_start'), c('county_geoid', 'start_date'))
  spend[, raw_total_spend := NULL][]
}

read_visits_month <- function(month) {
  paths <- visit_files$path[visit_files$month == month]
  visits <- rbindlist(lapply(paths, \(path) {
    open_lowercase(path) %>%
      filter(raw_visit_counts > 0, substr(as.character(naics_code), 1, 2) %in% DEFERRABLE_NAICS2) %>%
      select(placekey, visits_by_day, poi_cbg, date_range_start) %>%
      collect() %>%
      mutate(date_range_start = to_start_date(date_range_start))
  }))
  visits[, county_geoid := cbg_to_county(poi_cbg)]
  setnames(visits, 'date_range_start', 'start_date')
  visits[, poi_cbg := NULL]
  n_bad_cbg <- sum(is.na(visits$county_geoid))
  if (n_bad_cbg > 0) message(glue('    dropped {n_bad_cbg} visit rows with a missing poi_cbg'))
  n_off_month <- sum(format(visits$start_date, '%Y-%m') != format(month, '%Y-%m'))
  if (n_off_month > 0) message(glue('    {n_off_month} visit rows start outside {format(month, "%Y-%m")}'))
  visits[!is.na(county_geoid)]
}

build_month <- function(month) {
  message(glue('[{format(month, "%Y-%m")}]'))
  spend  <- read_spend_month(month)
  visits <- read_visits_month(month)

  spend[,  `:=`(is_matched = placekey %chin% visits$placekey, half = poi_half(placekey))]
  visits[, `:=`(is_matched = placekey %chin% spend$placekey,  half = poi_half(placekey))]

  matched_counties <- merge(unique(spend[(is_matched), .(placekey, spend_county = county_geoid)], by = 'placekey'),
                            unique(visits[(is_matched), .(placekey, visits_county = county_geoid)], by = 'placekey'),
                            by = 'placekey')
  diagnostics <- data.table(
    month                  = month,
    n_spend_places         = uniqueN(spend$placekey),
    n_visits_places        = uniqueN(visits$placekey),
    n_matched_places       = nrow(matched_counties),
    n_matched_county_differs = matched_counties[spend_county != visits_county, .N]
  )

  spend_sums  <- sum_daily_arrays(spend,  'spend_by_day',  'spend')
  visits_sums <- sum_daily_arrays(visits, 'visits_by_day', 'visits')
  month_sums  <- merge(spend_sums, visits_sums, by = c('county_geoid', 'half', 'is_matched', 'date'), all = TRUE)
  setnafill(month_sums, fill = 0, cols = c('spend', 'n_places_with_spend', 'visits', 'n_places_with_visits'))

  diagnostics[, `:=`(
    spend = sum(month_sums$spend),   spend_matched  = month_sums[(is_matched), sum(spend)],
    visits = sum(month_sums$visits), visits_matched = month_sums[(is_matched), sum(visits)]
  )]
  message(glue('    {diagnostics$n_matched_places} of {diagnostics$n_spend_places} spend and ',
               '{diagnostics$n_visits_places} visits places matched; they carry ',
               '{round(100 * diagnostics$spend_matched / diagnostics$spend, 1)}% of spend and ',
               '{round(100 * diagnostics$visits_matched / diagnostics$visits, 1)}% of visits'))
  list(sums = month_sums, diagnostics = diagnostics)
}

month_results <- lapply(seq_along(months), \(i) build_month(months[i]))  # lapply drops the Date class

county_daily <- rbindlist(lapply(month_results, `[[`, 'sums'))
county_daily <- county_daily[, lapply(.SD, sum), by = .(county_geoid, date, half, is_matched)]
county_daily[, date := as.Date(date, origin = '1970-01-01')]
last_day <- seq(LAST_MONTH, by = 'month', length.out = 2)[2] - 1
county_daily <- county_daily[between(date, FIRST_MONTH, last_day)]
setcolorder(county_daily, c('county_geoid', 'date', 'half', 'is_matched'))
setorder(county_daily, county_geoid, date, half, is_matched)
stopifnot(!anyNA(county_daily), all(county_daily$half %in% 1:2))

diagnostics <- rbindlist(lapply(month_results, `[[`, 'diagnostics'))
message('Matched share of spend and visits by year:')
print(diagnostics[, .(pct_spend_matched          = 100 * sum(spend_matched) / sum(spend),
                      pct_visits_matched         = 100 * sum(visits_matched) / sum(visits),
                      pct_matched_county_differs = 100 * sum(n_matched_county_differs) / sum(n_matched_places)),
                  by = .(year = format(month, '%Y'))])
message(glue('Half 1 holds {round(100 * county_daily[half == 1, sum(visits)] / sum(county_daily$visits), 1)}% of visits ',
             'and {round(100 * county_daily[half == 1, sum(spend)] / sum(county_daily$spend), 1)}% of spend'))

# ---- reconcile the totals with the production county builds, where they exist
county_totals <- county_daily[, .(spend = sum(spend), visits = sum(visits)), by = .(county_geoid, date)]
if (dir.exists(COUNTY_DAILY_SPEND_DIR)) {
  production_spend <- open_dataset(COUNTY_DAILY_SPEND_DIR, unify_schemas = TRUE) %>%
    select(county_geoid, spend_date_range_start, deferrable) %>%
    collect() %>%
    as.data.table()
  production_spend[, `:=`(county_geoid = formatC(as.integer(county_geoid), width = 5, flag = '0'),
                          date = as.Date(spend_date_range_start))]
  spend_check <- merge(county_totals, production_spend, by = c('county_geoid', 'date'))
  message(glue('Spend vs production on {nrow(spend_check)} county-days: ',
               'total ratio {round(sum(spend_check$spend) / sum(spend_check$deferrable), 4)}, ',
               'max abs diff {round(max(abs(spend_check$spend - spend_check$deferrable)), 2)}'))
}
if (dir.exists(COUNTY_DAILY_VISITS_DIR)) {
  production_visits <- open_dataset(COUNTY_DAILY_VISITS_DIR, unify_schemas = TRUE) %>%
    select(county_geoid, date_range_start, daily_visits_deferrable) %>%
    collect() %>%
    as.data.table()
  production_visits[, `:=`(county_geoid = formatC(as.integer(county_geoid), width = 5, flag = '0'),
                           date = to_start_date(date_range_start))]
  visits_check <- merge(county_totals, production_visits, by = c('county_geoid', 'date'))
  message(glue('Visits vs production on {nrow(visits_check)} county-days: ',
               'total ratio {round(sum(visits_check$visits) / sum(visits_check$daily_visits_deferrable), 4)}, ',
               'max abs diff {round(max(abs(visits_check$visits - visits_check$daily_visits_deferrable)), 2)}'))
}

# arrow stores R attributes in the file; data.table's auto-index on `half` (one
# integer per row) made a 277 MB footer that readers reject
setindex(county_daily, NULL)
dir.create(dirname(COUNTY_DAILY_MATCHED_POI_SPEND_VISITS_PATH), recursive = TRUE, showWarnings = FALSE)
write_parquet(county_daily, COUNTY_DAILY_MATCHED_POI_SPEND_VISITS_PATH)
message(glue('Wrote {nrow(county_daily)} county-day-half-match rows to {COUNTY_DAILY_MATCHED_POI_SPEND_VISITS_PATH}'))
