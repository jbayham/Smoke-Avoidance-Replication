################################################################################
# Average daily county spend, weighted by the share of POIs that have both spend
# and visits ("matched" POIs), from the county x day x half x is_matched panel
# written by 1_build/daily_consumer_data/add_daily_matched_poi_spend_and_visits_by_county.R
# (COUNTY_DAILY_MATCHED_POI_SPEND_VISITS_PATH).
#
# The idea. The spend-on-visits slope (matched_poi_and_split_sample_spend_on_visits.R)
# is cleanest where the spend and visits panels cover the same places. This script
# summarises each county's average daily spend and how well its two panels overlap,
# then forms an average daily spend that gives more weight to well-matched counties.
#
# "Percent of POIs that have both spend and visits." A POI has both iff it is
# matched (positive spend in the spend panel and positive visits in the patterns
# panel that month; see the build header). The aggregated daily file does not carry
# distinct placekeys -- it carries n_places_with_spend and n_places_with_visits, the
# counts of POIs *active* that day in each panel, and a matched POI is counted in
# both columns -- so the exact count of distinct matched POIs is not recoverable
# here. What is recoverable, per county, is the matched share of active POI-days:
#   match_share_spend  = matched spend-POI-days  / all spend-POI-days
#   match_share_visits = matched visit-POI-days  / all visit-POI-days
# match_share_spend is the fraction of spending POIs that also have visits;
# match_share_visits is the fraction of visited POIs that also have spend. Their
# geometric mean, poi_match_rate, is the count-based "percent of POIs with both".
# This is the count analog of the dollar/visit-weighted "agreement" the analysis
# script uses (agreement = sqrt(matched-spend-share x matched-visit-share by value));
# both are reported so they can be compared. (An exact distinct-POI match rate would
# require the build to emit per-county distinct matched / spend-only / visits-only
# placekey counts; that is the "alternative" note in the script that produced this file.)
#
# Spend is put in 2017 dollars with the annual CPI deflator from WEEKLY_DATA_FILEPATH
# (constant within a year), as in the matched-POI script. Sample matches that script:
# CONUS only, and the six no-visits days are dropped so the visit match share is not
# understated on days production has no foot traffic.
#
# Outputs:
#   - a per-county CSV (COUNTY_AVG_DAILY_SPEND_POI_MATCH_PATH) with average daily
#     spend, the match shares, poi_match_rate, and observed days;
#   - to the console, the headline POI-match-weighted average daily county spend,
#     next to the unweighted and spend-weighted means for context.
################################################################################
library(conflicted)
library(tidyverse)
library(data.table)
library(arrow)
library(glue)
conflicts_prefer(dplyr::filter, data.table::year)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# days with no visits in the foot-traffic data (same as the matched-POI script)
MISSING_VISIT_DAYS <- c(as.Date(paste0(2019:2023, '-11-30')), as.Date('2023-01-01'))

# ---- annual CPI deflator to 2017 dollars, one value per year, from the weekly reg data
deflator_by_year <- open_csv_dataset(WEEKLY_DATA_FILEPATH) %>%
  select(week_and_year, deflator) %>%
  collect() %>%
  as.data.table()
deflator_by_year[, year := as.integer(str_sub(str_pad(as.character(week_and_year), 6, pad = '0'), 3))]
deflator_by_year <- unique(deflator_by_year[, .(deflator = deflator[1]), by = year])
setkey(deflator_by_year, year)

# ---- county x day x half x is_matched panel
non_CONUS_FIPS <- get_non_US_or_CONUS_state_fips_codes(filter_for_CONUS = TRUE)
NON_US_FIPS    <- c('00', 'NA')
daily <- read_parquet(COUNTY_DAILY_MATCHED_POI_SPEND_VISITS_PATH) %>% as.data.table()
daily <- daily[!date %in% MISSING_VISIT_DAYS]
daily <- daily[!str_sub(county_geoid, end = 2) %in% c(non_CONUS_FIPS, NON_US_FIPS)]
daily[, year := year(date)]
daily <- daily[deflator_by_year, on = 'year', nomatch = 0]      # 2019-2023 only
daily[, spend_2017 := spend * deflator]

# ---- per-county summary
# daily county totals over all places (both halves, matched and not), then averaged
county_day_spend <- daily[, .(spend_2017 = sum(spend_2017)), by = .(county_geoid, date)]
avg_spend <- county_day_spend[, .(avg_daily_spend_2017 = mean(spend_2017), n_days = .N), by = county_geoid]

# matched share of active POI-days, by panel, then their geometric mean
match <- daily[, .(
  sp_all      = sum(n_places_with_spend),
  sp_matched  = sum(n_places_with_spend[is_matched]),
  vi_all      = sum(n_places_with_visits),
  vi_matched  = sum(n_places_with_visits[is_matched])
), by = county_geoid]
match[, `:=`(match_share_spend  = sp_matched / sp_all,
             match_share_visits = vi_matched / vi_all)]
match[, poi_match_rate := sqrt(match_share_spend * match_share_visits)]

# dollar/visit-weighted agreement, the analysis script's definition, for comparison
agreement <- daily[, .(
  spend_matched  = sum(spend[is_matched]),  spend  = sum(spend),
  visits_matched = sum(visits[is_matched]), visits = sum(visits)
), by = county_geoid]
agreement[, agreement_value := sqrt((spend_matched / spend) * (visits_matched / visits))]

county <- avg_spend[match, on = 'county_geoid'][agreement[, .(county_geoid, agreement_value)], on = 'county_geoid']

# a county needs some spend and some visits POIs for the match rate to be defined
county <- county[sp_all > 0 & vi_all > 0 & is.finite(poi_match_rate)]
setorder(county, -avg_daily_spend_2017)


# ---- headline: POI-match-weighted average daily county spend, vs other weightings
wmean <- function(x, w) sum(x * w) / sum(w)
summary_tbl <- tibble(
  weighting = c('POI-match-weighted', 'matched-visit-share-weighted',
                'unweighted (simple mean)', 'value-agreement-weighted', 'spend-weighted'),
  avg_daily_county_spend_2017 = c(
    wmean(county$avg_daily_spend_2017, county$poi_match_rate),
    wmean(county$avg_daily_spend_2017, county$match_share_visits),
    mean(county$avg_daily_spend_2017),
    wmean(county$avg_daily_spend_2017, county$agreement_value),
    wmean(county$avg_daily_spend_2017, county$avg_daily_spend_2017)
  )
)

write_csv(summary_tbl, AVERAGE_DAILY_SPEND_PATH)
message(glue('Wrote weighted_spend to {AVERAGE_DAILY_SPEND_PATH}'))
message(glue('\nAverage daily county spend (2017 $), {nrow(county)} CONUS counties, 2019-2023:'))
print(summary_tbl %>% mutate(avg_daily_county_spend_2017 = round(avg_daily_county_spend_2017)), n = Inf)



