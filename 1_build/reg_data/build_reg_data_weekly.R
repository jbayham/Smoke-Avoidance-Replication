################################################################################
# This code creates weekly dataset for the project!                             #
# We look at visits data because we have that on a daily basis.                #
################################################################################
library(conflicted)
library(tidyverse)
library(data.table)
library(fixest)
library(arrow)
library(glue)
library(scales)
library(slider)
conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::isoweek)
conflicts_prefer(lubridate::isoyear)
conflicts_prefer(lubridate::year)
conflicts_prefer(lubridate::month)
conflicts_prefer(lubridate::mday)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

MISSING_VISIT_DAYS <- c(as.Date(paste0(2019:2023, '-11-30')), as.Date('2023-01-01'))

# Weeks run Tuesday to Monday: spend is dated by posting date, so weekend spend
# lands on Monday and belongs with the week before it. Shifting the date back one
# day turns Tue-Mon into the ISO Mon-Sun week, which also gives the week-based year
# (a week spanning New Year's gets one year). week_and_year is the zero-padded week
# number followed by that year (e.g. 012019); week_start is the week's Tuesday.
add_tue_mon_week <- function(df, date_col) {
  df %>%
    mutate(
      week          = isoweek({{ date_col }} - 1),
      week_year     = isoyear({{ date_col }} - 1),
      week_and_year = sprintf('%02d%d', week, week_year),
      week_start    = floor_date(as_date({{ date_col }}) - 1, 'week', week_start = 1) + 1
    )
}

# Use the county totals rebuilt from the current monthly delivery, also used
# by the daily panel. This supersedes the externally processed older spend feed.
new_daily_spend <- open_dataset(COUNTY_DAILY_SPEND_DIR, format = 'parquet', unify_schemas = TRUE) %>%
  select(county_geoid, spend_date_range_start, daily_spend = total, daily_spend_deferrable = deferrable) %>%
  collect() %>%
  mutate(spend_date_range_start = as.Date(spend_date_range_start),
         date = spend_date_range_start,
         county_geoid = str_pad(as.character(county_geoid), width = 5, side = 'left', pad = '0')) %>%
  filter(date >= as.Date('2019-01-01'), date <= as.Date('2023-12-31')) %>%
  add_tue_mon_week(spend_date_range_start)

daily_smoke_and_visits <- read_csv_arrow(DAILY_DATA_FILEPATH) %>%
 select(county_geoid, 
        date, 
        adj_pm_smoke_med_1sd,
        starts_with("bin_d_"), 
        binned_pr, binned_tmmn, binned_tmmx, binned_rmax,
        pr, tmmn, tmmx, rmax,
        all_of(paste0(HETEROGENEITY_LABELS, '_q', recycle0 = TRUE)),
        daily_visits_deferrable,
        daily_visits
        ) %>% 
  mutate(
    county_geoid=str_pad(as.character(county_geoid), width=5, side='left', pad='0'),
    date = as_date(date)
  ) %>%
  add_tue_mon_week(date)




weekly_smoke_and_spend <- daily_smoke_and_visits %>% 
  mutate(
    n_days_weather_1_pr=binned_pr==1,
    n_days_weather_2_pr=binned_pr==2,
    n_days_weather_3_pr=binned_pr==3,
    n_days_weather_4_pr=binned_pr==4,
    n_days_weather_5_pr=binned_pr==5,
    
    n_days_weather_1_tmmn=binned_tmmn==1,
    n_days_weather_2_tmmn=binned_tmmn==2,
    n_days_weather_3_tmmn=binned_tmmn==3,
    n_days_weather_4_tmmn=binned_tmmn==4,
    n_days_weather_5_tmmn=binned_tmmn==5,
    
    n_days_weather_1_tmmx=binned_tmmx==1,
    n_days_weather_2_tmmx=binned_tmmx==2,
    n_days_weather_3_tmmx=binned_tmmx==3,
    n_days_weather_4_tmmx=binned_tmmx==4,
    n_days_weather_5_tmmx=binned_tmmx==5,
    
    n_days_weather_1_rmax=binned_rmax==1,
    n_days_weather_2_rmax=binned_rmax==2,
    n_days_weather_3_rmax=binned_rmax==3,
    n_days_weather_4_rmax=binned_rmax==4,
    n_days_weather_5_rmax=binned_rmax==5
  ) %>% 
  collect() %>%
  group_by(county_geoid, week_and_year) %>% 
  summarize(
    across(c(starts_with("n_days_weather"), starts_with("bin_d_"), starts_with("daily_visits")), ~sum(., na.rm=T)),
    across(c("adj_pm_smoke_med_1sd", all_of(c("pr", "tmmn", "tmmx", "rmax"))), ~mean(., na.rm=T)),
    across(all_of(c('week', 'week_year', 'week_start', paste0(HETEROGENEITY_LABELS, '_q', recycle0 = TRUE))), ~dplyr::first(., na_rm=T)),
  ) %>%
  mutate(
    # A week spanning two months is assigned to the month holding most of its days:
    # the month of its Friday (week_start + 3, day 4 of 7). Taking first() of the daily
    # state_year_month/season instead depends on row order, so ~20% of weeks got an
    # arbitrary month. Same formats as the daily panel (build_reg_data_daily.R).
    majority_day = week_start + 3,
    state_year_month = str_c(str_sub(county_geoid, end=2), year(majority_day), month(majority_day)),
    season = case_when(
      month(majority_day) %in% c(3, 4, 5)   ~ 1,  # spring
      month(majority_day) %in% c(6, 7, 8)   ~ 2,  # summer
      month(majority_day) %in% c(9, 10, 11) ~ 3,  # fall
      month(majority_day) %in% c(12, 1, 2)  ~ 4   # winter
    ),
    majority_day = NULL,
    asinh_daily_visits_deferrable = asinh(daily_visits_deferrable),
    asinh_daily_visits            = asinh(daily_visits),
    log_daily_visits_deferrable = log(1 + daily_visits_deferrable),
    log_daily_visits            = log(1 + daily_visits)
  )

non_CONUS_FIPS_codes <- get_non_US_or_CONUS_state_fips_codes(filter_for_CONUS=TRUE)
NON_US_FIPS <- c("00", "NA")
weekly_spend <- new_daily_spend %>% 
  filter(!str_sub(county_geoid, end=2) %in% c(non_CONUS_FIPS_codes, NON_US_FIPS),
         !date %in%  MISSING_VISIT_DAYS) %>% 
  group_by(county_geoid, week_and_year, week_year) %>% 
  summarize(
    weekly_spend = sum(daily_spend, na.rm=T),
    weekly_spend_deferrable = sum(daily_spend_deferrable, na.rm=T),
    log_weekly_spend_deferrable = log(weekly_spend_deferrable + 1),
    asinh_weekly_spend_deferrable = asinh(weekly_spend_deferrable)
    )


# ---- matched-POI spend and visits, split into the two fixed random halves, from
# add_daily_matched_poi_spend_and_visits_by_county.R. Aggregated to county x week for all
# deferrable places and, separately, for matched places (placekeys in both the spend and
# visits panels that month), then merged WIDE so weekly_df stays one row per county-week.
# The columns feed the split-sample IV / matched-places checks in
# 2_analysis/matched_poi_and_split_sample_spend_on_visits.R, 
# Nov 30 / 2023-01-01 (no visits in the foot-traffic data) are dropped, as above.
matched_daily <- read_parquet(COUNTY_DAILY_MATCHED_POI_SPEND_VISITS_PATH) %>% as.data.table()
matched_daily <- matched_daily[!date %in% MISSING_VISIT_DAYS]
matched_daily <- matched_daily[!str_sub(county_geoid, end = 2) %in% c(non_CONUS_FIPS_codes, NON_US_FIPS)]
# Tuesday-Monday weeks: date - 1 falls in the ISO (Monday-Sunday) week, so its ISO week and year
# key the week, and a week spanning New Year's gets one year (matches add_tue_mon_week above)
matched_daily[, week_and_year := sprintf('%02d%d', isoweek(date - 1), isoyear(date - 1))]

# one row per county-week with the two halves side by side, column names carrying the prefix
# (mpoi_spend_h1, mpoi_visits_h1, ...); fill = 0 so a county-week present in only one half is a 0.
to_wide <- function(cells, prefix) {
  wide <- dcast(cells[, .(spend = sum(spend), visits = sum(visits)), by = .(county_geoid, week_and_year, half)],
                county_geoid + week_and_year ~ half, value.var = c('spend', 'visits'), sep = '_h', fill = 0)
  val  <- setdiff(names(wide), c('county_geoid', 'week_and_year'))
  setnames(wide, val, paste0(prefix, val))[]
}

# all deferrable places, then the matched subset (a subset of the same county-weeks, so all.x)
weekly_matched_poi <- merge(
  to_wide(matched_daily,              'mpoi_'),
  to_wide(matched_daily[(is_matched)], 'mpoi_matched_'),
  by = c('county_geoid', 'week_and_year'), all.x = TRUE
) %>% as_tibble() 


# 2017 dollars: annual CPI deflator of the week's year (week_year), so a week
# spanning New Year's is deflated with one year's CPI
cpi_data_annual <- read_csv(FRED_ANNUAL_CPI_DATA_PATH) %>% select(year, deflator)

# the matched-POI columns join on county x week only (week_year is redundant with week_and_year
# and absent from weekly_matched_poi). The two halves of deferrable spend are deflated to 2017
# dollars the same way as weekly_spend; visits are left in levels.
weekly_df <- weekly_smoke_and_spend %>%
  full_join(weekly_spend, by=c('county_geoid', 'week_and_year', 'week_year')) %>%
  full_join(weekly_matched_poi, by=c('county_geoid', 'week_and_year')) %>%
  left_join(cpi_data_annual, by=c('week_year' = 'year')) %>%
  mutate(
    week_of_month = ceiling(mday(week_start) / 7),
    week_ym = format(week_start, '%Y-%m'),
    weekly_spend_2017 = weekly_spend * deflator,
    weekly_spend_deferrable_2017 = weekly_spend_deferrable * deflator,
    log_weekly_spend_deferrable_2017 = log(weekly_spend_deferrable_2017 + 1),
    asinh_weekly_spend_deferrable_2017 = asinh(weekly_spend_deferrable_2017),
    across(c(mpoi_spend_h1, mpoi_spend_h2, mpoi_matched_spend_h1, mpoi_matched_spend_h2),
           ~ .x * deflator),
    mpoi_matched_spend=mpoi_matched_spend_h1 + mpoi_matched_spend_h2,
    mpoi_spend=mpoi_spend_h1 + mpoi_spend_h2
  ) %>%
  filter(as.integer(str_sub(week_and_year, 3)) < 2024)


write_csv(weekly_df, WEEKLY_DATA_FILEPATH)
