################################################################################
# This code creates daily dataset for the project!                             #
# We look at visits data because we have that on a daily basis.                #
################################################################################
library(conflicted)
library(tidyverse)
library(fixest)
library(arrow)
library(glue)
library(scales)
library(slider)
conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::isoweek)
conflicts_prefer(lubridate::year)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

daily_spend <- open_dataset(COUNTY_DAILY_SPEND_DIR, format='parquet', unify_schemas = TRUE) %>%
  mutate(
    date=as_date(spend_date_range_start, tz=NULL),
    county_geoid = str_pad(as.character(county_geoid), width=5, side='left', pad='0')
  ) %>% 
  filter(year(date)>=2019, year(date)<=2023)

daily_smoke <- open_csv_dataset(COUNTY_DAILY_SMOKE_WITH_FILTERS_PATH) %>% 
  mutate(county_geoid=str_pad(as.character(county_geoid), width=5, side='left', pad='0'))

daily_weather <- open_dataset(DAILY_WEATHER_2019_2023_PATHS, format="csv") %>% 
  mutate(county_geoid = str_pad(as.character(fips), 5, side="left", pad='0')) %>% 
  select(-c('county_fips', 'fips'))

daily_fire <- read_csv(DAILY_FIRE_POINTS_PATH, show_col_types = FALSE) %>%
  select(county_fips, date, viirs_total, viirs_in_perimeter) %>%
  mutate(
    county_geoid = str_pad(as.character(county_fips), 5, side="left", pad='0'),
    date = as.Date(date)
  )

# Main dataset loading and processing
daily_visits <- open_dataset(COUNTY_DAILY_VISITS_DIR, format = "parquet", unify_schemas = TRUE) %>%
  mutate(
    date=as_date(date_range_start, tz=NULL),
    county_geoid = str_pad(as.character(county_geoid), width=5, side='left', pad='0')
  )

county_info <- open_dataset(COUNTY_INFO_COMBINED_PATH, format='csv') %>% 
  mutate(county_geoid=str_pad(as.character(county_geoid), side='left', pad='0', width=5))

non_CONUS_FIPS_codes <- get_non_US_or_CONUS_state_fips_codes(filter_for_CONUS=TRUE)

# Canadian GEOIDS data leak into the visits data, and turn into "00003" and "00005"
# The EPA AQI data has observations for Mexico, with the prefix "80", leading to "80002", "80026"
NON_US_COUNTY_GEOIDS <- c("00003", "00005", "80002", "80026")
# Half of vermont is missing visits data in July 2022 
INCOMPLETE_COUNTY_GEOIDS <- c("50007", "50013", "50015", "50017", "50019", "50001", "50005", "50009", "50011", "50023")

# Visits and spend both have one naics_<code> column per deferrable NAICS code. Name them
# naics_<code>_visits and naics_<code>_spend before joining: a join suffix alone would
# leave codes found on only one side unsuffixed, so the side could not be told from the name.
suffix_sector_columns <- function(ds, suffix) {
  sector_cols <- grep('^naics_', names(ds), value = TRUE)
  rename(ds, all_of(setNames(sector_cols, paste0(sector_cols, suffix))))
}
daily_visits <- suffix_sector_columns(daily_visits, '_visits')
daily_spend  <- suffix_sector_columns(daily_spend,  '_spend')

# Materialize each input before the multi-source full join. The server Arrow
# execution plan can lose dates in this lazy join even though the source keys
# are complete. The October reproduction used the same in-memory join strategy.
# Validate sources rather than dropping corrupted dates from a joined result.
materialize_daily_source <- function(ds, label) {
  ds <- collect(ds)
  if (anyNA(ds$county_geoid) || anyNA(ds$date) ||
      anyDuplicated(ds[, c('county_geoid', 'date')])) {
    stop('Missing or duplicate county/date keys in ', label)
  }
  ds
}
daily_visits <- materialize_daily_source(daily_visits, 'visits')
daily_spend <- materialize_daily_source(daily_spend, 'spend')
daily_weather <- materialize_daily_source(daily_weather, 'weather')
daily_smoke <- materialize_daily_source(daily_smoke, 'smoke')
daily_fire <- materialize_daily_source(daily_fire, 'fire')
county_info <- collect(county_info)
stopifnot(!anyNA(county_info$county_geoid), !anyDuplicated(county_info$county_geoid))

# Combine all datasets
full_df <- daily_visits %>%
  full_join(daily_spend,   by=join_by('date', 'county_geoid')) %>% 
  full_join(daily_weather, by=join_by('date', 'county_geoid')) %>% 
  full_join(daily_smoke,   by=join_by('date', 'county_geoid')) %>% 
  left_join(daily_fire,    by=join_by('date', 'county_geoid')) %>% 
  left_join(county_info,   by='county_geoid')


full_df <- full_df %>% 
  mutate(across(starts_with('daily_spend'), ~./population_estimate, .names="{.col}_per_capita"),
         state_year_month=str_c(str_sub(county_geoid, end=2), year(date), month(date)),
         yesterday = as_date(date - ddays(1)),
         # changing week from Tuesday to Monday to account for weekend transactions that aren't processed on the weekend
         week_of_year=isoweek(yesterday),
         day_of_week=wday(date),
         is_sunday=day_of_week==1,
         is_saturday=day_of_week==7,
         is_weekend = (is_sunday | is_saturday),
         day_of_month=day(date),
         week = week(date),
         state_day=str_c(str_sub(county_geoid, end=2), day_of_month),
         
         season=case_when(
           month(date) %in% c(3, 4, 5)   ~ 1,  # spring
           month(date) %in% c(6, 7, 8)   ~ 2,  # summer
           month(date) %in% c(9, 10, 11) ~ 3,  # fall
           month(date) %in% c(12, 1, 2)  ~ 4   # winter
         ),
         # change Kelvin into farenheit
         across(c(tmmn, tmmx), ~(.x - 273.15) * (9/5) + 32)
         ) %>% 
  collect() %>%
  # Drop rows with a missing county code. A missing county_geoid in one of the joined
  # sources comes through str_pad as the string "NA NA", so these rows survive the
  # full joins. They carry no visits, weather or smoke data, but were flagged as part of
  # the balanced panel. The 2026-11-16 build had 1,826 such rows (1,821 days flagged
  # balanced_panel plus 5 more), one row per date. The weekly panel built from this file
  # had 261 matching county-weeks.
  filter(!is.na(county_geoid), str_detect(county_geoid, '^[0-9]{5}$')) %>%
  # removes Alaska, Hawaii, and PR
  filter(
    !(str_sub(county_geoid, end=2) %in% non_CONUS_FIPS_codes),
    !(county_geoid %in% NON_US_COUNTY_GEOIDS)
  ) %>% 
  mutate(
    balanced_panel = case_when(
      # half of Vermont missing from visits data 
      (county_geoid %in% INCOMPLETE_COUNTY_GEOIDS) ~ FALSE,
      # November 30th missing in all visits data
      (month(date)==11 & day(date)==30) ~ FALSE,
      # January 1, 2023 has no visits in any county in the visits data
      (date == as_date('2023-01-01')) ~ FALSE,
      TRUE ~ TRUE
    )
  )

quantile_cuts <- seq(0, 1, length.out = 6)
tmmn_quantiles  <- quantile(full_df$tmmn,               quantile_cuts, type = 2, na.rm=T)
tmmx_quantiles  <- quantile(full_df$tmmx,               quantile_cuts, type = 2, na.rm=T)
pr_quantiles    <- quantile(full_df$pr[full_df$pr > 0], quantile_cuts, type = 2, na.rm=T)
rmax_quantiles  <- quantile(full_df$rmax,               quantile_cuts, type = 2, na.rm=T)


full_df <- full_df %>% mutate(
  binned_tmmn  = as.factor(.bincode(full_df$tmmn, tmmn_quantiles,  include.lowest = TRUE)),
  binned_tmmx  = as.factor(.bincode(full_df$tmmx, tmmx_quantiles,  include.lowest = TRUE)),
  binned_rmax  = as.factor(.bincode(full_df$rmax, rmax_quantiles,  include.lowest = TRUE)),
  # compute bins on non-zero precip, add in 0 precip as a non-null category.
  binned_pr = .bincode(full_df$pr, pr_quantiles, include.lowest = TRUE),
  binned_pr = as.factor(if_else(pr==0, 0, binned_pr)),
  
  across(c('area_weighted_pm', starts_with('area_weighted_smoke'), starts_with('adj_smoke_pm'), starts_with('adj_pm_smoke_')) & !contains('_bin_'), ~if_else((. > 0)   & (. <= 15) & !near(., 0), ., 0), .names='{.col}_bin_001_015'),
  across(c('area_weighted_pm', starts_with('area_weighted_smoke'), starts_with('adj_smoke_pm'), starts_with('adj_pm_smoke_')) & !contains('_bin_'), ~if_else((. > 15)  & (. <= 30),               ., 0), .names='{.col}_bin_015_030'),
  across(c('area_weighted_pm', starts_with('area_weighted_smoke'), starts_with('adj_smoke_pm'), starts_with('adj_pm_smoke_')) & !contains('_bin_'), ~if_else((. > 30)  & (. <= 60),               ., 0), .names='{.col}_bin_030_060'),
  across(c('area_weighted_pm', starts_with('area_weighted_smoke'), starts_with('adj_smoke_pm'), starts_with('adj_pm_smoke_')) & !contains('_bin_'), ~if_else((. > 60)  & (. <= 100),              ., 0), .names='{.col}_bin_060_100'),
  across(c('area_weighted_pm', starts_with('area_weighted_smoke'), starts_with('adj_smoke_pm'), starts_with('adj_pm_smoke_')) & !contains('_bin_'), ~if_else((. > 100) & (. <= 200),              ., 0), .names='{.col}_bin_100_200'),
  across(c('area_weighted_pm', starts_with('area_weighted_smoke'), starts_with('adj_smoke_pm'), starts_with('adj_pm_smoke_')) & !contains('_bin_'), ~if_else((. > 200),                           ., 0), .names='{.col}_bin_200_500'),

  across(starts_with('daily_vis'), ~log(. + 1), .names = 'log_{.col}'),
  asinh_daily_visits_deferrable = asinh(daily_visits_deferrable),
  
  # selecting the pm variable to be used in the rest of the analysis
  bin_d_001_015 = as.integer(adj_pm_smoke_med_1sd_bin_001_015 > 0),
  bin_d_015_030 = as.integer(adj_pm_smoke_med_1sd_bin_015_030 > 0),
  bin_d_030_060 = as.integer(adj_pm_smoke_med_1sd_bin_030_060 > 0),
  bin_d_060_100 = as.integer(adj_pm_smoke_med_1sd_bin_060_100 > 0),
  bin_d_100_200 = as.integer(adj_pm_smoke_med_1sd_bin_100_200 > 0),
  bin_d_200_500 = as.integer(adj_pm_smoke_med_1sd_bin_200_500 > 0),
  
  viirs_total        = coalesce(viirs_total, 0),
  viirs_in_perimeter = coalesce(viirs_in_perimeter, 0)
)

for (t in FIRE_THRESHOLD_LIST) {
  full_df[[paste0("confirmed_wildfire_", t)]]   <- as.integer(full_df$viirs_in_perimeter > t)
  full_df[[paste0("confirmed_other_fire_", t)]] <- as.integer(
    full_df$viirs_in_perimeter == 0 & full_df$viirs_total > t
  )
  full_df[[paste0("confirmed_any_fire_", t)]]   <- as.integer(full_df$viirs_total > t)
}


stopifnot(!anyNA(full_df$date), !anyNA(full_df$county_geoid),
          !anyDuplicated(full_df[, c('county_geoid', 'date')]))
write_csv(full_df, DAILY_DATA_FILEPATH)

# average smoke PM on the days that fall in each bin (one row per bin: bin, avg_pm)
avg_pm_per_bin <- full_df %>%
  summarize(across(all_of(names(BIN_LABELS)),
                   ~ mean(adj_pm_smoke_med_1sd[.x > 0], na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "bin", values_to = "avg_pm") %>%
  mutate(bin = str_remove(bin, "^bin_d_"))

write_csv(avg_pm_per_bin, AVG_PM_PER_BIN_PATH)

# full_df <- read_csv(DAILY_DATA_FILEPATH)
