library(tidyverse)
library(janitor)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R')) 

# filtering to October 2018 to allow the background window to reach back for
# early 2019 when there is not enough background pollution data. The buffer is
# 92 days, which covers the MAX_BACKFILL_DAYS = 90 reach-back below.

daily_smoke_data_avg  <- read_csv(COUNTY_DAILY_SMOKE_PM_AVG_PATH) %>% filter(date > as_date('2018-10-01'))
daily_smoke_data_med  <- read_csv(COUNTY_DAILY_SMOKE_PM_MED_PATH) %>% filter(date > as_date('2018-10-01'))
daily_aqi_data        <- read_csv(COUNTY_AQI_DATA_OUTPUT_PATH) %>% filter(year > 2018)

daily_smoke_data_avg <- daily_smoke_data_avg %>% 
  select(GEOID, date, area_weighted_pm, area_weighted_smoke, area_weighted_bg, area_weighted_hms, any_hms, within_county_variance) %>% 
  rename(
    county_geoid=GEOID,
    area_weighted_bg_avg=area_weighted_bg,
    area_weighted_smoke_avg=area_weighted_smoke,
    within_daily_smoke_var_avg=within_county_variance
  )
# Note that area_weighted_pm, area_weighted_hms, and any_hms are the same in both
# datasets, so I only pull them for the avg one. within_county_variance is the
# cross-grid-cell variance of smoke PM within the county on that day, and it does
# differ between the mean- and median-background products, so it is carried
# through from both.


daily_smoke_data_med <- daily_smoke_data_med %>% 
  select(GEOID, date, area_weighted_smoke, area_weighted_bg, within_county_variance) %>% 
  rename(
    county_geoid=GEOID,
    area_weighted_bg_med=area_weighted_bg,
    area_weighted_smoke_med=area_weighted_smoke,
    within_daily_smoke_var_med=within_county_variance
  )

daily_aqi_data <- clean_names(daily_aqi_data) %>% 
  mutate(county_geoid= str_c(state_code, county_code)) %>% 
  select(date, aqi, defining_parameter, aqi_boundary, county_geoid, category)

# Minimum number of smoke-free days behind a background sd. Below ~20 the
# estimate is both noisy and biased downward (E[s]/sigma ~ 0.75 at n=2), and a
# too-small sd lets background PM through the filter as if it were smoke.
MIN_BG_DAYS <- 20

# Ceiling on how far back in calendar time the window may reach for those days.
# Uncapped, 2023 Q3 counties were pulling baselines from as far back as
# mid-February, so a summer background sd was being built out of winter air.
# 90 days keeps the baseline inside roughly one season.
MAX_BACKFILL_DAYS <- 90

# bg_pm_sd for a single county's full time series.
#
# The background sd for a quarter is taken over that quarter's smoke-free days
# (any_hms == 0). When the quarter holds fewer than min_days of them -- 2023 Q3
# is the case that matters, where the Canadian smoke left a median of 4 -- the
# window widens backwards in calendar time, into the previous quarter, until
# min_days smoke-free days are in hand or it hits max_backfill_days before the
# quarter starts, whichever comes first. It never looks forward past the quarter
# and never pools the same calendar quarter across years, so the baseline stays
# contemporaneous.
#
# min_days is a floor on data, max_backfill_days a ceiling on time, and the
# ceiling wins: a county whose reach-back is exhausted keeps whatever smoke-free
# days fall inside it, and n_bg_days then reports fewer than min_days. Returns
# NA only when fewer than two smoke-free days fall in the allowed window. Row
# order does not matter: every row is mapped back to its own quarter's value.
backward_bg_pm_sd <- function(date, pm, any_hms, quarter_year,
                              min_days = MIN_BG_DAYS,
                              max_backfill_days = MAX_BACKFILL_DAYS) {
  d        <- as.integer(date)
  is_clean <- !is.na(any_hms) & any_hms == 0 & !is.na(pm)

  ord <- order(d[is_clean])
  cd  <- d[is_clean][ord]   # smoke-free dates, ascending
  cp  <- pm[is_clean][ord]  # matching background PM

  q       <- factor(quarter_year, levels = unique(quarter_year))
  q_start <- tapply(d, q, min)
  q_end   <- tapply(d, q, max)

  # j: last smoke-free day at or before the quarter ends
  # i: first smoke-free day at or after it starts
  j    <- findInterval(q_end, cd)
  i    <- findInterval(q_start - 1L, cd) + 1L
  n_in <- pmax(j - i + 1L, 0L)
  from <- if_else(n_in >= min_days, i, pmax(1L, j - min_days + 1L))

  # earliest: first smoke-free day the reach-back is allowed to touch. Only
  # binds on the widening branch, since from = i there otherwise.
  earliest <- findInterval(q_start - max_backfill_days - 1L, cd) + 1L
  from     <- pmax(from, earliest)

  usable    <- j >= 1L & from <= j & (j - from + 1L) >= 2L
  sds       <- rep(NA_real_,    length(j))
  n_used    <- rep(0L,          length(j))
  win_start <- rep(NA_integer_, length(j))
  for (k in which(usable)) {
    idx          <- from[k]:j[k]
    sds[k]       <- sd(cp[idx])
    n_used[k]    <- length(idx)
    win_start[k] <- cd[from[k]]
  }

  k <- as.integer(q)
  tibble(
    bg_pm_sd        = unname(sds[k]),
    n_bg_days       = unname(n_used[k]),
    bg_window_start = as.Date(unname(win_start[k]), origin = "1970-01-01")
  )
}

all_daily_smoke_data <- daily_smoke_data_avg %>%
  full_join(daily_aqi_data, by=join_by("county_geoid", "date")) %>%
  full_join(daily_smoke_data_med, by=c('county_geoid', 'date')) %>%
  mutate(
    year = lubridate::year(date),
    quarter = lubridate::quarter(date),
    quarter_year = str_c(year, quarter, sep="_")
  ) %>%
  arrange(county_geoid, date) %>%
  group_by(county_geoid) %>%
  mutate(
    bg = backward_bg_pm_sd(date, area_weighted_pm, any_hms, quarter_year)
  ) %>%
  ungroup() %>%
  unpack('bg')


lerp <- function(sa, fa, sp, fp, c){(c - sp)/(fp - sp)*(fa - sa) + sa}
# https://observablehq.com/@mbostock/pm25-to-aqi
convert_pm25_to_aqi <- function(pm25) {
  c <- floor(10 * pm25) / 10
  c <- case_when(
    c < 0     ~ 0,
    c <  9.1  ~ lerp(  0,  50,   0.0,   9.0, c),
    c <  35.5 ~ lerp( 51, 100,   9.1,  35.4, c),
    c <  55.5 ~ lerp(101, 150,  35.5,  55.4, c),
    c < 150.5 ~ lerp(151, 200,  55.5, 150.4, c),
    c < 250.5 ~ lerp(201, 300, 150.5, 250.4, c),
    c < 350.5 ~ lerp(301, 400, 250.5, 350.4, c),
    c < 500.5 ~ lerp(401, 500, 350.5, 500.4, c)
  )
  return(round(c))
}

non_zero_smoke_avg <- all_daily_smoke_data$area_weighted_smoke_avg[(all_daily_smoke_data$area_weighted_smoke_avg > 0) & !near(all_daily_smoke_data$area_weighted_smoke_avg, 0)]
quantiles_avg <- quantile(non_zero_smoke_avg, seq(0, 1, by=0.2), na.rm = TRUE)

non_zero_smoke_med <- all_daily_smoke_data$area_weighted_smoke_med[(all_daily_smoke_data$area_weighted_smoke_med > 0) & !near(all_daily_smoke_data$area_weighted_smoke_med, 0)]
quantiles_med <- quantile(non_zero_smoke_med, seq(0, 1, by=0.2), na.rm = TRUE)

daily_smoke_data_with_aqi <- all_daily_smoke_data  %>% 
  mutate(
    aqi_from_smoke_pm_mean = convert_pm25_to_aqi(area_weighted_smoke_avg),
    aqi_from_smoke_pm_median = convert_pm25_to_aqi(area_weighted_smoke_med),
    bg_aqi_median = convert_pm25_to_aqi(area_weighted_bg_med),
    bg_aqi_mean = convert_pm25_to_aqi(area_weighted_bg_avg),
    aqi_rm_background_median = aqi - bg_aqi_median,
    aqi_rm_background_mean = aqi - bg_aqi_mean,
    
    prop_non_aqi_from_background_mean = aqi_from_smoke_pm_mean/aqi_rm_background_mean,
    prop_non_aqi_from_background_median = aqi_from_smoke_pm_median/aqi_rm_background_median,
    
    smoke_quantile_mean = cut(area_weighted_smoke_avg, 
                              breaks = quantiles_avg,
                              labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                              include.lowest = TRUE),
    smoke_quantile_med = cut(area_weighted_smoke_med, 
                              breaks = quantiles_med,
                              labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                              include.lowest = TRUE),
    abs_prop_non_aqi_from_background_mean   = abs(prop_non_aqi_from_background_mean),
    abs_prop_non_aqi_from_background_median = abs(prop_non_aqi_from_background_median),
    
    adj_pm_smoke_med_1sd = if_else(abs(area_weighted_smoke_med) <= bg_pm_sd, 0, area_weighted_smoke_med),
    adj_pm_smoke_med_2sd = if_else(abs(area_weighted_smoke_med) <= 2*bg_pm_sd, 0, area_weighted_smoke_med),
    
    adj_pm_smoke_avg_1sd = if_else(abs(area_weighted_smoke_avg) <= bg_pm_sd, 0, area_weighted_smoke_avg),
    adj_pm_smoke_avg_2sd = if_else(abs(area_weighted_smoke_avg) <= 2*bg_pm_sd, 0, area_weighted_smoke_avg),
    
    adj_smoke_pm_med_0p05 = case_when(near(area_weighted_smoke_med, 0) ~ 0, is.na(aqi) ~ area_weighted_smoke_med, abs_prop_non_aqi_from_background_median < 0.05 ~ 0, TRUE ~ area_weighted_smoke_med),
    adj_smoke_pm_med_0p10 = case_when(near(area_weighted_smoke_med, 0) ~ 0, is.na(aqi) ~ area_weighted_smoke_med, abs_prop_non_aqi_from_background_median < 0.10 ~ 0, TRUE ~ area_weighted_smoke_med),
    adj_smoke_pm_med_0p20 = case_when(near(area_weighted_smoke_med, 0) ~ 0, is.na(aqi) ~ area_weighted_smoke_med, abs_prop_non_aqi_from_background_median < 0.20 ~ 0, TRUE ~ area_weighted_smoke_med),
    
    adj_smoke_pm_avg_0p05 = case_when(near(area_weighted_smoke_avg, 0) ~ 0, is.na(aqi) ~ area_weighted_smoke_avg, abs_prop_non_aqi_from_background_mean < 0.05 ~ 0, TRUE ~ area_weighted_smoke_avg),
    adj_smoke_pm_avg_0p10 = case_when(near(area_weighted_smoke_avg, 0) ~ 0, is.na(aqi) ~ area_weighted_smoke_avg, abs_prop_non_aqi_from_background_mean < 0.10 ~ 0, TRUE ~ area_weighted_smoke_avg),
    adj_smoke_pm_avg_0p20 = case_when(near(area_weighted_smoke_avg, 0) ~ 0, is.na(aqi) ~ area_weighted_smoke_avg, abs_prop_non_aqi_from_background_mean < 0.20 ~ 0, TRUE ~ area_weighted_smoke_avg),

    adj_smoke_pm_med_0p05_bg_avg_1sd = case_when(near(adj_pm_smoke_med_1sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_med_1sd, abs_prop_non_aqi_from_background_median < 0.05 ~ 0, TRUE ~ adj_pm_smoke_med_1sd),
    adj_smoke_pm_med_0p10_bg_avg_1sd = case_when(near(adj_pm_smoke_med_1sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_med_1sd, abs_prop_non_aqi_from_background_median < 0.10 ~ 0, TRUE ~ adj_pm_smoke_med_1sd),
    adj_smoke_pm_med_0p20_bg_avg_1sd = case_when(near(adj_pm_smoke_med_1sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_med_1sd, abs_prop_non_aqi_from_background_median < 0.20 ~ 0, TRUE ~ adj_pm_smoke_med_1sd),
    
    adj_smoke_pm_med_0p05_bg_avg_2sd = case_when(near(adj_pm_smoke_med_2sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_med_2sd, abs_prop_non_aqi_from_background_median < 0.05 ~ 0, TRUE ~ adj_pm_smoke_med_2sd),
    adj_smoke_pm_med_0p10_bg_avg_2sd = case_when(near(adj_pm_smoke_med_2sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_med_2sd, abs_prop_non_aqi_from_background_median < 0.10 ~ 0, TRUE ~ adj_pm_smoke_med_2sd),
    adj_smoke_pm_med_0p20_bg_avg_2sd = case_when(near(adj_pm_smoke_med_2sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_med_2sd, abs_prop_non_aqi_from_background_median < 0.20 ~ 0, TRUE ~ adj_pm_smoke_med_2sd),
    
    adj_smoke_pm_avg_0p05_bg_avg_1sd = case_when(near(adj_pm_smoke_avg_1sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_avg_1sd, abs_prop_non_aqi_from_background_mean < 0.05 ~ 0, TRUE ~ adj_pm_smoke_avg_1sd),
    adj_smoke_pm_avg_0p10_bg_avg_1sd = case_when(near(adj_pm_smoke_avg_1sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_avg_1sd, abs_prop_non_aqi_from_background_mean < 0.10 ~ 0, TRUE ~ adj_pm_smoke_avg_1sd),
    adj_smoke_pm_avg_0p20_bg_avg_1sd = case_when(near(adj_pm_smoke_avg_1sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_avg_1sd, abs_prop_non_aqi_from_background_mean < 0.20 ~ 0, TRUE ~ adj_pm_smoke_avg_1sd),
    
    adj_smoke_pm_avg_0p05_bg_avg_2sd = case_when(near(adj_pm_smoke_avg_2sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_avg_2sd, abs_prop_non_aqi_from_background_mean < 0.05 ~ 0, TRUE ~ adj_pm_smoke_avg_2sd),
    adj_smoke_pm_avg_0p10_bg_avg_2sd = case_when(near(adj_pm_smoke_avg_2sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_avg_2sd, abs_prop_non_aqi_from_background_mean < 0.10 ~ 0, TRUE ~ adj_pm_smoke_avg_2sd),
    adj_smoke_pm_avg_0p20_bg_avg_2sd = case_when(near(adj_pm_smoke_avg_2sd, 0) ~ 0, is.na(aqi) ~ adj_pm_smoke_avg_2sd, abs_prop_non_aqi_from_background_mean < 0.20 ~ 0, TRUE ~ adj_pm_smoke_avg_2sd)
    
  ) %>%
  # filtering to just the years of analysis
  filter(year(date) > 2018)

write_csv(daily_smoke_data_with_aqi, COUNTY_DAILY_SMOKE_WITH_FILTERS_PATH)
