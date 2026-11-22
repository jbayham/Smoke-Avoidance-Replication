library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# in the weekly reg data, daily_visits_deferrable is already summed over the week
weekly_df <- read_csv(WEEKLY_DATA_FILEPATH) %>%
  rename(year = week_year, weekly_visits_deferrable = daily_visits_deferrable)
cpi_data_annual <- read_csv(FRED_ANNUAL_CPI_DATA_PATH)

mean_match_weighted_daily_spend <- read_csv(AVERAGE_DAILY_SPEND_PATH) %>% 
  filter(weighting=='matched-visit-share-weighted') %>% 
  pull(avg_daily_county_spend_2017)


# The welfare effect uses the weekly spend-on-visits slope from the split-sample IV (ORIV)
# estimator on all places (matched_poi_and_split_sample_spend_on_visits.R) with the table 2 FEs.
SPEND_ON_VISITS_FE_LABEL <- 'table 2 FEs'   # value in the CSV's fixed_effects column
spend_on_visits_oriv <- read_csv(SPEND_ON_VISITS_SPLIT_SAMPLE_RESULTS_PATH) %>%
  filter(sample == 'matched places', estimator == 'Split-sample IV (ORIV)',
         fixed_effects == SPEND_ON_VISITS_FE_LABEL)
stopifnot(nrow(spend_on_visits_oriv) == 1)


yearly_avgs <- weekly_df  %>% 
  group_by(county_geoid, year) %>% 
  rename(weekly_mpoi_matched_spend=mpoi_matched_spend, weekly_mpoi_spend=mpoi_spend) %>% 
  summarize(
    across(c(weekly_spend_deferrable_2017, weekly_mpoi_spend, weekly_visits_deferrable), ~sum(., na.rm=T))
    ) %>%
  ungroup() %>% 
  group_by(year) %>% 
  summarize(
    across(c(weekly_spend_deferrable_2017, weekly_mpoi_spend, weekly_visits_deferrable), ~sum(., na.rm=T), .names="{sub('weekly', 'total', .col)}"),
    across(c(weekly_spend_deferrable_2017, weekly_mpoi_spend, weekly_visits_deferrable), ~mean(., na.rm=T), .names="{sub('weekly', 'avg_county', .col)}")
  ) %>% 
  filter(year < 2024)
  


# https://www.epa.gov/environmental-economics/mortality-risk-valuation
VSL_val_2006 <- 7.4*1e6

# Calculate the 2017 version of the mortality risk inflation

VSL_val_2017 <- cpi_data_annual %>% filter(year==2006) %>% pull(deflator) * VSL_val_2006 


us_consumer_spend_2017_dollars <- read_csv(WELFARE_ANNUAL_EXPENDITURES_CHAINED_PATH, skip=3, skip_empty_rows=TRUE, col_select=-Line) %>%
  rename(product_type=`...2`) 

us_consumer_spend_2017_dollars_filtered <- us_consumer_spend_2017_dollars %>% 
  filter(
    !is.na(product_type),
    product_type %in% c(
      'Recreation services',
      'Food services and accommodations'
    )) %>% 
  summarize(across(-product_type, sum)) %>% 
  pivot_longer(everything(), names_to = "year", values_to = "all_us_deferrable_spending_billions") %>% 
  mutate(
    all_us_deferrable_spending = all_us_deferrable_spending_billions*1e9,
    year = as.numeric(year)
  )


# one row: coef / lci_95 / uci_95 for the table-2-FEs slope (normal CIs from the
# county-clustered SEs)
spend_on_visits_slopes <- tibble(
  coef_spend_on_visits_table_2_fes   = spend_on_visits_oriv$estimate,
  lci_95_spend_on_visits_table_2_fes = spend_on_visits_oriv$estimate - 1.96 * spend_on_visits_oriv$se,
  uci_95_spend_on_visits_table_2_fes = spend_on_visits_oriv$estimate + 1.96 * spend_on_visits_oriv$se
)

spending_visit_coefficient_data <- yearly_avgs %>%
  inner_join(us_consumer_spend_2017_dollars_filtered, by="year") %>% 
  mutate(
    fraction_of_total_personal_consumption= total_spend_deferrable_2017/all_us_deferrable_spending
  ) %>% 
  summarize(across(c(total_visits_deferrable,total_spend_deferrable_2017,avg_county_visits_deferrable, avg_county_spend_deferrable_2017, fraction_of_total_personal_consumption), ~mean(., na.rm=T), .names="{sub('^total_', 'annual_', .col)}")) %>% 
  bind_cols(spend_on_visits_slopes) %>% 
  mutate(
    VSL_val_2017 = VSL_val_2017,
    avg_per_county_daily_spending = mean_match_weighted_daily_spend/round(fraction_of_total_personal_consumption, digits=4)
  )

write_csv(spending_visit_coefficient_data, WELFARE_SPENDING_VSL_DATA)


