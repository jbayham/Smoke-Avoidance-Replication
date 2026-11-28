library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(PLOTTING_HELPERS_PATH)


spending_visit_coefficient_data <- read_csv(WELFARE_SPENDING_VSL_DATA)
est_bin_base <- readRDS(file.path(REGRESSION_RESULTS_DIR, "binary_base_day_of_dose_response.rds"))


price_elasticity <- -1.0

# one row per smoke bin and slope
welfare_per_bin <- add_new_sample_and_sector(
  reg_results = est_bin_base,
  sector_name = "Deferrable Sectors",
  sample_label = "Overall Deferrable Foot Traffic",
  overall_sample_label = "Sample",
  vars_to_plot_start = "bin_d_"
) %>% 
  select(Estimate, Std..Error, vars) %>% 
  mutate(
    percent_change_in_visits_from_smoke = exp(Estimate) - 1,
    bin = str_sub(vars, start=7),
    bin_label = factor(BIN_LABELS[vars], levels = unname(BIN_LABELS)),
    percent_change_in_visits_from_smoke = if_else(bin %in% c('001_015', '015_030', '030_060'), 0, percent_change_in_visits_from_smoke),
  ) %>% 
  cross_join(spending_visit_coefficient_data %>% select(coef_spend_on_visits_table_2_fes, avg_per_county_daily_spending)) %>% 
  mutate(
    percent_change_in_spend_from_smoke=percent_change_in_visits_from_smoke*coef_spend_on_visits_table_2_fes,
    change_in_spend_from_smoke = percent_change_in_spend_from_smoke*avg_per_county_daily_spending,
    welfare = 0.5*percent_change_in_spend_from_smoke*change_in_spend_from_smoke/abs(price_elasticity)
  )


saveRDS(welfare_per_bin, ESTIMATED_WELFARE_PER_BIN_PATH)


canada_estimate <- tibble(days_060_100=1218,
                          days_100_200=645,
                          days_200_500=6) %>% 
  mutate(
    spend_060_100=days_060_100*welfare_per_bin$change_in_spend_from_smoke[welfare_per_bin$bin=='060_100'],
    spend_100_200=days_100_200*welfare_per_bin$change_in_spend_from_smoke[welfare_per_bin$bin=='100_200'],
    spend_200_500=days_200_500*welfare_per_bin$change_in_spend_from_smoke[welfare_per_bin$bin=='200_500'],
    welfare_060_100=-1*days_060_100*welfare_per_bin$welfare[welfare_per_bin$bin=='060_100'],
    welfare_100_200=-1*days_100_200*welfare_per_bin$welfare[welfare_per_bin$bin=='100_200'],
    welfare_200_500=-1*days_200_500*welfare_per_bin$welfare[welfare_per_bin$bin=='200_500']
  )

print(paste0('Canada Smoke Spend: ', sum(canada_estimate %>% select(starts_with('spend')) %>% t())))
print(paste0('Canada Smoke Welfare: ', sum(canada_estimate %>% select(starts_with('welfare')) %>% t())))

