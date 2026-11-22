library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(PLOTTING_HELPERS_PATH)

spending_visit_coefficient_data <- read_csv(WELFARE_SPENDING_VSL_DATA)

VSL_val_2017 <- spending_visit_coefficient_data %>% pull(VSL_val_2017)

# https://doi.org/10.1038/s41586-025-09611-w
qiu_extended_data_table_1 <- tibble(
  annual_bins_qiu = c('(0.1,0.25]', '(0.25,0.5]', '(0.5,0.75]','(0.75,1]','(1,2]','(2,3]','(3,4]', '>4'),
  mortality_qiu   = c(3.9, 5.5, 6.9, 9.4, 5.9, 4.4, 5.6, 23.6),
  lci_95_qiu      = c(1.9,3.1,3.4,4.7,1.2,-4.4,-5.7,14.7),
  uci_95_qiu      = c(6.1, 7.9, 10.7, 13.9, 11.0, 12.9, 18.0, 32.7)
  )

qiu_cut_break<-c(0.1, 0.25, 0.5, 0.75, 1, 2, 3, 4, Inf)

#https://doi.org/10.1073/pnas.2403960121
ma_mortality_lookup_table <- tibble(
  annual_bins_ma = c('(0.1,0.2]', '(0.2,0.3]', '(0.3,0.4]', '(0.4,0.5]', '(0.5,0.7]', '(0.7,1]', '(1,5]', '>5'),
  mortality_ma   = c(0.16, 0.40, 0.35, 0.34, 0.49, 0.63, 0.36, 2.11),
  lci_95_ma      = c(0.06, 0.26, 0.21, 0.19, 0.33, 0.44, 0.11, 1.24),
  uci_95_ma      = c(0.26, 0.54, 0.49, 0.50, 0.65, 0.83, 0.61, 2.99)
)

ma_cut_break <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 1, 5, Inf)

# Average smoke PM on the days in each bin (built by build_reg_data_daily.R)
annual_pm_contribution <- read_csv(AVG_PM_PER_BIN_PATH) %>%
  mutate(annual_contribution = avg_pm / 365)


# results from metaanalysis
#https://doi.org/10.1146/annurev-med-052422-020909

pct_increase_in_all_cause_mortality <- 0.0015
cause_of_death_files_directory_and_prefix <- file.path(ALL_CAUSE_MORTALITY_DIR, "Underlying Cause of Death ")

dir_paths <- sapply(sapply(2019:2023, as.character), \(yr) paste0(cause_of_death_files_directory_and_prefix, yr, '.csv'))
col_types = 'cccddd'
all_mortality <- bind_rows(
  read_csv(dir_paths[[1]], col_select=-Notes, col_types=col_types) %>% mutate(year=names(dir_paths[1])) %>% janitor::clean_names(),
  read_csv(dir_paths[[2]], col_select=-Notes, col_types=col_types) %>% mutate(year=names(dir_paths[2])) %>% janitor::clean_names(),
  read_csv(dir_paths[[3]], col_select=-Notes, col_types=col_types) %>% mutate(year=names(dir_paths[3])) %>% janitor::clean_names(),
  read_csv(dir_paths[[4]], col_select=-Notes, col_types=col_types) %>% mutate(year=names(dir_paths[4])) %>% janitor::clean_names(),
  read_csv(dir_paths[[5]], col_select=-Notes, col_types=col_types) %>% mutate(year=names(dir_paths[5])) %>% janitor::clean_names(),
)

all_mortality_averages <- all_mortality %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  summarize(
    across(c(deaths), ~sum(., na.rm=T), .names = "total_{.col}"),
    across(c(deaths, population, crude_rate), ~mean(., na.rm=T), .names="county_avg_{.col}")
  )

annual_avg_county_crude_rate <- mean(all_mortality_averages$county_avg_crude_rate)


annual_pm_contribution_with_mortality <- annual_pm_contribution %>%
  # --- Apply Qiu et al. mortality estimates ---
  mutate(
    # Create the Qiu bin for joining (intervals are right-closed: (a, b])
    annual_bins_qiu = cut(
      x = annual_contribution,
      breaks = qiu_cut_break,
      labels = qiu_extended_data_table_1$annual_bins_qiu,
      right = TRUE,
      include.lowest = FALSE
    )
  ) %>%
  # Join mortality data by the new bin
  left_join(qiu_extended_data_table_1, by = "annual_bins_qiu") %>%
  
  # --- Apply Ma et al. mortality estimates ---
  mutate(
    # Create the Ma bin for joining (intervals are right-closed: (a, b])
    annual_bins_ma = cut(
      x = annual_contribution,
      breaks = ma_cut_break,
      labels = ma_mortality_lookup_table$annual_bins_ma,
      right = TRUE,
      include.lowest = FALSE
    )
  ) %>%
  # Join mortality data by the new bin
  left_join(ma_mortality_lookup_table, by = "annual_bins_ma") %>% 
  mutate(
    across(starts_with("mortality"), ~.*VSL_val_2017),
    mortality_meta_analysis = VSL_val_2017*avg_pm*pct_increase_in_all_cause_mortality*annual_avg_county_crude_rate
    )

annual_pm_contribution_with_mortality

saveRDS(annual_pm_contribution_with_mortality, ESTIMATED_MORTALITY_COST_BY_BIN_PATH)
