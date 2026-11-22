################################################################################
# This code merges any county level information to be used in regressions.     #
# The information included is a measure of county natural amenity tourism      #
# dependency, county population, median household income, county income        #
# quantiles, county-specific naics code info for employment & establishments   #
# the CARR county rurality index, and background, non-smoke levels of PM.      #
# All except the rurality index are calculated for the year 2018               #
# The continuous characteristics are also split into county-level quartile     #
# dummies, which flow into the regression panel with the rest of this file.    #
################################################################################

library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

nat_amenity_tourism_dependency     <- read_csv(NATURAL_AMENITY_DEPENDENCY_PATH)
average_background_pm_by_county    <- read_csv(COUNTY_AVG_BACKGROUND_PM_2018_PATH)
county_census_information          <- read_csv(COUNTY_CENSUS_INFO_PATH)
cbp_naics_code_distribution_county <- read_csv(CBP_EST_AND_EMP_NAICS_CODE_DISTRIBUTION_2018_PATH)
carr_county_rurality_index         <- read_csv(CARR_COUNTY_RURALITY_INDEX_PATH)
bea_natural_amenity_tourism_state  <- read_csv(BEA_PERCENT_COMP_GDP_FROM_OUTDOOR_REC_PATH) %>% 
  mutate(state_fips=str_sub(geo_fips, end=2)) %>%
  filter(state_fips < "60") %>% 
  select(-c(geo_name, geo_fips, total_comp, total_gdp, rec_comp, rec_gdp))

non_CONUS_FIPS_codes <- get_non_US_or_CONUS_state_fips_codes(filter_for_CONUS=TRUE)

county_information <- nat_amenity_tourism_dependency %>% 
  full_join(county_census_information, by='county_geoid') %>% 
  full_join(average_background_pm_by_county, by='county_geoid') %>% 
  full_join(cbp_naics_code_distribution_county, by='county_geoid') %>%
  full_join(carr_county_rurality_index, by='county_geoid') %>%
  mutate(state_fips=str_sub(county_geoid, end=2)) %>% 
  full_join(bea_natural_amenity_tourism_state, by='state_fips') %>% 
  select(-state_fips) %>% 
  filter(!is.na(county_geoid), !(str_sub(county_geoid, end=2) %in% non_CONUS_FIPS_codes))

###### Turn the continuous county characteristics into quartiles ######
# The characteristics are county-level and time invariant, so the cutoffs come straight
# from the county file rather than from the county-day panel: each quartile then holds
# roughly a quarter of the counties instead of a quarter of the county-days. Counties
# sitting exactly on a cutoff fall in the lower quartile. When a characteristic has a mass
# point big enough that two cutoffs coincide (the outdoor shares are zero for more than
# half of the counties), the tied quartiles are merged into one group that keeps the label
# of the lowest quartile it covers, so e.g. only Q3 and Q4 remain alongside the baseline.

assign_quartiles <- function(values) {
  breaks <- c(-Inf, quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE), Inf)
  non_empty <- breaks[-1] > breaks[-length(breaks)]
  cut(values, breaks = unique(breaks), labels = paste0("Q", which(non_empty)))
}

for (column in names(HETEROGENEITY_LABELS)) {
  label <- HETEROGENEITY_LABELS[[column]]
  county_quartiles <- assign_quartiles(county_information[[column]])
  quartile_levels <- levels(county_quartiles)
  
  ## Uncomment the following for information about the quartiles for each heterogeneity value
  cat("\n", column, " quartile cutoffs (25th/50th/75th across counties): ",
      paste(signif(quantile(county_information[[column]], c(0.25, 0.5, 0.75), na.rm = TRUE), 4),
            collapse = ", "),
      "\n", sep = "")
  cat("Counties per quartile:\n")
  print(table(county_quartiles, useNA = "ifany"))
  if (length(quartile_levels) < 4) {
    cat("NOTE:", column, "has tied quartile cutoffs; the tied quartiles were merged into the",
        quartile_levels[1], "baseline group.\n")
  }

  # One dummy per non-baseline quartile, and the quartile itself so it can be crossed with
  # the day fixed effect. Counties with a missing characteristic get NA and are dropped
  # from that regression, exactly as they were under the continuous specification.
  for (quartile in quartile_levels[-1]) {
    county_information[[paste0(label, "_", quartile)]] <- as.integer(county_quartiles == quartile)
  }
  county_information[[paste0(label, "_q")]] <- as.character(county_quartiles)
}

write_csv(county_information, COUNTY_INFO_COMBINED_PATH)




