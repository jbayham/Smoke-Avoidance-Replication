################################################################################
# Aggregates the BEA Outdoor Recreation Satellite Account (ORSA) to the state   #
# level for 2018 and writes the two state-level share files consumed by         #
# merge_county_information.R and the day-of analysis scripts:                   #
#                                                                              #
#   1. bea_percent_state_comp_gdp_from_rec_2018.csv                            #
#        share of each state's GDP and compensation coming from outdoor rec,    #
#        plus the overall 25th/50th/75th percentile cutoffs of those shares.    #
#   2. bea_percent_state_gdp_from_rec_by_activity_2018.csv                     #
#        share of each state's GDP coming from each outdoor rec activity.       #
#                                                                              #
# BEA publishes ORSA in thousands of dollars and SAGDP1 in millions, so the     #
# SAGDP1 totals are scaled to thousands before any share is computed.           #
################################################################################

library(tidyverse)
library(janitor)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# BEA table line codes ---------------------------------------------------------
SAGDP1_GDP_LINE_CODE          <- 3   # Current-dollar GDP (millions of current dollars)
SAGDP1_COMPENSATION_LINE_CODE <- 4   # Compensation (millions of dollars)
ORSA_ALL_INDUSTRIES_LINE_CODE <- 1   # "All industries" total in the by-NAICS tables

MILLIONS_TO_THOUSANDS <- 1000

# Activity line codes carried into the by-activity output. This is a curated
# subset of the 46 codes in SAOACTVA, not a rule-based one: it takes the
# detailed conventional activities (4-24, with 22/23 standing in for their
# parent 21), the two non-conventional categories retained from "Other outdoor
# recreation" (34, 35), and supporting outdoor recreation (36) with its trip
# and travel components (38, 39). Aggregate parents (1, 2, 3, 21, 25) are
# dropped so the retained columns do not double count.
ORSA_ACTIVITY_LINE_CODES <- c(4, 5, 10, 11, 12, 15, 16, 17, 18, 20,
                              22, 23, 24, 34, 35, 36, 38, 39)

# BEA regional aggregates (91000-98000) have no ORSA counterpart and the US
# total is not a state, so shares are only ever defined for the 50 states + DC.
US_TOTAL_GEO_FIPS <- '00000'

#' Read one of BEA's downloaded CSVs.
#'
#' Each file carries three title lines above the header and a Legend/Footnotes
#' block below the data. Suppressed cells are marked "(D)" and become NA.
read_bea_table <- function(path, value_column = '2018') {
  read_csv(path, skip = 3, col_types = cols(.default = col_character())) %>%
    filter(str_detect(GeoFips, '^\\d{5}$')) %>%
    transmute(geo_fips    = GeoFips,
              geo_name    = GeoName,
              line_code   = as.integer(LineCode),
              description = str_squish(Description),
              value       = suppressWarnings(as.numeric(.data[[value_column]])))
}

# State GDP and compensation totals (SAGDP1) -----------------------------------
state_totals <- read_bea_table(BEA_OVERALL_GDP_COMPENSATION_PATH) %>%
  filter(line_code %in% c(SAGDP1_GDP_LINE_CODE, SAGDP1_COMPENSATION_LINE_CODE),
         geo_fips != US_TOTAL_GEO_FIPS) %>%
  mutate(measure = if_else(line_code == SAGDP1_GDP_LINE_CODE, 'total_gdp', 'total_comp'),
         value   = value * MILLIONS_TO_THOUSANDS) %>%
  select(geo_fips, geo_name, measure, value) %>%
  pivot_wider(names_from = measure, values_from = value)

# Outdoor recreation totals (SAOCOMP / SAOVA) ----------------------------------
read_orsa_all_industries <- function(path, column_name) {
  read_bea_table(path) %>%
    filter(line_code == ORSA_ALL_INDUSTRIES_LINE_CODE) %>%
    select(geo_fips, {{ column_name }} := value)
}

outdoor_rec_compensation <- read_orsa_all_industries(BEA_OUTDOOR_REC_COMPENSATION_BY_NAICS_PATH, rec_comp)
outdoor_rec_value_added  <- read_orsa_all_industries(BEA_OUTDOOR_REC_VALUE_ADDED_BY_NAICS_PATH, rec_gdp)

# Output 1: state GDP and compensation shares ----------------------------------
state_rec_shares <- state_totals %>%
  full_join(outdoor_rec_compensation, by = 'geo_fips') %>%
  full_join(outdoor_rec_value_added,  by = 'geo_fips') %>%
  mutate(percent_gdp_from_rec  = rec_gdp / total_gdp,
         percent_comp_from_rec = rec_comp / total_comp)


write_csv(state_rec_shares, BEA_PERCENT_COMP_GDP_FROM_OUTDOOR_REC_PATH)

# Output 2: state GDP shares by outdoor recreation activity ---------------------
outdoor_rec_by_activity <- read_bea_table(BEA_OUTDOOR_REC_GDP_BY_ACTIVITY_PATH) %>%
  filter(line_code %in% ORSA_ACTIVITY_LINE_CODES)

# Clean the column names off the distinct descriptions rather than the full
# column: make_clean_names() enforces uniqueness, so handing it one label per
# row would suffix every repeat (bicycling, bicycling_2, ...). Ordering the
# lookup by line code makes `activity` a factor whose level order pivot_wider
# then uses for the output column order.
activity_labels <- outdoor_rec_by_activity %>%
  distinct(line_code, description) %>%
  arrange(line_code) %>%
  mutate(activity = as_factor(make_clean_names(description))) %>%
  select(line_code, activity)

state_rec_shares_by_activity <- outdoor_rec_by_activity %>%
  left_join(activity_labels, by = 'line_code') %>%
  inner_join(state_totals, by = c('geo_fips', 'geo_name')) %>%
  mutate(percent_gdp_from_activity = value / total_gdp) %>%
  select(geo_fips, geo_name, activity, percent_gdp_from_activity, total_gdp) %>%
  pivot_wider(names_from = activity, values_from = percent_gdp_from_activity) %>%
  relocate(total_gdp, .after = last_col())

write_csv(state_rec_shares_by_activity, BEA_PERCENT_GDP_FROM_OUTDOOR_REC_BY_ACTIVITY_PATH)
