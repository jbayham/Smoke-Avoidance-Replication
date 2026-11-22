# Load required library
library(utils)
library(tidyverse)
library(glue)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# Read supplied snapshots once; never download or append a previous result.
total_aqi_flags <- tibble()
for (yr in 2006:2023){
  input_filepath <- glue('{AQI_RAW_DIR}/daily_aqi_by_county_{yr}.csv')
  year_aqi_df <- read_csv(input_filepath) %>%
    mutate(year=yr,
           aqi_boundary = case_when(
             # top 10% of lower category and bottom 10% of higher category
             (AQI > 45)  & (AQI <= 55)  ~ 'green_to_yellow',
             (AQI > 95)  & (AQI <= 105) ~ 'yellow_to_orange',
             (AQI > 145) & (AQI <= 155) ~ 'orange_to_red',
             (AQI > 195) & (AQI <= 205) ~ 'red_to_purple',
             (AQI > 290) & (AQI <= 310) ~ 'purple_to_maroon',
                                   TRUE ~ 'no_boundary'
           ))
  total_aqi_flags <- total_aqi_flags %>% bind_rows(year_aqi_df)
}

write_csv(total_aqi_flags, COUNTY_AQI_DATA_OUTPUT_PATH)


