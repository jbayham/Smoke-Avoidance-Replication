library(tigris)
library(tidyverse)
library(glue)
library(stringr)
library(sf)
library(jsonlite)
library(datawizard)
library(viridis)
options(tigris_use_cache = TRUE)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

frac_park_df             <- read_csv(COUNTY_PARK_FRACTION_PATH)
county_proportions_df    <- read_csv(COUNTY_TOURISM_EARN_EMP_PATH)

county_proportions_df <- county_proportions_df %>%
  mutate(higher_emp_or_earn_prop=if_else(both_emp_prop > both_earn_prop, both_emp_prop, both_earn_prop))


# The park fractions come out of a spatial intersection against USA_Parks.shp, which only
# emits a row where a park polygon actually overlaps a county: a county with no mapped
# parkland is absent from the file rather than present with zero park area, and indeed no
# row in it has county_percent_area_park == 0. Joining from the park side therefore dropped
# ~360 counties, concentrated in the plains and the interior south, whose amenity measure
# should be exactly zero -- the bottom of the distribution, so their loss pulled every
# quartile cutoff upward. Take the union of the two sources instead and let the coalesce
# below put the missing park fractions at zero. 
combined_amenity_and_tourism_df <- frac_park_df %>% 
  full_join(county_proportions_df %>% select(higher_emp_or_earn_prop, both_emp_prop, both_earn_prop, county_geoid), by="county_geoid") %>% 
  mutate(county_percent_area_park=coalesce(county_percent_area_park, 0),
         across(c(higher_emp_or_earn_prop, both_emp_prop, both_earn_prop), \(x) coalesce(x, 0)))


combined_amenity_and_tourism_df <- combined_amenity_and_tourism_df %>% 
  mutate(wind_emp = winsorize(both_emp_prop, threshold=0.01),
         wind_earn = winsorize(both_earn_prop, threshold=0.01),
         wind_park = winsorize(county_percent_area_park, threshold=0.01),
         
         norm_emp_w = wind_emp/(max(wind_emp, na.rm=T)),
         norm_earn_w = wind_earn/(max(wind_earn, na.rm=T)),
         norm_park_w = wind_park/max(wind_park, na.rm=T),
         higher_w_norm_emp_earn = if_else(norm_emp_w>norm_earn_w, norm_emp_w, norm_earn_w),
         comb_w_park_tourism = norm_park_w*higher_w_norm_emp_earn,
         
         norm_emp = both_emp_prop/(max(both_emp_prop, na.rm=T)),
         norm_earn = both_earn_prop/(max(both_earn_prop, na.rm=T)),
         higher_norm_emp_earn = if_else(norm_emp>norm_earn, norm_emp, norm_earn),
         comb_park_tourism = county_percent_area_park*higher_norm_emp_earn,
         percent_park_percentile = ecdf(county_percent_area_park)(county_percent_area_park),
         emp_percentile = ecdf(both_emp_prop)(both_emp_prop),
         earn_percentile = ecdf(both_earn_prop)(both_earn_prop),
         park_percentile = ecdf(county_percent_area_park)(county_percent_area_park),
         max_emp_percentile = if_else(emp_percentile>earn_percentile, emp_percentile, earn_percentile),
         percentile_product=max_emp_percentile*park_percentile,
         natural_amenity_tourism_measure=sqrt(sqrt(comb_park_tourism)),
         #normalize between 0-1
         natural_amenity_tourism_measure=natural_amenity_tourism_measure/max(natural_amenity_tourism_measure, na.rm=T))



df_to_output <- combined_amenity_and_tourism_df %>% 
  select(county_geoid, wind_emp, wind_earn, wind_park, natural_amenity_tourism_measure) %>% 
  rename(
    windsorized_tourism_employment_frac=wind_emp,
    windsorized_tourism_earnings_frac=wind_earn, 
    windsorized_park_area_frac=wind_park
  )

write_csv(df_to_output, NATURAL_AMENITY_DEPENDENCY_PATH)

