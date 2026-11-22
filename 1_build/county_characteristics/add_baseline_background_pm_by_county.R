library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

smoke_by_county_2018 <- read_csv(COUNTY_DAILY_SMOKE_PM_AVG_PATH) %>% 
  filter(year(date)==2018) %>% 
  mutate(county_geoid=str_pad(as.character(GEOID), width=5, side='left', pad='0')) %>% 
  rename(pm_background = area_weighted_bg) %>% 
  group_by(county_geoid) %>% 
  summarize(across(starts_with('pm'), mean, .names='{col}_mean'))


write_csv(smoke_by_county_2018, COUNTY_AVG_BACKGROUND_PM_2018_PATH)


