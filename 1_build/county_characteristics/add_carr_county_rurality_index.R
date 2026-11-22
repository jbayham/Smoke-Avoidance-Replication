library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# The CARR (County Assessment of Rurality and Ruralness) index is a continuous county-level
# rurality measure built from population density, developed land, distance to metro areas and
# access to services. Higher values are more rural. We use the publicly released rescaled
# version, which puts the index on a common 0-1 style scale across counties.


carr_raw <- read_csv(
  CARR_RURALITY_RAW_PATH,
  col_types = cols(.default = "c")  # geoid comes through unpadded, so read everything as text
)

carr <- carr_raw %>%
  mutate(
    county_geoid = str_pad(geoid, width = 5, side = "left", pad = "0"),
    rurality_index = suppressWarnings(as.numeric(CARR_countyrescale))
  ) %>%
  select(county_geoid, rurality_index)

print(summary(carr %>% select(rurality_index)))

write_csv(carr, CARR_COUNTY_RURALITY_INDEX_PATH)

