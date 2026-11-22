# Package-local paths. The runner sets the project root; direct use defaults to cwd.
library(glue)
CODE_DIR <- Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset=getwd())
CODE_DIR <- normalizePath(CODE_DIR, winslash='/', mustWork=TRUE)
DATA_DIR <- Sys.getenv('SMOKE_AVOIDANCE_DATA_DIR', unset=file.path(CODE_DIR, 'data'))
OUTPUT_DIR <- Sys.getenv('SMOKE_AVOIDANCE_OUTPUT_DIR', unset=file.path(CODE_DIR, 'output'))
package_path <- function(p) {
  if (startsWith(p, 'data/')) return(file.path(DATA_DIR, substring(p,6)))
  if (startsWith(p, 'output/')) return(file.path(OUTPUT_DIR, substring(p,8)))
  file.path(CODE_DIR,p)
}
.package_paths <- jsonlite::fromJSON(file.path(CODE_DIR,'config/paths.json'))
for (.key in names(.package_paths)) assign(.key, package_path(.package_paths[[.key]]))
# Only explicit delivery overrides; never fall back to another researcher's server.
for (.key in c('PLACE_DIR','DEWEY_SPEND_FILES_DIR','DEWEY_MONTHLY_PATTERNS_FILES_DIR')) {
  .val <- Sys.getenv(.key, unset='')
  if(nzchar(.val)) assign(.key,.val)
}
DAILY_WEATHER_2019_2023_PATHS <- file.path(WEATHER_FINAL_OUTPUTS_DIR,paste0('county_weather_',2019:2023,'.csv.gz'))
FIRE_THRESHOLD_LIST <- c(0,5,10,25,50,100,150,250)
# Offline geographic inputs: the accepted smoke/weather builds used 2020,
# while model county characteristics and spatial weights use 2019.
package_counties <- function(year=2020, cb=FALSE, ...) {
  name <- if(cb) paste0('cb_',year,'_us_county_500k.shp') else paste0('tl_',year,'_us_county.shp')
  sf::st_read(file.path(DATA_DIR,'raw/geography',name),quiet=TRUE)
}
package_states <- function(year=2020, ...) {
  sf::st_read(file.path(DATA_DIR,'raw/geography',paste0('tl_',year,'_us_state.shp')),quiet=TRUE)
}
get_non_US_or_CONUS_state_fips_codes <- function(filter_for_CONUS=FALSE) {
  if(filter_for_CONUS) c('02','15','60','66','69','72','78') else c('60','66','69','72','78')
}
get_county_shapefiles <- function(filter_for_CONUS=FALSE,low_resolution=FALSE) {
  x <- sf::st_transform(package_counties(year=2019,cb=low_resolution),5070)
  x[!x$STATEFP %in% get_non_US_or_CONUS_state_fips_codes(filter_for_CONUS),]
}
BIN_LABELS <- c('bin_d_001_015' = "(0, 15]",    'bin_d_015_030' = "(15, 30]",
                'bin_d_030_060' = "(30, 60]",   'bin_d_060_100' = "(60, 100]",
                'bin_d_100_200' = "(100, 200]", 'bin_d_200_500' = "(200, 500]")

BIN_COLORS <- setNames(c('forestgreen', 'gold', 'orange', 'red', 'purple', 'maroon'),
                       unname(BIN_LABELS))

BIN_AQI_LABELS <- c("Good", "Moderate", "Unhealthy for \nSensitive Groups",
                    "Unhealthy", "Very Unhealthy", "Hazardous")

######## Constants ############
STATE_CENSUS_DIVISIONS_MAP <- c(
  # New England
  "09" = "New Eng.",     "23" = "New Eng.",     "25" = "New Eng.",
  "33" = "New Eng.",     "44" = "New Eng.",     "50" = "New Eng.",
  # Middle Atlantic
  "34" = "Mid Atl.",     "36" = "Mid Atl.",     "42" = "Mid Atl.",
  # East North Central
  "17" = "EN Central",   "18" = "EN Central",   "26" = "EN Central",
  "39" = "EN Central",   "55" = "EN Central",
  # West North Central
  "19" = "WN Central",   "20" = "WN Central",   "27" = "WN Central",
  "29" = "WN Central",   "31" = "WN Central",   "38" = "WN Central",
  "46" = "WN Central",
  # South Atlantic
  "10" = "S Atlantic",   "12" = "S Atlantic",   "13" = "S Atlantic",
  "24" = "S Atlantic",   "37" = "S Atlantic",   "45" = "S Atlantic",
  "51" = "S Atlantic",   "54" = "S Atlantic",   "11" = "S Atlantic",
  # East South Central
  "01" = "ES Central",   "21" = "ES Central",   "28" = "ES Central",
  "47" = "ES Central",
  # West South Central
  "05" = "WS Central",   "22" = "WS Central",   "40" = "WS Central",
  "48" = "WS Central",
  # Mountain
  "04" = "Mountain",     "08" = "Mountain",     "16" = "Mountain",
  "30" = "Mountain",     "32" = "Mountain",     "35" = "Mountain",
  "49" = "Mountain",     "56" = "Mountain",
  # Pacific
  "02" = "Pacific",      "06" = "Pacific",      "15" = "Pacific",
  "41" = "Pacific",      "53" = "Pacific"
)
STATE_CENSUS_DIVISIONS_MAP_LEVELS <- c("New Eng.", "Mid Atl.", "EN Central", "WN Central",
                     "S Atlantic", "ES Central", "WS Central", "Mountain", "Pacific")

# The continuous county characteristics that get split into quartiles in
# 1_build/county_characteristics/merge_county_information.R. Each name is the column in the
# county file, and each value is the short label used for the quartile columns, the
# coefficient names, and the saved regression results.
HETEROGENEITY_LABELS <- c(
  pm_background_mean              = 'background_pm',
  median_household_income         = 'income',
  natural_amenity_tourism_measure = 'nat_amenity',
  percent_gdp_from_rec            = 'gdp_rec',
  percent_comp_from_rec           = 'comp_rec',
  cbp_outdoor_share_estab         = 'cbp_outdoor_estab',
  cbp_outdoor_share_payroll       = 'cbp_outdoor_payroll',
  cbp_hhi_estab                   = 'cbp_hhi_estab',
  cbp_hhi_payroll                 = 'cbp_hhi_payroll',
  rurality_index                  = 'rurality'
)




