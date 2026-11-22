library(tidyverse)
library(glue)
library(ncdf4)
library(tigris)
library(sf)
library(pracma)
library(reshape2)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR'), 'project_helpers_and_constants.R'))

options("scipen"=100, "digits"=4)


get_county_grid_weights <- function(){
  # Start by pulling in the grid of the smoke data.
  # 4326 is the World Geodetic System 1984, with lat lon
  # 5070 is the USA Contiguous Albers Equal Area Conic projection
  us_grid <- st_read(PIERCE_PM_GRID_SHAPEFILE_PATH) %>%
    st_set_crs("EPSG:4326") %>% 
    st_transform("EPSG:5070") %>% 
    mutate(ROWY=ROWY+1, # increasing by one to match R's dimensions (which start at 1 instead of 0)
           COLX=COLX+1)
  
  # Now pull in US county shapefiles, removing US colonies
  us_colonies <- c("GU", "VI", "MP", "PR", "AS")
  colony_codes <- (package_states() %>%
                     filter(STUSPS %in% us_colonies) %>%
                     st_set_geometry(NULL))$STATEFP
  
  
  us_counties <- package_counties(year=2020) %>% 
    st_transform("EPSG:5070") %>% 
    filter(!STATEFP %in% colony_codes)
  
  
  # Calculate the area of the county and the area of the part of the grid that 
  # is in the county. The ratio is the weight of that grid cell.
  county_grid_weights <- us_counties %>% 
    mutate(county_area=st_area(.)) %>% 
    st_intersection(us_grid) %>% 
    mutate(intersect_area = st_area(.),
           weight=as.numeric(intersect_area/county_area)) %>% 
    st_drop_geometry() %>% 
    select(GRID_ID, COLX, ROWY, STATEFP, COUNTYFP, GEOID, NAME, weight) %>%
    # increasing by one to match R's dimensions (which start at 1 instead of 0) 
    # and are the default when you use the `melt` function to reshape arrays.
    mutate(ROWY=ROWY+1, COLX=COLX+1)
  
  county_grid_weights
}

county_grid_weights <- get_county_grid_weights()

for (data_processing_fn in c('median', 'mean')){
  for (yr in 2006:2023){
    annual_path <- glue('{PIERCE_PM_COUNTY_AGG_DIR}/{data_processing_fn}/county_smoke_pm_{yr}.csv')
    SURFACE_LEVEL_DATA_FILEPATH <- glue('{PIERCE_PM_KRIGED_NC_DIR}/krigedPM25_{yr}_v2_statfix_{data_processing_fn}bk.nc')
    
    smoke_nc <- nc_open(SURFACE_LEVEL_DATA_FILEPATH)
    # get PM2.5 and background PM2.5 data and day of year
    doy        <- ncvar_get(smoke_nc, "doy")
    pm_data    <- ncvar_get(smoke_nc, "PM25")
    pm_data_bg <- ncvar_get(smoke_nc, "Background_PM25")
    smoke_hms  <- ncvar_get(smoke_nc, "HMS_Smoke")
    # subtract background to get smoke attributable PM2.5
    pm_over_baseline <- pm_data - pm_data_bg
    # Set any negatives to 0
    
    all_pm         <- melt(pm_data,                      varnames = c("COLX", "ROWY", "doy"), value.name = "all_pm_value")
    smoke_long     <- melt(pm_over_baseline * smoke_hms, varnames = c("COLX", "ROWY", "doy"), value.name = "smoke_value")
    bg_long        <- melt(pm_data_bg,                   varnames = c("COLX", "ROWY", "doy"), value.name = "bg_value")
    smoke_hms_long <- melt(smoke_hms,                    varnames = c("COLX", "ROWY", "doy"), value.name = "hms_value")
    
    # Join both datasets with weights
    smoke_bg_joined <- smoke_long %>% 
      left_join(bg_long,        by = c("COLX", "ROWY", "doy")) %>%
      left_join(smoke_hms_long, by = c("COLX", "ROWY", "doy")) %>%
      left_join(all_pm,         by = c("COLX", "ROWY", "doy")) %>%
      inner_join(county_grid_weights, join_by(COLX==COLX, ROWY==ROWY), relationship = "many-to-many")
    
    # Aggregate by county-day
    county_smoke_by_day <- smoke_bg_joined %>%
      group_by(GEOID, doy) %>%
      summarize(
        area_weighted_pm       = sum(weight * all_pm_value, na.rm = TRUE),
        area_weighted_smoke    = sum(weight * smoke_value, na.rm = TRUE),
        area_weighted_bg       = sum(weight * bg_value, na.rm = TRUE),
        area_weighted_hms      = sum(weight * hms_value, na.rm = TRUE),
        any_hms                = max(hms_value, na.rm = TRUE),
        within_county_variance = var(smoke_value, na.rm = TRUE),
        across(c(GRID_ID, STATEFP, COUNTYFP, NAME), first)
      ) %>%
      ungroup() %>%
      mutate(date = as_date(str_c(yr, doy), format = '%Y%j'))
    
    write_csv(county_smoke_by_day, paste0(annual_path,'.tmp'))
    stopifnot(file.rename(paste0(annual_path,'.tmp'), annual_path))
    nc_close(smoke_nc)
  }
  
  # create an empty base dataframe
  base_dfm <- tibble() 
  
  for (yr in 2006:2023){
    dfm <- read_csv(glue('{PIERCE_PM_COUNTY_AGG_DIR}/{data_processing_fn}/county_smoke_pm_{yr}.csv'))
    base_dfm <- bind_rows(base_dfm, dfm)
  }
  write_csv(base_dfm, glue('{PIERCE_PM_COUNTY_AGG_DIR}/{data_processing_fn}/county_smoke_pm_all_time_{data_processing_fn}.csv'))
}




