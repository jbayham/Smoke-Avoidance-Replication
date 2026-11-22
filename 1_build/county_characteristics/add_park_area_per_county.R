library(sf)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR'), 'project_helpers_and_constants.R'))

us_counties <- get_county_shapefiles()


# Start by pulling in the park data.
# 5070 is the USA Contiguous Albers Equal Area Conic projection
us_parks <- st_read(USA_PARKS_SHAPEFILE_PATH) %>%
  st_transform("EPSG:5070") %>%
  mutate(park_id=row_number())

# Calculate the area of the county and the area of the part of the grid that 
# is in the county.

county_park_area <- tibble(county_geoid=NA, county_area=NA, park_area_in_county=NA, county_percent_area_park=NA)

for (county_id in 1:nrow(us_counties)) {
  if (county_id %% 100 == 1) cat("County",county_id,"of",nrow(us_counties),"\n")
  county <- us_counties[county_id,]
  intersecting_county_parks <- st_join(county, us_parks)
  park_subset <- us_parks %>% filter(park_id %in% intersecting_county_parks$park_id)
  if (nrow(park_subset) == 0L) {
    county_area <- as.numeric(st_area(county))
    county_park_area <- county_park_area %>% add_row(county_geoid=county$GEOID,
      county_area=county_area,park_area_in_county=0,county_percent_area_park=0)
    cat('Empty park set; zero area:',county$GEOID,'\n')
    next
  }
  park_geometry <- park_subset %>% summarize(geometry=st_union(geometry))
  park_area_in_county <- (st_intersection(park_geometry, county) %>% mutate(park_area=as.numeric(st_area(.))))$park_area
  county_area <- (county %>% mutate(area=as.numeric(st_area(.))))$area
  county_park_area <- county_park_area %>% add_row(county_geoid=county$GEOID, county_area=county_area, park_area_in_county=park_area_in_county, county_percent_area_park=park_area_in_county/county_area)
}

write_csv(county_park_area[complete.cases(county_park_area),], COUNTY_PARK_FRACTION_PATH)






