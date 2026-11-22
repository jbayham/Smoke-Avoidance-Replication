library(ncdf4)
library(tigris)
library(tidyr)
library(conflicted)
library(future)
library(furrr)
library(arrow)
library(data.table)
library(glue)
conflicts_prefer(dplyr::filter)

# Get weather

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR'), 'project_helpers_and_constants.R'))
source(WEATHER_DATA_HELPERS_PATH)


filter.years <- c(2019, 2020, 2021, 2022, 2023)
##################################

folder.names <- c('pr', 'tmmn', 'tmmx', 'rmax')

# Inputs are checked by run.py; no downloads during reproduction.

##################################

#All gridmet files are on the same grid of lat and lons so grabbing one
file.names <- list.files(WEATHER_INPUTS_DIR,recursive = T,pattern = ".nc",full.names = T)

#Open the connection to the netCDF file
nc <- nc_open(file.names[1])


#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

#Use the lat and lon vectors to create a grid represented as two vectors (note:
#lon must go first to match with netcdf data)
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>% 
  mutate(lon = round(lon,5),
         lat = round(lat,5),
         cells=row_number())


##############################################
#Use GIS tools to aggregate data by chosen geography
##############################################
#Choose a projection to be used by all geographic files
readin.proj=4326 #because it works with the lat and lons provided

#Converting nc coordinates from vector form to simple feature (sf)
g.nc.coords <- st_as_sf(nc.coords, coords = c("lon","lat")) %>% 
  st_set_crs(readin.proj)

#ensure that county polygons are also in 4326
us_co <- package_counties(year=2020)
names(us_co) <- tolower(names(us_co))
us_co <- st_transform(us_co,4326)

#Attaching geographic data to netcdf grid -- ignore warning
bridge.county <- st_join(g.nc.coords,us_co,left=T) %>%
  dplyr::select(county=geoid,cells) %>%
  sfc_as_cols(.,names = c("lon","lat")) %>%
  st_set_geometry(NULL)

# counties too small to contain a gridMET centroid (VA independent cities)
missing_co <- us_co %>%
  filter(!geoid %in% bridge.county$county,
         !statefp %in% c("02","15","60","66","69","72","78"))  # gridMET is CONUS-only

if (nrow(missing_co) > 0) {
  nn <- st_nearest_feature(st_centroid(st_geometry(missing_co)), g.nc.coords)
  bridge.county <- bind_rows(
    bridge.county,
    tibble(county = missing_co$geoid,
           cells  = g.nc.coords$cells[nn],
           lon    = st_coordinates(g.nc.coords[nn, ])[, 1],
           lat    = st_coordinates(g.nc.coords[nn, ])[, 2])
  )
}

#######################

file.list <- expand.grid(folder.names,filter.years,stringsAsFactors = F) %>% 
  rename(var=Var1,year=Var2) %>%
  mutate(var=str_to_lower(var),
         file.name = str_c(var,"_",year,".nc")) %>% 
  arrange(var) 

file.list <- file.path(WEATHER_INPUTS_DIR, file.list$var, file.list$file.name)


fy = file.list[1]

#Begin loop over variables (folders)
plan(sequential) # reproduction: bound memory; same per-file computation
future_walk(
  file.list,
  function(fy){
    message(str_c("Beginning ",fy,"..."))
    
    vname=basename(dirname(fy))
    
    #Construct the dataframe with all days and cells
    nc <- nc_open(fy)
    var.id=names(nc$var)
    date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
    
    nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
    
    nc.data.df <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
      as_tibble(.name_repair = "universal") %>%
      rename_all(~str_c(date.vector))
    
    nc_lat <- ncvar_get(nc = nc, varid = "lat")
    nc_lon <- ncvar_get(nc = nc, varid = "lon")
    nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>% 
      mutate(lon = round(lon,5),
             lat = round(lat,5),
             cells=row_number())
    
    nc.df <- bind_cols(nc.coords,nc.data.df)
    
    #Join gridmet data with bridge 
    var.block <- inner_join(bridge.county %>% select(county,cells),
                            nc.df %>% select(-c(lat,lon)),
                            by=c("cells")) %>%
      drop_na(county) %>%
      select(-c(cells)) %>%
      as.data.table() %>% 
      data.table::melt(., id.vars = c("county"),
           variable.name = "date",
           value.name = "value",
           variable.factor=FALSE) 
    
    out <- var.block[,.(value=base::mean(value,na.rm=T)),by=.(county,date)][,`:=`(var=vname)]
    
    dir_name = file.path(WEATHER_INTERMEDIATE_OUTPUTS_DIR, vname)
    if(!dir.exists(dir_name)) dir.create(dir_name)
    write_csv(out, file.path(dir_name, paste0(vname,"_",str_sub(fy,-7,-4),".csv.gz")))

  },.progress = T)

for (year in filter.years){
  pr <- read_csv(file.path(WEATHER_INTERMEDIATE_OUTPUTS_DIR,   glue('pr/pr_{year}.csv.gz'))) %>% rename(pr=value) %>% select(-var)
  tmmx <- read_csv(file.path(WEATHER_INTERMEDIATE_OUTPUTS_DIR, glue('tmmx/tmmx_{year}.csv.gz'))) %>% rename(tmmx=value) %>% select(-var)
  tmmn <- read_csv(file.path(WEATHER_INTERMEDIATE_OUTPUTS_DIR, glue('tmmn/tmmn_{year}.csv.gz'))) %>% rename(tmmn=value) %>% select(-var)
  rmax <- read_csv(file.path(WEATHER_INTERMEDIATE_OUTPUTS_DIR, glue('rmax/rmax_{year}.csv.gz'))) %>% rename(rmax=value) %>% select(-var)
  weather <- pr %>% 
    full_join(tmmx, by=c('county', 'date')) %>%
    full_join(tmmn, by=c('county', 'date')) %>%
    full_join(rmax, by=c('county', 'date')) %>%
    rename(county_fips=county) %>%
    mutate(fips=as.numeric(county_fips))
  
  write_csv(weather, file.path(WEATHER_FINAL_OUTPUTS_DIR, glue('county_weather_{year}.csv.gz')))
  
}


