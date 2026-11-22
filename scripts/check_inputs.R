source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd()),'scripts/bootstrap.R'))
suppressPackageStartupMessages({library(jsonlite);library(arrow);library(ncdf4)})
source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd()),'project_helpers_and_constants.R'))
manifest <- fromJSON(file.path(CODE_DIR,'config/inputs.json'))
errors <- character()
for(i in seq_len(nrow(manifest))) {
 r <- manifest[i,];p <- package_path(r$path)
 cat('Checking',r$id,'\n');flush.console()
 tryCatch({
   if(r$id=='places') {
     files <- list.files(PLACE_DIR,pattern='\\.parquet$',recursive=TRUE,full.names=TRUE)
     cols <- tolower(names(open_dataset(PLACE_DIR,unify_schemas=TRUE)))
     stopifnot(all(strsplit(r$required_columns,',',fixed=TRUE)[[1]] %in% cols))
   } else if(grepl('^(visits|spend)_',r$id)) {
     for(f in head(Sys.glob(p),1L)) {
       cols <- tolower(names(open_dataset(f)))
       stopifnot(all(strsplit(r$required_columns,',',fixed=TRUE)[[1]] %in% cols))
     }
   } else if(endsWith(p,'.nc')) {
     nc <- nc_open(p)
     tryCatch({
       if(startsWith(r$id,'smoke'))stopifnot(all(c('doy','PM25','Background_PM25','HMS_Smoke') %in% c(names(nc$var),names(nc$dim))))
       else stopifnot(all(c('lon','lat','day') %in% names(nc$dim)))
     },finally=nc_close(nc))
   } else if(endsWith(p,'.csv')&&nzchar(r$required_columns)) {
     cols <- names(readr::read_csv(p,n_max=0,show_col_types=FALSE))
     stopifnot(all(strsplit(r$required_columns,',',fixed=TRUE)[[1]] %in% cols))
   }
 },error=function(e)errors<<-c(errors,paste(r$id,conditionMessage(e))))
}
if(length(errors))stop(paste(errors,collapse='\n'))
cat('PASS: required file patterns, representative monthly Parquet schemas, CSV headers and NetCDF variables. Every shard is checked again by its reader during the build.\n')
