source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd()),'scripts/bootstrap.R'))
if(!requireNamespace('jsonlite',quietly=TRUE))stop('Run scripts/setup.R first.')
lock <- jsonlite::fromJSON(file.path(root,'renv.lock'),simplifyVector=FALSE)
issues <- character()
if(as.character(getRversion())!=lock$R$Version)issues <- c(issues,paste('R:',getRversion(),'expected',lock$R$Version))
for(x in lock$Packages) {
 if(!nzchar(find.package(x$Package,quiet=TRUE)))issues <- c(issues,paste('Missing:',x$Package))
 else if(packageVersion(x$Package)!=package_version(x$Version))issues <- c(issues,paste(x$Package,packageVersion(x$Package),'expected',x$Version))
}
if(length(issues))stop(paste(issues,collapse='\n'))
# Exercise compiled dependencies, including Arrow's dataset support.
f <- tempfile(fileext='.parquet')
arrow::write_parquet(data.frame(id=c('01001','01003'),x=c(1,NA_real_)),f)
stopifnot(nrow(as.data.frame(arrow::open_dataset(f)))==2L);unlink(f)
print(sf::sf_extSoftVersion());print(arrow::arrow_info());print(sessionInfo())
cat('PASS: R and all locked package versions; Parquet round trip.\n')
