args <- commandArgs(TRUE)
stopifnot(length(args)==1L)
root <- Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset=getwd())
source(file.path(root,'scripts/bootstrap.R'))
options(readr.num_threads=as.integer(Sys.getenv('SMOKE_THREADS','4')), timeout=600)
suppressPackageStartupMessages(library(tidyverse))
source(file.path(root,'project_helpers_and_constants.R'))
threads <- as.integer(Sys.getenv('SMOKE_THREADS','4'))
if(requireNamespace('data.table',quietly=TRUE))data.table::setDTthreads(threads)
if(requireNamespace('fixest',quietly=TRUE))fixest::setFixest_nthreads(threads)
for(d in c(REGRESSION_RESULTS_DIR,FIGURES_DIR,TABLES_DIR,APPENDIX_FIGURES_DIR,
           APPENDIX_TABLES_DIR,INTERMEDIATE_REG_DATA_DIR,FINAL_REG_DATA_DIR,
           file.path(PIERCE_PM_COUNTY_AGG_DIR,c('mean','median')),
           file.path(WEATHER_INTERMEDIATE_OUTPUTS_DIR,c('pr','tmmn','tmmx','rmax')),
           WEATHER_FINAL_OUTPUTS_DIR))dir.create(d,recursive=TRUE,showWarnings=FALSE)
checks <- Sys.getenv('REPRO_STAGE_CHECKS',file.path(OUTPUT_DIR,'checks','manual'))
dir.create(checks,recursive=TRUE,showWarnings=FALSE)
writeLines(capture.output(sessionInfo()),file.path(checks,'sessionInfo_start.txt'))
cat('START',format(Sys.time(),tz='UTC'),args[1],'\n')
source(args[1],local=.GlobalEnv)
if(basename(args[1])=='add_daily_total_spend_and_visits_by_county.R')build_county_daily_totals()
export_model <- function(x,label) {
  if(inherits(x,'fixest')) {
    ct <- as.data.frame(fixest::coeftable(x));ct$term <- rownames(ct);ct$n <- nobs(x)
    readr::write_csv(ct,file.path(checks,paste0(label,'_coefficients.csv')))
  }
}
for(nm in ls(.GlobalEnv)) {
  x <- get(nm,.GlobalEnv);export_model(x,nm)
  if(is.list(x)&&!is.data.frame(x)&&!inherits(x,'fixest')&&length(x)<=100)
    for(j in seq_along(x))export_model(x[[j]],paste0(nm,'_',j))
}
for(nm in intersect(c('welfare_per_bin','monthly_summary'),ls(.GlobalEnv)))
  readr::write_csv(get(nm),file.path(checks,paste0(nm,'.csv')))
writeLines(capture.output(sessionInfo()),file.path(checks,'sessionInfo_end.txt'))
cat('COMPLETE',format(Sys.time(),tz='UTC'),args[1],'\n')
