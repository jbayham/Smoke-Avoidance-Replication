# Evaluate the actual manuscript join expression with both execution methods.
suppressPackageStartupMessages({library(arrow);library(tidyverse);library(data.table);library(conflicted)})
conflicts_prefer(dplyr::filter,lubridate::year,lubridate::isoweek,lubridate::month,lubridate::wday,lubridate::week)
root <- Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd())
expressions <- parse(file.path(root,'1_build/reg_data/build_reg_data_daily.R'))
joins <- Filter(function(x) is.call(x) && identical(x[[1]],as.name('<-')) &&
  identical(x[[2]],as.name('full_df')) && grepl('full_join',paste(deparse(x[[3]]),collapse=' '),fixed=TRUE),expressions)
stopifnot(length(joins)==1L)
keys <- tibble(county_geoid=c('01001','01001','01003','01003'),date=as.Date('2019-01-01')+c(0,1,0,1))
inputs <- list(daily_visits=mutate(keys[c(1,2,3),],visits=c(1,2,3)),
 daily_spend=mutate(keys[c(1,3,4),],spend=c(4,5,6)),
 daily_weather=mutate(keys,weather=c(7,8,9,10),tmmn=c(270,275,280,285),tmmx=c(280,285,290,295)),
 daily_smoke=mutate(keys[c(2,3),],smoke=c(11,12)),
 daily_fire=mutate(keys[c(1,4),],fire=c(13,14)),
 county_info=tibble(county_geoid=c('01001','01003')))
validation <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('<-')) &&
  identical(x[[2]], as.name('materialize_daily_source')), expressions)
stopifnot(length(validation) == 1L)
eval(validation[[1]])
stopifnot(isTRUE(all.equal(materialize_daily_source(arrow::Table$create(inputs$daily_visits), 'fixture'),
                          inputs$daily_visits, tolerance = 0)))
bad <- bind_rows(inputs$daily_visits, inputs$daily_visits[1, ])
stopifnot(inherits(try(materialize_daily_source(bad, 'duplicate fixture'), silent = TRUE), 'try-error'))
bad <- inputs$daily_visits; bad$date[1] <- NA
stopifnot(inherits(try(materialize_daily_source(bad, 'missing date fixture'), silent = TRUE), 'try-error'))
cat('PASS: materialization preserves valid values and rejects duplicate or missing date keys.\n')
execute <- function(use_arrow) {
 e <- list2env(inputs,parent=globalenv())
 if(use_arrow) for(n in c('daily_visits','daily_spend','daily_weather','daily_smoke'))
   assign(n,arrow::Table$create(inputs[[n]]),envir=e)
 eval(joins[[1]],envir=e)
 collect(e$full_df) %>% arrange(county_geoid,date)
}
original <- execute(TRUE)
materialized <- execute(FALSE)
stopifnot(nrow(materialized)==4L,isTRUE(all.equal(original,materialized,tolerance=0)))
cat('PASS: original join expression agrees across Arrow and materialized inputs, including unmatched keys and missing outcomes.\n')

transforms <- Filter(function(x) is.call(x) && identical(x[[1]],as.name('<-')) &&
  identical(x[[2]],as.name('full_df')) && grepl('state_year_month',paste(deparse(x[[3]]),collapse=' '),fixed=TRUE),expressions)
stopifnot(length(transforms)==1L)
e <- list2env(list(full_df=materialized, non_CONUS_FIPS_codes=character(),NON_US_COUNTY_GEOIDS=character(),INCOMPLETE_COUNTY_GEOIDS=character()),parent=globalenv())
eval(transforms[[1]],envir=e)
stopifnot(nrow(e$full_df)==4L, all(e$full_df$state_year_month=='0120191'),
          all(e$full_df$balanced_panel),
          isTRUE(all.equal(e$full_df$tmmn,(materialized$tmmn-273.15)*9/5+32)))
cat('PASS: actual calendar/temperature expression evaluates with both data.table and conflicted attached.\n')

# Exercise the remaining actual feature-building expressions on bounded input.
e$full_df <- mutate(e$full_df,pr=c(0,1,2,3),rmax=c(40,50,60,70),
 daily_visits_deferrable=c(1,2,0,NA_real_),adj_pm_smoke_med_1sd=c(0,16,70,220),
 area_weighted_pm=c(5,20,80,230),viirs_total=c(0,1,NA,5),viirs_in_perimeter=c(0,0,NA,2))
e$FIRE_THRESHOLD_LIST <- c(0,1)
e$BIN_LABELS <- setNames(rep('',6),c('bin_d_001_015','bin_d_015_030','bin_d_030_060','bin_d_060_100','bin_d_100_200','bin_d_200_500'))
start <- which(vapply(expressions,function(x) is.call(x) && identical(x[[1]],as.name('<-')) && identical(x[[2]],as.name('quantile_cuts')),logical(1)))
stopifnot(length(start)==1L)
for(x in expressions[start:length(expressions)]) {
 if(is.call(x) && identical(x[[1]],as.name('write_csv'))) next
 eval(x,envir=e)
}
stopifnot(nrow(e$full_df)==4L,e$full_df$bin_d_015_030[2]==1L,
 e$full_df$bin_d_060_100[3]==1L,e$full_df$bin_d_200_500[4]==1L,
 e$full_df$asinh_daily_visits_deferrable[3]==0,is.na(e$full_df$asinh_daily_visits_deferrable[4]))
cat('PASS: original weather-bin, exposure-bin, outcome and fire-feature expressions complete with expected fixture values.\n')
