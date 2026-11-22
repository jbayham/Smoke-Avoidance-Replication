# Reconstruct Appendix A.2 from the smoke and daily panels.
# Invoked by run.py; no historical run directory or manual cache is required.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
setDTthreads(4)
source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset=getwd()), 'project_helpers_and_constants.R'))
args <- character()
out <- file.path(OUTPUT_DIR,'diagnostics/aqi')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
smoke_path <- COUNTY_DAILY_SMOKE_WITH_FILTERS_PATH
panel_path <- DAILY_DATA_FILEPATH
cols <- c('county_geoid','date','year','aqi','defining_parameter','area_weighted_pm',
          'area_weighted_smoke_avg','area_weighted_smoke_med','area_weighted_bg_avg',
          'area_weighted_bg_med','any_hms','aqi_from_smoke_pm_mean','aqi_from_smoke_pm_median',
          'adj_pm_smoke_med_1sd','adj_pm_smoke_avg_1sd','bg_pm_sd')
if (length(args) == 3L) {
  stopifnot(args[3] == '--cached-inputs')
  x <- readRDS(file.path(out, 'plot_inputs.rds'))
} else {
  x <- fread(smoke_path, select = cols, colClasses = c(county_geoid='character'))
  panel <- fread(panel_path, select=c('county_geoid','date','balanced_panel'),
                 colClasses=c(county_geoid='character'))
  stopifnot(!anyDuplicated(panel[,.(county_geoid,date)]))
  x[,balanced_panel:=FALSE]
  x[panel, on=.(county_geoid,date), balanced_panel:=i.balanced_panel]
  saveRDS(x, file.path(out, 'plot_inputs.rds'), compress=FALSE)
}
stopifnot(all(c(cols,'balanced_panel') %in% names(x)),
          !anyDuplicated(x[,.(county_geoid,date)]), all(x$year %in% 2019:2023))

# Mirror the existing exposure builder exactly, including its documented first
# breakpoint mismatch and missing values above the top interval. Used only for
# plotting the 1-SD sensitivity, not to change the stored analysis data.
code_aqi <- function(pm, first_endpoint=9) {
  c <- floor(10*pm)/10
  low <- c(0,12.1,35.5,55.5,150.5,250.5,350.5)
  high <- c(first_endpoint,35.4,55.4,150.4,250.4,350.4,500.4)
  ilo <- c(0,51,101,151,201,301,401); ihi <- c(50,100,150,200,300,400,500)
  k <- findInterval(c, c(low,500.5))
  ans <- rep(NA_real_, length(c)); valid <- is.finite(c) & k>=1L & k<=7L
  j <- k[valid]; ans[valid] <- (c[valid]-low[j])/(high[j]-low[j])*(ihi[j]-ilo[j])+ilo[j]
  ans[is.finite(c)&c<0] <- 0
  round(ans)
}
stopifnot(isTRUE(all.equal(code_aqi(x$area_weighted_smoke_med),x$aqi_from_smoke_pm_median)))
x[,aqi_median_1sd := code_aqi(adj_pm_smoke_med_1sd)]
x[,aqi_median_first_endpoint_12 := code_aqi(area_weighted_smoke_med,12)]

specs <- data.table(
  id=c('median_all','mean_all','median_balanced','mean_balanced','median_1sd_all','median_1sd_balanced'),
  label=c('Median series | all matched days','Mean series | all matched days',
          'Median series | balanced panel','Mean series | balanced panel',
          'Median + 1 SD | all matched days','Median + 1 SD | balanced panel'),
  yvar=c('aqi_from_smoke_pm_median','aqi_from_smoke_pm_mean','aqi_from_smoke_pm_median',
         'aqi_from_smoke_pm_mean','aqi_median_1sd','aqi_median_1sd'),
  balanced=c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE))
summaries <- list(); plots <- list()
make_plot <- function(d, title=NULL, zoom=FALSE) {
  # Compress exact duplicate (integer AQI, integer smoke AQI) coordinates, not a
  # sample. Opacity is cumulative alpha for n overplotted points at alpha 0.10.
  # The coordinate/count export allows independent reconstruction of every mark.
  q <- d[,.(n=.N),by=.(aqi,y)]
  q[,opacity:=1-(1-0.10)^n]
  p <- ggplot(q,aes(aqi,y)) + geom_point(aes(alpha=opacity),shape=16,size=1.7) +
    scale_alpha_identity() + geom_abline(intercept=0,slope=1,color='red',linewidth=0.5) +
    labs(x='Overall AQI',y='AQI from smoke',title=title) +
    theme_minimal(base_size=12) + theme(panel.grid.minor=element_blank(),
                                       plot.margin=margin(10,20,10,10))
  if(zoom) p <- p + coord_cartesian(xlim=c(0,500),ylim=c(0,500),expand=FALSE)
  p
}
for (i in seq_len(nrow(specs))) {
  sp <- specs[i]
  eligible <- if (sp$balanced) x[balanced_panel==TRUE] else x
  d <- eligible[is.finite(aqi) & is.finite(get(sp$yvar)),.(county_geoid,date,aqi,y=get(sp$yvar))]
  summaries[[i]] <- data.table(id=sp$id,label=sp$label,eligible_rows=nrow(eligible),
    matched_rows=nrow(d),counties=uniqueN(d$county_geoid),max_aqi=max(d$aqi),max_smoke_aqi=max(d$y),
    n_overall_aqi_above_500=sum(d$aqi>500),n_positive_y=sum(d$y>0),
    n_positive_y_above_identity=sum(d$y>d$aqi))
  fwrite(d[,.(county_days=.N),by=.(aqi,smoke_aqi=y)],file.path(out,paste0(sp$id,'_coordinates.csv')))
  plots[[i]] <- make_plot(d,paste0(sp$label,'\nN = ',format(nrow(d),big.mark=',')))
  ggsave(file.path(out,paste0(sp$id,'.png')),make_plot(d),width=9,height=6,dpi=180)
  if (sp$id=='median_balanced') {
    chosen <- d
    ggsave(file.path(out,'median_balanced_zoom.png'),make_plot(d,zoom=TRUE),width=7,height=6,dpi=180)
  }
}
fwrite(rbindlist(summaries),file.path(out,'candidate_samples.csv'))
pdf(file.path(out,'candidate_series_comparison.pdf'),width=14,height=15)
grid::grid.newpage();grid::pushViewport(grid::viewport(layout=grid::grid.layout(3,2)))
for(i in seq_along(plots)) print(plots[[i]],newpage=FALSE,vp=grid::viewport(layout.pos.row=ceiling(i/2),layout.pos.col=(i-1)%%2+1))
dev.off()

# Thresholds are descriptive diagnostics, not shares of pollution attributable to
# smoke: county monitor-max, multi-pollutant AQI and county-area smoke PM differ.
stats <- list(); j <- 0
for (sample in c('all_matched','balanced_matched')) {
  d <- x[is.finite(aqi)&is.finite(area_weighted_smoke_med)]
  if(sample=='balanced_matched')d<-d[balanced_panel==TRUE]
  for(threshold in c(100,150)) {
    h <- d[aqi>threshold]
    for(smoke_limit in c(15,30)) {
      low <- h[area_weighted_smoke_med>0 & area_weighted_smoke_med<=smoke_limit]
      j <- j+1
      stats[[j]] <- data.table(sample=sample,aqi_threshold=threshold,smoke_limit=smoke_limit,
        n_matched=nrow(d),n_high_aqi=nrow(h),n_high_aqi_nonpositive_smoke=sum(h$area_weighted_smoke_med<=0),
        n_high_aqi_zero_smoke=sum(h$area_weighted_smoke_med==0),
        n_high_aqi_positive_low_smoke=nrow(low),
        pct_high_aqi_positive_low_smoke=100*nrow(low)/nrow(h),
        n_low_smoke_non_pm25=sum(low$defining_parameter!='PM2.5',na.rm=TRUE),
        n_low_smoke_pm25=sum(low$defining_parameter=='PM2.5',na.rm=TRUE))
    }
  }
}
fwrite(rbindlist(stats),file.path(out,'claim_support_counts.csv'))
d <- x[balanced_panel==TRUE & is.finite(aqi)&is.finite(area_weighted_smoke_med)]
fwrite(d[aqi>100,.(n=.N,zero_smoke=sum(area_weighted_smoke_med==0),
     positive_smoke_le15=sum(area_weighted_smoke_med>0&area_weighted_smoke_med<=15)),
     by=defining_parameter],file.path(out,'high_aqi_by_defining_pollutant.csv'))
fwrite(d[aqi>100 & area_weighted_smoke_med>0 & area_weighted_smoke_med<=15][order(-aqi)][1:30],
       file.path(out,'high_aqi_low_positive_smoke_examples.csv'))
fwrite(x[aqi>500],file.path(out,'aqi_above_500.csv'))
conversion <- x[balanced_panel==TRUE & is.finite(aqi_from_smoke_pm_median) &
                  is.finite(aqi_median_first_endpoint_12),
  .(n=.N,n_changed=sum(aqi_from_smoke_pm_median!=aqi_median_first_endpoint_12),
    max_absolute_difference=max(abs(aqi_from_smoke_pm_median-aqi_median_first_endpoint_12)))]
fwrite(conversion,file.path(out,'first_breakpoint_sensitivity.csv'))
fwrite(x[balanced_panel==TRUE & is.finite(aqi) & is.finite(area_weighted_smoke_med) &
           !is.finite(aqi_from_smoke_pm_median)],file.path(out,'matched_rows_missing_smoke_aqi.csv'))
alt <- x[balanced_panel==TRUE & is.finite(aqi)&is.finite(aqi_median_first_endpoint_12),
         .(aqi,y=aqi_median_first_endpoint_12)]
ggsave(file.path(out,'median_balanced_endpoint12.png'),make_plot(alt),width=9,height=6,dpi=180)
pdf(file.path(out,'median_diagnostic_full_and_zoom.pdf'),width=14,height=6)
grid::grid.newpage();grid::pushViewport(grid::viewport(layout=grid::grid.layout(1,2)))
print(make_plot(chosen,'(a) Full range: archived AQI values'),newpage=FALSE,vp=grid::viewport(layout.pos.col=1))
print(make_plot(chosen,'(b) Detail: both axes 0-500',TRUE),newpage=FALSE,vp=grid::viewport(layout.pos.col=2))
dev.off()
figure_dir <- APPENDIX_FIGURES_DIR
dir.create(figure_dir,recursive=TRUE,showWarnings=FALSE)
stopifnot(file.copy(file.path(out,'median_balanced.png'),
                   file.path(figure_dir,'smoke_non_smoke_AQI.png'),overwrite=TRUE))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
print(rbindlist(summaries));print(rbindlist(stats));print(conversion)
