library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

daily_sl_smoke <- read_csv(COUNTY_DAILY_SMOKE_PM_AVG_PATH) %>%
  group_by(doy) %>% 
  summarize(
    within_county_variance=mean(within_county_variance, na.rm=T),
    between_county_variance=var(area_weighted_smoke, na.rm=T)
  )



png(file.path(FIGURES_DIR, "appendix/figure_A3_within_and_between_variance.png"), width = 1200, height = 600)
par(mfrow = c(1, 2))  # 1 row, 2 columns

hist(daily_sl_smoke$within_county_variance, xlab="Mean Within County Variance", ylab="# Days", main="", cex.lab = 1.5, cex.axis = 1.6)

hist(daily_sl_smoke$between_county_variance, xlab="Mean Between County Variance", ylab="# Days", main="", cex.lab = 1.5, cex.axis = 1.6)

dev.off()

