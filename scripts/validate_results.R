source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd()),'scripts/bootstrap.R'))
suppressPackageStartupMessages(library(tidyverse))
source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd()),'project_helpers_and_constants.R'))
out <- file.path(OUTPUT_DIR,'validation');dir.create(out,recursive=TRUE,showWarnings=FALSE)
ref <- read_csv(file.path(CODE_DIR,'reference/regression_comparisons.csv'),show_col_types=FALSE)
comparisons <- pmap_dfr(ref,function(exhibit,pdf_page,column,term,measure,paper,reproduced,difference,same_rounded_3dp,within_0_001,status,evidence,...) {
  # Benchmark evidence paths are relative to the configured output directory.
  f <- file.path(OUTPUT_DIR,evidence)
  if(!file.exists(f))stop('Missing independently estimated coefficients: ',f)
  ct <- read_csv(f,show_col_types=FALSE);row <- ct[ct$term==term,]
  if(nrow(row)!=1L)stop('Missing/duplicate coefficient: ',term,' in ',f)
  val <- row[[if(measure=='estimate')'Estimate' else 'Std. Error']]
  tibble(exhibit,column,term,measure,accepted=reproduced,actual=val,difference=val-reproduced,pass=is.finite(val)&abs(val-reproduced)<0.001)
})
write_csv(comparisons,file.path(out,'regression_comparisons.csv'))
ns <- read_csv(file.path(CODE_DIR,'reference/sample_size_comparisons.csv'),show_col_types=FALSE)
ns$actual_n <- map2_dbl(ns$exhibit,ns$column,function(ex,col) {
  row <- ref %>% filter(exhibit==ex,column==col) %>% slice(1)
  f <- file.path(OUTPUT_DIR,row$evidence)
  read_csv(f,show_col_types=FALSE)$n[1]
})
ns$pass <- ns$actual_n==ns$reproduced_n
write_csv(ns,file.path(out,'sample_sizes.csv'))
spend_ref <- read_csv(file.path(CODE_DIR,'reference/spending_estimates.csv'),show_col_types=FALSE)
spend <- read_csv(SPEND_ON_VISITS_SPLIT_SAMPLE_RESULTS_PATH,show_col_types=FALSE)
# Compare using all shared identifying columns rather than assuming row order.
keys <- setdiff(intersect(names(spend_ref),names(spend)),c('Estimate','Std. Error','t value','Pr(>|t|)','n'))
# Actual export uses model/sample columns; discover fixed known names below.
keys <- intersect(c('fixed_effects','sample','model','term','vars','estimator'),keys)
if(!length(keys))stop('Unrecognized spending coefficient schema; inspect exports.')
z <- inner_join(spend_ref,spend,by=keys,suffix=c('_ref','_actual'),relationship='one-to-one')
if(nrow(z)!=nrow(spend_ref))stop('Spending estimate coverage differs.')
value_names <- intersect(c('estimate','se'),names(spend))
stopifnot(length(value_names)==2L)
for(k in value_names)stopifnot(all(abs(z[[paste0(k,'_ref')]]-z[[paste0(k,'_actual')]])<0.001))
stopifnot(all(z$n_obs_ref==z$n_obs_actual))
write_csv(z,file.path(out,'spending_comparisons.csv'))
w <- readRDS(ESTIMATED_WELFARE_PER_BIN_PATH)
counts <- c('060_100'=1218,'100_200'=645,'200_500'=6)
loss <- -sum(w$change_in_spend_from_smoke[match(names(counts),w$bin)]*counts)
write_csv(tibble(quantity='Regional spending loss',value=loss,accepted=85392835.04397771,relative_difference=abs(loss/85392835.04397771-1)),file.path(out,'spending_total.csv'))
stopifnot(all(comparisons$pass),all(ns$pass),is.finite(loss),abs(loss/85392835.04397771-1)<0.001)
classes <- read_csv(NAICS_CODES_INDOOR_OUTDOOR_CLASSIFICATIONS_PATH,col_types=cols(naics_code=col_character()))
class_ref <- read_csv(file.path(CODE_DIR,'reference/classification_comparisons.csv'),col_types=cols(naics_code=col_character()))
zclass <- left_join(class_ref,classes,by='naics_code',relationship='one-to-one')
stopifnot(nrow(zclass)==42L,all(zclass$reproduced==zclass$likely_outdoor_label))
write_csv(zclass,file.path(out,'classifications.csv'))
spatial <- readRDS(file.path(REGRESSION_RESULTS_DIR,'spatial_sdem_v4_dcs_units_results.rds'))
profile <- cbind(spatial$neighbor_profile,no_smoke=1-rowSums(spatial$neighbor_profile))
nref <- read_csv(file.path(CODE_DIR,'reference/neighbor_share_comparisons.csv'),show_col_types=FALSE)
nref$actual <- profile[cbind(nref$row,nref$column)]
nref$pass <- is.finite(nref$actual)&abs(nref$actual-nref$reproduced)<0.001
stopifnot(nrow(nref)==42L,all(nref$pass))
write_csv(nref,file.path(out,'neighbor_shares.csv'))
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
cat('PASS: 260 coefficient/SE comparisons, 21 sample sizes, spending coefficients and aggregate, 42 industry classifications and 42 neighbor shares.\n')
