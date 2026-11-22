library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(PLOTTING_HELPERS_PATH)

cat("Loading data...\n")
full_df <- read_csv(DAILY_DATA_FILEPATH, show_col_types = FALSE) %>%
  mutate(
    across(c(binned_pr, binned_tmmn, binned_tmmx, binned_rmax), ~as.factor(.), .names = "{.col}")
  )

###### Quartile levels of each characteristic ######
# The quartile dummies and the `<label>_q` column are built county-by-county in
# 1_build/county_characteristics/merge_county_information.R and merged into the panel with
# the rest of the county file, so all that is needed here is the list of quartiles that
# actually survived for each characteristic. Ties in the cutoffs can collapse the lower
# quartiles into one baseline group (the outdoor shares are zero for more than half of the
# counties), so a characteristic may have fewer than four levels. The first level is the
# omitted category.
quartile_levels <- setNames(
  lapply(HETEROGENEITY_LABELS, \(label) sort(unique(na.omit(full_df[[paste0(label, "_q")]])))),
  unname(HETEROGENEITY_LABELS)
)

for (label in names(quartile_levels)) {
  cat(label, "quartiles present in the panel:", paste(quartile_levels[[label]], collapse = ", "), "\n")
}

panel_df <- panel(full_df %>% filter(balanced_panel), ~ county_geoid + date)

# Every model below is estimated with lean = TRUE: fixest then drops the observation-level
# pieces (residuals, fitted values, scores, fixed-effect ids) from the saved object, which
# otherwise make each .rds 1-4 GB. Coefficients, the county-clustered vcov and fit
# statistics are kept, which is all the plotting scripts read; residuals/predictions and a
# different vcov cannot be recovered from the saved files.

# Re-run with binary versions of the bin variables on the base dataframe

cat("Running binary base regression...\n")
est_bin_base <- feols(asinh_daily_visits_deferrable ~
                        bin_d_001_015 + bin_d_015_030 +
                        bin_d_030_060 + bin_d_060_100 +
                        bin_d_100_200 + bin_d_200_500 +
                        binned_pr + binned_tmmn + binned_tmmx + binned_rmax |
                        date + state_year_month + day_of_week^county_geoid^season,
                      panel_df, cluster = ~county_geoid, lean = TRUE)
saveRDS(est_bin_base, file.path(REGRESSION_RESULTS_DIR,
        paste0("binary_base_day_of_dose_response.rds")))
cat("Binary base done.\n")

cat("Running binary weekend interaction...\n")
est_bin_weekend <- feols(
  asinh_daily_visits_deferrable ~
    bin_d_001_015:is_weekend + bin_d_015_030:is_weekend +
    bin_d_030_060:is_weekend + bin_d_060_100:is_weekend +
    bin_d_100_200:is_weekend + bin_d_200_500:is_weekend +
    binned_pr + binned_tmmn + binned_tmmx + binned_rmax |
    date + state_year_month + day_of_week^county_geoid^season,
  data = panel_df, cluster = ~county_geoid, lean = TRUE
)
# is_weekend is a deterministic function of the calendar day, so crossing the day fixed
# effect with it would just reproduce the day fixed effect: this spec is unchanged.
saveRDS(est_bin_weekend, file.path(REGRESSION_RESULTS_DIR,
        paste0("binary_weekend_interaction.rds")))
cat("Binary weekend done.\n")


###### Quartile interactions with the smoke bins ######

smoke_bins <- c('bin_d_001_015', 'bin_d_015_030', 'bin_d_030_060',
                'bin_d_060_100', 'bin_d_100_200', 'bin_d_200_500')
WEATHER_CONTROLS <- "binned_pr + binned_tmmn + binned_tmmx + binned_rmax"
FIXED_EFFECTS <- paste("county_geoid + date + state_year_month + week +",
                       "day_of_week^county_geoid^season")

# Each smoke bin enters on its own -- that coefficient is the effect for the first
# quartile -- and is interacted with the quartile 2/3/4 dummies, so those coefficients are
# differences relative to the first quartile. The quartile dummies themselves are left out
# of the regression: they do not vary within a county and so are collinear with the county
# fixed effect.
# The day fixed effect is the only piece that varies with the characteristic, so build the
# fixed effect string per variable rather than reusing the shared FIXED_EFFECTS constant.
quartile_fixed_effects <- function(label, add_day_quantile_term) {
  day_fe <- if (add_day_quantile_term) paste0("date^", label, "_q") else
                                 "date"
  paste("county_geoid +", day_fe,
        "+ state_year_month + week + day_of_week^county_geoid^season")
}

quartile_interaction_formula <- function(label, add_day_quantile_term) {
  non_baseline <- paste0(label, "_", quartile_levels[[label]][-1])
  interactions <- as.vector(t(outer(smoke_bins, non_baseline, paste, sep = ":")))

  as.formula(paste0(
    "asinh_daily_visits_deferrable ~ ",
    paste(c(smoke_bins, interactions), collapse = " + "),
    " + ", WEATHER_CONTROLS, " | ", quartile_fixed_effects(label, add_day_quantile_term)
  ))
}

run_quartile_interaction <- function(column, description, add_day_quantile_term=FALSE) {
  output_suffix <- if (add_day_quantile_term) "_quantile_day_interaction_FE" else ""
  label <- HETEROGENEITY_LABELS[[column]]
  est <- feols(quartile_interaction_formula(label, add_day_quantile_term), data = panel_df, cluster = ~county_geoid, lean = TRUE)
  saveRDS(est, file.path(REGRESSION_RESULTS_DIR,
          paste0("binary_quartile_", label, "_interaction", output_suffix, ".rds")))
  est
}
full_df %>% filter(balanced_panel, is.na(median_household_income))

est_bin_background_pm       <- run_quartile_interaction('pm_background_mean',              "background PM")
est_bin_income              <- run_quartile_interaction('median_household_income',         "income")
est_bin_nat_amenity         <- run_quartile_interaction('natural_amenity_tourism_measure', "natural amenity tourism")
est_bin_gdp_rec             <- run_quartile_interaction('percent_gdp_from_rec',            "GDP rec")
est_bin_comp_rec            <- run_quartile_interaction('percent_comp_from_rec',           "comp rec")
est_bin_cbp_outdoor_estab   <- run_quartile_interaction('cbp_outdoor_share_estab',         "outdoor share of establishments")
est_bin_cbp_outdoor_payroll <- run_quartile_interaction('cbp_outdoor_share_payroll',       "outdoor share of payroll")
est_bin_cbp_hhi_estab       <- run_quartile_interaction('cbp_hhi_estab',                   "HHI of establishments")
est_bin_cbp_hhi_payroll     <- run_quartile_interaction('cbp_hhi_payroll',                 "HHI of payroll")
est_bin_rurality            <- run_quartile_interaction('rurality_index',                  "CARR rurality index")

est_bin_background_pm_day_quartile_fe       <- run_quartile_interaction('pm_background_mean',              "background PM",                   add_day_quantile_term=TRUE)
est_bin_income_day_quartile_fe              <- run_quartile_interaction('median_household_income',         "income",                          add_day_quantile_term=TRUE)
est_bin_nat_amenity_day_quartile_fe         <- run_quartile_interaction('natural_amenity_tourism_measure', "natural amenity tourism",         add_day_quantile_term=TRUE)
est_bin_gdp_rec_day_quartile_fe             <- run_quartile_interaction('percent_gdp_from_rec',            "GDP rec",                         add_day_quantile_term=TRUE)
est_bin_comp_rec_day_quartile_fe            <- run_quartile_interaction('percent_comp_from_rec',           "comp rec",                        add_day_quantile_term=TRUE)
est_bin_cbp_outdoor_estab_day_quartile_fe   <- run_quartile_interaction('cbp_outdoor_share_estab',         "outdoor share of establishments", add_day_quantile_term=TRUE)
est_bin_cbp_outdoor_payroll_day_quartile_fe <- run_quartile_interaction('cbp_outdoor_share_payroll',       "outdoor share of payroll",        add_day_quantile_term=TRUE)
est_bin_cbp_hhi_estab_day_quartile_fe       <- run_quartile_interaction('cbp_hhi_estab',                   "HHI of establishments",           add_day_quantile_term=TRUE)
est_bin_cbp_hhi_payroll_day_quartile_fe     <- run_quartile_interaction('cbp_hhi_payroll',                 "HHI of payroll",                  add_day_quantile_term=TRUE)
est_bin_rurality_day_quartile_fe            <- run_quartile_interaction('rurality_index',                  "CARR rurality index",             add_day_quantile_term=TRUE)

