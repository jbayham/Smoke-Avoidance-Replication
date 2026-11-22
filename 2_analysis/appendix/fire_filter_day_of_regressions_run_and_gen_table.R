library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# Counties with NA on viirs data are filtered out.
full_df <- read_csv(DAILY_DATA_FILEPATH, show_col_types = FALSE) %>%
  mutate(
    across(c(binned_pr, binned_tmmn, binned_tmmx, binned_rmax), ~as.factor(.), .names = "{.col}"),
    # viirs_in_perimeter is coalesced to 0 on no-fire days in the build, so the log control
    # is log(1 + x) to keep those days in the sample at a value of 0.
    log_viirs_in_perimeter = log1p(viirs_in_perimeter)
  )
  
  
fire_types <- c("confirmed_wildfire", "confirmed_other_fire", "confirmed_any_fire")

bin_vars <- c("bin_d_001_015", "bin_d_015_030", "bin_d_030_060",
              "bin_d_060_100", "bin_d_100_200", "bin_d_200_500")
weather_vars <- "binned_pr + binned_tmmn + binned_tmmx + binned_rmax"
fe_vars <- "date + state_year_month + day_of_week^county_geoid^season"
first_part_of_formula <- paste0(
  "asinh_daily_visits_deferrable ~ ",
  paste(bin_vars, collapse = " + "),
  " + ", weather_vars)

formula_with_fire_control <- as.formula(paste0(
  first_part_of_formula, " + viirs_in_perimeter + I(viirs_in_perimeter^2)", " | ", fe_vars
))

formula_with_log_fire_control <- as.formula(paste0(
  first_part_of_formula, " + log_viirs_in_perimeter + I(log_viirs_in_perimeter^2)", " | ", fe_vars
))

formula_without_fire_control <- as.formula(paste0(
  first_part_of_formula, " | ", fe_vars
))

############### Table: is the dose response driven by nearby fire? ###############

# Six columns, all on the same day-of specification and the same fixed effects:
#   (1) the main estimate from 2_analysis/figure_1_2_day_of_analysis_run.R,
#   (2) the same regression with the VIIRS fire-radiative-power controls added
#       (est_fire_control from the top of this script), and
#   (3)-(6) the sample with heavy-fire county-days dropped rather than controlled for.
# confirmed_any_fire_{t} == 1 flags viirs_total > t and confirmed_wildfire_{t} == 1 flags
# viirs_in_perimeter > t (see 1_build/reg_data/build_reg_data_daily.R), so keeping the
# == 0 rows keeps the county-days below the threshold and drops the fire days.

est_bin_base <- readRDS(file.path(REGRESSION_RESULTS_DIR, "binary_base_day_of_dose_response.rds"))

fire_filter_vars <- c("confirmed_any_fire_25", "confirmed_any_fire_100",
                      "confirmed_wildfire_25", "confirmed_wildfire_100")

panel_df <- panel(full_df %>% filter(balanced_panel), ~ county_geoid + date)

# lean = TRUE on the two saved models drops the observation-level pieces (residuals, scores,
# fixed-effect ids) that make each .rds over 1 GB; etable needs none of them.
est_fire_control <- feols(formula_with_fire_control, data = panel_df, cluster = ~county_geoid, lean = TRUE)

saveRDS(est_fire_control,
        file.path(REGRESSION_RESULTS_DIR, "fire_control_viirs_in_perimeter.rds"))

est_fire_control_log <- feols(formula_with_log_fire_control, data = panel_df, cluster = ~county_geoid, lean = TRUE)

saveRDS(est_fire_control_log,
        file.path(REGRESSION_RESULTS_DIR, "fire_control_log_viirs_in_perimeter.rds"))


run_fire_filtered <- function(fire_var) {
  df_f <- full_df %>% filter(balanced_panel, .data[[fire_var]] == 0)
  cat("Running", fire_var, "== 0 filter:",
      format(nrow(df_f), big.mark = ","), "county-days kept (",
      format(sum(full_df$balanced_panel) - nrow(df_f), big.mark = ","), "dropped )\n")
  feols(formula_without_fire_control,
        data = panel(df_f, ~ county_geoid + date), cluster = ~county_geoid)
}

fire_filtered_models <- lapply(setNames(fire_filter_vars, fire_filter_vars), run_fire_filtered)

table_models <- c(list(est_bin_base, est_fire_control), unname(fire_filtered_models))
# Six columns of numbers leave little room for the row labels, so the sample description is
# split into a spanning row over short per-column thresholds rather than written out six
# times: "Drop Any Fire $>$ 25" as a column header is wider than any number underneath it,
# and so was setting the column width.
table_headers <- list(
  "Sample:" = c("Full sample" = 2, "Any fire dropped" = 2, "Wildfire dropped" = 2),
  " "       = c("Main", "VIIRS ctrl.", "$>$25", "$>$100", "$>$25", "$>$100")
)

etable_dict <- c(
  # The units sit in the table note instead of on all six rows: spelled out each time they
  # set the width of the first column and pushed the table past the text block.
  bin_d_001_015 = "Smoke Day (0, 15]",
  bin_d_015_030 = "Smoke Day (15, 30]",
  bin_d_030_060 = "Smoke Day (30, 60]",
  bin_d_060_100 = "Smoke Day (60, 100]",
  bin_d_100_200 = "Smoke Day (100, 200]",
  bin_d_200_500 = "Smoke Day (200, 500]",
  viirs_in_perimeter        = "VIIRS in perimeter",
  `I(viirs_in_perimeter^2)` = "VIIRS in perimeter$^2$",
  # "in perimeter" is left to the table note: spelled out here the label pushed the log
  # table to 474pt, past the 470pt text block.
  log_viirs_in_perimeter        = "log(1 + VIIRS)",
  `I(log_viirs_in_perimeter^2)` = "log(1 + VIIRS)$^2$",
  asinh_daily_visits_deferrable = "asinh(Deferrable Daily Visits)",
  county_geoid = "County",
  date = "Day",
  state_year_month = "State-Year-Month",
  day_of_week = "Day of Week",
  season = "Season",
  notes = "Clustered (County) standard-errors in parentheses"
)

fire_table_notes <- paste(
  "Smoke bins are indicators for the day's smoke-attributable PM$_{2.5}$, in",
  "\\textmu $g$/m$^3$; the omitted category is a day with no smoke PM$_{2.5}$.",
  "Every column uses flexible weather controls and Day, State-Year-Month, and",
  "Day of Week-County-Season fixed effects.",
  "Column (1) is the main day-of specification. Column (2) adds the VIIRS fire",
  "detections inside a fire perimeter and their square as controls. Columns (3)-(6)",
  "instead drop every county-day whose VIIRS detection count exceeds the stated",
  "threshold: total detections under \\emph{Any fire dropped}, detections inside a fire",
  "perimeter under \\emph{Wildfire dropped}."
)

# The controls and fixed effects are identical in all six columns, so the rows of "Yes"
# carried no information and cost the two widest labels in the first column. They are
# stated once in the table note instead; `keep` already excludes the weather bins, so no
# `group` argument is needed to hold them out of the coefficient block.
etable(table_models,
       dict         = etable_dict,
       headers      = table_headers,
       keep         = c("%bin_d_", "%viirs"),
       drop.section = "fixef")

fire_table_path <- file.path(APPENDIX_TABLES_DIR, "appendix_table_fire_robustness.tex")
# Even with the short labels six columns do not fit a 6.5in text block at body size: the
# measured natural width is 517pt against a 470pt block. \footnotesize plus a 4pt column
# separation brings it to 433pt (\small lands at 464pt, too close to the edge to trust). The cmidrules under the "Sample:" row are what make the
# spanning headers read as three groups of columns. The \setlength sits inside the
# \begingroup that etable emits, so it does not leak into the rest of the document.
# etable hands the postprocessor a character vector whose elements are blocks of the table
# rather than single lines, so the rule is spliced inside the block that holds the header
# row; returning the vector unchanged in shape lets etable strip its own %start:tab markers.
fit_to_page <- function(x) {
  x <- sub("\\begingroup", "\\begingroup\\setlength{\\tabcolsep}{4pt}", x, fixed = TRUE)
  i <- grep("Sample:", x, fixed = TRUE)
  if (length(i) == 1) {
    ln <- unlist(strsplit(x[i], "\n", fixed = TRUE))
    j  <- grep("Sample:", ln, fixed = TRUE)
    ln <- append(ln, "   \\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
                 after = j)
    x[i] <- paste0(paste(ln, collapse = "\n"), "\n")
  }
  x
}

etable(table_models,
       dict            = etable_dict,
       headers         = table_headers,
       keep            = c("%bin_d_", "%viirs"),
       drop.section    = "fixef",
       notes           = fire_table_notes,
       fontsize        = "footnotesize",
       postprocess.tex = fit_to_page,
       file            = fire_table_path,
       tex             = TRUE,
       replace         = TRUE)

############### Same table with the fire control in logs ###############

# Column (2) swaps the level and square of viirs_in_perimeter for log(1 + x) and its square;
# the other five columns have no fire control and are unchanged.
table_models_log <- c(list(est_bin_base, est_fire_control_log), unname(fire_filtered_models))
table_headers_log <- table_headers
table_headers_log[[" "]][2] <- "Log VIIRS ctrl."

fire_table_notes_log <- sub(
  "adds the VIIRS fire detections inside a fire perimeter and their square as controls",
  "adds the log of one plus the VIIRS fire detections inside a fire perimeter and its square as controls",
  fire_table_notes, fixed = TRUE)

etable(est_bin_base, est_fire_control, est_fire_control_log,
       dict         = etable_dict,
       headers      = list(" " = c("Main", "VIIRS ctrl.", "Log VIIRS ctrl.")),
       keep         = c("%bin_d_", "%viirs"),
       drop.section = "fixef")

etable(table_models_log,
       dict            = etable_dict,
       headers         = table_headers_log,
       keep            = c("%bin_d_", "%viirs"),
       drop.section    = "fixef",
       notes           = fire_table_notes_log,
       fontsize        = "footnotesize",
       postprocess.tex = fit_to_page,
       file            = file.path(APPENDIX_TABLES_DIR, "appendix_table_fire_robustness_log.tex"),
       tex             = TRUE,
       replace         = TRUE)


