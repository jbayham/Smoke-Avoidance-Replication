library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

full_df <- read_csv(DAILY_DATA_FILEPATH) %>%
  mutate(across(c(binned_pr, binned_tmmn, binned_tmmx, binned_rmax), ~as.factor(.), .names = "{.col}"))

panel_df <- panel(full_df %>% filter(balanced_panel), ~ county_geoid + date)

base_formula <- "asinh_daily_visits_deferrable ~ bin_d_001_015 + bin_d_015_030 + bin_d_030_060 + bin_d_060_100 + bin_d_100_200 + bin_d_200_500"
base_add_weather_bins <- str_c(base_formula, "binned_tmmn", "binned_tmmx", "binned_pr", "binned_rmax", sep="+")

fe_set1 <- "county_geoid + date + state_year_month"
fe_set2 <- "date + state_year_month + day_of_week^county_geoid"
fe_set3 <- "date + state_year_month + day_of_week^county_geoid^season"

fe_list <- c(fe_set1, fe_set2, fe_set3)
models_bins <- list()
n <- 1
for (i in seq_along(fe_list)){
  current_formula_bins <- as.formula(str_c(base_formula, fe_list[i], sep=" | "))
  models_bins[[n]] <- feols(current_formula_bins, panel_df, cluster = ~county_geoid) 
  n <- n + 1
  current_formula_bins_with_weather <- as.formula(str_c(base_add_weather_bins, fe_list[i], sep=" | "))
  models_bins[[n]] <-feols(current_formula_bins_with_weather, panel_df, cluster = ~county_geoid) 
  n <- n + 1
}

etable_dict <- c(
  bin_d_001_015 = "Smoke Day (0, 15]",
  bin_d_015_030 = "Smoke Day (15, 30]",
  bin_d_030_060 = "Smoke Day (30, 60]",
  bin_d_060_100 = "Smoke Day (60, 100]",
  bin_d_100_200 = "Smoke Day (100, 200]",
  bin_d_200_500 = "Smoke Day (200, 500]",
  asinh_daily_visits_deferrable = "asinh(Deferrable Daily Visits)",
  county_geoid = "County",
  date = "Day",
  state_year_month = "State-Year-Month",
  day_of_week = "DoW",
  season = "Season"
)

# Units live in the note rather than in every row label: the full
# "Smoke Day (100, 200] \\textmu $g$ PM$_{2.5}$" stub was 162.6pt wide on its
# own and pushed the 6-column table ~124pt past \\textwidth.
# etable's `fontsize` wraps the tabular but not the trailing note paragraph,
# so size/italics are set inline to match the other two note lines.
smoke_bin_note <- "\\footnotesize \\emph{Smoke day bins are daily smoke PM$_{2.5}$ in \\textmu g/m$^3$.}"

# One significant digit in scientific notation, e.g. -3.35e-05 -> $-3\\times 10^{-5}$.
sci_one_sig <- function(x) {
  e <- floor(log10(abs(x)))
  m <- round(x / 10^e)
  if (abs(m) >= 10) {              # rounding can carry the mantissa up to 10
    m <- m / 10
    e <- e + 1
  }
  sprintf("$%s\\times 10^{%d}$", format(m, trim = TRUE), e)
}

# digits = "r4" prints coefficients smaller than 5e-05 as an uninformative
# "0.0000". This postprocessing hook puts those cells back in scientific
# notation (one significant digit), keeping any significance stars attached.
# Note etable hands the hook the table as multi-line blocks, not one line per
# element, and does its column alignment afterwards, so split on "\n" here and
# don't bother matching the padding.
restore_rounded_zeros <- function(models, dict) {
  vars <- grep("^bin_d_", names(dict), value = TRUE)
  vals <- lapply(vars, function(v) {
    vapply(models, function(m) unname(coef(m)[v]), numeric(1))
  })
  names(vals) <- vars

  fix_line <- function(line) {
    cells <- strsplit(line, "&", fixed = TRUE)[[1]]
    if (length(cells) < 2) return(line)
    v <- vars[match(trimws(cells[1]), dict[vars])]
    if (is.na(v)) return(line)
    for (j in seq_along(models)) {
      if (j + 1 > length(cells)) break
      b <- vals[[v]][j]
      if (is.na(b) || b == 0) next
      core <- sub("\\\\\\\\.*$", "", cells[j + 1])   # cell minus the row terminator
      num  <- trimws(core)
      if (!grepl("^-?0\\.0+(\\$\\^\\{\\*+\\}\\$)?$", num)) next
      stars <- sub("^-?0\\.0+", "", num)
      cells[j + 1] <- paste0(" ", sci_one_sig(b), stars, " ",
                             substring(cells[j + 1], nchar(core) + 1))
    }
    paste(cells, collapse = "&")
  }

  function(tex) {
    vapply(tex, function(block) {
      out <- paste(vapply(strsplit(block, "\n", fixed = TRUE)[[1]],
                          fix_line, character(1)), collapse = "\n")
      if (grepl("\n$", block)) out <- paste0(out, "\n")
      out
    }, character(1), USE.NAMES = FALSE)
  }
}

etable(models_bins,
      dict = etable_dict,
      keep = "%^bin_d_",
      digits = "r4",
      digits.stats = "r5",
      group = list("Weather Controls" = "binned_"))

etable(models_bins,
       dict = etable_dict,
       keep = "%^bin_d_",
       digits = "r4",
       digits.stats = "r5",
       group = list("Weather Controls" = "binned_"),
       notes = smoke_bin_note,
       fontsize = "footnotesize",
       postprocess.tex = restore_rounded_zeros(models_bins, etable_dict),
       file = file.path(TABLES_DIR, "table_1_fixed_effect_comp_weather_bins.tex"),
       tex = TRUE,
       replace = TRUE)
