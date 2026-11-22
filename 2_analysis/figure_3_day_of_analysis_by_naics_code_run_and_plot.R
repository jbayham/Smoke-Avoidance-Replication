library(tidyverse)
library(fixest)
library(glue)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(PLOTTING_HELPERS_PATH)

cat("Loading data...\n")
full_df <- read_csv(DAILY_DATA_FILEPATH, show_col_types = FALSE) %>%
  mutate(
    across(c(binned_pr, binned_tmmn, binned_tmmx, binned_rmax), ~as.factor(.), .names = "{.col}")
  )

naics_codes_descriptions <- readxl::read_excel(NAICS_CODES_DESCRIPTIONS_PATH) %>%
  rename(naics_code=`2017 NAICS Code`, natural_language_category=`2017 NAICS Title`) %>%
  select(-...3) %>%
  filter(!is.na(naics_code)) %>%
  janitor::clean_names()

indoor_outdoor_classifications <- read_csv(
  NAICS_CODES_INDOOR_OUTDOOR_CLASSIFICATIONS_PATH, show_col_types = FALSE
) %>%
  mutate(naics_code = as.numeric(naics_code)) %>%
  select(naics_code, naics_description, likely_outdoor_label)

# One regression per classified NAICS code with a visits column (naics_<code>_visits).
# Codes with spend but no visits have no such column. The current deliveries also code
# some places only to 2-5 digits (e.g. 7212, 7111); those roll-ups are unclassified and
# would plot as an "NA" sector, so they are left out, as the original panel's
# naics_<code>.0.x columns were.
classified_visit_cols <- paste0('naics_', indoor_outdoor_classifications$naics_code, '_visits')
all_visit_cols <- full_df %>% select(starts_with('naics_') & ends_with('_visits')) %>% names()
naics_code_cols <- intersect(all_visit_cols, classified_visit_cols)
cat("Visit columns left out (unclassified):",
    str_c(setdiff(all_visit_cols, naics_code_cols), collapse = " "), "\n")

# Coverage rule: a sector is kept only if it has visits on at least 0.1% of county-days
# in the balanced panel (the estimation sample). Sectors below that are observed in a
# handful of counties (five or fewer), too few to estimate a national response.
MIN_SHARE_COUNTY_DAYS_WITH_VISITS <- 0.001
share_county_days_with_visits <- full_df %>%
  filter(balanced_panel) %>%
  summarize(across(all_of(naics_code_cols), ~mean(.x > 0, na.rm = TRUE))) %>%
  unlist()
sparse_cols <- names(which(share_county_days_with_visits < MIN_SHARE_COUNTY_DAYS_WITH_VISITS))
cat("Visit columns left out (visits on under",
    100 * MIN_SHARE_COUNTY_DAYS_WITH_VISITS, "% of county-days):\n")
print(round(100 * share_county_days_with_visits[sparse_cols], 3))
naics_code_cols <- setdiff(naics_code_cols, sparse_cols)
cat("Sectors plotted:", length(naics_code_cols), "\n")


# The official NAICS titles are too long for an axis label, so shorten the worst
# offenders. Anything not listed here falls back to the full NAICS title.
# Trailing comments give the official 2017 NAICS title being shortened.
NAICS_SHORT_LABELS <- c(
  '711110' = 'Theater and Dinner Theaters',        # Theater Companies and Dinner Theaters
  '7111'   = 'Performing Arts Companies (Group)',  # Performing Arts Companies (4-digit group; places not coded to 6 digits)
  '711190' = 'Other Performing Arts Companies',    # Other Performing Arts Companies
  '7212'   = 'RV Parks and Rec. Camps (Group)',    # RV (Recreational Vehicle) Parks and Recreational Camps (4-digit group)
  '711310' = 'Promoters of Events w/ Facilities',  # Promoters of Performing Arts, Sports, and Similar Events with Facilities
  '711320' = 'Promoters of Events w/o Facilities', # Promoters of Performing Arts, Sports, and Similar Events without Facilities
  '711410' = 'Agents and Managers for Artists',    # Agents and Managers for Artists, Athletes, Entertainers, and Other Public Figures
  '711510' = 'Independent Artists and Performers', # Independent Artists, Writers, and Performers
  '713990' = 'Other Amusement and Recreation',     # All Other Amusement and Recreation Industries
  '721199' = 'Other Traveler Accommodation',       # All Other Traveler Accommodation
  '721211' = 'RV Parks and Campgrounds',           # RV (Recreational Vehicle) Parks and Campgrounds
  '721214' = 'Recreational Camps',                 # Recreational and Vacation Camps (except Campgrounds)
  '721310' = 'Rooming and Boarding Houses',        # Rooming and Boarding Houses, Dormitories, and Workers' Camps
  '722514' = 'Cafeterias and Buffets'              # Cafeterias, Grill Buffets, and Buffets
)

# Add asinh transformed NAICS columns to panel_df
for (naics_col in naics_code_cols) {
  asinh_col_name <- paste0("asinh_", naics_col)
  full_df[[asinh_col_name]] <- asinh(full_df[[naics_col]])
}


naics_panel_df <- panel(full_df %>% filter(balanced_panel), ~ county_geoid + date)
# Function to run regression for a specific NAICS code
run_naics_regression <- function(naics_col) {
  asinh_col_name <- paste0("asinh_", naics_col)
  
  formula_str <- paste0(
    asinh_col_name, " ~ ",
    "bin_d_001_015 + bin_d_015_030 +",
    "bin_d_030_060 + bin_d_060_100 +",
    "bin_d_100_200 + bin_d_200_500 +",
    "binned_pr + binned_tmmn + binned_tmmx + binned_rmax | ",
    "date + state_year_month + day_of_week^county_geoid^season"
  )
  
  feols(as.formula(formula_str), naics_panel_df, cluster = ~county_geoid)
}


# Run regressions for each NAICS code
results_list <- list()
for (naics_col in naics_code_cols) {
  model <- run_naics_regression(naics_col)
  
  # Clean up NAICS code for labeling
  naics_label <- str_remove(naics_col, "_visits$")
  
  results_list[[naics_col]] <- add_new_sample_and_sector(
    model, 
    "Sector", 
    sample_id = NULL, 
    sample_label = naics_label, 
    overall_sample_label = "NAICS Code", 
    vars_to_plot_start = "bin_d_"
  )
}

# Combine all results
results_dfm <- bind_rows(results_list)

write_csv(results_dfm, file.path(REGRESSION_RESULTS_DIR, 'sector_specific_naics_results.csv'))

# Create plot data
plot_data <- results_dfm %>%
  mutate(
    low_ci = Estimate - 1.96 * Std..Error,
    high_ci = Estimate + 1.96 * Std..Error,
    # exp(b) - 1 puts the coefficient on a proportional-change scale. exp() is
    # monotone, so transforming the CI ENDPOINTS carries the coverage over
    # exactly; transforming the standard error instead would not (the delta
    # method here is se_g = exp(b) * se, and the resulting symmetric interval
    # can dip below -100%).
    low_ci   = exp(low_ci) - 1,
    high_ci  = exp(high_ci) - 1,
    Estimate = exp(Estimate) - 1,
    bin = factor(BIN_LABELS[vars], levels = unname(BIN_LABELS)),
    naics_code = as.numeric(str_sub(`NAICS Code`, start=7)),
    `NAICS Code` = factor(`NAICS Code`, levels = naics_code_cols %>% str_remove("_visits$"))
  ) %>% 
  left_join(naics_codes_descriptions, by='naics_code') %>% 
  mutate(combined_naics_code_and_desc = paste0(naics_code, ': ', natural_language_category))

setting_plot_data <- plot_data %>%
  left_join(indoor_outdoor_classifications, by = 'naics_code') %>%
  mutate(
    sector_label = coalesce(
      unname(NAICS_SHORT_LABELS[as.character(naics_code)]),
      naics_description,
      natural_language_category
    )
  )
stopifnot(!anyNA(setting_plot_data$likely_outdoor_label))


# Wrap the labels and pad them all out to the same number of lines: with the
# 45-degree rotation, unequal line counts make short labels collide with the
# lower lines of their taller neighbours.
wrap_sector_labels <- function(labels, width = 27) {
  wrapped <- str_wrap(labels, width)
  n_lines <- str_count(wrapped, '\n') + 1
  paste0(strrep('\n', max(n_lines) - n_lines), wrapped)
}

# Breaks on whole percentage points only, with the step snapped to a round
# number so that the free y scales never fall back on fractional-percent labels.
whole_percent_breaks <- function(limits) {
  pct <- limits * 100
  nice_steps <- c(1, 2, 5, 10, 20, 25, 50, 100)
  step <- nice_steps[which(nice_steps >= diff(pct) / 5)[1]]
  if (is.na(step)) step <- 100
  seq(ceiling(pct[1] / step) * step, floor(pct[2] / step) * step, by = step) / 100
}

SETTING_LEVELS    <- c('Likely Outdoors', 'Likely Indoors')
SETTING_SHAPES    <- c('Likely Indoors' = 16, 'Likely Outdoors' = 17)
SETTING_LINETYPES <- c('Likely Indoors' = 'dashed', 'Likely Outdoors' = 'solid')

# Group the sectors by setting (all outdoor, then all indoor) and, within each
# setting, order by the estimated response in the (60, 100] bin (most negative
# first). The same ordering is reused in every bin-specific panel below.
FOCUS_BIN <- unname(BIN_LABELS['bin_d_060_100'])

sector_order <- setting_plot_data %>%
  filter(bin == FOCUS_BIN) %>%
  arrange(factor(likely_outdoor_label, levels = SETTING_LEVELS), Estimate) %>%
  pull(sector_label)

setting_plot_data <- setting_plot_data %>%
  mutate(
    likely_outdoor_label = factor(likely_outdoor_label, levels = SETTING_LEVELS),
    sector_label = factor(
      sector_label,
      levels = union(sector_order, sort(unique(sector_label)))
    )
  )

# Plot 1: the (60, 100] bin only, every sector on one axis, outdoor then indoor.
naics_setting_plot <- ggplot(
  setting_plot_data %>% filter(bin == FOCUS_BIN),
  aes(x = sector_label, y = Estimate, ymin = low_ci, ymax = high_ci,
      color = bin, shape = likely_outdoor_label)
) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  facet_wrap(~bin) +
  # The whisker is drawn separately from its end caps so that the dashed
  # indoor linetype applies only to the vertical line: the caps stay solid.
  geom_linerange(aes(linetype = likely_outdoor_label), linewidth = 1) +
  geom_errorbar(aes(ymax = low_ci), width = 0.25, linewidth = 1) +
  geom_errorbar(aes(ymin = high_ci), width = 0.25, linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = BIN_COLORS) +
  scale_shape_manual("Setting", values = SETTING_SHAPES) +
  scale_linetype_manual("Setting", values = SETTING_LINETYPES) +
  scale_x_discrete(NULL, labels = function(x) wrap_sector_labels(x, 27)) +
  scale_y_continuous("Daily Change in Visits",
                     labels = scales::label_percent()) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    strip.text = element_text(face = "bold", size = 18)
  ) +
  guides(color = "none", linetype = "none")

naics_setting_plot

ggsave(file.path(FIGURES_DIR, "figure_3a_day_of_analysis_naics_by_setting_60_100.pdf"),
       plot = naics_setting_plot,
       width = 20, height = 8, dpi = 300, units = "in")

# Plot 2: one row per bin, sharing plot 1's single outdoor-then-indoor ordering.
naics_setting_by_bin_plot <- ggplot(
  setting_plot_data,
  aes(x = sector_label, y = Estimate, ymin = low_ci, ymax = high_ci,
      color = bin, shape = likely_outdoor_label)
) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_linerange(aes(linetype = likely_outdoor_label), linewidth = 0.7) +
  geom_errorbar(aes(ymax = low_ci), width = 0.5, linewidth = 0.7) +
  geom_errorbar(aes(ymin = high_ci), width = 0.5, linewidth = 0.7) +
  geom_point(size = 4) +
  facet_grid(bin ~ ., scales = "free_y") +
  scale_color_manual(values = BIN_COLORS) +
  scale_shape_manual("Setting", values = SETTING_SHAPES) +
  scale_linetype_manual("Setting", values = SETTING_LINETYPES) +
  scale_x_discrete("NAICS Sector", labels = function(x) wrap_sector_labels(x, 27)) +
  scale_y_continuous("Daily Change in Visits",
                     breaks = whole_percent_breaks,
                     labels = scales::label_percent(accuracy = 1)) +
  theme_minimal(base_size = 24) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = "none", linetype = "none")

naics_setting_by_bin_plot

ggsave(file.path(FIGURES_DIR, "figure_3b_day_of_analysis_naics_by_setting_and_bin.pdf"),
       plot = naics_setting_by_bin_plot,
       width = 20, height = 20, dpi = 300, units = "in")

cat("Completed NAICS indoor/outdoor setting plots\n")