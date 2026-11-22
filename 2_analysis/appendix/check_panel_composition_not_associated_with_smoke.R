# Checks that the composition of the Advan device panel does not move with smoke. The
# visits outcome is built from raw device counts, so if smoke changed how many devices the
# panel sampled in a county, part of the estimated drop in visits could be a sampling
# artifact rather than avoidance.
#
# The measure is the scaling multiple normalized_visits_by_state_scaling / raw_visit_counts:
# the number of residents each sampled device stands for, set by the state's population
# relative to the devices Advan observes there that month. It is carried onto every
# county-day from the first POI in the county-month (1_build/daily_consumer_data/
# add_daily_total_spend_and_visits_by_county.R), and counties in the same state and month differ by
# only about 1% (rounding in the POI's normalized visits). Since the multiple is set at the
# state level, the analysis is run on state-months, with standard errors clustered by state.
#
# Outputs:
#   figures/appendix/panel_scaling_multiple_distribution.pdf
#     (a) the median and mean of the multiple across states in each month, (b) its histogram
#   tables/appendix/appendix_table_panel_composition_smoke.tex
#     the multiple regressed on the state-month's count of county-days in each smoke bin
library(tidyverse)
library(fixest)
library(cowplot)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

SMOKE_BINS <- names(BIN_LABELS)

# balanced_panel drops the ten incomplete Vermont counties and Nov 30 of every year, which
# is missing from the visits data everywhere. Aggregating to months is not a lag, so
# dropping Nov 30 here only makes November a 29-day month; the month fixed effect absorbs it.
full_df <- read_csv(
  DAILY_DATA_FILEPATH,
  col_select = c(county_geoid, date, balanced_panel, raw_visit_counts,
                 normalized_visits_by_state_scaling, all_of(SMOKE_BINS)),
  show_col_types = FALSE
) %>%
  filter(balanced_panel) %>%
  mutate(multiple   = normalized_visits_by_state_scaling / raw_visit_counts,
         month_date = floor_date(date, "month"))

# Bins are summed over the state's county-days, so each is a count of county-days in the bin;
# the multiple is averaged over the same county-days.
monthly_df <- full_df %>%
  mutate(state_fips = substr(county_geoid, 1, 2)) %>%
  group_by(state_fips, month_date) %>%
  summarize(across(all_of(SMOKE_BINS), sum),
            multiple = mean(multiple),
            .groups = "drop")

############### Figure: distribution of the scaling multiple ###############

# No percentile bands around the median: states differ so little within a month that the
# bands were no wider than the median line. Nearly all the variation in the multiple is over time.
monthly_summary <- monthly_df %>%
  group_by(month_date) %>%
  summarize(mean = mean(multiple), median = median(multiple), .groups = "drop")
print(monthly_summary, n = Inf)

MULTIPLE_LABEL <- "Residents per Sampled Device"

line_colors <- c("Median" = "firebrick", "Mean" = "black")

time_plot <- ggplot(monthly_summary, aes(x = month_date)) +
  geom_line(aes(y = median, color = "Median"), linewidth = 0.8) +
  geom_line(aes(y = mean, color = "Mean"), linewidth = 0.8, linetype = 2) +
  scale_color_manual(NULL, values = line_colors, breaks = names(line_colors)) +
  scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
  labs(y = MULTIPLE_LABEL) +
  theme_minimal(base_size = 13) +
  # The legend sits in the empty lower-left corner (2019-2020 never drops below 12.5): under
  # the panel it pushed panel (a)'s x-axis above panel (b)'s. It also keys the median and
  # mean lines in (b).
  theme(legend.position = "inside", legend.position.inside = c(0.01, 0.02),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank())

# One observation per state-month. The peaks are different stretches of the sample (see
# panel a), not different groups of states. Line colors match the legend in panel (a).
hist_plot <- ggplot(monthly_df, aes(x = multiple)) +
  geom_histogram(binwidth = 0.25, boundary = 0, fill = "grey60", color = "white", linewidth = 0.2) +
  geom_vline(xintercept = median(monthly_df$multiple), color = line_colors[["Median"]],
             linewidth = 0.8) +
  geom_vline(xintercept = mean(monthly_df$multiple), color = line_colors[["Mean"]],
             linewidth = 0.8, linetype = 2) +
  scale_y_continuous("State-Months", labels = scales::label_comma(),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = MULTIPLE_LABEL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

multiple_figure <- plot_grid(time_plot, hist_plot,
                             nrow = 1, rel_widths = c(1.4, 1),
                             labels = c("(a)", "(b)"), label_fontface = "plain",
                             label_size = 13, align = "h", axis = "tb")

ggsave(file.path(APPENDIX_FIGURES_DIR, "panel_scaling_multiple_distribution.pdf"),
       plot = multiple_figure, width = 12, height = 5, dpi = 300, units = "in")

############### Table: is the multiple associated with smoke? ###############

panel_month_df <- panel(monthly_df, ~ state_fips + month_date)

est_smoke_panel <- feols(multiple ~
                           bin_d_001_015 + bin_d_015_030 +
                           bin_d_030_060 + bin_d_060_100 +
                           bin_d_100_200 + bin_d_200_500
                         | state_fips + month_date,
                         data = panel_month_df, cluster = ~state_fips)


saveRDS(est_smoke_panel, file.path(REGRESSION_RESULTS_DIR, "panel_composition_smoke.rds"))

etable_dict <- c(
  bin_d_001_015 = "Smoke Days (0, 15]",
  bin_d_015_030 = "Smoke Days (15, 30]",
  bin_d_030_060 = "Smoke Days (30, 60]",
  bin_d_060_100 = "Smoke Days (60, 100]",
  bin_d_100_200 = "Smoke Days (100, 200]",
  bin_d_200_500 = "Smoke Days (200, 500]",
  multiple      = "Residents per Sampled Device",
  county_geoid  = "County",
  state_fips    = "State",
  month_date    = "Year - Month"
)

panel_table_notes <- paste(
  "Each observation is a state-month in the balanced panel, 2019-2023. The dependent",
  "variable is Advan's scaling multiple (normalized visits by state scaling divided by",
  "raw visit counts), the number of residents each sampled device represents, averaged",
  "over the state's county-days. Smoke bins count the county-days in the state-month",
  "whose smoke-attributable PM$_{2.5}$ falls in each bin, in \\textmu $g$/m$^3$."
)

etable(est_smoke_panel,
       dict    = etable_dict,
       fitstat = ~ my + n + r2 + wr2,
       digits  = 3)

panel_table_path <- file.path(APPENDIX_TABLES_DIR, "appendix_table_panel_composition_smoke.tex")
etable(est_smoke_panel,
       dict    = etable_dict,
       fitstat = ~ my + n + r2 + wr2,
       digits  = 3,
       notes   = panel_table_notes,
       file    = panel_table_path,
       tex     = TRUE,
       replace = TRUE)

cat("\nWrote", panel_table_path, "\n")
