# Counts county-days by calendar year and exports both the annual-average panel
# and each bin's percent
# change from its 2020 count (2020 = 0%). 2020 is the base because it is the first year with
# county-days in every bin: (100, 200] and (200, 500] have none in 2019, so their lines
# start in 2020.
library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

SMOKE_BINS <- names(BIN_LABELS)
FIRST_YEAR <- 2019
BASE_YEAR  <- 2020

full_df <- read_csv(
  DAILY_DATA_FILEPATH,
  col_select = c(county_geoid, date, balanced_panel, all_of(SMOKE_BINS)),
  show_col_types = FALSE
) %>%
  filter(balanced_panel) %>%
  mutate(year = year(date))

# Panel coverage by year: a percent change is only comparable across years that cover the
# same stretch of the calendar with the same counties.
year_coverage <- full_df %>%
  group_by(year) %>%
  summarize(first_date = min(date), last_date = max(date),
            n_dates = n_distinct(date), n_counties = n_distinct(county_geoid),
            n_county_days = n(), .groups = "drop")
print(year_coverage)

yearly_bin_counts <- full_df %>%
  group_by(year) %>%
  summarize(across(all_of(SMOKE_BINS), ~sum(. == 1, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(all_of(SMOKE_BINS), names_to = "bin", values_to = "n_smoke_days") %>%
  mutate(bin = factor(BIN_LABELS[bin], levels = unname(BIN_LABELS)))

pct_change <- yearly_bin_counts %>%
  group_by(bin) %>%
  mutate(pct_change = n_smoke_days / n_smoke_days[year == BASE_YEAR] - 1) %>%
  ungroup() %>%
  filter(!(year == FIRST_YEAR & n_smoke_days == 0))

print(yearly_bin_counts %>% pivot_wider(names_from = bin, values_from = n_smoke_days))
print(pct_change %>% select(year, bin, pct_change) %>%
        pivot_wider(names_from = bin, values_from = pct_change))

X_AXIS_LABEL <- expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))

# Linear axis: every change is bounded below by -100%, so the 2022 zero in (200, 500] plots.
pct_change_plot <- ggplot(pct_change, aes(x = year, y = pct_change, color = bin)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_color_manual(X_AXIS_LABEL, values = BIN_COLORS) +
  scale_y_continuous(paste0("Percent Change Compared to\n", BASE_YEAR, " Levels of Smoke"),
                     breaks = seq(-1, 6, by = 1),
                     labels = scales::label_percent(style_positive = "plus")) +
  scale_x_continuous("Year", breaks = sort(unique(pct_change$year))) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  guides(color = guide_legend(nrow = 2))

dir.create(APPENDIX_FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
source(file.path(CODE_DIR, "2_analysis", "appendix", "smoke_annual_average_helpers.R"))
annual_average <- write_smoke_annual_average(yearly_bin_counts, BIN_LABELS, BIN_COLORS,
                                           APPENDIX_FIGURES_DIR)
ggsave(file.path(APPENDIX_FIGURES_DIR, "smoke_days_by_bin_pct_change_from_2020.pdf"),
       plot = pct_change_plot, width = 7, height = 5.5, dpi = 300, units = "in")
