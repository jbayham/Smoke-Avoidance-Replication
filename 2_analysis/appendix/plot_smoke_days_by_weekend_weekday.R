# Counts the county-days in each smoke PM bin that fall on a weekend versus a weekday.
# Weekends are only 2 of the 7 days in a week, so the raw counts alone cannot say whether
# smoke days are disproportionately weekend days, so the second figure rescales them into
# the weekend share of each bin, against the weekend share of all county-days in the panel.
library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

SMOKE_BINS <- names(BIN_LABELS)

full_df <- read_csv(
  DAILY_DATA_FILEPATH,
  col_select = c(county_geoid, date, is_weekend, balanced_panel, all_of(SMOKE_BINS)),
  show_col_types = FALSE
) %>%
  filter(balanced_panel)

day_type_totals <- full_df %>%
  count(is_weekend, name = "n_county_days")

bin_counts <- full_df %>%
  group_by(is_weekend) %>%
  summarize(across(all_of(SMOKE_BINS), ~sum(. == 1, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(all_of(SMOKE_BINS), names_to = "bin", values_to = "n_smoke_days") %>%
  left_join(day_type_totals, by = "is_weekend") %>%
  mutate(
    `Day Type` = factor(if_else(is_weekend, "Weekend", "Weekday"),
                        levels = c("Weekday", "Weekend")),
    bin = factor(BIN_LABELS[bin], levels = unname(BIN_LABELS)),
    share_of_days = n_smoke_days / n_county_days
  )

# Share of every county-day in the panel that is a weekend day: the null the weekend share
# of a bin's smoke days is compared against (2/7 when the panel holds whole weeks).
weekend_share_all_days <- with(day_type_totals, n_county_days[is_weekend] / sum(n_county_days))

weekend_share_by_bin <- bin_counts %>%
  select(bin, `Day Type`, n_smoke_days) %>%
  pivot_wider(names_from = `Day Type`, values_from = n_smoke_days) %>%
  mutate(n_smoke_days = Weekday + Weekend,
         weekend_share = Weekend / n_smoke_days)

print(bin_counts %>% select(`Day Type`, bin, n_smoke_days, n_county_days, share_of_days))
cat("Weekend share of all county-days:", scales::percent(weekend_share_all_days, accuracy = 0.1), "\n")
print(weekend_share_by_bin)

DAY_TYPE_COLORS <- c("Weekday" = "grey55", "Weekend" = "#1f78b4")

X_AXIS_LABEL <- expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))

base_bar <- function(df, y_var) {
  ggplot(df, aes(x = bin, y = .data[[y_var]], fill = `Day Type`)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_fill_manual(values = DAY_TYPE_COLORS) +
    labs(x = X_AXIS_LABEL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
}

# The bins span four orders of magnitude, so the counts are on a log scale: on a linear
# axis every bin above (15, 30] is a flat line against the (0, 15] bar.
count_plot <- base_bar(bin_counts, "n_smoke_days") +
  geom_text(aes(label = scales::comma(n_smoke_days)),
            position = position_dodge(width = 0.75), vjust = -0.4, size = 2.7) +
  scale_y_log10("County-Days in Bin", labels = scales::label_comma(),
                expand = expansion(mult = c(0, 0.18)))

share_plot <- ggplot(weekend_share_by_bin, aes(x = bin, y = weekend_share)) +
  geom_col(fill = "grey55", width = 0.6) +
  geom_hline(yintercept = weekend_share_all_days, linetype = 2, color = "#1f78b4",
             linewidth = 0.8) +
  annotate("text", x = 0.55, y = weekend_share_all_days, hjust = 0, vjust = -1.1, size = 3.2,
           color = "#1f78b4",
           label = paste0("All county-days: ",
                          scales::percent(weekend_share_all_days, accuracy = 0.1))) +
  geom_text(aes(label = scales::percent(weekend_share, accuracy = 0.1)),
            vjust = -0.5, size = 2.7) +
  scale_y_continuous("Weekend Share of Bin's Smoke Days",
                     labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = X_AXIS_LABEL) +
  theme_minimal(base_size = 13)

dir.create(APPENDIX_FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(APPENDIX_FIGURES_DIR, "smoke_days_by_weekend_weekday_counts.pdf"),
       plot = count_plot, width = 7, height = 5, dpi = 300, units = "in")
ggsave(file.path(APPENDIX_FIGURES_DIR, "smoke_days_by_weekend_weekday_weekend_share.pdf"),
       plot = share_plot, width = 7, height = 5, dpi = 300, units = "in")
