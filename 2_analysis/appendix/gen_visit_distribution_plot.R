# Histogram of daily deferrable visits across balanced-panel county-days, on an asinh x-axis.
library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

full_df <- read_csv(
  DAILY_DATA_FILEPATH,
  col_select = c(county_geoid, date, balanced_panel, daily_visits_deferrable),
  show_col_types = FALSE
) 

full_df <- full_df %>% filter(balanced_panel)
grouped <- full_df %>% filter(date!=as_date('2023-01-01'))  %>% 
  group_by(county_geoid) %>% 
  summarize(had_visits = sum(daily_visits_deferrable>0), 
            had_some_non_visits=sum(daily_visits_deferrable==0),
            frac_zero_visits = had_some_non_visits/n())
over_50_zero <- grouped %>% filter(frac_zero_visits> 0.5) %>% pull(county_geoid)

lab_kmb <- function(x) {
  dplyr::case_when(
    is.na(x)      ~ NA_character_,
    abs(x) >= 1e6 ~ paste0(x / 1e6, "M"),
    abs(x) >= 1e3 ~ paste0(x / 1e3, "K"),
    TRUE          ~ as.character(x)
  )
}

(ggplot(
  full_df , 
  aes(x = daily_visits_deferrable)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  scale_x_continuous(
    transform = "asinh",
    breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = lab_kmb
  ) +
  scale_y_continuous(
    labels = lab_kmb
  ) +
  labs(
    x = "Daily deferrable visits (asinh scale)",
    y = "# County-Days"
  ) + theme_minimal(base_size=18) ) %>% 
ggsave(file.path(APPENDIX_FIGURES_DIR, "daily_deferrable_visits_distribution.pdf"),
       plot = ., width = 8, height = 5.5, dpi = 300, units = "in")

(ggplot(
  full_df %>% 
    filter(date!=as_date('2023-01-01'), !(county_geoid %in% over_50_zero)), 
  aes(x = daily_visits_deferrable)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  scale_x_continuous(
    transform = "asinh",
    breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = lab_kmb
  ) +
  scale_y_continuous(
    labels = lab_kmb
  ) +
  labs(
    x = "Daily deferrable visits (asinh scale)",
    y = "# County-Days"
  ) + theme_minimal(base_size=18)) %>% 
  ggsave(file.path(APPENDIX_FIGURES_DIR, "daily_deferrable_visits_distribution_filter_out_zero_visit_counties.pdf"),
         plot = ., width = 8, height = 5.5, dpi = 300, units = "in")


