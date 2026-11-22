library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# Counties without fire coverage get NA on viirs vars; treat as no fire.
full_df <- read_csv(DAILY_DATA_FILEPATH, show_col_types = FALSE,
                    col_select = c('balanced_panel', 'viirs_in_perimeter', 'viirs_total')) %>%
  filter(balanced_panel)

############### VIIRS fire-point distributions, total vs confirmed #############

# viirs_in_perimeter is a subset of viirs_total; the rest fall outside any
# confirmed perimeter. One palette drives every panel and both insets, so a
# colour means the same thing everywhere.
fire_fill <- c(
  "No fire"                = "grey75",
  "All fire points"        = "grey35",
  "Unconfirmed"            = "goldenrod",
  "In confirmed perimeter" = "firebrick"
)
fire_levels   <- names(fire_fill)
series_levels <- c("All VIIRS fire points", "Points in a confirmed perimeter")

# --- shared pie builder -------------------------------------------------------
# Percentages ride in the legend labels: the smallest wedge here is 0.4%, far
# too thin to hold text of its own.
make_share_pie <- function(df, title, show_legend) {
  df <- df %>%
    mutate(category = factor(category, levels = fire_levels)) %>%
    arrange(category) %>%
    mutate(
      share = n / sum(n),
      ymax  = cumsum(share),
      ymin  = lag(ymax, default = 0),
      ymid  = (ymin + ymax) / 2
    )

  labs_with_share <- setNames(
    sprintf("%s (%s)", df$category, scales::percent(df$share, accuracy = 0.1)),
    as.character(df$category)
  )

  p <- ggplot(df, aes(x = 1, y = share, fill = category)) +
    geom_col(width = 1, color = "white", linewidth = 0.2,
             position = position_stack(reverse = TRUE)) +
    coord_polar(theta = "y", clip = "off") +
    scale_fill_manual(values = fire_fill, labels = labs_with_share, drop = TRUE) +
    scale_x_continuous(limits = c(0.4, 1.6), oob = scales::oob_keep) +
    labs(title = title) +
    theme_void(base_size = 14) +
    theme(
      plot.title      = element_text(size = 12, face = "bold", hjust = -0.5,
                                     margin = margin(b = 9)),
      plot.margin     = margin(5, 7, 3, 7),
      plot.background = element_rect(fill = "white", color = "grey80", linewidth = 0.3)
    )

  if (show_legend) {
    p <- p + theme(
      legend.position  = "right",
      legend.direction = "vertical",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 10),
      legend.key.size  = unit(0.42, "cm"),
      legend.margin    = margin(l = -4)
    )
  } else {
    p <- p + guides(fill = "none")
  }

  p
}

# --- inset 1: every county-day, split three ways ------------------------------
share_all <- full_df %>% filter(balanced_panel) %>% 
  transmute(category = case_when(
    is.na(viirs_total) | viirs_total == 0                ~ "No fire",
    !is.na(viirs_in_perimeter) & viirs_in_perimeter > 0  ~ "In confirmed perimeter",
    TRUE                                                 ~ "Unconfirmed"
  )) %>%
  count(category)

inset_all <- make_share_pie(share_all, "County-days in sample", show_legend = TRUE)

# --- inset 2: of all VIIRS fire points, how many fall inside a perimeter ------
# Note the unit switch: this pie counts fire *points*, not county-days, so its
# title says so. 37.8% of points but only 4.8% of fire county-days are
# confirmed -- confirmed fires are rare days that carry a lot of detections.
share_points <- full_df %>%
  filter(!is.na(viirs_total)) %>%
  summarise(
    `In confirmed perimeter` = sum(coalesce(viirs_in_perimeter, 0)),
    Unconfirmed              = sum(viirs_total - coalesce(viirs_in_perimeter, 0))
  ) %>%
  pivot_longer(everything(), names_to = "category", values_to = "n")

inset_fire <- make_share_pie(share_points, "VIIRS fire points", show_legend = TRUE)

# --- faceted histograms -------------------------------------------------------
# Panel 1 bins each fire county-day by its total points; panel 2 re-bins the
# confirmed days by their confirmed-point count, so free_y lets its much smaller
# counts show shape. Firebrick means "in a confirmed perimeter" in both the
# right panel and the insets.
hist_df <- bind_rows(
  full_df %>%
    filter(!is.na(viirs_total), viirs_total > 0) %>%
    transmute(series     = series_levels[1],
              n_points   = viirs_total,
              point_type = "All fire points"),
  full_df %>%
    filter(!is.na(viirs_in_perimeter), viirs_in_perimeter > 0) %>%
    transmute(series     = series_levels[2],
              n_points   = viirs_in_perimeter,
              point_type = "In confirmed perimeter")
) %>%
  mutate(series     = factor(series, levels = series_levels),
         point_type = factor(point_type, levels = fire_levels))

log_breaks <- c(1, 3, 10, 30, 100, 300, 1000, 3000)

viirs_hist <- ggplot(hist_df) +
  geom_histogram(aes(x = n_points, fill = point_type), bins = 30,
                 color = "white", linewidth = 0.15) +
  facet_wrap(~series, scales = "free_y") +
  scale_x_continuous(transform = "log", breaks = log_breaks,
                     labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_fill_manual(values = fire_fill, drop = FALSE) +
  guides(fill = "none") +   # the inset legend keys the whole figure
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 13)) +
  labs(y = '# county-days', x = 'VIIRS fire points per county-day')

# Place each pie in its own panel. annotation_custom spans every panel by
# default, so swap the layer's data for a one-row frame naming the facet, and
# read the placement off that panel's built ranges (x already log-transformed).
built       <- ggplot_build(viirs_hist)
panel_index <- setNames(built$layout$layout$PANEL, built$layout$layout$series)

inset_layer <- function(grob, series_value, xlo, xhi, ylo, yhi) {
  pp    <- built$layout$panel_params[[panel_index[[series_value]]]]
  at    <- function(rng, lo, hi) rng[1] + c(lo, hi) * diff(rng)
  x_pos <- at(pp$x.range, xlo, xhi)
  y_pos <- at(pp$y.range, ylo, yhi)
  lyr   <- annotation_custom(grob, xmin = x_pos[1], xmax = x_pos[2],
                             ymin = y_pos[1], ymax = y_pos[2])
  lyr$data <- tibble(series = factor(series_value, levels = series_levels))
  lyr
}

viirs_hist_with_insets <- viirs_hist +
  inset_layer(ggplotGrob(inset_all),  series_levels[1], 0.30, 1.00, 0.50, 1.00) +
  inset_layer(ggplotGrob(inset_fire), series_levels[2], 0.30, 1.00, 0.50, 1.00)

ggsave(file.path(APPENDIX_FIGURES_DIR, "viirs_fire_point_distribution.pdf"),
       plot = viirs_hist_with_insets, width = 12, height = 5, dpi = 300, units = "in")


