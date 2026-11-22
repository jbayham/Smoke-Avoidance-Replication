# Render Figure 1(a) from the same balanced-panel year/bin counts as Figure 1(b).
# Zero-count year/bin cells must be present: the denominator is all five years.
write_smoke_annual_average <- function(yearly_bin_counts, bin_labels, bin_colors,
                                      output_dir, years = 2019:2023) {
  counts <- as.data.frame(yearly_bin_counts)
  counts$bin <- as.character(counts$bin)
  labels <- unname(bin_labels)
  expected <- expand.grid(year = years, bin = labels, stringsAsFactors = FALSE)
  keys <- paste(counts$year, counts$bin)
  stopifnot(!anyDuplicated(keys),
            setequal(keys, paste(expected$year, expected$bin)),
            all(is.finite(counts$n_smoke_days)),
            all(counts$n_smoke_days >= 0),
            all(counts$n_smoke_days == floor(counts$n_smoke_days)))
  annual_average <- data.frame(
    bin = factor(labels, levels = labels),
    average_county_days_per_year = vapply(labels, function(bin) {
      sum(counts$n_smoke_days[counts$bin == bin]) / length(years)
    }, numeric(1))
  )
  stopifnot(all(annual_average$average_county_days_per_year > 0))
  annual_average$printed_count <- round(annual_average$average_county_days_per_year)
  p <- ggplot2::ggplot(annual_average,
      ggplot2::aes(x = bin, y = average_county_days_per_year, fill = bin)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(printed_count, accuracy = 1)),
                       vjust = -0.35, size = 3) +
    ggplot2::scale_fill_manual(values = bin_colors, guide = "none") +
    ggplot2::scale_y_log10("Average County-Days per Year", breaks = c(1, 100, 10000),
                          labels = scales::label_comma(),
                          expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(output_dir, "smoke_days_by_bin_yearly_average.pdf"),
                 plot = p, width = 7, height = 5.5, units = "in", dpi = 300)
  return(annual_average)
}
