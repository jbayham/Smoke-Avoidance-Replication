library(tidyverse)
library(sf)
library(scales)
library(glue)
library(cowplot)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# --- Load county shapefile (CONUS only) ---
counties_sf <- get_county_shapefiles(filter_for_CONUS = TRUE, low_resolution=TRUE)

# --- Load county-level data ---
main_vars <- c("county_geoid", "pm_background_mean", "median_household_income",
               "natural_amenity_tourism_measure", "percent_gdp_from_rec",
               "percent_comp_from_rec", "cbp_outdoor_share_estab", "cbp_outdoor_share_payroll",
               "cbp_hhi_estab", "cbp_hhi_payroll", "rurality_index")

county_data <- read_csv(COUNTY_INFO_COMBINED_PATH,
                        col_select = all_of(main_vars),
                        col_types = cols(county_geoid = col_character(),
                                         .default = col_double()),
                        show_col_types = FALSE) %>%
  distinct(county_geoid, .keep_all = TRUE)

# Join to shapefile
map_data <- counties_sf %>%
  left_join(county_data, by = c("GEOID" = "county_geoid"))

# --- Quartile breaks ---
# The cut points come straight from the county file, the same source and the same
# probabilities that assign_quartiles() in figure_1_2_day_of_analysis_run.R uses, so the
# boundaries drawn on the colorbar are the quartiles the heterogeneity regressions are run
# on. Where a mass point makes two cut points coincide (the outdoor shares are zero for more
# than half of the counties) only the last of the tied cut points is kept, which is the
# quartile label that survives the merge there.
compute_quartile_breaks <- function(values) {
  brk  <- quantile(values, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  keep <- rep(TRUE, length(brk))
  for (i in seq(2, length(brk) - 1)) keep[i] <- brk[i] != brk[i + 1]
  brk[keep]
}

# --- Variable specifications ---
var_specs <- list(
  list(var = "pm_background_mean",
       title = "Background PM2.5",
       legend_title = "PM[2.5]~(mu*g/m^3)",
       parse_title = TRUE,
       label_fn = label_number(accuracy = 0.1),
       filename = "map_background_pm.pdf"),

  list(var = "median_household_income",
       title = "Median Household Income",
       legend_title = "Income",
       label_fn = label_dollar(scale = 1e-3, suffix = "K", accuracy = 1),
       filename = "map_income.pdf"),

  list(var = "natural_amenity_tourism_measure",
       title = "Natural Amenity Tourism",
       legend_title = "Index",
       label_fn = label_number(accuracy = 0.01, drop0trailing = TRUE),
       filename = "map_nat_amenity.pdf"),

  list(var = "percent_gdp_from_rec",
       title = "% GDP from Recreation",
       legend_title = "Share of GDP",
       label_fn = label_percent(accuracy = 0.1),
       filename = "map_gdp_rec.pdf"),

  list(var = "percent_comp_from_rec",
       title = "% Compensation from Recreation",
       legend_title = "Share of Comp.",
       label_fn = label_percent(accuracy = 0.1),
       filename = "map_comp_rec.pdf"),

  list(var = "cbp_outdoor_share_estab",
       title = "Outdoor Share of Establishments",
       legend_title = "Share (sqrt scale)",
       label_fn = label_percent(accuracy = 0.1),
       filename = "map_cbp_outdoor_estab.pdf",
       trans = "sqrt"),

  list(var = "cbp_outdoor_share_payroll",
       title = "Outdoor Share of Payroll",
       legend_title = "Share (sqrt scale)",
       label_fn = label_percent(accuracy = 0.1),
       filename = "map_cbp_outdoor_payroll.pdf",
       trans = "sqrt"),

  list(var = "cbp_hhi_estab",
       title = "HHI of Establishments",
       legend_title = "HHI",
       label_fn = label_number(accuracy = 0.01),
       filename = "map_cbp_hhi_estab.pdf"),

  list(var = "cbp_hhi_payroll",
       title = "HHI of Payroll",
       legend_title = "HHI",
       label_fn = label_number(accuracy = 0.01),
       filename = "map_cbp_hhi_payroll.pdf"),

  list(var = "rurality_index",
       title = "Rurality",
       legend_title = "CARR Index",
       label_fn = label_number(accuracy = 0.01),
       filename = "map_rurality.pdf")
)

# --- Helper: push 1D positions apart so none are closer than min_gap ---
stagger_1d <- function(pos, min_gap = 0.10) {
  if (length(pos) <= 1) return(pos)
  ord <- order(pos)
  p   <- pos[ord]
  n   <- length(p)
  for (iter in seq_len(60)) {
    moved <- FALSE
    for (i in seq(2, n)) {
      if (p[i] - p[i - 1] < min_gap) {
        p[i] <- p[i - 1] + min_gap
        moved <- TRUE
      }
    }
    # If we pushed the top above 1, slide everything down
    if (p[n] > 1) p <- p - (p[n] - 1)
    # If that pushed the bottom below 0, slide back up
    if (p[1] < 0) p <- p - p[1]
    if (!moved) break
  }
  pos[ord] <- p
  pos
}

# --- Helper: build a custom colorbar ggplot for cowplot overlay ---
make_legend_plot <- function(quartile_breaks, label_fn, legend_title,
                             trans = "identity", parse_title = FALSE) {
  pct_map   <- c("25%" = "P25", "50%" = "P50", "75%" = "P75")
  brk_names <- names(quartile_breaks)
  brk_vals  <- unname(quartile_breaks)
  q_min     <- quartile_breaks[["0%"]]
  q_max     <- quartile_breaks[["100%"]]

  # Transformation to bar position [0, 1]
  tfm    <- as.transform(trans)
  t_min  <- tfm$transform(q_min)
  t_max  <- tfm$transform(q_max)
  t_rng  <- max(t_max - t_min, .Machine$double.eps)
  to_bar <- function(v) pmax(0, pmin(1, (tfm$transform(v) - t_min) / t_rng))

  # Build label text for each break: "value Pxx" or just "value"
  labels_raw <- character(length(brk_vals))
  for (i in seq_along(brk_vals)) {
    val_str <- label_fn(brk_vals[i])
    annot   <- pct_map[brk_names[i]]
    labels_raw[i] <- if (!is.na(annot)) paste(val_str, annot) else val_str
  }

  # Deduplicate by bar position (keep label with Pxx annotation when present)
  bar_pos_raw <- to_bar(brk_vals)
  rounded     <- round(bar_pos_raw, 4)
  keep_idx    <- integer(0)
  for (upos in unique(rounded)) {
    cands <- which(rounded == upos)
    has_p <- grepl("P\\d", labels_raw[cands])
    keep_idx <- c(keep_idx, cands[if (any(has_p)) which(has_p)[1] else 1])
  }
  bar_pos <- bar_pos_raw[keep_idx]
  labels  <- labels_raw[keep_idx]

  # Stagger label positions so nothing overlaps
  lab_pos <- stagger_1d(bar_pos, min_gap = 0.12)

  # Gradient data for the colorbar. Height on the bar is already the rescaled position in
  # transformed space, which is exactly what the map's fill scale maps onto the palette, so
  # colour the bar by its height rather than by the value it stands for: colouring by value
  # would leave the bar linear in the value while the map is linear in the transform.
  n_steps  <- 400
  grad_raw <- seq(0, 1, length.out = n_steps)
  bar_df   <- data.frame(x = 0.1, y = grad_raw, fill = grad_raw)

  # x coords (data units within the legend plot)
  bar_xmax  <- 0.20
  tick_x    <- 0.27   # end of horizontal tick from bar
  label_x   <- 0.30   # text starts here

  tick_df    <- data.frame(bar_y = bar_pos, lab_y = lab_pos, label = labels)
  quartile_df <- tick_df[grepl("P\\d", tick_df$label), ]

  ggplot() +
    # Gradient bar
    geom_tile(data = bar_df, aes(x = x, y = y, fill = fill),
              width = bar_xmax, height = 1.5 / n_steps) +
    scale_fill_viridis_c(limits = c(0, 1), guide = "none", oob = squish) +
    # Bar frame
    annotate("rect", xmin = 0, xmax = bar_xmax, ymin = 0, ymax = 1,
             fill = NA, color = "grey30", linewidth = 0.4) +
    # Dashed lines across the bar at each quartile boundary (same style as connectors)
    geom_segment(data = quartile_df,
                 aes(x = 0, xend = bar_xmax, y = bar_y, yend = bar_y),
                 color = "grey40", linewidth = 0.35, linetype = "dashed") +
    # Horizontal tick from bar edge to tick_x
    geom_segment(data = tick_df,
                 aes(x = bar_xmax, xend = tick_x, y = bar_y, yend = bar_y),
                 color = "grey20", linewidth = 0.45) +
    # Connector from tick_x at bar_y to label_x at lab_y (diagonal when staggered)
    geom_segment(data = tick_df,
                 aes(x = tick_x, xend = label_x - 0.01, y = bar_y, yend = lab_y),
                 color = "grey40", linewidth = 0.35, linetype = "dashed") +
    # Labels to the right
    geom_text(data = tick_df,
              aes(x = label_x, y = lab_y, label = label),
              hjust = 0, vjust = 0.4, size = 6.5 / .pt, color = "grey10") +
    # Legend title above bar
    annotate("text",
             x = bar_xmax / 2, y = 1.11,
             label = legend_title,
             hjust = 0.5, vjust = 0,
             size = 7.5 / .pt, fontface = "bold", color = "grey10",
             parse = parse_title) +
    coord_cartesian(xlim = c(-0.02, 1.35), ylim = c(-0.06, 1.20), clip = "off") +
    theme_void()
}

# --- Main map function (map + cowplot legend overlay) ---
make_county_map <- function(sf_data, var_name, map_title, legend_title,
                            label_fn, quartile_breaks, trans = "identity",
                            parse_title = FALSE) {
  q_min <- quartile_breaks[["0%"]]
  q_max <- quartile_breaks[["100%"]]

  p_map <- ggplot(sf_data) +
    geom_sf(aes(fill = .data[[var_name]]), lwd = 0, color = NA) +
    scale_fill_viridis_c(
      limits    = c(q_min, q_max),
      oob       = squish,
      transform = trans,
      na.value  = "grey85",
      guide     = "none"
    ) +
    labs(title = map_title) +
    theme_void(base_size = 14) +
    theme(
      plot.title  = element_text(hjust = 0.5, face = "bold", size = 16,
                                 margin = margin(b = 5)),
      plot.margin = margin(5, 5, 5, 5)
    )

  p_legend <- make_legend_plot(quartile_breaks, label_fn, legend_title,
                               trans = trans, parse_title = parse_title)

  # Overlay legend bottom-left (x, y = bottom-left corner; width/height in normalized units)
  # Figure is 10x7 in. Legend ~1.3 in wide, ~2.0 in tall -> 0.130 x 0.286 normalized.
  # y=0.036 places legend bottom ~0.25 in from figure bottom edge.
  ggdraw(p_map) +
    draw_plot(p_legend, x = 0.115, y = 0.072, width = 0.130, height = 0.286)
}

# --- Generate and save all maps ---
for (spec in var_specs) {
  cat(glue("Generating map for {spec$var}...\n\n"))

  breaks      <- compute_quartile_breaks(county_data[[spec$var]])
  fill_trans  <- if (!is.null(spec$trans)) spec$trans else "identity"
  parse_title <- isTRUE(spec$parse_title)

  p <- make_county_map(
    sf_data         = map_data,
    var_name        = spec$var,
    map_title       = spec$title,
    legend_title    = spec$legend_title,
    label_fn        = spec$label_fn,
    quartile_breaks = breaks,
    trans           = fill_trans,
    parse_title     = parse_title
  )

  ggsave(
    file.path(APPENDIX_FIGURES_DIR, spec$filename),
    plot  = p,
    width = 10, height = 7, dpi = 300, units = "in"
  )
  cat(glue("  Saved {spec$filename}\n\n"))
}

cat("\nAll heterogeneity maps complete!\n")
