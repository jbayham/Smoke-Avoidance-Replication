# Heterogeneity in the day-of smoke effect, by quartile of the county characteristics
# (and by weekend). Produces two figures:
#   1. levels      -- the effect for each group, quartile 1 taken from the smoke bin
#                     coefficients and quartiles 2-4 adding their interaction term to it
#   2. differences -- the interaction terms on their own, i.e. quartiles 2-4 relative to
#                     quartile 1 and weekend relative to weekday
# Both are drawn for each of the two specifications that
# figure_1_2_day_of_analysis_run.R fits: the plain day fixed effect and the day-by-quartile
# fixed effect. Every regression is read from REGRESSION_RESULTS_DIR up front, so the script
# runs the same way whatever happens to be in the session already. For the plain day fixed
# effect, each characteristic is also drawn on a figure of its own for slides, in
# figures/for_presentation.

library(tidyverse)
library(fixest)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# The two specifications, drawn one after the other. `output_suffix` is what the run script
# appended to the saved regressions and is carried through to the figure names. Everything
# from the day-by-quartile specification is appendix material, hence `appendix_only`; where
# each figure lands is otherwise decided per figure in save_figure().
SPECIFICATION_VARIANTS <- list(
  list(output_suffix = "",
       description   = "day fixed effect",
       appendix_only = FALSE),
  list(output_suffix = "_quantile_day_interaction_FE",
       description   = "day-by-quartile fixed effect",
       appendix_only = TRUE)
)

smoke_bins <- names(BIN_LABELS)

# Shape carries the quartile. The overall effect and the weekday/weekend effects are not
# quartiles of anything, so they share one shape that is kept out of the legend.
QUARTILE_SHAPES <- c("Overall" = 16, "Q1" = 16, "Q1+Q2" = 1,
                     "Q2" = 17, "Q3" = 15, "Q4" = 18)

# Line type separates weekend from weekday; every other regression pools all days
DAY_TYPE_LINETYPES <- c("All days" = "solid", "Weekday" = "dotted", "Weekend" = "dashed")

# Each entry is one regression: `label` is both the prefix of its quartile dummies and the
# stem of the file it was saved to, `sample` is the facet label
heterogeneity_specs <- list(
  list(label = "background_pm",       sample = "Background PM"),
  list(label = "income",              sample = "Median Household Income"),
  list(label = "nat_amenity",         sample = "Natural Amenity Tourism"),
  list(label = "gdp_rec",             sample = "% GDP from Recreation"),
  list(label = "comp_rec",            sample = "% Compensation from Recreation"),
  list(label = "cbp_outdoor_estab",   sample = "CBP Outdoor Share (Estab)"),
  list(label = "cbp_outdoor_payroll", sample = "CBP Outdoor Share (Payroll)"),
  list(label = "cbp_hhi_estab",       sample = "CBP HHI (Estab)"),
  list(label = "cbp_hhi_payroll",     sample = "CBP HHI (Payroll)"),
  list(label = "rurality",            sample = "Rurality Index")
)

# --- Every regression the figures need, loaded once ------------------------------------
# The saved fits run to about 4 GB apiece and there are 21 of them, so they cannot all be
# held as they are. Nothing below uses more of a fit than its coefficient vector and that
# vector's variance-covariance matrix -- a few hundred numbers each -- so a file is read,
# reduced to that pair, and released. Past this point the script touches no files: it reads
# fits out of REGRESSIONS, keyed by characteristic and specification.
regression_key <- function(label, variant) paste0(label, variant$output_suffix)

load_regression <- function(file_name) {
  cat("Loading", file_name, "\n")
  est <- readRDS(file.path(REGRESSION_RESULTS_DIR, file_name))
  fit <- list(betas = coef(est), vcov = vcov(est))
  rm(est)
  invisible(gc(verbose = FALSE))
  fit
}

REGRESSIONS <- list()
# Weekend status is determined by date. It has no distinct date-by-group
# robustness specification, so its fit appears only in the main specification.
REGRESSIONS[["weekend"]] <- load_regression("binary_weekend_interaction.rds")
for (variant in SPECIFICATION_VARIANTS) {
  for (spec in heterogeneity_specs) {
    REGRESSIONS[[regression_key(spec$label, variant)]] <- load_regression(
      paste0("binary_quartile_", spec$label, "_interaction", variant$output_suffix, ".rds")
    )
  }
}
cat("Loaded", length(REGRESSIONS), "regressions\n")

# fixest names an interaction "bin:var" or "var:bin" depending on which variable turns up
# first in the formula, so accept either order. An interaction can also be missing outright
# when no county-day in that group falls in that smoke bin (the top bin never happens in
# some groups), in which case there is nothing to plot for that cell.
interaction_coefficient <- function(coefficient_names, bin, suffix) {
  candidates <- c(paste0(bin, ":", suffix), paste0(suffix, ":", bin))
  match <- candidates[candidates %in% coefficient_names]
  if (length(match) == 0) {
    cat("  no", bin, "x", suffix, "interaction was estimated; leaving that point out\n")
    return(NA_character_)
  }
  match[1]
}

# Effect for each group: the bin coefficient on its own is the baseline group, every other
# group adds its interaction term (with the variance of the sum, not of the interaction)
extract_group_levels <- function(fit, suffixes, group_labels, baseline_label, sample_label) {
  betas <- fit$betas
  V <- fit$vcov

  map_dfr(smoke_bins, function(bin) {
    baseline <- tibble(
      bin = bin, Group = baseline_label,
      Estimate = betas[[bin]], Std..Error = sqrt(V[bin, bin])
    )
    interactions <- map_dfr(seq_along(suffixes), function(i) {
      term <- interaction_coefficient(names(betas), bin, suffixes[i])
      if (is.na(term)) {
        return(tibble(bin = bin, Group = group_labels[i],
                      Estimate = NA_real_, Std..Error = NA_real_))
      }
      tibble(
        bin = bin, Group = group_labels[i],
        Estimate = betas[[bin]] + betas[[term]],
        Std..Error = sqrt(V[bin, bin] + V[term, term] + 2 * V[bin, term])
      )
    })
    bind_rows(baseline, interactions)
  }) %>% mutate(Sample = sample_label)
}

# The interaction terms on their own: each group relative to the baseline group
extract_group_differences <- function(fit, suffixes, group_labels, sample_label) {
  betas <- fit$betas
  V <- fit$vcov

  map_dfr(smoke_bins, function(bin) {
    map_dfr(seq_along(suffixes), function(i) {
      term <- interaction_coefficient(names(betas), bin, suffixes[i])
      if (is.na(term)) {
        return(tibble(bin = bin, Group = group_labels[i],
                      Estimate = NA_real_, Std..Error = NA_real_))
      }
      tibble(
        bin = bin, Group = group_labels[i],
        Estimate = betas[[term]], Std..Error = sqrt(V[term, term])
      )
    })
  }) %>% mutate(Sample = sample_label)
}

# The weekend model estimates two group slopes directly, without a standalone
# smoke-bin coefficient. Its contrast is weekend minus weekday, with covariance.
extract_weekend_estimates <- function(fit) {
  betas <- fit$betas
  V <- fit$vcov
  levels <- list()
  differences <- list()
  for (bin in smoke_bins) {
    weekday <- interaction_coefficient(names(betas), bin, "is_weekendFALSE")
    weekend <- interaction_coefficient(names(betas), bin, "is_weekendTRUE")
    terms <- c(weekday, weekend)
    levels[[bin]] <- tibble(
      bin = bin, Group = c("Weekday", "Weekend"),
      Estimate = vapply(terms, function(term) {
        if (is.na(term)) NA_real_ else betas[[term]]
      }, numeric(1), USE.NAMES = FALSE),
      Std..Error = vapply(terms, function(term) {
        if (is.na(term)) NA_real_ else sqrt(V[term, term])
      }, numeric(1), USE.NAMES = FALSE)
    )
    available <- !anyNA(terms)
    differences[[bin]] <- tibble(
      bin = bin, Group = "Weekend",
      Estimate = if (available) betas[[weekend]] - betas[[weekday]] else NA_real_,
      Std..Error = if (available) {
        sqrt(V[weekend, weekend] + V[weekday, weekday] - 2 * V[weekend, weekday])
      } else NA_real_
    )
  }
  list(levels = bind_rows(levels) %>% mutate(Sample = "Weekend"),
       differences = bind_rows(differences) %>% mutate(Sample = "Weekend"))
}

# Quartiles actually estimated for a characteristic. Quartiles with tied cutoffs were
# merged into the baseline group, so a variable can come back with only Q3 and Q4.
estimated_quartiles <- function(fit, label) {
  found <- str_extract(names(fit$betas), paste0(label, "_Q\\d"))
  sort(unique(found[!is.na(found)]))
}

# All the coefficients one specification needs, pulled out of the loaded regressions: the
# levels and the differences, one row per smoke bin x group x characteristic.
collect_group_estimates <- function(variant) {
  levels_dfs <- list()
  difference_dfs <- list()

  if (!variant$appendix_only) {
    weekend_estimates <- extract_weekend_estimates(REGRESSIONS[["weekend"]])
    levels_dfs[["weekend"]] <- weekend_estimates$levels
    difference_dfs[["weekend"]] <- weekend_estimates$differences
  }

  for (spec in heterogeneity_specs) {
    cat("Extracting", spec$sample, "\n")
    fit <- REGRESSIONS[[regression_key(spec$label, variant)]]

    suffixes <- estimated_quartiles(fit, spec$label)
    group_labels <- str_extract(suffixes, "Q\\d")
    # whatever is not estimated separately sits in the omitted baseline group
    baseline_label <- paste(setdiff(paste0("Q", 1:4), group_labels), collapse = "+")

    levels_dfs[[spec$label]] <- extract_group_levels(
      fit, suffixes, group_labels, baseline_label, spec$sample
    )
    difference_dfs[[spec$label]] <- extract_group_differences(
      fit, suffixes, group_labels, spec$sample
    )
  }

  list(levels = levels_dfs, differences = difference_dfs)
}

sample_levels <- c("Weekend", map_chr(heterogeneity_specs, "sample"))

prepare_plot_data <- function(dfs) {
  bind_rows(dfs) %>%
    filter(!is.na(Estimate)) %>%
    mutate(
      # The translation between % change and the estimate is approximate,
      # The line below changes the estimates to exact percent change.
      low_ci = Estimate - 1.96 * Std..Error,
      high_ci = Estimate + 1.96 * Std..Error,
      Estimate = exp(Estimate) - 1,
      low_ci   = exp(low_ci) - 1,
      high_ci  = exp(high_ci) - 1,
      bin = factor(BIN_LABELS[bin], levels = unname(BIN_LABELS)),
      Quartile = factor(if_else(Group %in% c("Overall", "Weekday", "Weekend"),
                                "Overall", Group),
                        levels = names(QUARTILE_SHAPES)),
      DayType = factor(if_else(Group %in% c("Weekday", "Weekend"), Group, "All days"),
                       levels = names(DAY_TYPE_LINETYPES)),
      Sample = factor(Sample, levels = sample_levels)
    )
}

# Move the legend into the empty facet cell, so it sits inside the grid instead of taking a
# column of its own on the right. Placing it by normalised coordinates would only be roughly
# centred -- with free y scales every panel carries its own axis, so the facet columns are
# not evenly spaced -- hence the legend grob is lifted out of the assembled plot and dropped
# straight into the vacant cell of its gtable. Plots whose panels fill the grid exactly (the
# main levels figure) come back untouched, legend still on the right.
# Returns a gtable rather than a ggplot, so do this last: ggsave takes it, but `+` does not.
#
# h_shift / v_shift nudge the legend away from the centre of that cell: negative moves it
# left and down, positive right and up. Bare numbers are fractions of the cell ("npc"), so
# -0.1 is a tenth of the cell's width or height; pass a unit() (e.g. unit(-1, "cm")) for an
# absolute nudge instead. Clipping is off, so the legend may hang outside the cell.
LEGEND_H_SHIFT <- -0.05
LEGEND_V_SHIFT <- -0.12
# How the stacked guides line up with each other inside the box, once the box sits in a facet
# cell: "center", "left", "right". Only used for plots that have a vacancy to fill -- against
# the right hand edge the default left alignment is the tidier of the two.
LEGEND_BOX_JUST <- "center"

place_legend_in_vacant_facet <- function(plot,
                                         h_shift = LEGEND_H_SHIFT,
                                         v_shift = LEGEND_V_SHIFT,
                                         box_just = LEGEND_BOX_JUST) {
  g <- ggplotGrob(plot)

  # facet_wrap still lays out a cell for the vacancy, it just fills it with an empty grob
  panels <- which(grepl("^panel", g$layout$name))
  vacant <- panels[map_lgl(g$grobs[panels], ~ inherits(.x, "zeroGrob"))]
  if (length(vacant) == 0) return(plot)

  # the two guides are stacked in one box and left-aligned by default, so the wider "Day of
  # Week" guide sticks out to the right of the narrower "Quartile" one: centring the box
  # justification lines their midpoints up, which is what lets the pair sit square in the
  # cell. It only makes sense for the legend once it is inside the grid, hence re-assembling
  # the grob here rather than asking every plot to carry the setting.
  g <- ggplotGrob(plot + theme(legend.box.just = box_just))

  legend_cell <- which(g$layout$name == "guide-box-right")
  legend <- g$grobs[[legend_cell]]
  g$grobs[[legend_cell]] <- zeroGrob()

  # reclaim the width the legend used to take, along with the spacing column in front of it
  legend_column <- g$layout$l[legend_cell]
  g$widths[legend_column] <- unit(0, "cm")
  if (!any(g$layout$l == legend_column - 1)) g$widths[legend_column - 1] <- unit(0, "cm")

  # wrap rather than edit the legend's own viewport: the guide box sets one of its own, and
  # nesting keeps whatever it does inside while shifting the whole thing bodily
  as_shift <- function(x) if (grid::is.unit(x)) x else unit(x, "npc")
  legend <- grid::grobTree(
    legend,
    vp = grid::viewport(x = unit(0.5, "npc") + as_shift(h_shift),
                        y = unit(0.5, "npc") + as_shift(v_shift))
  )

  gtable::gtable_add_grob(g, legend, clip = "off",
                          t = g$layout$t[vacant[1]], l = g$layout$l[vacant[1]],
                          name = "guide-box-vacant-facet")
}





heterogeneity_plot <- function(plot_data, y_label, ncol = 3) {
  # group by both aesthetics so the error bars and the points dodge together
  ggplot(plot_data, aes(x = bin, y = Estimate, color = bin, ymin = low_ci, ymax = high_ci,
                        group = interaction(Quartile, DayType))) +
    geom_hline(yintercept = 0, color = "darkgrey") +
    
    geom_linerange(aes(linetype = DayType), position = position_dodge(width = 0.6),
                   width = 0.3, linewidth = 1) +
    geom_errorbar(aes(ymax = low_ci), position = position_dodge(width = 0.6),
                  width = 0.5, linewidth = 1) +
    geom_errorbar(aes(ymin = high_ci), position = position_dodge(width = 0.6),
                  width = 0.5, linewidth = 1) +
    geom_point(aes(shape = Quartile), position = position_dodge(width = 0.6), size = 3) +
    facet_wrap(~Sample, scales = "free_y", ncol = ncol) +
    scale_color_manual("Smoke PM AQI\nBin equivalent",
                       values = BIN_COLORS, labels = BIN_AQI_LABELS) +
    scale_shape_manual("Quartile", values = QUARTILE_SHAPES,
                       breaks = setdiff(names(QUARTILE_SHAPES), "Overall")) +
    # only legend the day types that actually turn up: the difference figure has the weekend
    # interaction on its own, with no weekday effect to show alongside it
    scale_linetype_manual("Day of Week", values = DAY_TYPE_LINETYPES,
                          breaks = intersect(c("Weekday", "Weekend"),
                                             as.character(plot_data$DayType))) +
    scale_y_continuous(y_label, labels = scales::label_percent()) +
    labs(x = expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))) +
    theme_minimal(base_size = 24) +
    # the two guides are stacked in one box and left-aligned by default, so the wider "Day of
    # Week" guide sticks out to the right of the narrower "Quartile" one: centring the box
    # justification lines their midpoints up, which is what makes the pair sit square in the
    # facet cell once place_legend_in_vacant_facet() puts it there. "left"/"right" go back to
    # flush alignment on either side.
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    guides(color = "none")
}

# --- The (60, 100] bin on its own -------------------------------------------------------
# The figures above put every smoke bin on the x axis and separate the groups by dodging,
# which makes the gradient across quartiles the hard thing to read and the bin the easy one.
# This one keeps only the bin the paper leads with and swaps the two: the groups go on the x
# axis, so each panel is that characteristic's quartile gradient at a single smoke level.
FOCUS_BIN <- unname(BIN_LABELS['bin_d_060_100'])
FOCUS_BIN_AQI <- BIN_AQI_LABELS[match(FOCUS_BIN, unname(BIN_LABELS))]

# x axis order: the weekday/weekend pair first, then the quartile groups by name, which puts
# a merged baseline ("Q1+Q2") ahead of the quartiles estimated on their own. Panels take free
# x scales, so each shows only the groups that characteristic actually has rather than
# leaving a gap for every group belonging to some other panel.
group_x_levels <- function(groups) {
  groups <- unique(as.character(groups))
  c(intersect(c("Weekday", "Weekend"), groups),
    sort(setdiff(groups, c("Weekday", "Weekend"))))
}

focus_bin_plot <- function(plot_data, y_label, ncol = 3) {
  plot_data <- plot_data %>%
    filter(bin == FOCUS_BIN) %>%
    mutate(Group = factor(Group, levels = group_x_levels(Group)))

  # one x position per group, so nothing has to dodge
  ggplot(plot_data, aes(x = Group, y = Estimate, color = bin, ymin = low_ci, ymax = high_ci)) +
    geom_hline(yintercept = 0, color = "darkgrey") +
    geom_linerange(aes(linetype = DayType), linewidth = 1) +
    geom_errorbar(aes(ymax = low_ci), width = 0.4, linewidth = 1) +
    geom_errorbar(aes(ymin = high_ci), width = 0.4, linewidth = 1) +
    geom_point(aes(shape = Quartile), size = 4) +
    facet_wrap(~Sample, scales = "free", ncol = ncol) +
    scale_color_manual(values = BIN_COLORS) +
    scale_shape_manual("Quartile", values = QUARTILE_SHAPES,
                       breaks = setdiff(names(QUARTILE_SHAPES), "Overall")) +
    scale_linetype_manual("Day of Week", values = DAY_TYPE_LINETYPES,
                          breaks = intersect(c("Weekday", "Weekend"),
                                             as.character(plot_data$DayType))) +
    scale_y_continuous(y_label, labels = scales::label_percent()) +
    # the bin is no longer on an axis or a strip, so it is named here instead
    labs(x = NULL,
         subtitle = bquote(paste("Smoke ", PM[2.5], " ", .(FOCUS_BIN), " ", mu, "g/", m^3,
                                 "  (", .(FOCUS_BIN_AQI), ")"))) +
    theme_minimal(base_size = 24) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    guides(color = "none")
}

VARS_IN_MAIN_SAMPLE <- c(
  'Median Household Income', 'Rurality Index',  'Background PM',
  'Natural Amenity Tourism', 'Weekend'
)
# everything the main figures leave out goes on the appendix panel of the same figure
APPENDIX_VARS <- setdiff(sample_levels, VARS_IN_MAIN_SAMPLE)

# the main-sample panels are ordered as VARS_IN_MAIN_SAMPLE lists them, the appendix ones
# keep the order they come out of heterogeneity_specs in
main_sample_panels <- function(plot_data) {
  plot_data %>%
    filter(Sample %in% VARS_IN_MAIN_SAMPLE) %>%
    mutate(Sample = factor(Sample, levels = VARS_IN_MAIN_SAMPLE)) %>%
    arrange(Sample)
}

appendix_panels <- function(plot_data) plot_data %>% filter(Sample %in% APPENDIX_VARS)

# Only the level figures for the main sample under the plain day fixed effect belong to the
# paper itself. The differences, the panels of the characteristics the main figures leave out,
# and everything from the day-by-quartile specification are appendix material and go to the
# appendix folder; `appendix = TRUE` marks the first two, the specification marks the last.
save_figure <- function(variant, stem, plot, appendix = FALSE) {
  figures_dir <- if (appendix || variant$appendix_only) APPENDIX_FIGURES_DIR else FIGURES_DIR
  path <- file.path(figures_dir, paste0(stem, variant$output_suffix, ".pdf"))
  ggsave(path, plot = plot, width = 16, height = 12, dpi = 300, units = "in")
  cat("  saved", path, "\n")
}

# Every figure for one specification. The figure names are unchanged; the day-by-quartile
# specification keeps its "_quantile_day_interaction_FE" suffix.
save_specification_figures <- function(variant) {
  cat("\n=== ", variant$description, " specification ===\n", sep = "")
  dir.create(APPENDIX_FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

  estimates <- collect_group_estimates(variant)

  # --- Levels: quartile 1 from the smoke bin coefficients, quartiles 2-4 adding their
  # --- interaction term on top of it
  levels_plot_data <- prepare_plot_data(estimates$levels)

  # --- Differences: quartiles 2-4 relative to quartile 1, weekday and weekend as effects
  difference_plot_data <- prepare_plot_data(estimates$differences)

  save_figure(variant, "figure_8_day_of_heterogeneity_daily_effect",
              place_legend_in_vacant_facet(heterogeneity_plot(
                main_sample_panels(levels_plot_data), "Daily Change in Visits",
                ncol = if (variant$appendix_only) 2 else 3)))

  save_figure(variant, "figure_8b_heterogeneity_differences_daily_effect",
              place_legend_in_vacant_facet(heterogeneity_plot(
                main_sample_panels(difference_plot_data),
                "Difference in Daily Change in Visits")),
              appendix = TRUE)

  save_figure(variant, "figure_8_day_of_heterogeneity_daily_effect_appendix",
              place_legend_in_vacant_facet(heterogeneity_plot(
                appendix_panels(levels_plot_data), "Daily Change in Visits")),
              appendix = TRUE)

  save_figure(variant, "figure_8b_heterogeneity_differences_daily_effect_appendix",
              place_legend_in_vacant_facet(heterogeneity_plot(
                appendix_panels(difference_plot_data),
                "Difference in Daily Change in Visits")),
              appendix = TRUE)

  # --- The same levels, restricted to the (60, 100] bin, with the groups on the x axis
  save_figure(variant, "figure_8c_day_of_heterogeneity_daily_effect_60_100",
              place_legend_in_vacant_facet(focus_bin_plot(
                main_sample_panels(levels_plot_data), "Daily Change in Visits")))

  save_figure(variant, "figure_8c_day_of_heterogeneity_daily_effect_60_100_appendix",
              place_legend_in_vacant_facet(focus_bin_plot(
                appendix_panels(levels_plot_data), "Daily Change in Visits")),
              appendix = TRUE)

  invisible(NULL)
}

# --- One characteristic per figure, for slides ---------------------------------------------
# The same three plots (levels, differences, and levels at the (60, 100] bin), but with each
# characteristic on a figure of its own rather than as a panel of a grid, sized for a 16:9
# slide. Every characteristic is drawn, the appendix ones included. With a single panel there
# is no vacant facet cell, so the legend stays on the right.
PRESENTATION_FIGURES_DIR <- file.path(FIGURES_DIR, "for_presentation")

# file name stem for each panel: weekend, then the regression labels
SAMPLE_FILE_STEMS <- c(
  "Weekend" = "weekend",
  set_names(map_chr(heterogeneity_specs, "label"), map_chr(heterogeneity_specs, "sample"))
)

PRESENTATION_FIGURES <- list(
  list(stem = "heterogeneity_levels",        estimates = "levels",
       plot = heterogeneity_plot, y_label = "Daily Change in Visits"),
  list(stem = "heterogeneity_differences",   estimates = "differences",
       plot = heterogeneity_plot, y_label = "Difference in Daily Change in Visits"),
  list(stem = "heterogeneity_levels_60_100", estimates = "levels",
       plot = focus_bin_plot,     y_label = "Daily Change in Visits")
)

save_presentation_figures <- function(variant) {
  cat("\n=== presentation figures, ", variant$description, " specification ===\n", sep = "")
  dir.create(PRESENTATION_FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

  estimates <- collect_group_estimates(variant)
  plot_data <- map(estimates, prepare_plot_data)

  for (figure in PRESENTATION_FIGURES) {
    for (sample in sample_levels) {
      panel_data <- plot_data[[figure$estimates]] %>% filter(Sample == sample)
      path <- file.path(PRESENTATION_FIGURES_DIR,
                        paste0(figure$stem, "_", SAMPLE_FILE_STEMS[[sample]],
                               variant$output_suffix, ".pdf"))
      # the facet strip is the only title the panel has, and the default legend keys are too
      # short for the dotted/dashed weekday-weekend lines to be told apart on a slide
      plot <- figure$plot(panel_data, figure$y_label) +
        theme(strip.text = element_text(size = rel(1.2)),
              legend.key.height = unit(1.2, "cm"),
              legend.key.width = unit(1, "cm"))
      ggsave(path, plot = plot, width = 13.33, height = 7.5, units = "in")
      cat("  saved", path, "\n")
    }
  }

  invisible(NULL)
}

for (variant in SPECIFICATION_VARIANTS) save_specification_figures(variant)

# slides only use the plain day fixed effect
for (variant in SPECIFICATION_VARIANTS) {
  if (!variant$appendix_only) save_presentation_figures(variant)
}

cat("\nAll heterogeneity figures saved.\n")
