library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))
source(PLOTTING_HELPERS_PATH)

# Mortality cost by smoke bin, from estimate_welfare_losses_from_health_effects.R
annual_pm_contribution_with_mortality <- readRDS(ESTIMATED_MORTALITY_COST_BY_BIN_PATH)
welfare_df <- readRDS(ESTIMATED_WELFARE_PER_BIN_PATH)

full_back_of_envelope_comparison <- welfare_df %>%
  select(bin_label, bin, change_in_spend_from_smoke, welfare) %>%
  left_join(
    annual_pm_contribution_with_mortality %>% select(bin, starts_with('mortality')) %>% rename_with(~sub('mortality', 'VSL', .), .cols=-bin),
    by="bin"
  ) %>%
  mutate(
    across(starts_with('VSL'), ~if_else(is.na(.), 0, .*-1/365)),
    across(ends_with('ma'), ~.*365/30),
    welfare=-welfare
  )

# ---- shared plotting pieces -------------------------------------------------

# recode the numeric bin into the labelled, ordered factor used on the x axis
bin_factor <- function(bin) factor(BIN_LABELS[paste0("bin_d_", bin)], levels = unname(BIN_LABELS))

# lollipop from zero to `value`
lollipop_layers <- function(point_size = 3, linewidth = 1) {
  list(
    geom_hline(yintercept = 0, color = "darkgrey"),
    geom_segment(aes(xend = bin, y = 0, yend = value), linewidth = linewidth),
    geom_point(aes(y = value), size = point_size)
  )
}

# axis labels, bin colouring and theme shared by every welfare panel
welfare_scales_theme <- function(base_size = 18) {
  list(
    labs(x = expression(paste("Smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))),
    scale_color_manual(values = BIN_COLORS, labels = BIN_LABELS),
    scale_y_continuous("Daily Change per Additional\nAverage Day in Bin", labels = scales::label_dollar()),
    theme_minimal(base_size = base_size),
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)),
    guides(color = "none")
  )
}

# a lollipop panel over a data frame with `bin` (factor) and `value` columns,
# optionally faceted by `facet_var`
welfare_lollipop <- function(df, base_size = 18, facet_var = NULL, panel_spacing = NULL) {
  p <- ggplot(df, aes(x = bin, color = bin)) +
    lollipop_layers() +
    welfare_scales_theme(base_size)
  if (!is.null(facet_var)) p <- p + facet_wrap(vars(.data[[facet_var]]), scales = "free_y")
  if (!is.null(panel_spacing)) p <- p + theme(panel.spacing.x = unit(panel_spacing, "lines"))
  p
}

# ---- three-panel figure: spending / welfare / mortality ---------------------

p2_three_panel_df <- full_back_of_envelope_comparison %>%
  transmute(bin, spending_drop = change_in_spend_from_smoke, welfare_impact = welfare, VSL_ma) %>%
  pivot_longer(-bin, names_to = "measure", values_to = "value") %>%
  mutate(measure = factor(measure,
                          levels = c("spending_drop", "welfare_impact", "VSL_ma"),
                          labels = c("(a) Spending drop", "(b) Consumption Welfare (DWL)", "(c) Mortality (Ma et al. 2024)")),
         bin = bin_factor(bin))

p2_three_panel <- welfare_lollipop(p2_three_panel_df, base_size = 20, facet_var = "measure", panel_spacing = 2)

ggsave(file.path(FIGURES_DIR, "figure_10b_welfare_comparison_ma_2024_per_day_three_panel_updated.pdf"),
       plot = p2_three_panel,
       width = 15, height = 5.5, dpi = 300, units = "in")

# ---- single-panel figures ---------------------------------------------------

# Mortality cost for one health estimate, as a single faceted lollipop panel
mortality_panel_df <- function(mortality_col, panel_title) {
  full_back_of_envelope_comparison %>%
    transmute(bin = bin_factor(bin), value = .data[[mortality_col]], panel = panel_title)
}

qiu_mortality_plot <- welfare_lollipop(mortality_panel_df("VSL_qiu", "Mortality cost (Qiu et al. 2025)"), facet_var = "panel")

ggsave(file.path(APPENDIX_FIGURES_DIR, "figure_10c_welfare_comparison_qiu_2025.pdf"),
       plot = qiu_mortality_plot,
       width = 7, height = 5.5, dpi = 300, units = "in")

gould_mortality_plot <- welfare_lollipop(mortality_panel_df("VSL_meta_analysis", "Mortality cost (Gould et al. 2024)"), facet_var = "panel")

ggsave(file.path(APPENDIX_FIGURES_DIR, "figure_10a_welfare_comparison_gould_2024.pdf"),
       plot = gould_mortality_plot,
       width = 7, height = 5.5, dpi = 300, units = "in")
