extract_interacted_effects <- function(reg_results, interaction_var, label = "Deferrable Sectors", sample_label, vars_to_plot_start = "adj_pm_smoke_bin") {
  coefs <- reg_results$coeftable
  vcov_mat <- vcov(reg_results)
  
  # Rename for consistent handling
  colnames(coefs)[colnames(coefs) == "Std. Error"] <- "Std..Error"
  
  base_rows <- rownames(coefs) %>%
    keep(~ str_starts(., vars_to_plot_start) & !str_detect(., ":"))
  
  base_df <- coefs[base_rows, , drop = FALSE] %>%
    as.data.frame() %>%
    rownames_to_column("vars")
  
  # --- Uninteracted (control group)
  base_clean <- base_df %>%
    mutate(
      Sector = label,
      Sample = sample_label,
      with_interaction = FALSE
    ) %>%
    select(vars, Estimate, Std..Error, Sector, Sample, with_interaction)
  
  # --- Interacted (treated group)
  interaction_df <- base_df %>%
    rowwise() %>%
    mutate(
      interaction_term = {
        term1 <- paste0(vars, ":", interaction_var)
        term2 <- paste0(interaction_var, ":", vars)
        if (term1 %in% rownames(coefs)) term1
        else if (term2 %in% rownames(coefs)) term2
        else NA_character_
      },
      interaction_effect = if (!is.na(interaction_term)) coefs[interaction_term, "Estimate"] else NA_real_,
      Estimate = Estimate + interaction_effect,
      Std..Error = if (!is.na(interaction_term)) {
        sqrt(
          vcov_mat[vars, vars] +
            vcov_mat[interaction_term, interaction_term] +
            2 * vcov_mat[vars, interaction_term]
        )
      } else {
        sqrt(vcov_mat[vars, vars])
      },
      vars = paste0(vars, ":", interaction_var),
      Sector = label,
      Sample = sample_label,
      with_interaction = TRUE
    ) %>%
    ungroup() %>%
    select(vars, Estimate, Std..Error, Sector, Sample, with_interaction)
  
  bind_rows(base_clean, interaction_df)
}

# Baseline coefficients from the non-interacted model
baseline_df <- add_new_sample_and_sector(
  reg_results = est_visits_def_log,
  sector_name = "Deferrable Sectors",
  sample_label = "Overall",
  overall_sample_label = "Sample",
  vars_to_plot_start = "adj_pm_smoke_bin"
) %>% mutate(with_interaction=FALSE)

# Interaction-adjusted coefficients
interaction_df_pollution <- extract_interacted_effects(
  reg_results = est_visits_def_log_background_pm,
  interaction_var = "background_pm_above_medianTRUE",
  sample_label = "Background PM Above Median"
)

# Interaction-adjusted coefficients
interaction_df_weekend <- extract_interacted_effects(
  reg_results = est_visits_def_log_weekend,
  interaction_var = "is_weekendTRUE",
  sample_label = "Weekend Smoke"
)


interaction_df_income <- extract_interacted_effects(
  reg_results = est_visits_def_log_income,
  interaction_var = "income_above_medianTRUE",
  sample_label = "County Income Above Median"
)

interaction_df_natural_amenities <- extract_interacted_effects(
  reg_results = est_visits_def_log_nat_amenity_tourism,
  interaction_var = "is_natural_amenity_tourism_dependentTRUE",
  sample_label = "Natural Amenity Tourism Dependent"
)


interaction_df_smoke_intensity <- extract_interacted_effects(
  reg_results = est_visits_def_log_exposure_length_2_days,
  interaction_var = "is_at_least_second_day_of_smokeTRUE",
  sample_label = "Is 2nd+ Day of Smoke"
)

interaction_df_smoke_intensity_3 <- extract_interacted_effects(
  reg_results = est_visits_def_log_exposure_length_3_days,
  interaction_var = "is_at_least_third_day_of_smokeTRUE",
  sample_label = "Is 3rd+ Day of Smoke"
)

# Combine both sets
plot_df <- bind_rows(baseline_df, interaction_df_pollution, interaction_df_weekend, interaction_df_natural_amenities, interaction_df_smoke_intensity, interaction_df_smoke_intensity_3)

plot_data <- plot_df %>%
  mutate(
    low_ci = Estimate - 1.96 * Std..Error,
    high_ci = Estimate + 1.96 * Std..Error,
    bin = str_remove(str_extract(vars, "bin_\\d+_\\d+"), "bin_"),
    Sample = factor(Sample, levels = c(
      "Overall",  # put your desired first sample name here
      setdiff(unique(Sample), "Overall")  # preserve order for the rest
    ))
  )

heterogeneity_plot <- ggplot(plot_data, aes(x = bin, y = Estimate, shape = with_interaction, color = bin, ymin = low_ci, ymax = high_ci)) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.3, linewidth=1.1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~Sample, scales="free") +
  labs(x = "Smoke PM Bin", shape = "With Interaction") +
  scale_color_manual(
    "Smoke PM AQI\nBin equivalent",
    values = c('forestgreen', 'gold', 'orange', 'red', 'purple', 'maroon'),
    labels = c("Good", "Moderate", "Unhealthy for \nSensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
  ) +
  scale_y_continuous("Marginal Change in Visits", labels = scales::label_percent()) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(color = "none")


ggsave(file.path(FIGURES_DIR, "figure_8_day_of_heterogeneity.pdf"),
       plot = heterogeneity_plot,
       width = 12, height = 7, dpi = 300, units = "in")


# overall average smoke PM in each bin (built by build_reg_data_daily.R); scale every
# sample's per-unit effect by the same daily binned PM
avg_pm_lookup <- read_csv(AVG_PM_PER_BIN_PATH)

plot_data_scaled <- plot_data %>%
  left_join(avg_pm_lookup, by = "bin") %>%
  mutate(
    Estimate = Estimate * avg_pm,
    low_ci = low_ci * avg_pm,
    high_ci = high_ci * avg_pm,
    Sample = factor(Sample, levels = c(
      "Overall",  # put your desired first sample name here
      setdiff(unique(Sample), "Overall")  # preserve order for the rest
    ))
  )


heterogeneity_plot_average_effect <- ggplot(plot_data_scaled, aes(x = bin, y = Estimate, shape = with_interaction, color = bin, ymin = low_ci, ymax = high_ci)) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~Sample, scales="free") +
  labs(x = "Smoke PM Bin", shape = "With Interaction") +
  scale_color_manual(
    "Smoke PM AQI\nBin equivalent",
    values = c('forestgreen', 'gold', 'orange', 'red', 'purple', 'maroon'),
    labels = c("Good", "Moderate", "Unhealthy for \nSensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
  ) +
  scale_y_continuous("Average Change in Visits", labels = scales::label_percent()) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(color = "none")

ggsave(file.path(FIGURES_DIR, "figure_8_day_of_heterogeneity_average_effect.pdf"),
       plot = heterogeneity_plot_average_effect,
       width = 12, height = 7, dpi = 300, units = "in")



