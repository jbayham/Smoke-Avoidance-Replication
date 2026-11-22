# Pipeline

Stages run sequentially in dependency order. Files listed below are required completion outputs; scripts may also write diagnostics and additional presentation variants.

## consumer_totals

- Script: `1_build/daily_consumer_data/add_daily_total_spend_and_visits_by_county.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/consumer/daily_spend`
  - `data/intermediate/consumer/daily_visits`

## consumer_matched

- Script: `1_build/daily_consumer_data/add_daily_matched_poi_spend_and_visits_by_county.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/consumer/county_daily_matched_poi_spend_and_visits.parquet`

## smoke_aggregation

- Script: `1_build/smoke_pm/aggregate_surface_level_smoke_pm_daily.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/smoke/mean/county_smoke_pm_all_time_mean.csv`
  - `data/intermediate/smoke/median/county_smoke_pm_all_time_median.csv`

## aqi

- Script: `1_build/smoke_pm/aqi_download_and_process.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/aqi/all_years_aqi_by_county_w__boundary_flags.csv`

## smoke_filter

- Script: `1_build/smoke_pm/combine_and_process_smoke_with_aqi_filtering.R`
- Depends on: `smoke_aggregation`, `aqi`
- Outputs:
  - `data/intermediate/smoke/smoke_pm_with_aqi_filters.csv`

## weather

- Script: `1_build/weather/pull_weather_data.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/weather/final/county_weather_2019.csv.gz`
  - `data/intermediate/weather/final/county_weather_2020.csv.gz`
  - `data/intermediate/weather/final/county_weather_2021.csv.gz`
  - `data/intermediate/weather/final/county_weather_2022.csv.gz`
  - `data/intermediate/weather/final/county_weather_2023.csv.gz`

## parks

- Script: `1_build/county_characteristics/add_park_area_per_county.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/county/county_fraction_park.csv`

## amenity

- Script: `1_build/county_characteristics/combine_tourism_dependency_and_natural_resource_dep.R`
- Depends on: `parks`
- Outputs:
  - `data/intermediate/county/natural_amenity_tourism_dependence.csv`

## background

- Script: `1_build/county_characteristics/add_baseline_background_pm_by_county.R`
- Depends on: `smoke_aggregation`
- Outputs:
  - `data/intermediate/county/county_avg_background_smoke_dist_2018.csv`

## cbp

- Script: `1_build/county_characteristics/add_cbp_county_composition.R`
- Depends on: `classification`
- Outputs:
  - `data/intermediate/county/cbp_county_composition_2018.csv`

## rurality

- Script: `1_build/county_characteristics/add_carr_county_rurality_index.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/county/county_rurality_index.csv`

## bea_county

- Script: `1_build/county_characteristics/add_bea_outdoor_rec_state_shares.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/county/bea_percent_state_comp_gdp_from_rec_2018.csv`
  - `data/intermediate/county/bea_percent_state_gdp_from_rec_by_activity_2018.csv`

## county_merge

- Script: `1_build/county_characteristics/merge_county_information.R`
- Depends on: `amenity`, `background`, `cbp`, `rurality`, `bea_county`
- Outputs:
  - `data/intermediate/county/county_info_all.csv`

## classification

- Script: `1_build/classify_naics_codes_outdoor_activities.R`
- Depends on: raw inputs only
- Outputs:
  - `data/intermediate/county/deferrable_naics_code_classified_indoor_outdoor.csv`
  - `output/tables/appendix/appendix_table_indoor_outdoor_classifications.tex`

## daily

- Script: `1_build/reg_data/build_reg_data_daily.R`
- Depends on: `consumer_totals`, `consumer_matched`, `smoke_filter`, `weather`, `county_merge`
- Outputs:
  - `data/processed/daily.csv`
  - `data/processed/avg_pm_per_smoke_bin.csv`

## weekly

- Script: `1_build/reg_data/build_reg_data_weekly.R`
- Depends on: `daily`, `consumer_matched`
- Outputs:
  - `data/processed/weekly.csv`

## baseline

- Script: `2_analysis/figure_1_2_day_of_analysis_run.R`
- Depends on: `daily`
- Outputs:
  - `output/models/binary_base_day_of_dose_response.rds`
  - `output/models/binary_quartile_rurality_interaction.rds`
  - `output/models/binary_quartile_rurality_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_weekend_interaction.rds`
  - `output/models/binary_quartile_background_pm_interaction.rds`
  - `output/models/binary_quartile_background_pm_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_quartile_income_interaction.rds`
  - `output/models/binary_quartile_income_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_quartile_nat_amenity_interaction.rds`
  - `output/models/binary_quartile_nat_amenity_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_quartile_gdp_rec_interaction.rds`
  - `output/models/binary_quartile_gdp_rec_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_quartile_comp_rec_interaction.rds`
  - `output/models/binary_quartile_comp_rec_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_quartile_cbp_outdoor_estab_interaction.rds`
  - `output/models/binary_quartile_cbp_outdoor_estab_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_quartile_cbp_outdoor_payroll_interaction.rds`
  - `output/models/binary_quartile_cbp_outdoor_payroll_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_quartile_cbp_hhi_estab_interaction.rds`
  - `output/models/binary_quartile_cbp_hhi_estab_interaction_quantile_day_interaction_FE.rds`
  - `output/models/binary_quartile_cbp_hhi_payroll_interaction.rds`
  - `output/models/binary_quartile_cbp_hhi_payroll_interaction_quantile_day_interaction_FE.rds`

## baseline_figure

- Script: `2_analysis/figure_1_day_of_analysis_plot.R`
- Depends on: `daily`, `baseline`
- Outputs:
  - `output/figures/figure_1a_dose_response_daily_effect.pdf`

## spending_models

- Script: `2_analysis/matched_poi_and_split_sample_spend_on_visits.R`
- Depends on: `weekly`
- Outputs:
  - `output/models/spend_on_visits_matched_poi_split_sample_monthly_matched_asinh.csv`
  - `output/tables/appendix/appendix_table_spend_on_visits_ols_and_split_sample_iv_asinh.tex`

## weighted_spending

- Script: `1_build/daily_consumer_data/avg_daily_county_spend_weighted_by_poi_match_rate.R`
- Depends on: `weekly`, `consumer_matched`
- Outputs:
  - `data/intermediate/spending/county_avg_daily_spend_poi_match_weighted.csv`

## spending_summary

- Script: `2_analysis/compile_spending_visits_relationship_summary.R`
- Depends on: `spending_models`, `weighted_spending`, `weekly`
- Outputs:
  - `data/intermediate/spending/annual_spending_visits_and_relationship_data.csv`

## welfare_spending

- Script: `2_analysis/estimate_welfare_losses_from_spending.R`
- Depends on: `spending_summary`, `baseline`
- Outputs:
  - `output/models/welfare_per_bin_by_spend_on_visits_slope.rds`

## welfare_health

- Script: `2_analysis/estimate_welfare_losses_from_health_effects.R`
- Depends on: `spending_summary`, `daily`
- Outputs:
  - `output/models/estimated_mortality_cost_by_smoke_bin.rds`

## welfare_figures

- Script: `2_analysis/plot_welfare_effect_plots.R`
- Depends on: `welfare_spending`, `welfare_health`
- Outputs:
  - `output/figures/figure_10b_welfare_comparison_ma_2024_per_day_three_panel_updated.pdf`
  - `output/figures/appendix/figure_10a_welfare_comparison_gould_2024.pdf`
  - `output/figures/appendix/figure_10c_welfare_comparison_qiu_2025.pdf`

## sectors

- Script: `2_analysis/figure_3_day_of_analysis_by_naics_code_run_and_plot.R`
- Depends on: `daily`, `classification`
- Outputs:
  - `output/models/sector_specific_naics_results.csv`
  - `output/figures/figure_3a_day_of_analysis_naics_by_setting_60_100.pdf`
  - `output/figures/figure_3b_day_of_analysis_naics_by_setting_and_bin.pdf`

## fire_robustness

- Script: `2_analysis/appendix/fire_filter_day_of_regressions_run_and_gen_table.R`
- Depends on: `daily`, `baseline`
- Outputs:
  - `output/tables/appendix/appendix_table_fire_robustness_log.tex`

## count_models

- Script: `2_analysis/appendix/robustness_count_models.R`
- Depends on: `daily`, `baseline`
- Outputs:
  - `output/tables/appendix/appendix_table_count_models.tex`

## additional_fixed_effects

- Script: `2_analysis/appendix/robustness_additional_fixed_effects.R`
- Depends on: `daily`
- Outputs:
  - `output/tables/appendix/appendix_table_additional_fixed_effects.tex`

## within_county_variance

- Script: `2_analysis/appendix/test_heterogeneity_by_within_county_variance.R`
- Depends on: `daily`, `baseline`
- Outputs:
  - `output/tables/appendix/appendix_table_within_county_smoke_variance.tex`

## aqi_filters

- Script: `2_analysis/appendix/test_different_aqi_filters.R`
- Depends on: `daily`, `baseline`
- Outputs:
  - `output/figures/appendix/aqi_filter_grid_dose_response_median_only.pdf`
  - `output/models/aqi_filter_grid_dose_response_binary_coefs.rds`

## panel_composition

- Script: `2_analysis/appendix/check_panel_composition_not_associated_with_smoke.R`
- Depends on: `daily`
- Outputs:
  - `output/tables/appendix/appendix_table_panel_composition_smoke.tex`
  - `output/figures/appendix/panel_scaling_multiple_distribution.pdf`

## visit_distribution

- Script: `2_analysis/appendix/gen_visit_distribution_plot.R`
- Depends on: `daily`
- Outputs:
  - `output/figures/appendix/daily_deferrable_visits_distribution.pdf`
  - `output/figures/appendix/daily_deferrable_visits_distribution_filter_out_zero_visit_counties.pdf`

## smoke_by_year

- Script: `2_analysis/appendix/plot_smoke_days_by_year.R`
- Depends on: `daily`
- Outputs:
  - `output/figures/appendix/smoke_days_by_bin_pct_change_from_2020.pdf`
  - `output/figures/appendix/smoke_days_by_bin_yearly_average.pdf`

## smoke_by_weekday

- Script: `2_analysis/appendix/plot_smoke_days_by_weekend_weekday.R`
- Depends on: `daily`
- Outputs:
  - `output/figures/appendix/smoke_days_by_weekend_weekday_counts.pdf`

## smoke_variance_plot

- Script: `2_analysis/appendix/plot_within_and_between_county_variance.R`
- Depends on: `daily`, `smoke_aggregation`
- Outputs:
  - `output/figures/appendix/figure_A3_within_and_between_variance.png`

## fire_distribution

- Script: `2_analysis/appendix/plot_viirs_fire_distribution.R`
- Depends on: `daily`
- Outputs:
  - `output/figures/appendix/viirs_fire_point_distribution.pdf`

## spending_figure

- Script: `2_analysis/appendix/plot_spend_visit_coefficients_by_model.R`
- Depends on: `spending_models`
- Outputs:
  - `output/figures/appendix/spend_visit_coefficients_by_model_table_2_fes.pdf`

## county_maps

- Script: `2_analysis/appendix/map_heterogeneity_variables.R`
- Depends on: `county_merge`
- Outputs:
  - `output/figures/appendix/map_income.pdf`
  - `output/figures/appendix/map_rurality.pdf`
  - `output/figures/appendix/map_background_pm.pdf`
  - `output/figures/appendix/map_nat_amenity.pdf`

## county_correlation

- Script: `2_analysis/appendix/heterogeneity_correlation_matrix.R`
- Depends on: `daily`, `county_merge`
- Outputs:
  - `output/figures/appendix/heterogeneity_correlation_matrix.pdf`

## heterogeneity_figures

- Script: `2_analysis/figure_2_day_of_analysis_heterogeneities_plot.R`
- Depends on: `baseline`
- Outputs:
  - `output/figures/figure_8_day_of_heterogeneity_daily_effect.pdf`
  - `output/figures/appendix/figure_8_day_of_heterogeneity_daily_effect_quantile_day_interaction_FE.pdf`

## spatial_panel

- Script: `1_build/reg_data/build_spatial_weight_matrix.R`
- Depends on: `daily`
- Outputs:
  - `output/models/sdem_prepared_panel.rds`

## spatial_models

- Script: `2_analysis/spatial_substitution_estimate_run.R`
- Depends on: `spatial_panel`
- Outputs:
  - `output/models/spatial_sdem_v4_dcs_units_results.rds`

## spatial_figure

- Script: `2_analysis/spatial_substitution_estimate_plot.R`
- Depends on: `spatial_models`
- Outputs:
  - `output/figures/spatial_sdem_direct_indirect_typical_episode_v4_dcs_units.pdf`

## spatial_neighbors

- Script: `2_analysis/appendix/sdem_neighbor_profile_table.R`
- Depends on: `spatial_models`
- Outputs:
  - `output/tables/appendix/appendix_table_sdem_neighbor_profile.tex`

## lpdid_thresholds

- Script: `2_analysis/appendix/lpdid_threshold_robustness.R`
- Depends on: `daily`
- Outputs:
  - `output/models/lpdid_threshold_robustness/estimates_and_used_samples.csv`
  - `output/models/lpdid_threshold_robustness/sample_reductions_and_estimation_attrition.csv`
  - `data/intermediate/reg_data/lpdid_prepped_panel.rds`
  - `output/figures/appendix/lpdid_threshold_robustness_over_60.pdf`
  - `output/figures/appendix/lpdid_threshold_robustness_smoke_as_dv.pdf`

## lpdid_main

- Script: `2_analysis/lpdid_daily_visits_analysis.R`
- Depends on: `lpdid_thresholds`
- Outputs:
  - `output/figures/lpdid_cc_over_60.pdf`
  - `output/figures/lpdid_cc_smoke_as_dv.pdf`
  - `output/figures/lpdid_cc_spell_progressive_3.pdf`

## lpdid_legacy

- Script: `2_analysis/appendix/lpdid_prior_smoke_filter_run_and_plot.R`
- Depends on: `lpdid_main`
- Outputs:
  - `output/figures/appendix/lpdid_prior_smoke_treated_cap_smoke.pdf`
  - `output/figures/appendix/lpdid_prior_smoke_treated_cap_visits.pdf`

## baseline_table

- Script: `2_analysis/table_1_comparing_fixed_effects_run_and_output.R`
- Depends on: `daily`
- Outputs:
  - `output/tables/table_1_fixed_effect_comp_weather_bins.tex`

## aqi_diagnostic

- Script: `2_analysis/appendix/reconstruct_smoke_aqi_diagnostic.R`
- Depends on: `daily`
- Outputs:
  - `output/figures/appendix/smoke_non_smoke_AQI.png`

## schematic

- Script: `scripts/copy_schematic.R`
- Depends on: raw inputs only
- Outputs:
  - `output/figures/spatial_sub_for_paper.png`

