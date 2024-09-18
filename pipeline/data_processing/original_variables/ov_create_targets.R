# Create original variable targets

# Transform, merge, and update raw data
export_orig <- function(orig_statistics_output_p2,
                        orig_statistics_dataset_p1,
                        original_inftest_dataset,
                        orig_statistics_manual_data_entry,
                        orig_vars_qa,
                        tagtable_covid_p1,
                        orig_input_changelog) {

  orig_extended <- transform_orig_output(orig_statistics_output_p2)

  merge_orig_input(orig_statistics_dataset_p1,
                   original_inftest_dataset,
                   orig_statistics_manual_data_entry,
                   orig_vars_qa,
                   tagtable_covid_p1) %>%
    update_orig_input(orig_input_changelog) %>%
    mutate(
      # Prioritize which version of duplicates to keep
      data_source_priority = case_match(original_data_source,
                                        "p2_manual" ~ 1,
                                        "p2_power" ~ 2,
                                        "p2_pipeline" ~ 3,
                                        "orig_via_repro_form" ~ 4,
                                        "pipeline_orig" ~ 5,
                                        "repro" ~ 6),
      # Standardize values for original_effect_size_type_reported and
      # original_effect_size_type_reference
      across(
        c(original_effect_size_type_reported,
          original_effect_size_type_reference),
        ~ case_match(
          .x,
          c("Cohen's d", "d", "d_sample", "d_sample (one)") ~ "cohen_d",
          "Cohen's dz" ~ "cohen_dz",
          c("Cohen's f-squared", "cohenf2") ~ "cohen_f_squared",
          c("Cohen's w", "cohens_w") ~ "cohen_w",
          "Cramer's V" ~ "cramer_v",
          "Eta-squared" ~ "eta_squared",
          "Hazard ratio" ~ "hazard_ratio",
          c("adjusted_odds_ratio", "Odds ratio") ~ "odds_ratio",
          "Partial eta-squared" ~ "partial_eta_squared",
          "path_coef" ~ "path_coefficient",
          c("Pearson's r", "r") ~ "pearson_r",
          "Phi" ~ "phi",
          "probit_coefficient_estimate" ~ "probit_coefficient",
          c("multiple_regression_coefficient",
            "raw_ols_coefficient",
            "raw_regression_coefficient",
            "raw_regression_coefficient_from_a_two_step_system_gmm",
            "raw_regression_coefficient_beta") ~ "regression_coefficient",
          "relative risk ratio" ~ "relative_risk_ratio",
          c("SER method", "SER-Method") ~ "ser_method",
          "SER-t method" ~ "ser_method_t",
          c("standardized_regression_coefficient",
            "standardized_coefficient") ~ "standardized_regression_coefficient",
          .default = .x
        )
      )
    ) %>%
    arrange(unique_claim_id, data_source_priority) %>%
    relocate(data_source_priority, .after = original_data_source) %>%
    distinct(unique_claim_id, .keep_all = TRUE) %>%
    select(-c(data_source_priority)) %>%
    left_join(orig_extended, join_by(paper_id, claim_id))

}

# Create Original Variable Analytic Dataset
create_ov_analytic <- function(orig_dataset,
                               complex_bushel,
                               effectsize_orig,
                               effectsize_outcome) {
  
  bushel <- transform_complex_bushel(complex_bushel)

  orig <- orig_dataset %>%
    left_join(effectsize_orig, by = join_by(unique_claim_id == claim_id)) %>%
    mutate(claim_id = unique_claim_id,
           # Coalesce reported into reference
           orig_sample_size_value = coalesce(
             original_analytic_sample_size_value_reported,
             original_analytic_sample_size_value_reference
           ),
           sample_size_value_effective = coalesce(
             original_effective_sample_size,
             orig_sample_size_value
           ),
           orig_stat_type = coalesce(
             original_statistic_type_reported,
             original_statistic_type_reference
           ),
           orig_stat_dof_1 = coalesce(
             original_statistic_df1_reported,
             original_statistic_df1_reference
           ),
           df_1_effective = coalesce(original_effective_df1_reference,
                                     orig_stat_dof_1),
           orig_stat_dof_2 = coalesce(
             original_statistic_df2_reported,
             original_statistic_df2_reference
           ),
           orig_stat_value = coalesce(
             original_statistic_value_reported,
             original_statistic_value_reference
           ),
           orig_stat_interaction = coalesce(
             original_statistic_effect_type_reported,
             original_statistic_effect_type_reference
           ),
           orig_effect_size_text = coalesce(
             original_effect_size_fulltext_reported,
             original_effect_size_fulltext_reference
           ),
           orig_effect_size_type_repro = coalesce(
             original_effect_size_type_reported,
             original_effect_size_type_reference
           ),
           orig_effect_size_value_repro = coalesce(
             original_effect_size_value_reported,
             original_effect_size_value_reference
           ),
           orig_coef_type = coalesce(
             original_coefficient_type_reported,
             original_coefficient_type_reference
           ),
           orig_coef_value = coalesce(
             original_coefficient_value_reported,
             original_coefficient_value_reference
           ),
           orig_coef_se = coalesce(
             original_coefficient_se_reported,
             original_coefficient_se_reference
           ),
           orig_p_value_type = coalesce(
             original_p_value_type_reported,
             original_p_value_type_reference
           ),
           orig_p_value = coalesce(
             original_p_value_value_reported,
             original_p_value_value_reference
           ),
           orig_p_value_tails = coalesce(
             original_p_value_tails_reported,
             original_p_value_tails_reference
           ),
           orig_stat_type = case_match(orig_stat_type,
                                       c("f", "Fvalue") ~ "F",
                                       "chi-squared" ~ "chi_squared",
                                       .default = orig_stat_type)
           ) %>%
    rename(
      orig_sample_size_units = original_analytic_sample_size_units_reported,
      orig_analysis_type = original_statistic_analysis_type,
      orig_total_model_params = original_total_model_parameters,
      orig_sample_size_50_for_100 = rr_threshold_analytic_sample_size,
      orig_sample_size_90_for_75 = rr_stage1_analytic_sample_size,
      orig_sample_size_90_for_50 = rr_stage2_analytic_sample_size,
      orig_stat_type_for_repli = original_statistic_analysis_type_statsteam,
      orig_pearsons_r_defined = original_pearsons_r_defined,
      orig_pearsons_r_value = original_pearsons_r_numeric,
      orig_pearsons_r_ci_lb = original_es_lb_ci_pearson,
      orig_pearsons_r_ci_ub = original_es_ub_ci_pearson,
      orig_effect_size_type_repli = original_effect_size_type_statsteam,
      orig_effect_size_value_repli = original_effect_size_value_statsteam,
      orig_effect_size_ci_lb = original_es_lb_ci_nativeunits,
      orig_effect_size_ci_ub = original_es_ub_ci_nativeunits,
      orig_power_threshold_small = original_power_small,
      orig_power_threshold_medium = original_power_medium,
      orig_power_for_50_effect = original_power_50_original_effect,
      orig_power_for_75_effect = original_power_75_original_effect
    ) %>%
    # Indicator for if the evidence for a bushel claim deviates from the
    # prototypical SCORE evidence format (a single, statistically significant
    # test result)
    left_join(bushel, by = "unique_claim_id") %>%
    # Kill unneeded variables
    select(-c(unique_claim_id,
              original_data_source,
              orig_stat_version,
              original_analytic_subsample_a_00,
              original_analytic_subsample_b_01,
              original_analytic_subsample_c_10,
              original_analytic_subsample_d_11,
              original_analytic_subsample_n1,
              original_analytic_subsample_n2,
              original_statistic_fulltext_reported,
              original_statistic_fulltext_reference,
              original_poweranalysis_link,
              original_samplesize_calculation_contributor,
              original_analytic_sample_size_value_reference,
              original_analytic_sample_size_value_reported,
              original_statistic_type_reference,
              original_statistic_type_reported,
              original_statistic_df1_reference,
              original_statistic_df1_reported,
              original_statistic_df2_reference,
              original_statistic_df2_reported,
              original_statistic_value_reference,
              original_statistic_value_reported,
              original_statistic_effect_type_reference,
              original_statistic_effect_type_reported,
              original_effect_size_fulltext_reference,
              original_effect_size_fulltext_reported,
              original_effect_size_type_reference,
              original_effect_size_type_reported,
              original_effect_size_value_reference,
              original_effect_size_value_reported,
              original_coefficient_type_reference,
              original_coefficient_type_reported,
              original_coefficient_value_reference,
              original_coefficient_value_reported,
              original_coefficient_se_reference,
              original_coefficient_se_reported,
              original_p_value_type_reference,
              original_p_value_type_reported,
              original_p_value_value_reference,
              original_p_value_value_reported,
              original_p_value_tails_reference,
              original_p_value_tails_reported,
              original_cos_notes,
              original_materials_link,
              original_poweranalysis_notes,
              Tilburg_team_finished,
              original_effective_sample_size,
              original_effective_df1_reference))
  
  manual <- effectsize_outcome %>%
    rename(claim_id = `...1`,
           cos_r = r,
           cos_r_lb = r_lb,
           cos_r_ub = r_ub) %>%
    filter(claim_id %in% orig$claim_id)
  
  effect_sizes <- convert_to_cosr(orig, "claim_id")
  
  orig %>%
    left_join(effect_sizes, by = "claim_id") %>%
    rows_update(manual, by = "claim_id") %>%
    select(-c(sample_size_value_effective,
              df_1_effective)) %>%
    rename(orig_conv_r = cos_r,
           orig_conv_r_lb = cos_r_lb,
           orig_conv_r_ub = cos_r_ub)

}