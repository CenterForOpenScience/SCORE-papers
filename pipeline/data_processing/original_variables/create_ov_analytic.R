# Create Original Variable Analytic Dataset

create_ov_analytic <- function(orig_dataset,
                               complex_bushel) {

  orig_dataset %>%
    mutate(claim_id = unique_claim_id) %>%
    rename(
      orig_analysis_type = original_statistic_analysis_type,
      orig_sample_size_units = original_analytic_sample_size_units_reported,
      orig_sample_size_value = original_analytic_sample_size_value_reported,
      orig_stat_type = original_statistic_type_reported,
      orig_stat_dof_1 = original_statistic_df1_reported,
      orig_stat_dof_2 = original_statistic_df2_reported,
      orig_stat_value = original_statistic_value_reported,
      orig_stat_interaction = original_statistic_effect_type_reported,
      orig_effect_size_text = original_effect_size_fulltext_reported,
      orig_effect_size_type_repro = original_effect_size_type_reported,
      orig_effect_size_value_repro = original_effect_size_value_reported,
      orig_coef_type = original_coefficient_type_reported,
      orig_coef_value = original_coefficient_value_reported,
      orig_coef_se = original_coefficient_se_reported,
      orig_total_model_params = original_total_model_parameters,
      orig_p_value_type = original_p_value_type_reported,
      orig_p_value = original_p_value_value_reported,
      orig_p_value_tails = original_p_value_tails_reported,
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
    left_join(complex_bushel, by = "unique_claim_id") %>%
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
              original_poweranalysis_link,
              original_samplesize_calculation_contributor,
              original_analytic_sample_size_value_reference,
              original_statistic_fulltext_reference,
              original_statistic_type_reference,
              original_statistic_df1_reference,
              original_statistic_df2_reference,
              original_statistic_value_reference,
              original_statistic_effect_type_reference,
              original_effect_size_fulltext_reference,
              original_effect_size_type_reference,
              original_effect_size_value_reference,
              original_coefficient_type_reference,
              original_coefficient_value_reference,
              original_coefficient_se_reference,
              original_p_value_type_reference,
              original_p_value_value_reference,
              original_p_value_tails_reference,
              original_cos_notes,
              is_covid,
              original_materials_link,
              original_poweranalysis_notes,
              Tilburg_team_finished))

}