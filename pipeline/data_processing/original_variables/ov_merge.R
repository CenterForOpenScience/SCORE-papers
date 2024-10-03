# Merge original variable input datasets
merge_orig_input <- function(orig_statistics_dataset_p1,
                             original_inftest_dataset,
                             orig_statistics_manual_data_entry,
                             orig_vars_qa,
                             tagtable_covid_p1,
                             orig_cases) {

  map(list(transform_orig_p1(orig_statistics_dataset_p1),
           transform_orig_inftest(original_inftest_dataset),
           transform_orig_manual(orig_statistics_manual_data_entry),
           transform_orig_qa(orig_vars_qa),
           orig_cases),
      \(x) select(x,
                  any_of(c(
                    # keys and reference variables
                    "paper_id",
                    "claim_id",
                    "unique_claim_id",
                    "orig_stat_version",
                    "original_data_source",
                    # reported variables
                    "original_statistic_analysis_type",
                    # sample size reported
                    "original_analytic_sample_size_units_reported",
                    "original_analytic_sample_size_value_reported",
                    "original_analytic_subsample_a_00",
                    "original_analytic_subsample_b_01",
                    "original_analytic_subsample_c_10",
                    "original_analytic_subsample_d_11",
                    "original_analytic_subsample_n1",
                    "original_analytic_subsample_n2",
                    # statistic reported
                    "original_statistic_fulltext_reported",
                    "original_statistic_type_reported",
                    "original_statistic_df1_reported",
                    "original_statistic_df2_reported",
                    "original_statistic_value_reported",
                    "original_statistic_effect_type_reported",
                    # effect size reported
                    "original_effect_size_fulltext_reported",
                    "original_effect_size_type_reported",
                    "original_effect_size_value_reported",
                    # coefficient reported
                    "original_coefficient_type_reported",
                    "original_coefficient_value_reported",
                    "original_coefficient_se_reported",
                    "original_total_model_parameters",
                    # p-value reported
                    "original_p_value_type_reported",
                    "original_p_value_value_reported",
                    "original_p_value_tails_reported",
                    # begin reference variables
                    "original_poweranalysis_link",
                    "original_samplesize_calculation_contributor",
                    # sample size reference
                    "original_analytic_sample_size_value_reference",
                    # statistic reference
                    "original_statistic_fulltext_reference",
                    "original_statistic_type_reference",
                    "original_statistic_df1_reference",
                    "original_statistic_df2_reference",
                    "original_statistic_value_reference",
                    "original_statistic_effect_type_reference",
                    # effect size reference
                    "original_effect_size_fulltext_reference",
                    "original_effect_size_type_reference",
                    "original_effect_size_value_reference",
                    # coefficient reference
                    "original_coefficient_type_reference",
                    "original_coefficient_value_reference",
                    "original_coefficient_se_reference",
                    # p-value reference
                    "original_p_value_type_reference",
                    "original_p_value_value_reference",
                    "original_p_value_tails_reference",
                    # power analysis notes
                    "original_cos_notes",
                    # power analysis sample estimates
                    "rr_threshold_analytic_sample_size",
                    "rr_stage1_analytic_sample_size",
                    "rr_stage2_analytic_sample_size",
                    # misc
                    "is_covid",
                    "original_materials_link"
                  )))) %>%
    reduce(bind_rows) %>%
    mutate(orig_stat_version = 1,
           unique_claim_id = str_c(paper_id,
                                   "_",
                                   claim_id),
           is_covid = (paper_id %in% tagtable_covid_p1$paper_id))

}