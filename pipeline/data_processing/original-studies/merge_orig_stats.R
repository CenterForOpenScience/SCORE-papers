# Merge original variable input datasets
merge_orig_stats <- function(orig_stats_p1,
                             orig_claims_p1_covid,
                             orig_stats_p2_pwr,
                             orig_stats_p2_manual,
                             orig_stats_p2_form,
                             orig_stats_p2_additional) {

  map(
    list(transform_orig_p1(orig_stats_p1),
         transform_orig_inftest(orig_stats_p2_pwr),
         transform_orig_manual(orig_stats_p2_manual),
         transform_orig_qa(orig_stats_p2_form),
         orig_stats_p2_additional),
    \(x) select(
      x,
      any_of(
        c(# keys and reference variables
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
          "original_materials_link")
      )
    )
  ) %>%
    reduce(bind_rows) %>%
    mutate(
      orig_stat_version = 1,
      unique_claim_id = str_c(paper_id,
                              "_",
                              claim_id),
      is_covid = (paper_id %in% orig_claims_p1_covid$paper_id)
    )

}

# Sub-functions ----
# Transform original variables manual data
transform_orig_manual <- function(orig_stats_p2_manual) {
  
  orig_stats_p2_manual %>%
    mutate(
      original_data_source = "p2_manual",
      # sometimes sample size units are identified under a made up variable -
      # original_analytic_sample_size_units_reference - but
      # original_analytic_sample_size_units_reported is the canonical original
      # sample size units field, so bring those values over where they are
      # missing in the expected field
      original_analytic_sample_size_units_reported = coalesce(
        original_analytic_sample_size_units_reported,
        original_analytic_sample_size_units_reference
      )
    ) %>%
    # These variables were misnamed during this coding pipeline and need to
    # be standardized to merge with other sources of original variables
    rename(
      claim_id = c4_claim_id,
      original_statistic_analysis_type = original_analysis_type_cos,
      original_poweranalysis_link = rr_poweranalysis_link,
      original_statistic_effect_type_reported =
        original_statistic_interaction_reported,
      original_statistic_effect_type_reference =
        original_statistic_interaction_reference
    )
}

# Transform original variables inftest data
transform_orig_inftest <- function(orig_stats_p2_pwr) {
  
  orig_stats_p2_pwr %>%
    mutate(
      claim_id = "single-trace",
      original_data_source = "p2_power",
      # Sometimes analysis type was set under a different variable name.
      # If that is missing in the expected field
      # (original_statistic_analysis_type), try to fill it in from
      # original_statistic_analysis_cos
      original_statistic_analysis_type = coalesce(
        original_statistic_analysis_type,
        original_analysis_type_cos
      ),
      # Sometimes sample size units are identified under a made up variable -
      # original_analytic_sample_size_units_reference - but
      # original_analytic_sample_size_units_reported is the canonical original
      # sample size units field, so bring those values over where they are
      # missing in the expected field
      original_analytic_sample_size_units_reported = coalesce(
        original_analytic_sample_size_units_reported,
        original_analytic_sample_size_units_reference
      )
    ) %>%
    # These variables were misnamed during this coding pipeline and need to
    # be standardized to merge with other sources of original variables
    rename(
      original_poweranalysis_link = rr_poweranalysis_link,
      original_statistic_effect_type_reported =
        original_statistic_interaction_reported,
      original_statistic_effect_type_reference =
        original_statistic_interaction_reference
    )
}

# Transform P1 original variables data
transform_orig_p1 <- function(orig_stats_p1) {
  
  orig_stats_p1 %>%
    # Where there are duplicates, prefer version coded orig_via_repro_form
    # under the assumption it was an update to a previous entry
    arrange(paper_id, original_data_source) %>%
    distinct(paper_id, .keep_all = TRUE) %>%
    mutate(
      # Records coded orig_via_repro_form have some unexpected new fields that
      # only contain values for such cases and seem to correspond to
      # canonical variables - put those values where they seem to belong.
      # Of note, original_p_value_type_reference and
      # original_effect_size_fulltext_reference also fall into this category
      # of being unique to orig_via_repro_form cases. However, they are valid
      # columns in phase 2 original variables coding, so I do not move their
      # values to analogous fields during this step.
      original_analysis_type_COS = coalesce(
        original_analysis_type_COS,
        original_statistic_analysis_type_reference
      ),
      original_statistic_effect_type_reported = coalesce(
        original_statistic_effect_type_reported,
        original_statistic_interaction_reported
      ),
      original_statistic_effect_type_reference = coalesce(
        original_statistic_effect_type_reference,
        original_statistic_interaction_reference
      ),
      claim_id = "single-trace"
    ) %>%
    rename(
      original_statistic_analysis_type = original_analysis_type_COS,
      original_samplesize_calculation_contributor = original_cos_contributor
    )
  
}

# Transform original variables entered through the Google form
transform_orig_qa <- function(orig_stats_p2_form) {
  
  orig_stats_p2_form %>%
    mutate(original_data_source = "p2_pipeline") %>%
    rename(claim_id = c4_claim_id)
  
}
