# Original Variable Transform Functions

# Transform Tilburg extension data
transform_orig_output <- function(orig_statistics_output_p2) {

  orig_statistics_output_p2 %>%
    # All character values and variable names are encased in quotes, so we need
    # to deal with that to get all of the lines to read in correctly, even when
    # there is quoted text within a value. Values that contain actual
    # quotations will have double quotes replaced with single quotes.
    mutate(
      across(where(is.character), ~ str_remove_all(.x, "\"")),
      across(where(is.character), ~ str_replace_all(.x, "\\\\", "\'"))
    ) %>%
    rename_with(~ str_remove_all(.x, "\"")) %>%
    select(paper_id,
           claim_id = c4_claim_id,
           original_statistic_analysis_type_statsteam = analysis_type,
           original_pearsons_r_defined,
           original_pearsons_r_numeric,
           original_es_lb_ci_pearson,
           original_es_ub_ci_pearson,
           original_effect_size_type_statsteam,
           original_effect_size_value_statsteam,
           original_es_lb_ci_nativeunits,
           original_es_ub_ci_nativeunits,
           original_power_small,
           original_power_medium,
           original_power_50_original_effect,
           original_power_75_original_effect,
           Tilburg_team_finished) %>%
    # Including "skipped" as finished because Tilburg has finished what they
    # can/will do with that claim
    mutate(Tilburg_team_finished = case_match(Tilburg_team_finished,
                                              "skipped" ~ TRUE,
                                              "finished" ~ TRUE,
                                              NA ~ NA,
                                              .default = FALSE))

}

# Transform P1 original variables data
transform_orig_p1 <- function(orig_statistics_dataset_p1) {

  orig_statistics_dataset_p1 %>%
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

# Transform original variables inftest data
transform_orig_inftest <- function(original_inftest_dataset) {

  original_inftest_dataset %>%
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

# Transform original variables manual data
transform_orig_manual <- function(orig_statistics_manual_data_entry) {

  orig_statistics_manual_data_entry %>%
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

# Transform original variables entered through the Google form
transform_orig_qa <- function(orig_vars_qa) {

  orig_vars_qa %>%
    mutate(original_data_source = "p2_pipeline") %>%
    rename(claim_id = c4_claim_id)

}

# Transform complex bushel
transform_complex_bushel <- function(complex_bushel) {
  
  complex_bushel %>%
  mutate(unique_claim_id = str_c(paper_id, "_", claim_id),
         complex = as.logical(complex)) %>%
        select(unique_claim_id,
               bushel_complex = complex)
}
