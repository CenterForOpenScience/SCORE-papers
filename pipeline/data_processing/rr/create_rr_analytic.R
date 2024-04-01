# Create Replications Analytic Dataset
create_repli_analytic <- function(repli_export,
                                  rr_attempts_minted,
                                  rr_statistics_output_p2,
                                  status,
                                  stitched_claims) {
  
  # Determine what delivery each RR was from
  type_p1 <- status %>%
    select(paper_id, p1_delivery) %>%
    filter(p1_delivery)
  
  type_p2 <- status %>%
    select(paper_id, p2_delivery) %>%
    filter(p2_delivery)
  
  type_bushel <- repli_export %>%
    filter(rr_type_internal %in% c("Direct Replication",
                                   "Data Analytic Replication")) %>%
    group_by(rr_id) %>%
    mutate(n = length(unique(claim_id))) %>%
    filter(n > 1)
  
  # Determine if a bushel claim has an equivalent single-trace claim
  single_equiv <- stitched_claims %>%
    select(paper_id, p1_equivalent, claim4_id) %>%
    filter(p1_equivalent) %>%
    mutate(claim_id = str_c(paper_id, "_", claim4_id))
  
  # Establish which version is the "version of record" for replications
  # Previously "repli_primary"
  repli_vor <- repli_export %>%
    filter(rr_type_internal %in% c("Direct Replication", 
                                   "Data Analytic Replication")) %>%
    arrange(rr_id,
            claim_id,
            sample_preference) %>%
    distinct(rr_id,
             claim_id,
             .keep_all = TRUE) %>%
    arrange(paper_id,
            claim_id,
            ml_preference) %>%
    distinct(paper_id,
             claim_id,
             .keep_all = TRUE) %>%
    select(unique_report_id)
  
  generalized <- rr_attempts_minted %>%
    filter(rr_type == "Generalizability") %>%
    select(rr_id)
  
  factors <- c("rr_statistic_interaction_reported",
               "rr_statistic_type_reported",
               "rr_coefficient_type_reported",
               "rr_p_value_confirmation_reported",
               "rr_effect_size_type_reported",
               "rr_repl_subjective_replicated_reported",
               "rr_effect_size_type_reference",
               "repli_type",
               "power_for_effect_size",
               "manylabs_type")
  
  repli_export %>%
    filter(rr_type_internal %in% c("Direct Replication",
                                   "Data Analytic Replication")) %>%
    select(-c(sample_preference, 
              ml_preference, 
              rr_type_internal)) %>%
    # Extended variables from Tilburg
    left_join(rr_statistics_output_p2, by = "unique_report_id") %>%
    mutate(
      claim_id = str_c(paper_id,
                       "_",
                       claim_id),
      repli_type = case_match(rr_type,
                              "Direct Replication" ~ "new data",
                              "Data Analytic Replication" ~ "secondary data",
                              "Hybrid" ~ "original and secondary data"),
      power_for_effect_size = case_match(
        rr_analytic_sample_stage,
        "stage 1" ~ "90% for 75%",
        "stage 2" ~ "90% for 50%",
        "threshold" ~ "50% for 100%",
        "no target" ~ "not performed",
        "lab target" ~ "lab power analysis"
      ),
      is_manylabs = case_match(rr_is_manylabs,
                               "non_ml" ~ FALSE,
                               .default = TRUE),
      manylabs_type = case_match(rr_is_manylabs,
                                 "ml_aggregation" ~ "aggregation",
                                 "ml_count" ~ "count",
                                 "ml_instance_primary" ~ "instance_primary",
                                 "non_ml" ~ NA,
                                 .default = rr_is_manylabs),
      repli_version_of_record = case_when(
        (unique_report_id %in% repli_vor$unique_report_id) ~ TRUE,
        .default = FALSE
      ),
      repli_is_generalizability = case_when(
        rr_id %in% generalized$rr_id ~ TRUE,
        .default = FALSE
      ),
      rr_statistic_type_reported = case_match(
        rr_statistic_type_reported,
        "chi_squared" ~ "chi-squared",
        "f" ~ "F",
        .default = rr_statistic_type_reported),
      rr_statistic_interaction_reported  = case_match(
        rr_statistic_interaction_reported,
        "main effect" ~ "main or simple effect",
        "simple/main effect" ~ "main or simple effect",
        .default = rr_statistic_interaction_reported
      ),
      repli_pearsons_r_defined = case_match(
        rr_pearsons_r_defined,
        "yes" ~ TRUE,
        "no" ~ FALSE
      ),
      type_internal = case_when(
        is_covid == TRUE ~ "covid",
        rr_id %in% type_bushel$rr_id ~ "bushel",
        paper_id %in% type_p1$paper_id ~ "p1",
        paper_id %in% type_p2$paper_id ~ "p2"
      ),
      single_trace_equivalent = case_when(
        type_internal == "bushel" & claim_id %in% single_equiv$claim_id ~ TRUE,
        .default = FALSE
      ),
      across(all_of(factors), as.factor)) %>%
    select(-c(rr_type,
              rr_analytic_sample_stage,
              rr_is_manylabs)) %>%
    rename(report_id = unique_report_id) %>%
    relocate(report_id, .after = rr_id) %>%
    relocate(claim_id, .after = paper_id) %>%
    # Name changes -----
    rename(
      repli_sample_size_units = rr_analytic_sample_size_units_reported,
      repli_sample_size_value = rr_analytic_sample_size_value_reported,
      repli_sample_structure = rr_analytic_sample_cells_reported,
      repli_analysis_type = rr_statistic_analysis_type_reported,
      repli_stat_interaction = rr_statistic_interaction_reported,
      repli_stat_type = rr_statistic_type_reported,
      repli_stat_value = rr_statistic_value_reported,
      repli_stat_dof_1 = rr_statistic_df1_reported,
      repli_stat_dof_2 = rr_statistic_df2_reported,
      repli_coef_type = rr_coefficient_type_reported,
      repli_coef_value = rr_coefficient_value_reported,
      repli_coef_se = rr_coefficient_se_reported,
      repli_p_value_confirmed = rr_p_value_confirmation_reported,
      repli_p_value = rr_p_value_value_reported,
      repli_effect_size_text_raw = rr_effect_size_fulltext_reported,
      repli_effect_size_type_raw = rr_effect_size_type_reported,
      repli_effect_size_value_raw = rr_effect_size_value_reported,
      repli_pattern_criteria = rr_repl_pattern_criteria_reported,
      repli_pattern_description = rr_repl_pattern_description_reported,
      repli_pattern_criteria_met = rr_repl_pattern_replicated_reported,
      repli_effect_direction = rr_repl_effect_direction_reported,
      repli_interpret_supported = rr_repl_subjective_replicated_reported,
      repli_interpret_support_notes = rr_repl_subjective_description_reported,
      repli_total_model_params = rr_total_model_parameters,
      repli_score_criteria_met = rr_repl_exact_replicated_reference,
      repli_effect_size_type = rr_effect_size_type_reference,
      repli_effect_size_value = rr_effect_size_value_reference,
      repli_effect_size_ci_ub = rr_es_ub_ci_nativeunits,
      repli_effect_size_ci_lb = rr_es_lb_ci_nativeunits,
      repli_pearsons_r_value = rr_pearsons_r_value,
      repli_pearsons_r_ci_ub = rr_es_ub_ci_pearson,
      repli_pearsons_r_ci_lb = rr_es_lb_ci_pearson,
      repli_effect_size_pi_ub = pi_ub_nativeunits,
      repli_effect_size_pi_lb = pi_lb_nativeunits,
      repli_power_threshold_small = rr_power_small,
      repli_power_threshold_medium = rr_power_medium,
      repli_power_for_50_effect = rr_power_50_original_effect,
      repli_power_for_75_effect = rr_power_75_original_effect
    ) %>%
    # Columns to drop ----
    select(-c(rr_original_data_overlap,
              rr_analysis_link,
              rr_expected_sample_reached_reported,
              rr_analytic_subsample_n1,
              rr_analytic_subsample_n2,
              rr_analytic_subsample_a_00,
              rr_analytic_subsample_b_01,
              rr_analytic_subsample_c_10,
              rr_analytic_subsample_d_11,
              rr_statistic_fulltext_reported,
              rr_repl_exact_replicated_reported,
              rr_labteam_notes,
              rr_cos_notes,
              rr_replication_difference_notes,
              rr_input_source,
              rr_stat_version,
              pdf_filename,
              is_covid,
              rr_pearsons_r_defined,
              # These two, previously repli_pearsons_r_ub and 
              # repli_pearsons_r_lb, appear not to actually be used
              pi_ub_pearson,
              pi_lb_pearson))
  
}

create_repro_analytic <- function(repro_export,
                                  repro_supplementary){
  
  repro_export %>%
    left_join(repro_supplementary, by = "unique_report_id") %>%
    mutate(
      claim_id = str_c(paper_id,
                       "_",
                       claim_id),
      repro_outcome_overall = case_when(
        repro_outcome_overall == "precise" & 
          rr_type_internal == "Push Button Reproduction" ~ "push button",
        .default = repro_outcome_overall
      )
    ) %>%
    select(
      paper_id,
      rr_id,
      claim_id,
      repro_type = rr_type_internal,
      orig_sample_criterion = orig_analytic_sample_size_value_criterion_reported,
      repro_sample_size_value = rr_analytic_sample_size_value_reported,
      repro_p_value = rr_p_value_value_reported,
      repro_coef_value = rr_coefficient_value_reported,
      repro_stat_type = rr_statistic_type_reported,
      repro_stat_value = rr_statistic_value_reported,
      repro_effect_size_type = rr_effect_size_type_reported,
      repro_effect_size_value = rr_effect_size_value_reported,
      repro_pattern_criteria = rr_repro_pattern_criteria_reported,
      repro_pattern_criteria_met = rr_repro_success_reported,
      repro_pattern_description = rr_repro_pattern_description_reported,
      repro_interpret_supported = rr_repro_analyst_success_reported,
      report_id = unique_report_id,
      repro_lb_sample,
      repro_ub_sample,
      repro_outcome_sample,
      repro_lb_coef,
      repro_ub_coef,
      repro_outcome_coef,
      repro_lb_stat,
      repro_ub_stat,
      repro_outcome_stat,
      repro_lb_p,
      repro_ub_p,
      repro_outcome_p,
      repro_lower_effect,
      repro_upper_effect,
      repro_outcome_effect,
      repro_outcome_overall,
      repro_secondary_criteria
    )
  
}
