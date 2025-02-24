# Create Replications Analytic Dataset
create_repli_analytic <- function(repli_export,
                                  rr_attempts_minted,
                                  rr_statistics_output_p2,
                                  status,
                                  stitched_claims,
                                  effectsize_outcome,
                                  orig_outcomes,
                                  paper_metadata,
                                  repli_case_exclusions) {
  
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
    select(unique_report_id) %>%
    # These were originally thought to be ML studies, but ended up not,
    # so the original coding is inaccurate for them
    rbind("y2312_91_1", "m707Z7y")
  
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
  
  repli <- repli_export %>%
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
      is_manylabs = case_when(
        rr_is_manylabs == "non_ml" ~ "not manylabs",
        !is.na(new_is_ml) ~ new_is_ml,
        .default = "is manylabs"
      ),
      manylabs_type = case_match(rr_is_manylabs,
                                 "ml_aggregation" ~ "aggregation",
                                 "ml_count" ~ "count",
                                 "ml_instance_primary" ~ "instance_primary",
                                 "non_ml" ~ NA,
                                 .default = rr_is_manylabs),
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
      repli_version_of_record = case_when(
        # bushel replications that get used in separate many-labs aggregations
        # shouldn't be VOR
        single_trace_equivalent == TRUE & is_manylabs != "not manylabs" ~ FALSE,
        # otherwise use the normal VOR filtering scheme
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
      across(all_of(factors), as.factor),
      rr_statistic_type_reported = case_match(
        rr_statistic_type_reported,
        "chi-squared" ~ "chi_squared",
        .default = rr_statistic_type_reported),
      repli_stat_df_1 = coalesce(repli_effective_df1, 
                                  rr_statistic_df1_reported),
      sample_size_value_effective = coalesce(
        repli_effective_sample_size,
        rr_analytic_sample_size_value_reported)
      ) %>%
    select(-c(rr_type,
              rr_analytic_sample_stage,
              rr_is_manylabs)) %>%
    rename(report_id = unique_report_id) %>%
    relocate(report_id, .after = rr_id) %>%
    relocate(claim_id, .after = paper_id) %>%
    # Name changes -----
    rename(
      repli_stat_dof_1 = rr_statistic_df1_reported,
      repli_sample_size_value = rr_analytic_sample_size_value_reported,
      repli_sample_size_units = rr_analytic_sample_size_units_reported,
      repli_sample_structure = rr_analytic_sample_cells_reported,
      repli_analysis_type = rr_statistic_analysis_type_reported,
      repli_stat_interaction = rr_statistic_interaction_reported,
      repli_stat_type = rr_statistic_type_reported,
      repli_stat_value = rr_statistic_value_reported,
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
              rr_pearsons_r_defined,
              new_is_ml,
              # These two, previously repli_pearsons_r_ub and 
              # repli_pearsons_r_lb, appear not to actually be used
              pi_ub_pearson,
              pi_lb_pearson))
  
  manual <- effectsize_outcome %>%
    rename(report_id = `...1`,
           cos_r = r,
           cos_r_lb = r_lb,
           cos_r_ub = r_ub) %>%
    filter(report_id %in% repli$report_id)
  
  effect_sizes <- convert_to_cosr(repli, "report_id")
  
  repli %>%
    left_join(effect_sizes, by = "report_id") %>%
    rows_update(manual, by = "report_id") %>%
    select(-c(repli_stat_df_1,
              sample_size_value_effective,
              lor_conversion)) %>%
    rename(repli_conv_r = cos_r,
           repli_conv_r_lb = cos_r_lb,
           repli_conv_r_ub = cos_r_ub) %>%
    filter(!(report_id %in% repli_case_exclusions$report_id))
  
}
