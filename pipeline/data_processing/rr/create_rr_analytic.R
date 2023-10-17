
# Create Replications Analytic Dataset
create_repli_analytic <- function(repli_export,
                                  rr_attempts_minted,
                                  rr_statistics_output_p2) {
  
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
               "rr_input_source",
               "rr_effect_size_type_reference",
               "repli_type",
               "power_for_effect_size",
               "manylabs_type")
  
  repli_outcomes <- repli_export %>%
    # May remove at some point?
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
                                 "non_ml" ~ NA,
                                 .default = rr_is_manylabs),
      is_vor = case_when(
        (unique_report_id %in% repli_vor$unique_report_id) ~ TRUE,
        .default = FALSE
      ),
      is_generalizability = case_when(
        rr_id %in% generalized$rr_id ~ TRUE,
        .default = FALSE
      ),
      rr_statistic_type_reported = case_match(
        rr_statistic_type_reported,
        "chi_squared" ~ "chi-squared",
        .default = rr_statistic_type_reported),
      across(factors, as.factor)) %>%
    select(-c(rr_type,
              rr_analytic_sample_stage,
              rr_is_manylabs)) %>%
    rename(report_id = unique_report_id) %>%
    relocate(report_id, .after = rr_id) %>%
    relocate(claim_id, .after = paper_id)
  
}
