# Apply changelog updates
# Applies fixes identified after initial data entry to rr_replication_outcomes
update_repli_input <- function(repli_merged, changes_repli_merged) {
  
  rr_replication_outcomes <- repli_merged %>%
    apply_changelog(
      changes_repli_merged %>% 
        # A field was dropped, removed for smooth updating
        filter(col_name != "rr_analysis_link"),
      "unique_report_id"
    )
  
  # Data type changes 
  mutate(
    rr_replication_outcomes,
    rr_statistic_value_reported = as.double(rr_statistic_value_reported)
  ) %>%
    filter(!(ready_for_export %in% c("Never", "Issues"))) %>%
    select(-c(ready_for_export))
  
}

# Deduplicates claim evaluations within the same RR attempt, retaining last
# (i.e. most recent) form response. Calculates replication success variable.
update_transform_repli <- function(repli_merged, changes_repli_merged) {
  
  repli_data_entry <- update_repli_input(repli_merged, changes_repli_merged)
  
  repli_data_entry %>% 
    # Drop P1 coding in favor of P2 where duplicates exist
    arrange(desc(rr_input_source)) %>%
    distinct(
      rr_id,
      claim_id,
      rr_analytic_sample_stage,
      .keep_all = TRUE
    ) %>%
    mutate(
      # Calculate replication success variable
      rr_repl_exact_replicated_reference = 
        rr_repl_pattern_replicated_reported & rr_p_value_value_reported < 0.05,
      # Set sample preference order
      sample_preference = case_match(
        str_to_lower(rr_analytic_sample_stage),
        "stage 2" ~ 1,
        "stage 1" ~ 2,
        .default = 3
      ),
      # Set manylabs preference order
      ml_preference = case_match(
        rr_is_manylabs,
        "ml_aggregation" ~ 1,
        "ml_instance_primary" ~ 2,
        .default = 3
      ),
      # Replications classified as "Generalizability" can be treated as Direct
      # Replications for these purposes
      rr_type_internal = case_match(
        rr_type_internal,
        "Generalizability" ~ "Direct Replication",
        .default = rr_type_internal
      ),
      rr_type = case_match(
        rr_type,
        "Generalizability" ~ "Direct Replication",
        .default = rr_type
      )
    )
}
