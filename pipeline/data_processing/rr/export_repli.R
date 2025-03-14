# Finalize RR attempt output
# Deduplicates claim evaluations within the same RR attempt, retaining last
# (i.e. most recent) form response. Calculates replication success variable.
export_repli <- function(repli_merged,
                         repli_input_changelog) {
  
  repli_data_entry <- update_repli_input(repli_merged,
                                         repli_input_changelog)
  
  repli_data_entry %>% 
    # Drop P1 coding in favor of P2 where duplicates exist
    arrange(desc(rr_input_source)) %>%
    distinct(rr_id,
             claim_id,
             rr_analytic_sample_stage,
             .keep_all = TRUE) %>%
    mutate(
      # Calculate replication success variable
      rr_repl_exact_replicated_reference = 
        rr_repl_pattern_replicated_reported & rr_p_value_value_reported < 0.05,
      # Set sample preference order
      sample_preference = case_match(str_to_lower(rr_analytic_sample_stage),
                                     "stage 2" ~ 1,
                                     "stage 1" ~ 2,
                                     .default = 3),
      # Set manylabs preference oder
      ml_preference = case_match(rr_is_manylabs,
                                 "ml_aggregation" ~ 1,
                                 "ml_instance_primary" ~ 2,
                                 .default = 3),
      # Replications classified as "Generalizability" can be treated as Direct
      # Replications for these purposes
      rr_type_internal = case_match(rr_type_internal,
                                    "Generalizability" ~ "Direct Replication",
                                    .default = rr_type_internal),
      rr_type = case_match(rr_type,
                           "Generalizability" ~ "Direct Replication",
                           .default = rr_type)
    )
}
