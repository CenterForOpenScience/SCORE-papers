# Merge P1 and P2 replication outcomes
# Gathers replication outcomes from P1 and P2, standardizes shared variables,
# and creates a merged table.
merge_repli_input <- function(repli_stats_p1,
                              replication_qa,
                              repli_stats_p2_additional,
                              repli_effective) {
  
  # Bring in P1 replication outcomes 
  p1_rr <- repli_stats_p1 %>%
    select(-c(rr_labteam_email, 
              rr_z_possible, 
              rr_z_statistic,
              rr_z_difference,
              rr_repro_exact_reproduced_reference, 
              Timestamp, 
              rr_data_source, 
              original_complete_analysis, 
              rr_complete_datacollection, 
              rr_labteam_contributor,
              rr_analysis_link)) %>%
    filter(rr_type %in% c("Direct Replication", 
                          "Data Analytic Replication",
                          "Hybrid")) %>%
    add_column(ready_for_export = NA)
  
  repli_effective <- rename(repli_effective, unique_report_id = `...1`)
  
  # Bring in extra many labs replication cases
  repli_cases <- repli_stats_p2_additional %>%
    mutate(rr_stat_version = 1) %>%
    select(-c(
      # These are calculated automatically later, not needed here
      rr_repl_exact_replicated_reference,
      sample_preference,
      ml_preference,
      rr_analysis_link
    )) %>%
    add_column(ready_for_export = NA)
  
  p2_repli <- replication_qa %>%
    add_column(rr_analytic_subsample_n1 = NA,
               rr_analytic_subsample_n2 = NA,
               rr_analytic_subsample_a_00 = NA,
               rr_analytic_subsample_b_01 = NA,
               rr_analytic_subsample_c_10 = NA,
               rr_analytic_subsample_d_11 = NA,
               rr_cos_notes = NA)
  
  rbind(p1_rr, p2_repli, repli_cases) %>%
    left_join(repli_effective, by = "unique_report_id") %>%
    # Changes needed in a field that exists in repli_outcomes only
    add_column(new_is_ml = NA_character_)
  
}