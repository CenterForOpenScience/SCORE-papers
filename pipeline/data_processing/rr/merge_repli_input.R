# Merge P1 and P2 replication outcomes
# Gathers replication outcomes from P1 and P2, standardizes shared variables,
# and creates a merged table.
merge_repli_input <- function(rr_outcomes_dataset_p1,
                              replication_qa,
                              rr_reporting_checkin,
                              replication_cases,
                              effectsize_repli) {
  
  # Bring in P1 replication outcomes 
  p1_rr <- rr_outcomes_dataset_p1 %>%
    select(-c('rr_labteam_email', 
              'rr_z_possible', 
              'rr_z_statistic',
              'rr_z_difference',
              'rr_repro_exact_reproduced_reference', 
              'Timestamp', 
              'rr_data_source', 
              'original_complete_analysis', 
              'rr_complete_datacollection', 
              'rr_labteam_contributor')) %>%
    filter(rr_type %in% c("Direct Replication", 
                          "Data Analytic Replication",
                          "Hybrid")) %>%
    add_column(ready_for_export = NA)
  
  repli_effective <- rename(effectsize_repli, unique_report_id = `...1`)
  
  # Bring in extra many labs replication cases
  repli_cases <- replication_cases %>%
    mutate(rr_stat_version = 1) %>%
    select(-c(
      # These are calculated automatically later, not needed here
      rr_repl_exact_replicated_reference,
      sample_preference,
      ml_preference
    )) %>%
    add_column(ready_for_export = NA)
  
  # Analysis links for P2 projects come from report check-ins
  checkin <- select(rr_reporting_checkin,
                    rr_id,
                    rr_analysis_link = rr_osf_vol)
  
  p2_repli <- replication_qa %>%
    add_column(rr_analytic_subsample_n1 = NA,
               rr_analytic_subsample_n2 = NA,
               rr_analytic_subsample_a_00 = NA,
               rr_analytic_subsample_b_01 = NA,
               rr_analytic_subsample_c_10 = NA,
               rr_analytic_subsample_d_11 = NA,
               rr_cos_notes = NA) %>%
    left_join(checkin, 
              by = "rr_id",
              # There appears to be duplicate rr_ids, so we'll match all
              multiple = "all",
              relationship = "many-to-many")
  
  rbind(p1_rr, p2_repli, repli_cases) %>%
    left_join(repli_effective, by = "unique_report_id") %>%
    # Changes needed in a field that exists in repli_outcomes only
    add_column(new_is_ml = NA_character_)
  
}