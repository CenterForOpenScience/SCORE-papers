# Update replications status

# Reads changelog, original variables, Tilburg input, and current replication 
# outcomes. Determines outcome-level status for whether the outcome:
# - is in the changelog
# - has corresponding original variables coded
# - has been delivered to Tilburg
# - has been selected for RR audit
# - is canonical version of rr_id x claim_id x rr_type x sample_stage outcome
#   (i.e., is active)
# - Save current status
update_repli_outcomes_status <- function(replication_outcome_status,
                                         repli_input_changelog,
                                         orig_dataset,
                                         rr_statistics_input,
                                         repli_export){

  # Read comparison tables
  changelog <- select(repli_input_changelog, unique_report_id) %>%
    pull()
  
  orig_vars <- select(orig_dataset, unique_claim_id) %>%
    pull()
  
  tilburg <- select(rr_statistics_input, unique_report_id) %>%
    pull()
  
  # Gather currently coded outcomes
  repli_outcomes <- select(repli_export, unique_report_id)
  
  # Perserve column order of replication_outcome_status
  status_cols <- names(replication_outcome_status)
  
  # Add new outcomes, retain previously established records
  status <- add_row(replication_outcome_status,
                    repli_outcomes) %>%
    distinct(unique_report_id, .keep_all = TRUE) %>%
    mutate(
      # Create reference unique claim ID column to compare against orig vars
      unique_claim_id = str_c(paper_id,
                              "_",
                              claim_id),
      in_changelog = case_match(unique_report_id,
                                changelog ~ TRUE,
                                .default = FALSE),
      original_vars_coded = case_match(unique_claim_id,
                                       orig_vars ~ TRUE,
                                       .default = FALSE),
      active = case_match(unique_report_id,
                          pull(repli_outcomes) ~ TRUE,
                          .default = FALSE),
      audit = replace_na(audit,
                         FALSE),
      tilburg_status = case_when(
        rr_type_internal == "Hybrid" ~ "no-extension",
        unique_report_id %in% tilburg ~ "sent",
        .default = "not-sent"
      )
    ) %>%
    select(all_of(status_cols))
  
  write_csv(status,
            here(str_c("data/rr/status/",
                       "replication_outcome_status.csv")))
  
  here(str_c("data/rr/status/",
             "replication_outcome_status.csv"))
  
}

# - Reads rr_attempt_status.csv and repli/hybrid outcome status table
# - Checks which repli-type projects have been checked-in and have 
#   as many outcomes available as were indicated during report checkin
# - Updates repli coding status per attempt
# - Saves rr_attempt_status.csv
update_repli_attempts_status <- function(replication_outcome_status,
                                        rr_attempt_status){
  
  status_cols <- names(rr_attempt_status)
  
  # These projects were checked in with some number of outcomes (x) greater 
  # than what was coded (y), and have been verified to be correct insofar as 
  # they do have x claim evaluations, but only y were eligible; the difference 
  # between x-y is attributable to complex claims which have no simple test 
  # eligible inferential test, and accordingly only produce complex evidence 
  # that is not coded
  complete_repli_projects <- c(
    '6m34m', # 6 expected, 5 actual
    '24m16', # 6 expected, 4 actual
    '6m33m', # 5 expected, 4 actual
    '2wyw2', # 6 expected, 4 actual
    '658y7', # 6 expected, 4 actual
    '6z3o2', # 8 expected, 4 actual
    '23g12' # 6 expected, 4 actual
  )
  
  rr_summary <- select(replication_outcome_status,
                       paper_id,
                       rr_id,
                       claim_id,
                       rr_type_internal,
                       active) %>%
    filter(active) %>%
    # Gather summary of outcomes by rr_id
    count(rr_id, rr_type_internal) %>%
    mutate(rr_type_internal = case_match(
      rr_type_internal,
      "Direct Replication" ~ "dr_outcomes_actual",
      "Data Analytic Replication" ~ "dar_outcomes_actual",
      "Hybrid" ~ "hybrid_outcomes_actual")) %>%
    pivot_wider(names_from = rr_type_internal,
                values_from = n,
                values_fill = 0)
  
  rr_status <- rr_attempt_status %>%
    left_join(rr_summary, by = "rr_id") %>%
    replace_na(list(dr_outcomes_actual = 0,
                    dar_outcomes_actual = 0,
                    hybrid_outcomes_actual = 0)) %>%
    mutate(report_checkin_status = 
      repli_outcomes_status == TRUE & (
          (
            dr_outcomes_actual == rr_outcomes_count_dr &
            dar_outcomes_actual == rr_outcomes_count_dar &
            hybrid_outcomes_actual == rr_outcomes_count_hybrid
          ) | (
            rr_id %in% complete_repli_projects
          )
      )
    ) %>%
    select(all_of(status_cols))
  
  write_csv(rr_summary,
            here(str_c("data/rr/status/",
                       "rr_attempts_status.csv")))
  
  here(str_c("data/rr/status/",
             "rr_attempts_status.csv"))
    
}
