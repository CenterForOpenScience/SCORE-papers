# Export Replication

# Merge P1 and P2 replication outcomes
# Gathers replication outcomes from P1 and P2, standardizes shared variables,
# and creates a merged table.
merge_repli_input <- function(rr_outcomes_dataset_p1,
                              replication_qa,
                              rr_reporting_checkin,
                              replication_cases) {
  
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
                          "Hybrid"))
  
  # Bring in extra many labs replication cases
  repli_cases <- replication_cases %>%
    mutate(rr_stat_version = 1) %>%
    select(-c(
      # These are calculated automatically later, not needed here
      rr_repl_exact_replicated_reference,
      sample_preference,
      ml_preference
      ))
  
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
    mutate(rr_is_manylabs = case_when(
      rr_id %in% c("92g",
                   "24716",
                   "944y",
                   "10g2",
                   "9977",
                   "128g6",
                   "7g66",
                   "2kgg2",
                   "z51636O9",
                   "651m6_45_6",
                   "5g9",
                   "6ow46",
                   "9kzy",
                   "24316",
                   "89z7",
                   "288g2",
                   "6738",
                   "6m396",
                   "276",
                   "2y486") ~ "ml_count",
      .default = rr_is_manylabs
    ))
  
}

# Apply changelog updates
# Applies fixes identified after initial data entry to rr_replication_outcomes
update_repli_input <- function(rr_outcomes_dataset_p1,
                               replication_qa,
                               rr_reporting_checkin,
                               replication_cases,
                               repli_input_changelog) {
  
  rr_replication_outcomes <- merge_repli_input(rr_outcomes_dataset_p1,
                                               replication_qa,
                                               rr_reporting_checkin,
                                               replication_cases)
  
  # We only want to work with the highest version for each claim
  changelog <- repli_input_changelog %>% 
    arrange(unique_report_id,
            col_name,
            desc(rr_stat_version)) %>%
    distinct(unique_report_id,
             col_name,
             .keep_all = TRUE)
  
  versions <- changelog %>%
    group_by(unique_report_id) %>%
    summarise(max_version = max(rr_stat_version))
  
  # List of variables that should be numeric for the purposes of adding data
  # from the changelog to the dataset
  num_list <- c("rr_analytic_sample_size_value_reported",
                "rr_analytic_subsample_n1",
                "rr_analytic_subsample_n2",
                "rr_analytic_subsample_a_00",
                "rr_analytic_subsample_b_01",
                "rr_analytic_subsample_c_10",
                "rr_analytic_subsample_d_11",
                "rr_statistic_df1_reported",
                "rr_statistic_df2_reported",
                "rr_coefficient_value_reported",
                "rr_coefficient_se_reported",
                "rr_p_value_value_reported",
                "rr_effect_size_value_reported",
                "rr_total_model_parameters",
                "rr_stat_version",
                "rr_statistic_value_reported")
  
  bool_list <- c("rr_repl_effect_direction_reported",
                 "rr_expected_sample_reached_reported",
                 "rr_repl_pattern_description_reported",
                 "rr_repl_exact_replicated_reported",
                 "is_covid")
  
  for (i in 1:nrow(changelog)) {
    
    col_name <- changelog[i, ]$col_name
    
    version <- versions %>%
      filter(unique_report_id == changelog[i, ]$unique_report_id) %>%
      pull(max_version)
    
    if (col_name %in% num_list) {
      change_to <- as.numeric(changelog[i, ]$change_to)
    } else if (col_name %in% bool_list) {
      change_to <- as.logical(changelog[i, ]$change_to)
    } else {
      change_to <- changelog[i, ]$change_to
    }
    
    change_tbl <- tibble(unique_report_id = changelog[i, ]$unique_report_id,
                         rr_stat_version = version,
                         {{ col_name }} := change_to)
    
    rr_replication_outcomes <- rr_replication_outcomes %>% 
      rows_update(change_tbl, by = "unique_report_id")

  }
  
  # Data type changes 
  mutate(rr_replication_outcomes,
         rr_statistic_value_reported = as.double(rr_statistic_value_reported))
  
}

# Finalize RR attempt output
# Deduplicates claim evaluations within the same RR attempt, retaining last
# (i.e. most recent) form response. Calculates replication success variable.
export_repli <- function(rr_outcomes_dataset_p1,
                         replication_qa,
                         rr_reporting_checkin,
                         replication_cases,
                         repli_input_changelog) {
  
  repli_data_entry <- update_repli_input(rr_outcomes_dataset_p1,
                                         replication_qa,
                                         rr_reporting_checkin,
                                         replication_cases,
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
