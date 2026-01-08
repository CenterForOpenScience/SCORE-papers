transform_repli_stats_p2 <- function(repli_stats_p2,
                                     score_rr_types) {
  
  repli_stats_p2 %>%
    # Original row index is used to create unique record id later
    mutate(original_index = as.character(row_number()-1)) %>%
    mutate(
      pdf_filename = str_extract(asana_ticket_name, ".+?(?= - )"),
      paper_id = str_extract(pdf_filename, "([^_]*)$"),
      rr_id = str_extract(asana_ticket_name, "([^(?= - )]*)$"),
      # These are convenience variables for merging with P1
      is_covid = str_detect(pdf_filename, "covid"),
      rr_input_source = "p2_repli_form",
      rr_is_manylabs = "non_ml",
      # Some QA entries have multiple records that need to be pivoted out
      across(where(is.character), ~ str_split(.x, "\\n"))
    ) %>%
    unnest(everything()) %>%
    group_by(original_index) %>%
    mutate(response_n = as.character(row_number())) %>%
    ungroup() %>%
    rename(claim_id = c4_claim_id) %>%
    mutate(
      unique_report_id = str_c(rr_id, "_", original_index, "_", response_n),
      across(where(is.character), ~ na_if(.x, "na")),
      # Make sure everything is the correct data type again
      across(where(is.list), as.character),
      # One value is reported in a nonstandard way that results in a 
      # warning/NA value. Fixed in change log.
      across(all_of(c("rr_num_claims_report",                  
                      "rr_num_outcomes_report",                
                      "rr_analytic_sample_size_value_reported",
                      "rr_coefficient_value_reported",         
                      "rr_coefficient_se_reported",            
                      "rr_total_model_parameters",             
                      "rr_statistic_value_reported",           
                      "rr_statistic_df1_reported",             
                      "rr_statistic_df2_reported",             
                      "rr_p_value_value_reported",             
                      "rr_effect_size_value_reported")), ~ as.double(.x)),
      across(all_of(c("rr_original_data_overlap",              
                      "rr_analytic_sample_stage",              
                      "rr_expected_sample_reached_reported",   
                      "rr_statistic_analysis_type_reported",   
                      "rr_statistic_interaction_reported",     
                      "rr_coefficient_type_reported",          
                      "rr_statistic_type_reported",            
                      "rr_p_value_confirmation_reported",      
                      "rr_effect_size_type_reported",          
                      "rr_repl_effect_direction_reported",     
                      "rr_repl_pattern_replicated_reported",   
                      "rr_repl_exact_replicated_reported",     
                      "rr_repl_subjective_replicated_reported")), 
             ~ as.factor(.x) %>% str_to_lower()),
      rr_stat_version = 1,
      rr_effect_size_type_reported = case_match(
        rr_effect_size_type_reported,
        "cohen's f-squared" ~ "cohen_f_squared",
        "cohen's d" ~ "cohen_d",
        "ser method" ~ "ser_method",
        "pearson's r" ~ "pearson_r",
        "cohen's dz" ~ "cohen_dz",
        "cohen's w" ~ "cohen_w",
        "partial eta-squared" ~ "partial_eta_squared",
        "eta-squared" ~ "eta_squared",
        "partial correlation" ~ "partial_correlation",
        "cramer's v" ~ "cramer_v",
        "cohen's q" ~ "cohen_q",
        "odds ratio" ~ "odds_ratio",
        "log-odds ratio" ~ "log_odds_ratio",
        "hazard ratio" ~ "hazard_ratio",
        "spearman's rho" ~ "spearman_rho",
        .default = rr_effect_size_type_reported
      ),
      across(
        c(rr_expected_sample_reached_reported,
          rr_repl_pattern_replicated_reported,
          rr_repl_effect_direction_reported,
          rr_repl_exact_replicated_reported), 
        ~ .x == "yes"
      )
    ) %>%
    # Minting data is used to determine RR type
    left_join(
      select(score_rr_types, 
             rr_type,
             rr_id,
             is_hsr), 
      by = "rr_id"
    ) %>%
    rename(minted_as = rr_type) %>%
    mutate(
      rr_type_internal = case_when(
        minted_as == "Generalizability" ~ "Generalizability",
        rr_original_data_overlap == "hybrid" ~ "Hybrid",
        is_hsr == "Non-HSR" ~ "Data Analytic Replication",
        is_hsr == "HSR" ~ "Direct Replication",
        .default = "Undefined"
      ),
      rr_type = rr_type_internal
    ) %>%
    select(
      pdf_filename,
      paper_id, 
      rr_id,
      claim_id,
      unique_report_id,
      rr_input_source,
      rr_is_manylabs,
      rr_original_data_overlap, 
      rr_analytic_sample_stage,
      rr_analytic_sample_size_value_reported,
      rr_analytic_sample_size_units_reported,
      rr_expected_sample_reached_reported,
      rr_analytic_sample_cells_reported, 
      rr_statistic_fulltext_reported,
      rr_statistic_analysis_type_reported,
      rr_statistic_interaction_reported,
      rr_coefficient_type_reported,
      rr_coefficient_value_reported,
      rr_coefficient_se_reported,
      rr_total_model_parameters, 
      rr_statistic_type_reported,
      rr_statistic_value_reported,
      rr_statistic_df1_reported,
      rr_statistic_df2_reported,
      rr_p_value_value_reported,
      rr_p_value_confirmation_reported, 
      rr_effect_size_fulltext_reported,
      rr_effect_size_type_reported,
      rr_effect_size_value_reported,
      rr_repl_effect_direction_reported, 
      rr_replication_difference_notes,
      rr_repl_pattern_criteria_reported,
      rr_repl_pattern_description_reported,
      rr_repl_pattern_replicated_reported,
      rr_repl_exact_replicated_reported,
      rr_repl_subjective_replicated_reported,
      rr_repl_subjective_description_reported, 
      rr_labteam_notes,
      is_covid,
      rr_type_internal,
      rr_type,
      rr_stat_version,
      ready_for_export
    )
}
