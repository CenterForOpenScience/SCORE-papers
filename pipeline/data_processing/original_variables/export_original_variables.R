#######################
# EXPORT ORIGINAL VARIABLES
# Scripts to make and update the original variables from most claims that were 
# used in RR.
#######################

# Merge original variables datasets
# Create table of raw original variables set by merging from various sources
merge_orig_input <- function(orig_statistics_dataset_p1,
                             original_inftest_dataset,
                             orig_statistics_manual_data_entry,
                             orig_vars_qa,
                             tagtable_covid_p1) {
  
  orig_columns <- c(
    # keys and reference variables
    'paper_id',
    'claim_id',
    'unique_claim_id',
    'orig_stat_version',
    'original_data_source',
    # begin reported variables
    'original_statistic_analysis_type',
    # sample size reported
    'original_analytic_sample_size_units_reported',
    'original_analytic_sample_size_value_reported',
    'original_analytic_subsample_a_00',
    'original_analytic_subsample_b_01',
    'original_analytic_subsample_c_10',
    'original_analytic_subsample_d_11',
    'original_analytic_subsample_n1',
    'original_analytic_subsample_n2',
    # statistic reported
    'original_statistic_fulltext_reported',
    'original_statistic_type_reported',
    'original_statistic_df1_reported',
    'original_statistic_df2_reported',
    'original_statistic_value_reported',
    'original_statistic_effect_type_reported',
    # effect size reported
    'original_effect_size_fulltext_reported',
    'original_effect_size_type_reported',
    'original_effect_size_value_reported',
    # coefficient reported
    'original_coefficient_type_reported',
    'original_coefficient_value_reported',
    'original_coefficient_se_reported',
    'original_total_model_parameters',
    # p-value reported
    'original_p_value_type_reported',
    'original_p_value_value_reported',
    'original_p_value_tails_reported',
    # begin reference variables
    'original_poweranalysis_link',
    'original_samplesize_calculation_contributor',
    # sample size reference
    'original_analytic_sample_size_value_reference',
    # statistic reference
    'original_statistic_fulltext_reference',
    'original_statistic_type_reference',
    'original_statistic_df1_reference',
    'original_statistic_df2_reference',
    'original_statistic_value_reference',
    'original_statistic_effect_type_reference',
    # effect size reference
    'original_effect_size_fulltext_reference',
    'original_effect_size_type_reference',
    'original_effect_size_value_reference',
    # coefficient reference
    'original_coefficient_type_reference',
    'original_coefficient_value_reference',
    'original_coefficient_se_reference',
    # p-value reference
    'original_p_value_type_reference',
    'original_p_value_value_reference',
    'original_p_value_tails_reference',
    # power analysis notes
    'original_cos_notes',
    # power analysis sample estimates
    'rr_threshold_analytic_sample_size',
    'rr_stage1_analytic_sample_size',
    'rr_stage2_analytic_sample_size',
    # misc
    'is_covid',
    'original_materials_link'
  )

  # Raw original variables from the end of Phase 1, cleaned to merge with P2
  p1_orig <- orig_statistics_dataset_p1 %>% 
    select(any_of(orig_columns))

  # Raw original variables with a formal power analysis
  p2_power <- original_inftest_dataset %>%
    select(any_of(orig_columns))

  # Raw original variables from manual data entry (no formal power analysis)
  p2_manual <- orig_statistics_manual_data_entry %>%
    select(any_of(orig_columns))

  # Raw P2 Original Variables from the Google Sheet
  p2_orig <- orig_vars_qa %>%
    select(any_of(orig_columns))

  bind_rows(p1_orig,
            p2_power,
            p2_manual,
            p2_orig) %>%
    mutate(orig_stat_version = 1,
           unique_claim_id = str_c(paper_id,
                                   "_",
                                   claim_id),
           # Denote papers from COVID set
           is_covid = (paper_id %in% tagtable_covid_p1$paper_id)) %>%
    select(any_of(orig_columns))

}

# Apply changelog entries
# Applies fixes identified after initial data entry of original variables
update_orig_input <- function(orig_input_changelog,
                              orig_statistics_dataset_p1,
                              original_inftest_dataset,
                              orig_statistics_manual_data_entry,
                              orig_vars_qa,
                              tagtable_covid_p1) {
  
  orig_variables <- merge_orig_input(orig_statistics_dataset_p1,
                                     original_inftest_dataset,
                                     orig_statistics_manual_data_entry,
                                     orig_vars_qa,
                                     tagtable_covid_p1)

  # We only want to work with the highest version for each claim
  changelog <- orig_input_changelog %>%
    arrange(unique_claim_id,
            col_name,
            desc(orig_stat_version)) %>%
    distinct(unique_claim_id,
             col_name,
             .keep_all = TRUE) 

  versions <- changelog %>%
    group_by(unique_claim_id) %>%
    summarise(max_version = max(orig_stat_version))
  
  # Some of the changelog entries add information to 
  # original_poweranalysis_notes, which does not exist in the orig_variables 
  # variable list
  data_entry <- add_column(orig_variables, 
                           original_poweranalysis_notes = NA) %>%
    mutate(
      # The column gets added as logical, but it needs to be character
      original_poweranalysis_notes = as.character(original_poweranalysis_notes)
      )

  # List of variables that should be numeric for the purposes of adding data
  # from the changelog to the dataset
  num_list <- c("orig_stat_version",
                "original_analytic_sample_size_value_reported",
                "original_analytic_subsample_a_00",
                "original_analytic_subsample_b_01",
                "original_analytic_subsample_c_10",
                "original_analytic_subsample_d_11",
                "original_analytic_subsample_n1",
                "original_analytic_subsample_n2",
                "original_statistic_df1_reported",
                "original_statistic_df1_reference",
                "original_statistic_df2_reported",
                "original_statistic_value_reported",
                "original_effect_size_value_reported",
                "original_coefficient_value_reported",
                "original_coefficient_se_reported",
                "original_total_model_parameters",
                "original_p_value_value_reported",
                "original_analytic_sample_size_value_reference",
                "original_statistic_df2_reference",
                "original_statistic_value_reference",
                "original_effect_size_value_reference",
                "original_coefficient_value_reference",
                "original_coefficient_se_reference",
                "original_p_value_value_reference",
                "rr_threshold_analytic_sample_size",
                "rr_stage1_analytic_sample_size",
                "rr_stage2_analytic_sample_size")
  
  for(i in 1:nrow(changelog)){

    col_name <- changelog[i,]$col_name
    
    version <- versions %>%
      filter(unique_claim_id == changelog[i, ]$unique_claim_id) %>%
      pull(max_version)
    
    change_to <- ifelse(col_name %in% num_list, 
                        as.numeric(changelog[i, ]$change_to),
                        changelog[i, ]$change_to)
    
    change_tbl <- tibble(unique_claim_id = changelog[i, ]$unique_claim_id,
                         orig_stat_version = version,
                         {{ col_name }} := change_to)
    
    data_entry <- data_entry %>%
      rows_update(change_tbl, by = "unique_claim_id")

  }
  
  return(data_entry)

}

# Create original variables dataset
# Combines the updated original dataset variables with the original statistics
# output from Tilburg to create the most up-to-date version of the original 
# statistics dataset.
export_orig <- function(orig_input_changelog,
                        orig_statistics_dataset_p1,
                        original_inftest_dataset,
                        orig_statistics_manual_data_entry,
                        orig_vars_qa,
                        tagtable_covid_p1,
                        orig_statistics_output_p2) {
  
  orig_extended <- orig_statistics_output_p2 %>%
    select('paper_id',
           'c4_claim_id',
           'orig_stat_version',
           'analysis_type',
           'original_pearsons_r_defined',
           'original_pearsons_r_numeric',
           'original_es_lb_ci_pearson',
           'original_es_ub_ci_pearson',
           'original_effect_size_type_statsteam',
           'original_effect_size_value_statsteam',
           'original_es_lb_ci_nativeunits',
           'original_es_ub_ci_nativeunits',
           'original_power_small',
           'original_power_medium',
           'original_power_50_original_effect',
           'original_power_75_original_effect',
           'Tilburg_team_finished') %>%
    rename(claim_id = c4_claim_id,
           original_statistic_analysis_type_statsteam = analysis_type) %>%
    # Including "skipped" as finished because Tilburg has finished what they 
    # can/will do with that claim
    mutate(Tilburg_team_finished = case_match(Tilburg_team_finished,
                                              "skipped" ~ TRUE,
                                              "finished" ~ TRUE,
                                              NA ~ NA,
                                              .default = FALSE))

  update_orig_input(orig_input_changelog,
                    orig_statistics_dataset_p1,
                    original_inftest_dataset,
                    orig_statistics_manual_data_entry,
                    orig_vars_qa,
                    tagtable_covid_p1) %>%
    
    mutate(
      # Prioritize which version of duplicates to keep
      data_source_priority = case_match(original_data_source,
                                             "p2_manual" ~ 1,
                                             "p2_power" ~ 2,
                                             "p2_pipeline" ~ 3,
                                             "orig_via_repro_form" ~ 4,
                                             "pipeline_orig" ~ 5,
                                             "repro" ~ 6),
      # Standardize values for original_effect_size_type_reported and 
      # original_effect_size_type_reference
      across(
        c(original_effect_size_type_reported, 
          original_effect_size_type_reference),
        ~ case_match(
          .x,
          c("Cohen's d", "d", "d_sample", "d_sample (one)") ~ "cohen_d",
          "Cohen's dz" ~ "cohen_dz",
          c("Cohen's f-squared", "cohenf2") ~ "cohen_f_squared",
          c("Cohen's w", "cohens_w") ~ "cohen_w",
          "Cramer's V" ~ "cramer_v",
          "Eta-squared" ~ "eta_squared",
          "Hazard ratio" ~ "hazard_ratio",
          c("adjusted_odds_ratio", "Odds ratio") ~ "odds_ratio",
          "Partial eta-squared" ~ "partial_eta_squared",
          "path_coef" ~ "path_coefficient",
          c("Pearson's r", "r") ~ "pearson_r",
          "Phi" ~ "phi",
          "probit_coefficient_estimate" ~ "probit_coefficient",
          c("multiple_regression_coefficient", 
            "raw_ols_coefficient",
            "raw_regression_coefficient",
            "raw_regression_coefficient_from_a_two_step_system_gmm",
            "raw_regression_coefficient_beta") ~ "regression_coefficient",
          "relative risk ratio" ~ "relative_risk_ratio",
          c("SER method", "SER-Method") ~ "ser_method",
          "SER-t method" ~ "ser_method_t",
          c("standardized_regression_coefficient",
            "standardized_coefficient") ~ "standardized_regression_coefficient",
          .default = .x
          )
        )
      ) %>%
    arrange(unique_claim_id, data_source_priority) %>%
    relocate(data_source_priority, .after = original_data_source) %>%
    distinct(unique_claim_id, .keep_all = TRUE) %>%
    select(-c(data_source_priority)) %>%
    left_join(orig_extended, join_by(paper_id, claim_id, orig_stat_version))

}