#################
# LOAD AND QC ORIGINAL VARIABLES FILES
# Functions to properly load in data, check for obviously incorrect entries, 
# and make any transformations before use in the data pipeline.
#################

# Google Drive Files ----
## COS SCORE Data ----
# Load orig_statistics_dataset_p1.tsv
# Manually set data types for variables, check for valid IDs, and clean data
# for merging with other datasets
load_orig_statistics_p1 <- function(orig_statistics_p1_file,
                                    valid_ids,
                                    orig_statistics_p1_moddate) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(orig_statistics_p1_moddate))) {
    stop(simpleError("File modification date invalid."))
  }

  # Many columns load in as the incorrect data type when imported without 
  # manually setting column types. I have noted where variable types have been
  # manually set.
  col_types <- list(
    paper_id = col_character(),
    original_materials_link = col_character(),
    original_poweranalysis_link = col_character(),
    coded_claim3b = col_character(),
    coded_claim4 = col_character(),
    original_analysis_type_COS = col_character(),
    original_analytic_sample_size_units_reported = col_character(),
    original_analytic_sample_size_value_reported = col_double(),
    original_analytic_subsample_n1 = col_double(),
    original_analytic_subsample_n2 = col_double(),
    original_analytic_subsample_a_00 = col_double(),
    original_analytic_subsample_b_01 = col_double(),
    original_analytic_subsample_c_10 = col_double(),
    original_analytic_subsample_d_11 = col_double(),
    original_statistic_fulltext_reported = col_character(),
    original_statistic_effect_type_reported = col_character(),
    original_statistic_type_reported = col_character(),
    original_statistic_value_reported = col_double(), # originally: character
    original_statistic_df1_reported = col_double(), # originally: character
    original_statistic_df2_reported = col_double(), # originally: character
    original_coefficient_type_reported = col_character(),
    # There is a weird data corruption in this field, but it gets fixed in the
    # changelog:
    original_coefficient_value_reported = col_double(), # originally: character
    original_coefficient_se_reported = col_double(), # originally: character
    original_p_value_type_reported = col_character(),
    original_p_value_tails_reported = col_character(),
    original_p_value_value_reported = col_double(),
    original_effect_size_fulltext_reported = col_character(),
    original_effect_size_type_reported = col_character(),
    original_effect_size_value_reported = col_double(), # originally: character
    # originally: character
    original_analytic_sample_size_value_reference = col_double(), 
    original_statistic_fulltext_reference = col_character(),
    original_statistic_effect_type_reference = col_character(),
    original_statistic_type_reference = col_character(),
    original_statistic_value_reference = col_double(), # originally: character
    original_statistic_df1_reference = col_double(), # originally: character
    original_statistic_df2_reference = col_double(), # originally: character
    original_coefficient_type_reference = col_character(),
    original_coefficient_value_reference = col_double(),
    original_coefficient_se_reference = col_double(), # originally: character
    original_p_value_tails_reference = col_character(),
    original_p_value_value_reference = col_double(), # originally: character
    original_effect_size_type_reference = col_character(),
    original_effect_size_value_reference = col_double(), # originally: character
    rr_threshold_analytic_sample_size = col_double(), # originally: character
    rr_stage1_analytic_sample_size = col_double(), # originally: character
    rr_stage2_analytic_sample_size = col_double(), # originally: character
    original_total_model_parameters = col_double(), # originally: character
    original_negative_coding_warning = col_character(),
    original_thresholded_p_value_warning = col_character(),
    notes_analysis_type = col_character(),
    original_extended_vars_returned = col_character(),
    original_statsteam_contributor = col_character(),
    original_effect_size_type_statsteam = col_character(),
    original_effect_size_value_statsteam = col_double(),
    original_es_ub_ci_nativeunits = col_double(),
    original_es_lb_ci_nativeunits = col_double(),
    original_pearsons_r_defined = col_logical(),
    original_es_ub_ci_pearson = col_double(),
    original_es_lb_ci_pearson = col_double(),
    original_power_small = col_double(),
    original_power_medium = col_double(),
    original_power_50_original_effect = col_double(),
    original_power_75_original_effect = col_double(),
    original_statsteam_notes = col_character(),
    original_cos_notes = col_character(),
    original_data_source = col_character(),
    original_statistic_interaction_reported = col_character(),
    original_statistic_analysis_type_reference = col_character(),
    original_statistic_interaction_reference = col_character(),
    original_effect_size_fulltext_reference = col_character(),
    original_p_value_type_reference = col_character(),
    original_z_possible = col_double(),
    original_report_date = col_character(),
    original_pearsons_r_value = col_double(),
    original_z_statistic = col_double(),
    pdf_filename = col_character(),
    is_covid = col_logical(),
    original_analysis_type_statsteam = col_character(),
    original_cos_contributor = col_character(),
    original_complete_analysis = col_logical()
  ) %>%
    unname()
  
  na_values <- c("NA", "NC", "N/A", "na", "n/a", "")

  orig_statistics_dataset_p1 <- orig_statistics_p1_file %>%
    googledrive::as_id() %>%
    drive_read_string() %>%
    read_tsv(col_types = col_types,
             na = na_values,
             show_col_types = FALSE)
  
  # QC check
  if(any(!(orig_statistics_dataset_p1$paper_id %in% valid_ids[[1]]$paper_id))){
    stop(simpleError(
      "Invalid paper IDs present in orig_statistics_dataset_p1."
    ))
  }
  
  # Prep dataset to be merged with P2 data
  orig_statistics_dataset_p1 %>%
    # Where there are duplicates, prefer version coded orig_via_repro_form 
    # under the assumption it was an update to a previous entry
    arrange(paper_id, original_data_source) %>%
    distinct(paper_id, .keep_all = TRUE) %>%
    mutate(
      # Records coded orig_via_repro_form have some unexpected new fields that
      # only contain values for such cases and seem to correspond to
      # canonical variables - put those values where they seem to belong.
      # Of note, original_p_value_type_reference and
      # original_effect_size_fulltext_reference also fall into this category
      # of being unique to orig_via_repro_form cases. However, they are valid
      # columns in phase 2 original variables coding, so I do not move their
      # values to analogous fields during this step.
      original_analysis_type_COS = coalesce(
        original_analysis_type_COS,
        original_statistic_analysis_type_reference
      ),
      original_statistic_effect_type_reported = coalesce(
        original_statistic_effect_type_reported,
        original_statistic_interaction_reported
      ),
      original_statistic_effect_type_reference = coalesce(
        original_statistic_effect_type_reference,
        original_statistic_interaction_reference
      ),
      claim_id = "single-trace"
    ) %>%
    rename(
      original_statistic_analysis_type = original_analysis_type_COS,
      original_samplesize_calculation_contributor = original_cos_contributor
    )
  
}

# Load orig_statistics_manual_data_entry.tsv
# Manually set data types for variables, check for valid IDs, and clean data
# for merging with other datasets
load_orig_statistics_manual <- function(orig_statistics_manual_file,
                                        valid_ids,
                                        orig_statistics_manual_moddate) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(orig_statistics_manual_moddate))) {
    stop(simpleError("File modification date invalid."))
  }
  
  # A few columns load in as the incorrect data type when imported without 
  # manually setting column types. I have noted where variable types have been
  # manually set.
  col_types <- list(
    pdf_filename = col_character(),
    paper_id = col_character(),
    c4_claim_id = col_character(),
    unique_claim_id = col_character(),
    original_samplesize_calculation_contributor = col_character(),
    original_analysis_type_cos = col_character(),
    original_materials_link = col_character(),
    rr_poweranalysis_link = col_character(),
    original_statistic_fulltext_reported = col_character(),
    original_analytic_sample_size_units_reported = col_character(),
    original_analytic_sample_size_value_reported = col_double(),
    original_statistic_interaction_reported = col_character(),
    original_statistic_type_reported = col_character(), # originally: logical
    original_statistic_value_reported = col_double(),
    original_statistic_df1_reported = col_double(),
    original_statistic_df2_reported = col_double(),
    original_coefficient_type_reported = col_character(),
    original_coefficient_value_reported = col_double(),
    original_coefficient_se_reported = col_double(),
    original_p_value_type_reported = col_character(),
    original_p_value_tails_reported = col_logical(),
    original_p_value_value_reported = col_double(),
    original_effect_size_fulltext_reported = col_logical(),
    original_effect_size_type_reported = col_logical(),
    original_effect_size_value_reported = col_double(),
    original_statistic_fulltext_reference = col_logical(),
    original_analytic_sample_size_units_reference = col_character(),
    original_analytic_sample_size_value_reference = col_double(),
    original_statistic_interaction_reference = col_character(),
    original_statistic_type_reference = col_character(), # originally: logical
    original_statistic_value_reference = col_double(),
    original_statistic_df1_reference = col_double(),
    original_statistic_df2_reference = col_double(),
    original_coefficient_type_reference = col_character(),
    original_coefficient_value_reference = col_double(),
    original_coefficient_se_reference = col_double(),
    original_p_value_type_reference = col_logical(),
    original_p_value_tails_reference = col_logical(),
    original_p_value_value_reference = col_logical(),
    original_effect_size_fulltext_reference = col_logical(),
    original_effect_size_type_reference = col_character(),
    original_effect_size_value_reference = col_double(),
    rr_stage1_analytic_sample_size = col_double(),
    rr_stage2_analytic_sample_size = col_double(),
    rr_threshold_analytic_sample_size = col_double(),
    original_cos_notes = col_character()
  )
  
  na_values <- c("NA", "NC", "N/A", "na", "n/a", "")
  
  orig_statistics_manual_data_entry <- orig_statistics_manual_file %>%
    googledrive::as_id() %>%
    drive_read_string() %>%
    read_tsv(col_types = col_types,
             na = na_values,
             show_col_types = FALSE)
  
  # QC check
  valid_papers <- valid_ids[[1]] %>%
    pull(paper_id)
  
  if(any(!(orig_statistics_manual_data_entry$paper_id %in% valid_papers))){
    stop(simpleError(
      "Invalid paper IDs present in orig_statistics_manual_data_entry."
    ))
  }
  
  orig_statistics_manual_data_entry %>%
    mutate(
      original_data_source = "p2_manual",
      # sometimes sample size units are identified under a made up variable -
      # original_analytic_sample_size_units_reference - but
      # original_analytic_sample_size_units_reported is the canonical original
      # sample size units field, so bring those values over where they are
      # missing in the expected field
      original_analytic_sample_size_units_reported = coalesce(
        original_analytic_sample_size_units_reported,
        original_analytic_sample_size_units_reference
      )
    ) %>%
    # These variables were misnamed during this coding pipeline and need to
    # be standardized to merge with other sources of original variables
    rename(
      claim_id = c4_claim_id,
      original_statistic_analysis_type = original_analysis_type_cos,
      original_poweranalysis_link = rr_poweranalysis_link,
      original_statistic_effect_type_reported =
        original_statistic_interaction_reported,
      original_statistic_effect_type_reference =
        original_statistic_interaction_reference
    )
}

# Load original_inftest_dataset.tsv
# Check for valid IDs and clean data for merging with other datasets
load_original_inftest_dataset <- function(original_inftest_file,
                                          valid_ids,
                                          mod_date) {
  
  na_values <- c("NA", "NC", "N/A", "na", "n/a", "")
  
  original_inftest_dataset <- original_inftest_file %>%
    read_google_tsv(mod_date) %>%
    mutate(claim_id = "single-trace")
  
  # QC check
  if(any(!(original_inftest_dataset$paper_id %in% valid_ids[[1]]$paper_id))){
    stop(simpleError(
      "Invalid paper IDs present in orig_inftest_dataset."
    ))
  }
  
  original_inftest_dataset %>%
    mutate(
      original_data_source = "p2_power",
      # Sometimes analysis type was set under a different variable name.
      # If that is missing in the expected field
      # (original_statistic_analysis_type), try to fill it in from
      # original_statistic_analysis_cos
      original_statistic_analysis_type = coalesce(
        original_statistic_analysis_type,
        original_analysis_type_cos
      ),
      # Sometimes sample size units are identified under a made up variable -
      # original_analytic_sample_size_units_reference - but
      # original_analytic_sample_size_units_reported is the canonical original
      # sample size units field, so bring those values over where they are
      # missing in the expected field
      original_analytic_sample_size_units_reported = coalesce(
        original_analytic_sample_size_units_reported,
        original_analytic_sample_size_units_reference
      )
    ) %>%
    # These variables were misnamed during this coding pipeline and need to
    # be standardized to merge with other sources of original variables
    rename(
      original_poweranalysis_link = rr_poweranalysis_link,
      original_statistic_effect_type_reported =
        original_statistic_interaction_reported,
      original_statistic_effect_type_reference =
        original_statistic_interaction_reference
    )
}

## SCORE ----
# Load SCORE-P2-103: Original variables coding form (Responses)
# Download file if it has been updated, correct data types, check for valid IDs,
# and clean for merging with other datasets
load_orig_vars_qa <- function(orig_vars_qa_gsheet,
                              orig_vars_qa_mod_date,
                              valid_ids) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(orig_vars_qa_mod_date))){
    stop(simpleError("File modification date invalid."))
  }
  
  orig_vars_qa <- read_sheet(orig_vars_qa_gsheet) %>%
    # For some reason these fields import as lists
    mutate(
      across(c("paper_id", 
               "c4_claim_id", 
               "original_effect_size_fulltext_reported"), 
             ~ as.character(.x))
    ) %>%
    # c4_claim_id has been deprecated in favor of claim_id across
    # orig + rr variables
    rename(claim_id = c4_claim_id)
  
  # QC checks
  if(any(!(orig_vars_qa$paper_id %in% valid_ids[[1]]$paper_id))){
    stop(simpleError("Invalid paper IDs present in orig_vars_qa."))
  }
  
  valid_claims <- valid_ids[[2]]
  
  to_check <- select(orig_vars_qa, paper_id, claim_id) %>%
    anti_join(valid_claims, join_by("paper_id", "claim_id"))
  
  if(nrow(to_check) > 0){
    stop(simpleError("Invalid claim IDs present in orig_vars_qa"))
  }
  
  orig_vars_qa %>%
    mutate(original_data_source = "p2_pipeline")
  
}

# OSF Files ----

# Repository: SCORE - Phase 1 RR and original article analyses 
# (https://osf.io/3fsbm/)

# Load orig_statistics_output_p2.txt
# Downloads file from OSF if it has been updated, stores the output, and 
# deletes the temp downloaded file
load_orig_statistics_output_p2 <- function(
    orig_statistics_output_p2_osf,
    orig_statistics_output_p2_mod_date
  ) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(orig_statistics_output_p2_mod_date))){
    stop(simpleError("File modification date invalid."))
  }
  
  orig_statistics_output_p2_file <- orig_statistics_output_p2_osf %>%
    osf_retrieve_file() %>%
    osf_download(path = here("pipeline",
                             "data_processing",
                             "temp"),
                 conflicts = "overwrite") %>%
    pull(local_path)
  
  # All character values and variable names are encased in quotes, so we need 
  # to deal with that to get all of the lines to read in correctly when there 
  # is quoted text within a value. Values that contain actual quotations will
  # have double quotes replaced with single quotes.
  orig_statistics_output_p2 <- read_tsv(orig_statistics_output_p2_file,
                                        show_col_types = FALSE,
                                        quote = "\\\"") %>%
    mutate(across(where(is.character), ~ str_remove_all(.x, "\""))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "\\\\", "\'"))) %>%
    rename_with(~ str_remove_all(.x, "\""))
  
  file.remove(orig_statistics_output_p2_file)
  
  return(orig_statistics_output_p2)
}

load_complex_bushel <- function(complex_bushel_gsheet,
                                complex_bushel_mod_date) {
  
  read_google_sheet(complex_bushel_gsheet, 
                                      complex_bushel_mod_date) %>%
    mutate(unique_claim_id = str_c(paper_id, "_", claim_id)) %>%
    select(unique_claim_id, 
           bushel_complex = complex)
  
}
