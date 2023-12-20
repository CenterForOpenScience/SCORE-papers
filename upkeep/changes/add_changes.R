# This file assumes you are using the targets package. Be sure to run
# targets::tar_make() after update_changelog() to ensure the changes are 
# incorporated upstream
library(tidyverse)
library(targets)
library(googlesheets4)
library(here)

here("upkeep",
     "changes",
     "update_changelogs.R") %>%
  source()

options(gargle_oauth_email = Sys.getenv("google_oauth_email"))

# Changes to be made -----
reported_by <- "" # Add your name here

type <- ""

outcome_id <- "" # Add outcome ID here

# Changes should be added to this list in the form:
# [variable name] = "value",
# [variable name] = "value"

# RR Checking Replication Key:
# analysis_type = rr_statistic_analysis_type_reported
# coefficient_type = rr_coefficient_type_reported
# effect_fulltext = rr_effect_size_fulltext_reported
# effect_type = rr_effect_size_type_reported
# effect_value = rr_effect_size_value_reported
# p_value = rr_p_value_value_reported
# p_confirmation = rr_p_value_confirmation_reported
# sample_cells = rr_analytic_sample_cells_reported
# sample_units = rr_analytic_sample_size_units_reported
# sample_value = rr_analytic_sample_size_value_reported
# statistic_df1 = rr_statistic_df1_reported
# statistic_df2 = rr_statistic_df2_reported
# statistic_fulltext = rr_statistic_fulltext_reported
# statistic_interaction = rr_statistic_interaction_reported
# statistic_type = rr_statistic_type_reported
# statistic_value = rr_statistic_value_reported
# subsample_a00 = rr_analytic_subsample_a_00
# subsample_b01 = rr_analytic_subsample_b_01
# subsample_c10 = rr_analytic_subsample_c_10
# subsample_d11 = rr_analytic_subsample_d_11
# subsample_n1 = rr_analytic_subsample_n1
# subsample_n2 = rr_analytic_subsample_n2

changes_dict <- list(
  # Add changes here
  
)

rationale <- "" # Add rationale here

# Check type to determine appropriate source data and changelog
if (type == "Replication") {
  
  source_data <- tar_read(repli_export)
  
  if (!(outcome_id %in% pull(source_data, unique_report_id))) {
    
    stop(simpleError("Outcome ID does not exist for replications."))
  
  }
  
  change_sheet <- "1jbkVYHLinIN9zXBuPTok26bHjDt69-7D_Z7yVOin3mM"
  
} else if (type == "Original Data") {
  
  source_data <- tar_read(orig_dataset)
  
  if (!(outcome_id %in% pull(source_data, unique_claim_id))) {
    
    stop(simpleError("Outcome ID does not exist for original data."))
    
  }
  
  change_sheet <- "1M8H_76ajxwdhVuuSyIo18BxRrJbMF3hXpAkzYY39g2k"
  
} else if (type == "Reproduction") {
  
  source_data <- tar_read(reproduction_qa)
  
  if (!(outcome_id %in% pull(source_data, unique_report_id))) {
    
    stop(simpleError("Outcome ID does not exist for reproductions."))
    
  }
  
  change_sheet <- "1c2i_k-RMzTWAC7RD6cqhheToihtS8_C5r5powvRuMDE"
  
} else if (type == "Process Reproducibility") {
  
  source_data <- tar_read(pr_data_form)
  
  if (!(outcome_id %in% pull(source_data, paper_id))) {
    
    stop(simpleError("Outcome ID does not exist for process repoducibility."))
    
  }
  
  change_sheet <- "1zek9Lu-s303ZXs3B84h2vy-sJ1avOZSgpZEsna40pdA"
  
} else {
  
  stop(simpleError("Invalid type provided. Perhaps this is a hybrid entry?"))
  
}

# Check that outcome_id is valid for source data

if (any(!(names(changes_dict) %in% names(source_data)))) {
  
  stop(simpleError("Invalid column names present in changes_dict."))
  
}

update_changelog(change_sheet,
                 source_data,
                 outcome_id,
                 changes_dict,
                 reported_by,
                 rationale)
