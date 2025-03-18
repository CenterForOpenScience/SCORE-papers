# This file assumes you are using the targets package. 

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
# variable_name = "value",
# variable_name = "value"

# data_available = pr_data_available
# data_location = pr_data_location
# data_location_description = pr_data_location_description
# data_complete = pr_data_complete
# data_date = pr_data_date
# data_notes = pr_data_notes
# pr_notes = other_notes
# paper_instructions = paper_data_instructions
# paper_instructions_content = paper_data_instructions_content
# code_available = pr_code_available
# code_location = pr_code_location
# code_location_description = pr_code_location_description
# code_complete = pr_code_complete
# paper_link = paper_data_link
# paper_link_content = paper_data_link_content

changes_dict <- list(
  # Add changes here
  
)

rationale <- "" # Add rationale here

switch(
  type,
  Replication = update_repli_changes(outcome_id, 
                                     changes_dict, 
                                     reported_by,
                                     rationale),
  Hybrid = update_repli_changes(outcome_id, 
                                changes_dict, 
                                reported_by,
                                rationale),
  Reproduction = update_repro_changes(outcome_id, 
                                      changes_dict, 
                                      reported_by,
                                      rationale),
  `Original Data` = update_orig_changes(outcome_id, 
                                        changes_dict, 
                                        reported_by,
                                        rationale),
  `Process Reproducibility` = update_pr_changes(outcome_id, 
                                                changes_dict, 
                                                reported_by,
                                                rationale),
  `OA Outreach` = update_oa_changes(outcome_id, 
                                    changes_dict, 
                                    reported_by,
                                    rationale),
  `Repro Supplement` = update_repo_supplement_changes(outcome_id, 
                                                      changes_dict, 
                                                      reported_by,
                                                      rationale),
  stop("Invalid type provided. Perhaps this is a hybrid entry?")
)
