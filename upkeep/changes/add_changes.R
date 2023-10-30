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
changes_dict <- list(
  # Add changes here
  
)

rationale <- "" # Add rationale here

# Check type to determine appropriate source data and changelog
if (type == "Replication") {
  
  source_data <- tar_read(repli_export)
  
  if(!(outcome_id %in% pull(source_data, unique_report_id))) {
    
    stop(simpleError("Outcome ID does not exist for replications."))
  
  }
  
  change_sheet <- "1jbkVYHLinIN9zXBuPTok26bHjDt69-7D_Z7yVOin3mM"
  
} else if (type == "Original Data") {
  
  source_data <- tar_read(orig_dataset)
  
  if(!(outcome_id %in% pull(source_data, unique_claim_id))) {
    
    stop(simpleError("Outcome ID does not exist for original data."))
    
  }
  
  change_sheet <- "1M8H_76ajxwdhVuuSyIo18BxRrJbMF3hXpAkzYY39g2k"
  
} else {
  
  stop(simpleError("Invalid type provided. Perhaps this is a repro entry?"))
  
}

# Check that outcome_id is valid for source data

if (any(!(names(changes_dict) %in% names(source_data)))) {
  
  stop(simpleError("Invalid column names present in changes_dict."))
  
}

update_changelog(changelog,
                 source_data,
                 outcome_id,
                 changes_dict,
                 reported_by,
                 rationale)
