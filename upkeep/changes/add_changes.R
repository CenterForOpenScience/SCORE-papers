# This file assumes you are using the targets package. Be sure to run
# targets::tar_make() after update_changelog() to ensure the changes are 
# incorporated upstream

# Changes to be made -----
outcome_id <- "0DO8_single-trace" # Add outcome ID here

# Changes should be added to this list in the form:
# [variable name] = "value",
# [variable name] = "value"
changes_dict <- list(
  # Add changes here
  "original_coefficient_value_reported" = "25.4",
  "original_coefficient_se_reported" = "11.65094"
  
)

reported_by <- "Andrew Tyner" # Add your name here

rationale <- "The standard error is calculated using the coefficient value (25.4), p-value (0.029), and sample size (either 78, 78*8, or 78*8*7 depending on how original data is structured, but all produce the same standard error)." # Add rationale here

# Make changes ----
tar_load(repli_export)
tar_load(orig_dataset)

# Check type to determine appropriate source data and changelog
if (outcome_id %in% pull(repli_export, unique_report_id)) {
  
  source_data <- repli_export
  change_sheet <- "1jbkVYHLinIN9zXBuPTok26bHjDt69-7D_Z7yVOin3mM"
  
} else if (outcome_id %in% pull(orig_dataset, unique_claim_id)) {
  
  source_data <- orig_dataset
  change_sheet <- "1M8H_76ajxwdhVuuSyIo18BxRrJbMF3hXpAkzYY39g2k"
  
} else {
  
  stop(simpleError("Invalid type provided. Perhaps this is a repro entry?"))
  
}

# Check that outcome_id is valid for source data

if (any(!(names(changes_dict) %in% names(source_data)))) {
  
  stop(simpleError("Invalid column names present in changes_dict."))
  
}

changelog <- change_sheet %>%
  read_sheet()

unique_id <- names(select(changelog, starts_with("unique")))

version_col <- names(select(changelog, ends_with("_stat_version")))

new_changes <- tibble(reported_by = as.character(),
                      date_implemented = as.character(),
                      {{ unique_id }} := as.character(),
                      col_name = as.character(),
                      change_to = as.character(),
                      change_from = as.character(),
                      {{ version_col }} := as.numeric(),
                      rationale = as.character())

if (outcome_id %in% changelog[[unique_id]]) {
  
  this_outcome <- changelog %>%
    filter(get({{ unique_id }}) == outcome_id)
  
  this_version <- max(this_outcome[[version_col]]) + 1
  
  for (i in 1:length(changes_dict)) {
    
    col <- names(changes_dict[i])
    
    change_to <- changes_dict[[i]]
    
    if (col %in% this_outcome$col_name) {
      
      change_from <- this_outcome %>%
        group_by(col_name, {{ version_col }}) %>%
        filter(col_name == col,
               {{ version_col }} == max({{ version_col }})) %>%
        pull(change_to) %>%
        as.character()
      
    } else {
      
      change_from <- source_data %>%
        filter(get({{ unique_id }}) == outcome_id) %>%
        pull(col) %>%
        as.character()
      
    }
    
    new_changes <- new_changes %>%
      add_row(reported_by = reported_by,
              date_implemented = as.character(Sys.Date()),
              {{ unique_id }} := outcome_id,
              col_name = col,
              change_to = change_to,
              change_from = change_from,
              {{ version_col }} := this_version,
              rationale = rationale)
  }
  
} else {
  
  for (i in 1:length(changes_dict)) {
    
    col <- names(changes_dict[i])
    
    change_to <- changes_dict[[i]]
    
    # Get original value to change from
    change_from <- source_data %>%
      filter(get({{ unique_id }}) == outcome_id) %>%
      pull(col) %>%
      as.character()
    
    new_changes <- new_changes %>%
      add_row(reported_by = reported_by,
              date_implemented = as.character(Sys.Date()),
              {{ unique_id }} := outcome_id,
              col_name = col,
              change_to = change_to,
              change_from = change_from,
              {{ version_col }} := 2,
              rationale = rationale)
  }
}

new_changes <- new_changes %>%
  mutate(date_implemented = as_date(date_implemented))

sheet_append(change_sheet,
             new_changes)

# Remember to run tar_make() after ensuring changes are correct to incorporate
# changes in downstream files
tar_make()
