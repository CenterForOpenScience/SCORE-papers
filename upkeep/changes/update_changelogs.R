# Update changelog
# Takes a new entry to the changelog as input, adds date, version number, and
# previous value, reads current changelog, adds new line to reflect new entry,
# and writes updated changelog. Works for original variables and replication 
# outcomes

update_changelog <- function(changelog,
                             source_data,
                             type,
                             outcome_id,
                             changes_dict,
                             reported_by,
                             rationale = NA) {
  
  if (any(!(names(changes_dict) %in% names(source_data)))) {
    
    stop(simpleError("Invalid column names present in changes_dict."))
    
  }
  
  unique_id <- names(select(changelog, starts_with("unique")))
  
  version_col <- names(select(changelog, ends_with("_stat_version")))
  
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
          pull(change_to)
        
      } else {
        
        change_from <- source_data %>%
          filter(get({{ unique_id }}) == outcome_id) %>%
          pull(col) %>%
          as.character()
        
      }
      
      changelog <- changelog %>%
        add_row(reported_by = reported_by,
                date_implemented = Sys.Date(),
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
      
      changelog <- changelog %>%
        add_row(reported_by = reported_by,
                date_implemented = Sys.Date(),
                {{ unique_id }} := outcome_id,
                col_name = col,
                change_to = change_to,
                change_from = change_from,
                {{ version_col }} := 2,
                rationale = rationale)
    }
  }

  file_name <- switch(
    type,
    orig = here("data",
                "original_variables",
                "changes",
                "orig_input_changelog.tsv"),
    repli = here("data",
                 "rr",
                 "changes",
                 "repli_input_changelog.tsv"),
    stop(simpleError("Theresa, add the repro changelog please"))
  )

  write_tsv(x = changelog,
            file = file_name,
            quote = "needed")
  
  return(file_name)
  
}

