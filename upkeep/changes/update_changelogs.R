# Update changelog
# Takes a new entry to the changelog as input, adds date, version number, and
# previous value, reads current changelog, adds new line to reflect new entry,
# and writes updated changelog. Works for original variables and replication 
# outcomes

update_changelog <- function(change_sheet,
                             source_data,
                             outcome_id,
                             changes_dict,
                             reported_by,
                             rationale) {
  
  changelog <- change_sheet %>%
    read_sheet()
  
  unique_id <- if_else(
    change_sheet == "1zek9Lu-s303ZXs3B84h2vy-sJ1avOZSgpZEsna40pdA",
    "paper_id",
    names(select(changelog, starts_with("unique")))
  )
  
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
  
}
