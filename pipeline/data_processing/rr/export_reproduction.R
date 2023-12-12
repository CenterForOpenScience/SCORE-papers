# Export Reproductions

# Apply changelog updates
# Applies fixes identified after initial data entry to rr_reproduction_outcomes
update_p2_repro_input <- function(reproduction_qa,
                                  p2_repro_input_changelog) {
  
  rr_reproduction_outcomes <- reproduction_qa
  
  # We only want to work with the highest version for each claim
  changelog <- p2_repro_input_changelog %>% 
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
  num_list <- c("orig_analytic_sample_size_value_criterion_reported",
                "rr_analytic_sample_size_value_reported",
                "rr_p_value_value_reported",
                "rr_coefficient_value_reported",
                "rr_statistic_value_reported",
                "rr_effect_size_value_reported",
                "rr_stat_version")
  
  for (i in 1:nrow(changelog)) {
    
    col_name <- changelog[i, ]$col_name
    
    version <- versions %>%
      filter(unique_report_id == changelog[i, ]$unique_report_id) %>%
      pull(max_version)
    
    change_to <- ifelse(col_name %in% num_list,
                        as.numeric(changelog[i, ]$change_to),
                        changelog[i, ]$change_to)
    
    change_tbl <- tibble(unique_report_id = changelog[i, ]$unique_report_id,
                         rr_stat_version = version,
                         {{ col_name }} := change_to)
    
    rr_reproduction_outcomes <- rr_reproduction_outcomes %>% 
      rows_update(change_tbl, by = "unique_report_id")
    
  }
  
  rr_reproduction_outcomes %>%
    distinct(rr_id, 
             claim_id,
             .keep_all = TRUE)

}

# Finalize RR attempt output
# Remove responses failing QA
export_repro <- function(reproduction_qa,
                         p2_repro_input_changelog) {
  
  repro_data_entry <- update_p2_repro_input(reproduction_qa,
                                            p2_repro_input_changelog)
  
  repro_data_entry %>%
    filter(if_any(ends_with("_qa"), ~ .)) %>%
    select(-c(ends_with("_qa")))
  
}