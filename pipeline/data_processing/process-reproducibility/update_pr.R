update_pr <- function(pr_transformed, changes_pr_data_form) {
  
  # We only want to work with the highest version for each
  changelog <- changes_pr_data_form %>%
    arrange(paper_id,
            col_name,
            desc(pr_stat_version)) %>%
    distinct(paper_id,
             col_name,
             .keep_all = TRUE) 
  
  versions <- changelog %>%
    group_by(paper_id) %>%
    summarise(max_version = max(pr_stat_version))
  
  data_entry <- pr_transformed %>%
    add_column(pr_stat_version = 1)
  
  for (i in 1:nrow(changelog)) {
    
    col_name <- changelog[i,]$col_name
    
    version <- versions %>%
      filter(paper_id == changelog[i, ]$paper_id) %>%
      pull(max_version)
    
    change_to <- changelog[i, ]$change_to
    
    change_tbl <- tibble(paper_id = changelog[i, ]$paper_id,
                         pr_stat_version = version,
                         {{ col_name }} := change_to)
    
    data_entry <- data_entry %>%
      rows_update(change_tbl, by = "paper_id")
    
  }
  
  return(data_entry)
  
}