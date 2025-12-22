# Apply changelog updates
# Applies fixes identified after initial data entry to rr_replication_outcomes
update_repli_input <- function(repli_merged,
                               repli_input_changelog) {
  
  rr_replication_outcomes <- repli_merged %>%
    apply_changelog(repli_input_changelog %>% 
                      # This field gets dropped, removed for smooth updating
                      filter(col_name != "rr_analysis_link"), 
                    "unique_report_id")
  
  # Data type changes 
  mutate(
    rr_replication_outcomes,
    rr_statistic_value_reported = as.double(rr_statistic_value_reported)
  ) %>%
    filter(!(ready_for_export %in% c("Never", "Issues"))) %>%
    select(-c(ready_for_export))
  
}