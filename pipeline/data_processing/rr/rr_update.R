# Update Reproduction and Replicaton Data

# Applies fixes identified after initial data entry to the reproduction dataset
update_repro <- function(repro_merged,
                         p2_repro_input_changelog) {
  
  # Some duplicates are in the changelog, so we will drop them after applying
  # changes
  repro_merged %>%
    apply_changelog(p2_repro_input_changelog, "unique_report_id") %>%
    distinct(rr_id,
             claim_id,
             unique_report_id,
             .keep_all = TRUE)
  
}

# Apply changelog updates
# Applies fixes identified after initial data entry to rr_replication_outcomes
update_repli_input <- function(repli_merged,
                               repli_input_changelog) {
  
  rr_replication_outcomes <- repli_merged %>%
    apply_changelog(repli_input_changelog, "unique_report_id")
  
  # Data type changes 
  mutate(
    rr_replication_outcomes,
    rr_statistic_value_reported = as.double(rr_statistic_value_reported)
  ) %>%
    filter(!(ready_for_export %in% c("Never", "Issues"))) %>%
    select(-c(ready_for_export))
  
}

update_attempts_minted <- function(rr_attempts_minted_raw,
                                   rr_attempts_minted_changelog) {
  
  rr_attempts_minted_raw %>%
    add_column(minted_version = 1) %>%
    apply_changelog(rr_attempts_minted_changelog,
                    "rr_id")
  
}