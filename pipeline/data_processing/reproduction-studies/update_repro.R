# Applies fixes identified after initial data entry to the reproduction dataset
update_repro <- function(repro_merged, changes_repro_merged) {
  
  # Some duplicates are in the change log, so we will drop them after applying
  # changes
  repro_merged %>%
    apply_changelog(changes_repro_merged, "unique_report_id") %>%
    distinct(rr_id,
             claim_id,
             .keep_all = TRUE)
  
}
