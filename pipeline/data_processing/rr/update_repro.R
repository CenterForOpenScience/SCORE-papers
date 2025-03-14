# Applies fixes identified after initial data entry to the reproduction dataset
update_repro <- function(repro_merged,
                         p2_repro_input_changelog) {
  
  # Some duplicates are in the changelog, so we will drop them after applying
  # changes
  repro_merged %>%
    apply_changelog(p2_repro_input_changelog, "unique_report_id") %>%
    # Filter backwards
    #map_df(rev) %>%
    distinct(rr_id,
             claim_id,
             .keep_all = TRUE)
  
}
