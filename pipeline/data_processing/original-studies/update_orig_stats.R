# Update original variables
# Applies fixes identified after initial data entry of original variables
update_orig_stats <- function(orig_merged,
                              changes_orig_merged) {

  orig_merged %>%
    # original_poweranalysis_notes was dropped at some point but there
    # are entries in the change log that add a value to it.
    add_column(original_poweranalysis_notes = NA_character_) %>%
    apply_changelog(changes_orig_merged, "unique_claim_id")

}
