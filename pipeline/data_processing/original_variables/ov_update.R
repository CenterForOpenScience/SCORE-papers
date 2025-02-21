# Update original variables
# Applies fixes identified after initial data entry of original variables
update_orig_input <- function(orig_dataset,
                              orig_input_changelog) {

  # Original dataset doesn't include original_poweranalysis_notes, but there
  # are entries in the change log that add a value to it.
  orig_export <- add_column(orig_dataset,
                            original_poweranalysis_notes = NA_character_)

  apply_changelog(orig_export, orig_input_changelog, "unique_claim_id")

}
