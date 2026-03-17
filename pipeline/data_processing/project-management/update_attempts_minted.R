update_score_rr_types <- function(score_rr_types_raw,
                                   changes_score_rr_types) {
  
  score_rr_types_raw %>%
    add_column(minted_version = 1) %>%
    apply_changelog(changes_score_rr_types,
                    "rr_id")
  
}