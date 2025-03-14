update_attempts_minted <- function(rr_attempts_minted_raw,
                                   rr_attempts_minted_changelog) {
  
  rr_attempts_minted_raw %>%
    add_column(minted_version = 1) %>%
    apply_changelog(rr_attempts_minted_changelog,
                    "rr_id")
  
}