update_oa <- function(oa_raw,
                      oa_changelog) {
  
  oa_raw %>%
    add_column(oa_stat_version = 1) %>%
    apply_changelog(oa_changelog, "paper_id")
  
}