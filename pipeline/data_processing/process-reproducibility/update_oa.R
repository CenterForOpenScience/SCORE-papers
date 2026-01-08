update_oa <- function(materials_author_outreach,
                      changes_author_outreach) {
  
  materials_author_outreach %>%
    add_column(oa_stat_version = 1) %>%
    apply_changelog(changes_author_outreach, "paper_id")
  
}