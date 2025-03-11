update_repro_supplement <- function(repro_supplement_raw,
                                    repro_supplement_changelog) {
  
  repro_supplement_raw %>%
    add_column(supplement_stat_version = 1) %>%
    apply_changelog(repro_supplement_changelog, "unique_report_id")
  
}