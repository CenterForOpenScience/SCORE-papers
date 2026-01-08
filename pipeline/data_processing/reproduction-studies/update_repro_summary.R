update_repro_summary <- function(repro_summary_raw,
                                 changes_repro_summary) {
  
  repro_summary_raw %>%
    add_column(supplement_stat_version = 1) %>%
    apply_changelog(changes_repro_summary, "unique_report_id")
  
}