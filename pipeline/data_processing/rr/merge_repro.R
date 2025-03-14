merge_repro <- function(repro_data_entry,
                        reproduction_cases,
                        repro_vor) {
  
  vor <- repro_vor %>%
    select(unique_report_id = report_id, 
           repro_version_of_record)
  
  new_cases <- select(reproduction_cases, -c("rr_repro_cos_notes", 
                                             "rr_input_source")) %>%
    mutate(rr_stat_version = 1,
           is_covid = FALSE)
  
  repro_data_entry %>%
    rbind(new_cases) %>%
    left_join(vor, by = "unique_report_id")
  
}