repro_merge <- function(repro_data_entry,
                        reproduction_cases) {
  
  new_cases <- select(reproduction_cases, -c("rr_repro_cos_notes", 
                                             "rr_input_source")) %>%
    mutate(rr_stat_version = 1)
  
  repro_data_entry %>%
    rbind(new_cases)
  
}