# Transform reproduction raw data entry
transform_repro_input <- function(repro_stats) {
  
  repro_stats %>%
    mutate(
      # Original row index is used to create unique record id later
      original_index = as.character(row_number()-1),
      rr_id = str_extract(asana_ticket_name, "([^(?= - )]*)$"),
      unique_report_id = str_c(rr_id,
                               "_",
                               original_index,
                               "_1"),
      rr_stat_version = 1,
      paper_id = str_extract(asana_ticket_name,
                             "(?<=_)[:alnum:]*(?=[:blank:])"),
      claim_id = str_c(paper_id,
                       "_",
                       c4_claim_id),
      is_covid = str_detect(asana_ticket_name, "covid")
    ) %>%
    select(paper_id,
           claim_id,
           rr_id,
           rr_primary_criteria_available,
           rr_type_internal,
           orig_analytic_sample_size_value_criterion_reported,
           rr_analytic_sample_size_value_reported,
           rr_p_value_value_reported,
           rr_coefficient_value_reported,
           rr_statistic_type_reported,
           rr_statistic_value_reported,
           rr_effect_size_type_reported,
           rr_effect_size_value_reported,
           rr_repro_pattern_criteria_reported,
           rr_repro_success_reported,
           rr_repro_pattern_description_reported,
           rr_repro_analyst_success_reported,
           unique_report_id,
           rr_stat_version,
           is_covid)
  
}
