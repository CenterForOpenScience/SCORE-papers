# Create rr analytic datasets

# Create the reproduction analytic dataset
create_repro_analytic <- function(repro_export,
                                  repro_supplement){

  repro_export %>%
    left_join(repro_supplement, by = "unique_report_id") %>%
    mutate(
      repro_outcome_overall = case_when(
        repro_outcome_overall == "precise" &
          rr_type_internal == "Push Button Reproduction" ~ "push button",
        .default = repro_outcome_overall
      ),
      # Not strictly necessary - left for consistency with old version
      across(c(rr_statistic_type_reported,
               rr_effect_size_type_reported,
               rr_repro_success_reported,
               rr_repro_analyst_success_reported),
             str_to_lower)
    ) %>%
    select(
      paper_id,
      rr_id,
      claim_id,
      report_id = unique_report_id,
      repro_type = rr_type_internal,
      orig_sample_criterion = orig_analytic_sample_size_value_criterion_reported,
      repro_sample_size_value = rr_analytic_sample_size_value_reported,
      repro_p_value = rr_p_value_value_reported,
      repro_coef_value = rr_coefficient_value_reported,
      repro_stat_type = rr_statistic_type_reported,
      repro_stat_value = rr_statistic_value_reported,
      repro_effect_size_type = rr_effect_size_type_reported,
      repro_effect_size_value = rr_effect_size_value_reported,
      repro_pattern_criteria = rr_repro_pattern_criteria_reported,
      repro_pattern_criteria_met = rr_repro_success_reported,
      repro_pattern_description = rr_repro_pattern_description_reported,
      repro_interpret_supported = rr_repro_analyst_success_reported,
      repro_lb_sample,
      repro_ub_sample,
      repro_outcome_sample,
      repro_lb_coef,
      repro_ub_coef,
      repro_outcome_coef,
      repro_lb_stat,
      repro_ub_stat,
      repro_outcome_stat,
      repro_lb_p,
      repro_ub_p,
      repro_outcome_p,
      repro_lower_effect,
      repro_upper_effect,
      repro_outcome_effect,
      repro_outcome_overall,
      repro_secondary_criteria,
      is_covid,
      repro_version_of_record
    )

}