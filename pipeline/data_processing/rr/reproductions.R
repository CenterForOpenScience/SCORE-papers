# Functions for creating the reproduction analytic dataset

# Transform reproduction dataset to standardized form
transform_repro_input <- function(reproduction_qa) {

  reproduction_qa %>%
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
                       c4_claim_id)
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
           rr_stat_version)

}

# Apply change log updates
# Applies fixes identified after initial data entry to the reproduction dataset
update_repro <- function(repro_data_entry,
                         p2_repro_input_changelog) {

  # Some duplicates are in the changelog, so we will drop them after applying
  # changes
  repro_data_entry %>%
    apply_changelog(p2_repro_input_changelog, "unique_report_id") %>%
    distinct(rr_id,
             claim_id,
             .keep_all = TRUE)

}

calculate_repro_summary <- function(repro_export,
                                    orig_dataset,
                                    rr_confrontations_repro_claim){

  # Identify cases with secondary criteria ----
  repro_secondary <- rr_confrontations_repro_claim %>%
    mutate(
      claim_id = str_c(paper_id, "_", confrontation_claim4_id),
      repro_secondary_criteria = confrontation_repro_secondary_criteria_num > 0
    ) %>%
    select(claim_id, rr_id, repro_secondary_criteria)

  # Key original variables ----
  # Don't need sample size; instead use the version from repro_export
  orig_variables <- orig_dataset %>%
    select(paper_id,
           claim_id = unique_claim_id,
           orig_coef = original_coefficient_value_reported,
           orig_stat = original_statistic_value_reported,
           orig_ptype = original_p_value_type_reported,
           orig_pvalue = original_p_value_value_reported,
           orig_es = original_effect_size_value_reported)

  # Calculate lower and upper bounds ----
  outcomes_thresholds <- repro_export %>%
    left_join(select(orig_variables, -paper_id),
              by = "claim_id") %>%
    rename(orig_ss = orig_analytic_sample_size_value_criterion_reported) %>%
    mutate(
      # Ignore cases where effect size equals coefficient since these aren't
      # real effect sizes (they're just the SER method)
      orig_es = ifelse(
        orig_es == orig_coef & !is.na(orig_es) & !is.na(orig_coef),
        NA_real_,
        orig_es
      ),
      # For everything besides p-value, bounds defined by 15% (round for sample
      # size)
      lower_ss = round_off(orig_ss - (orig_ss*.15)),
      upper_ss = round_off(orig_ss + (orig_ss*.15)),
      lower_stat = orig_stat - (abs(orig_stat)*.15),
      upper_stat = orig_stat + (abs(orig_stat)*.15),
      lower_coef = orig_coef - (abs(orig_coef)*.15),
      upper_coef = orig_coef + (abs(orig_coef)*.15),
      lower_es = orig_es - (abs(orig_es)*.15),
      upper_es = orig_es + (abs(orig_es)*.15),
      # p-value bounds depend on original type:
      # If it was a less-than, have to make the lower bound the same as the
      # threshold, since anything less than threshold is precise
      lower_pvalue = ifelse(orig_ptype == "less-than",
                            orig_pvalue,
                            orig_pvalue - .05),
      # If it was a greater-than, have to to make the upper bound the same as
      # the threshold, since anything greater than threshold is precise
      upper_pvalue = ifelse(orig_ptype == "greater-than",
                            orig_pvalue,
                            orig_pvalue + .05),
      # give p-values sensible bounds
      lower_pvalue = ifelse(lower_pvalue < 0, 0, lower_pvalue),
      upper_pvalue = ifelse(upper_pvalue > 1, 1, upper_pvalue)
    ) %>%
    rename(
      rr_ss = rr_analytic_sample_size_value_reported,
      rr_pvalue = rr_p_value_value_reported,
      rr_coef = rr_coefficient_value_reported,
      rr_stat = rr_statistic_value_reported,
      rr_es = rr_effect_size_value_reported,
    ) %>%
    mutate(
      # calculate digits of the original variables for rounding purposes below
      digits_coef = str_extract(orig_coef, "([^(?=\\.)]*)$") %>%
        nchar(),
      digits_stat = str_extract(orig_stat, "([^(?=\\.)]*)$") %>%
        nchar(),
      digits_pvalue = str_extract(orig_pvalue, "([^(?=\\.)]*)$") %>%
        nchar(),
      digits_es = str_extract(orig_es, "([^(?=\\.)]*)$") %>%
        nchar(),
      # Round to the same number of digits as the original if original is
      # available. If original is not available, do not round
      rr_coef = ifelse(!is.na(digits_coef),
                       round_off(rr_coef, digits_coef),
                       rr_coef),
      rr_stat = ifelse(!is.na(digits_stat),
                       round_off(rr_stat, digits_stat),
                       rr_stat),
      rr_pvalue = ifelse(!is.na(digits_pvalue) & orig_ptype == "exact",
                         round_off(rr_pvalue, digits_pvalue),
                         rr_pvalue),
      rr_es = ifelse(!is.na(digits_es),
                     round_off(rr_es, digits_es),
                     rr_es)
    ) %>%
    select(-contains("digits"))

  # Evaluate the outcomes of each measure ----
  # Evaluate if the outcome of each measure is precise, approximate, not, or
  # non-outcome when data is unavailable, then create a summary measure
  # (outcome_overall) that summarizes the overall reproducibility of the claim
  repro_added <- outcomes_thresholds %>%
    mutate(
      outcome_ss = case_when(
        rr_ss == orig_ss ~ "precise",
        rr_ss != orig_ss & between(rr_ss,lower_ss, upper_ss) ~ "approximate",
        rr_ss < lower_ss | rr_ss > upper_ss ~ "not",
        is.na(orig_ss) | is.na(rr_ss) ~ "non-outcome",
      ),
      outcome_stat = case_when(
        rr_stat == orig_stat ~ "precise",
        rr_stat != orig_stat & between(rr_stat,
                                       lower_stat,
                                       upper_stat) ~ "approximate",
        rr_stat < lower_stat | rr_stat > upper_stat ~ "not",
        is.na(orig_stat) | is.na(rr_stat) ~ "non-outcome",
      ),
      outcome_coef = case_when(
        rr_coef == orig_coef ~ "precise",
        rr_coef != orig_coef & between(rr_coef,
                                       lower_coef,
                                       upper_coef) ~ "approximate",
        rr_coef < lower_coef | rr_coef > upper_coef ~ "not",
        is.na(orig_coef) | is.na(rr_coef) ~ "non-outcome",
      ),
      outcome_es = case_when(
        rr_es == orig_es ~ "precise",
        rr_es != orig_es & between(rr_es, lower_es, upper_es) ~ "approximate",
        rr_es < lower_es | rr_es > upper_es ~ "not",
        is.na(orig_es) | is.na(rr_es) ~ "non-outcome",
      ),
      outcome_pvalue = case_when(
        orig_ptype == "exact" & rr_pvalue == orig_pvalue ~ "precise",
        orig_ptype == "less-than" & rr_pvalue < orig_pvalue ~ "precise",
        orig_ptype == "greater-than" & rr_pvalue > orig_pvalue ~ "precise",
        rr_pvalue != orig_pvalue & between(rr_pvalue,
                                           lower_pvalue,
                                           upper_pvalue) ~ "approximate",
        orig_ptype %in% c("less-than", "greater-than") &
          rr_pvalue == orig_pvalue ~ "approximate",
        rr_pvalue < lower_pvalue | rr_pvalue > upper_pvalue ~ "not",
        is.na(orig_pvalue) | is.na(rr_pvalue) ~ "non-outcome",
      )
    ) %>%
    rowwise() %>%
    mutate(
      outcome_overall = case_when(
        # If all available outcomes are precise, then outcome_overall
        # is precise
        any(across(starts_with("outcome")) == "precise") &
          all(across(starts_with("outcome")) == "precise" |
                across(starts_with("outcome")) == "non-outcome") ~ "precise",
        # If available outcomes are a combo of precise and approximate, then
        # outcome_overall is approximate
        any(across(starts_with("outcome")) == "approximate") &
          all(
            across(starts_with("outcome")) == "precise" |
              across(starts_with("outcome")) == "approximate" |
              across(starts_with("outcome")) == "non-outcome"
          ) ~ "approximate",
        # If at least one available outcome is not reproduced, then
        # outcome_overall is not reproduced
        any(across(starts_with("outcome")) == "not") ~ "not",
        all(across(starts_with("outcome")) == "non-outcome") ~ "none",
        TRUE ~ "other"
      )
    ) %>%
    # As a final step, add the repro_secondary_criteria indicator created above
    left_join(repro_secondary, by = c("claim_id", "rr_id")) %>%
    select(unique_report_id,
           repro_lb_sample = lower_ss,
           repro_ub_sample = upper_ss,
           repro_outcome_sample = outcome_ss,
           repro_lb_coef = lower_coef,
           repro_ub_coef = upper_coef,
           repro_outcome_coef = outcome_coef,
           repro_lb_stat = lower_stat,
           repro_ub_stat = upper_stat,
           repro_outcome_stat = outcome_stat,
           repro_lb_p = lower_pvalue,
           repro_ub_p = upper_pvalue,
           repro_outcome_p = outcome_pvalue,
           repro_lower_effect = lower_es,
           repro_upper_effect = upper_es,
           repro_outcome_effect = outcome_es,
           repro_outcome_overall = outcome_overall,
           repro_secondary_criteria)

}

create_repro_export <- function(reproduction_qa,
                                p2_repro_input_changelog) {
  
  reproduction_qa %>%
    transform_repro_input() %>%
    update_repro(p2_repro_input_changelog)
  
}

create_repro_analytic <- function(repro_export,
                                  orig_dataset,
                                  rr_confrontations_repro_claim){


  repro_supplementary <- calculate_repro_summary(repro_export,
                                                 orig_dataset,
                                                 rr_confrontations_repro_claim)

  repro_export %>%
    left_join(repro_supplementary, by = "unique_report_id") %>%
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
      repro_secondary_criteria
    )

}