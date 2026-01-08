# Remove duplicates, standardize values, and arrange columns in sensible order
transform_orig_updated <- function(orig_updated, orig_extended_updated) {
  
  orig_updated %>%
    mutate(
      # Prioritize which version of duplicates to keep
      data_source_priority = case_match(original_data_source,
                                        "p2_manual" ~ 1,
                                        "p2_power" ~ 2,
                                        "p2_pipeline" ~ 3,
                                        "orig_via_repro_form" ~ 4,
                                        "pipeline_orig" ~ 5,
                                        "repro" ~ 6),
      # Standardize values for original_effect_size_type_reported and
      # original_effect_size_type_reference
      across(
        c(original_effect_size_type_reported,
          original_effect_size_type_reference),
        ~ case_match(
          .x,
          c("Cohen's d", "d", "d_sample", "d_sample (one)") ~ "cohen_d",
          "Cohen's dz" ~ "cohen_dz",
          c("Cohen's f-squared", "cohenf2") ~ "cohen_f_squared",
          c("Cohen's w", "cohens_w") ~ "cohen_w",
          "Cramer's V" ~ "cramer_v",
          "Eta-squared" ~ "eta_squared",
          "Hazard ratio" ~ "hazard_ratio",
          c("adjusted_odds_ratio", "Odds ratio") ~ "odds_ratio",
          "Partial eta-squared" ~ "partial_eta_squared",
          "path_coef" ~ "path_coefficient",
          c("Pearson's r", "r") ~ "pearson_r",
          "Phi" ~ "phi",
          "probit_coefficient_estimate" ~ "probit_coefficient",
          c("multiple_regression_coefficient",
            "raw_ols_coefficient",
            "raw_regression_coefficient",
            "raw_regression_coefficient_from_a_two_step_system_gmm",
            "raw_regression_coefficient_beta") ~ "regression_coefficient",
          "relative risk ratio" ~ "relative_risk_ratio",
          c("SER method", "SER-Method") ~ "ser_method",
          "SER-t method" ~ "ser_method_t",
          c("standardized_regression_coefficient",
            "standardized_coefficient") ~ "standardized_regression_coefficient",
          .default = .x
        )
      )
    ) %>%
    arrange(unique_claim_id, data_source_priority) %>%
    relocate(data_source_priority, .after = original_data_source) %>%
    distinct(unique_claim_id, .keep_all = TRUE) %>%
    select(-c(data_source_priority)) %>%
    left_join(orig_extended_updated, join_by(paper_id, claim_id))
}
