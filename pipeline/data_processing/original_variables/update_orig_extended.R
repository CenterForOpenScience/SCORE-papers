# Transform Tilburg extension data
transform_orig_output <- function(orig_stats_extended) {
  
  orig_stats_extended %>%
    # All character values and variable names are encased in quotes, so we need
    # to deal with that to get all of the lines to read in correctly, even when
    # there is quoted text within a value. Values that contain actual
    # quotations will have double quotes replaced with single quotes.
    mutate(
      across(where(is.character), ~ str_remove_all(.x, "\"")),
      across(where(is.character), ~ str_replace_all(.x, "\\\\", "\'"))
    ) %>%
    rename_with(~ str_remove_all(.x, "\"")) %>%
    select(paper_id,
           claim_id = c4_claim_id,
           original_statistic_analysis_type_statsteam = analysis_type,
           original_pearsons_r_defined,
           original_pearsons_r_numeric,
           original_es_lb_ci_pearson,
           original_es_ub_ci_pearson,
           original_effect_size_type_statsteam,
           original_effect_size_value_statsteam,
           original_es_lb_ci_nativeunits,
           original_es_ub_ci_nativeunits,
           original_power_small,
           original_power_medium,
           original_power_50_original_effect,
           original_power_75_original_effect,
           Tilburg_team_finished) %>%
    # Including "skipped" as finished because Tilburg has finished what they
    # can/will do with that claim
    mutate(Tilburg_team_finished = case_match(Tilburg_team_finished,
                                              "skipped" ~ TRUE,
                                              "finished" ~ TRUE,
                                              NA ~ NA,
                                              .default = FALSE))
  
}

update_orig_extended <- function(orig_stats_extended,
                                 orig_stats_extended_changelog) {
  
  transform_orig_output(orig_stats_extended) %>%
    apply_changelog(orig_stats_extended_changelog, "paper_id")
  
}


