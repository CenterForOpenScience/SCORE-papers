# Process data from prereg checkin
transform_repro_claims <- function(prereg_checkin_repro) {
  
  criteria_cols <- c("criteria_name",
                     "original_value",
                     "precise_reproduction",
                     "approx_reproduction",
                     "non_reproduction")
  
  repro_checkin <- prereg_checkin_repro %>%
    filter(is.na(confrontation_status)) %>%
    mutate(
      pdf_filename = str_extract(asana_ticket_name, ".+?(?= - )"),
      paper_id = str_extract(pdf_filename, "([^_]*)$"),
      rr_id = str_extract(asana_ticket_name, "([^(?= - )]*)$")
    ) 
  
  primary_df <- repro_checkin %>%
    # Information is formatted like a table within a text box for 
    # confrontation_repro_primary_criteria. Split it out into appropriate rows 
    # and columns
    mutate(
      primary_criteria = str_split(confrontation_repro_primary_criteria, "\\n")
    ) %>%
    unnest(primary_criteria) %>%
    separate_wider_delim(primary_criteria,
                         delim = regex("\\t"),
                         names = c("criteria_name",
                                   "original_value",
                                   "precise_reproduction",
                                   "approx_reproduction",
                                   "non_reproduction")) %>%
    mutate(
      across(all_of(criteria_cols), str_trim),
      criteria_type = case_match(
        criteria_name,
        "Sample size" ~ "sample_size",
        "Focal variable coefficient" ~ "coefficient",
        "Effect size of focal variable" ~ "effect_size",
        "Focal variable P-value" ~ "p_value",
        "Focal variable p-value" ~ "p_value",
        "Focal test statistic" ~ "test_statistic",
        .default = "undefined"
      ),
      across(all_of(criteria_cols), na_if, "NA"),
      across(all_of(criteria_cols), na_if, "nan"),
      across(all_of(criteria_cols), na_if, "N/A"),
      across(all_of(criteria_cols), na_if, "n/a"),
      across(all_of(criteria_cols), na_if, "N/a"),
      across(all_of(criteria_cols), na_if, "na"),
      primary_criteria_check = if_else(
        str_detect(confrontation_primary_criteria_available,
                   criteria_type),
        !is.na(original_value) & 
          !is.na(precise_reproduction) & 
          !is.na(non_reproduction),
        is.na(original_value) & 
          is.na(precise_reproduction) & 
          is.na(non_reproduction))
    ) %>%
    # bespoke hotfix for Travers (claim id: zqwm_single-trace, RRID: 2w7w2) 
    # because there are no primary criteria
    rows_update(tibble(rr_id = "2w7w2",
                       confrontation_claim4_id = "single-trace",
                       criteria_type = "sample_size",
                       primary_criteria_check = TRUE),
                by = c("rr_id",
                       "confrontation_claim4_id",
                       "criteria_type")) %>%
    group_by(across(-c(all_of(criteria_cols), criteria_type))) %>%
    nest() %>%
    select(-c(data)) %>%
    ungroup() %>%
    select(rr_id,
           confrontation_claim4_id,
           primary_criteria_check)
  
  repro_checkin %>%
    select(paper_id, 
           rr_id, 
           confrontation_claim4_id, 
           confrontation_data_source_url,
           confrontation_data_source_description, 
           confrontation_p_value_type_reported,
           confrontation_repro_analyst_criteria,
           confrontation_primary_criteria_available,
           confrontation_repro_secondary_criteria_num,
           confrontation_status) %>%
    left_join(primary_df, join_by(rr_id, confrontation_claim4_id))
  
}
