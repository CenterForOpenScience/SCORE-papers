#################
# LOAD AND QC RR FILES
# Functions to properly load in data, check for obviously incorrect entries, 
# and make any transformations before use in the data pipeline.
#################

# Replication Files ----

## COS SCORE Data ----
# Load rr_attempts_minted.csv
# Load the file and manually fix missing is_HSR values
load_rr_attempts_minted <- function(rr_attempts_minted_file,
                                    rr_attempts_minted_moddate) {
  
  read_google_csv(rr_attempts_minted_file,
                  rr_attempts_minted_moddate) %>%
    # Some of the minted projects didn't get assigned an is_HSR value. 
    # Manually assign those values here
    mutate(is_hsr = case_when(rr_id %in% c("996g", 
                                           "19y4", 
                                           "8z81",
                                           "2y2g",
                                           "927",
                                           "9y8y",
                                           "5196",
                                           "2z3g",
                                           "2g5g",
                                           "2637",
                                           "8zz7",
                                           "y7g1",
                                           "0y68",
                                           "yk20",
                                           "5738",
                                           "m7k3",
                                           "2yyg",
                                           "9y2g",
                                           "6557",
                                           "gyg1",
                                           "6547",
                                           "4zz0",
                                           "0056",
                                           "94ky",
                                           "3g4k",
                                           "329k",
                                           "y791",
                                           "756g",
                                           "95my",
                                           "6g7k",
                                           "4968",
                                           "68",
                                           "m47",
                                           "ykg6") ~ "Non-HSR",
                              rr_id %in% c("8g1", 
                                           "99gy", 
                                           "8m71", 
                                           "618k", 
                                           "9ky", 
                                           "k17", 
                                           "m5y9", 
                                           "528",
                                           "618",
                                           "y2312",
                                           "20g",
                                           "g7g1",
                                           "4z02",
                                           "g9mm",
                                           "999g",
                                           "m7y9",
                                           "99m7",
                                           "99yg",
                                           "k144",
                                           "8m17",
                                           "m5m7",
                                           "556",
                                           "z189",
                                           "548",
                                           "7976",
                                           "1y22",
                                           "3zz",
                                           "0y08",
                                           "wz9",
                                           "g941",
                                           "y01",
                                           "4182",
                                           "0m7",
                                           "8g91",
                                           "356",
                                           "y50") ~ "HSR",
                              .default = is_hsr))
}

## SCORE ----

# Load SCORE-P2_081: Variable form - replications (Responses)
# Downloads the file from the SCORE Google Drive, checks that IDs are valid,
# and cleans and transforms it 
load_replication_qa <- function(replication_qa_gsheet,
                                p2_repli_vf,
                                valid_ids,
                                rr_attempts_minted) {
  
  # # This triggers tar_make() to load data if the modification date has
  # # changed since last run
  # if (!is.Date(as.Date(replication_qa_mod_date))) {
  #   stop(simpleError("File modification date invalid."))
  # }
  
  replication_qa_mod_date <- get_google_mod_date(replication_qa_gsheet)
  
  # The replication codebook indicates what data cleaning needs to be done
  to_pivot <- p2_repli_vf %>%
    filter(to_explode == TRUE) %>%
    pull(p2_form_variable)

  na_allowed <- p2_repli_vf %>%
    filter(na_allowed == TRUE) %>%
    pull(p2_form_variable)
  
  to_numeric <- p2_repli_vf %>%
    filter(allowed_values == "numeric") %>%
    pull(p2_form_variable)
  
  to_factors <- p2_repli_vf %>%
    filter(str_detect(allowed_values, ";")) %>%
    pull(p2_form_variable)
  
  # Read, clean, and transform ----
  replication_qa <- read_google_sheet(replication_qa_gsheet,
                                      replication_qa_mod_date) %>%
    # Original row index is used to create unique record id later
    mutate(original_index = as.character(row_number()-1)) %>%
    filter(!(ready_for_export %in% c("Never", "Issues"))) %>%
    select(-c(ready_for_export)) %>%
    mutate(
      pdf_filename = str_extract(asana_ticket_name, ".+?(?= - )"),
      paper_id = str_extract(pdf_filename, "([^_]*)$"),
      rr_id = str_extract(asana_ticket_name, "([^(?= - )]*)$"),
      # These are convenience variables for merging with P1
      is_covid = str_detect(pdf_filename, "covid"),
      rr_input_source = "p2_repli_form",
      rr_is_manylabs = "non_ml",
      # Some QA entries have multiple records that need to be pivoted out
      across(all_of(to_pivot), ~ str_split(.x, "\\n"))
    ) %>%
    unnest(all_of(to_pivot)) %>%
    group_by(original_index) %>%
    mutate(response_n = as.character(row_number())) %>%
    ungroup() %>%
    rename(claim_id = c4_claim_id) %>%
    mutate(
      unique_report_id = str_c(rr_id, "_", original_index, "_", response_n),
      across(all_of(na_allowed), ~ na_if(.x, "na")),
      # One value is reported in a nonstandard way that results in a 
      # warning/NA value. Fixed in change log.
      across(all_of(to_numeric), ~ as.double(.x)),
      across(all_of(to_factors), ~ as.factor(.x) %>% str_to_lower()),
      rr_stat_version = 1,
      rr_effect_size_type_reported = case_match(
        rr_effect_size_type_reported,
        "cohen's f-squared" ~ "cohen_f_squared",
        "cohen's d" ~ "cohen_d",
        "ser method" ~ "ser_method",
        "pearson's r" ~ "pearson_r",
        "cohen's dz" ~ "cohen_dz",
        "cohen's w" ~ "cohen_w",
        "partial eta-squared" ~ "partial_eta_squared",
        "eta-squared" ~ "eta_squared",
        "partial correlation" ~ "partial_correlation",
        "cramer's v" ~ "cramer_v",
        "cohen's q" ~ "cohen_q",
        "odds ratio" ~ "odds_ratio",
        "log-odds ratio" ~ "log_odds_ratio",
        "hazard ratio" ~ "hazard_ratio",
        "spearman's rho" ~ "spearman_rho",
        .default = rr_effect_size_type_reported
      ),
      across(c(rr_expected_sample_reached_reported,
               rr_repl_pattern_replicated_reported,
               rr_repl_effect_direction_reported,
               rr_repl_exact_replicated_reported), ~ .x == "yes")
    )
  
  # QC Checks for IDs ----
  if (any(!(replication_qa$paper_id %in% valid_ids[[1]]$paper_id))) {
    stop(simpleError("Invalid paper IDs present in replication_qa."))
  }
  
  if (any(!(replication_qa$claim_id %in% valid_ids[[2]]$claim_id))) {
    stop(simpleError("Invalid claim IDs present in replication_qa"))
  }

  if (any(!(replication_qa$rr_id %in% valid_ids[[3]]$rr_id))) {
    stop(simpleError("Invalid rr IDs present in replication_qa"))
  }
  
  # Minting data is used to determine RR type ----
  minted <- select(rr_attempts_minted, 
                   rr_type,
                   rr_id,
                   is_hsr)
  
  replication_qa %>%
    left_join(minted, by = "rr_id") %>%
    rename(minted_as = rr_type) %>%
    mutate(
      rr_type_internal = case_when(
        minted_as == "Generalizability" ~ "Generalizability",
        rr_original_data_overlap == "hybrid" ~ "Hybrid",
        is_hsr == "Non-HSR" ~ "Data Analytic Replication",
        is_hsr == "HSR" ~ "Direct Replication",
        .default = "Undefined"),
      rr_type = rr_type_internal
    ) %>%
    select(
      "pdf_filename",
      "paper_id", 
      "rr_id",
      "claim_id",
      "unique_report_id",
      "rr_input_source",
      "rr_is_manylabs",
      "rr_original_data_overlap", 
      "rr_analytic_sample_stage",
      "rr_analytic_sample_size_value_reported",
      "rr_analytic_sample_size_units_reported",
      "rr_expected_sample_reached_reported",
      "rr_analytic_sample_cells_reported", 
      "rr_statistic_fulltext_reported",
      "rr_statistic_analysis_type_reported",
      "rr_statistic_interaction_reported",
      "rr_coefficient_type_reported",
      "rr_coefficient_value_reported",
      "rr_coefficient_se_reported",
      "rr_total_model_parameters", 
      "rr_statistic_type_reported",
      "rr_statistic_value_reported",
      "rr_statistic_df1_reported",
      "rr_statistic_df2_reported",
      "rr_p_value_value_reported",
      "rr_p_value_confirmation_reported", 
      "rr_effect_size_fulltext_reported",
      "rr_effect_size_type_reported",
      "rr_effect_size_value_reported",
      "rr_repl_effect_direction_reported", 
      "rr_replication_difference_notes",
      "rr_repl_pattern_criteria_reported",
      "rr_repl_pattern_description_reported",
      "rr_repl_pattern_replicated_reported",
      "rr_repl_exact_replicated_reported",
      "rr_repl_subjective_replicated_reported",
      "rr_repl_subjective_description_reported", 
      "rr_labteam_notes",
      "is_covid",
      "rr_type_internal",
      "rr_type",
      "rr_stat_version",
    )
}

# OSF Files ----

# Repository: SCORE - Phase 1 RR and original article analyses 
# (https://osf.io/3fsbm/)

# Load rr_statistics_output_p2.txt
# Downloads file from OSF if it has been updated, stores the output, and 
# deletes the temp downloaded file
load_rr_statistics_output_p2 <- function(
    rr_statistics_output_p2_osf,
    rr_statistics_output_p2_mod_date
) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(rr_statistics_output_p2_mod_date))){
    stop(simpleError("File modification date invalid."))
  }
  
  rr_statistics_output_p2_file <- rr_statistics_output_p2_osf %>%
    osf_retrieve_file() %>%
    osf_download(path = here("pipeline",
                             "data_processing",
                             "temp"),
                 conflicts = "overwrite") %>%
    pull(local_path)
  
  rr_statistics_output_p2 <- read_tsv(rr_statistics_output_p2_file,
                                      show_col_types = FALSE)
  
  file.remove(rr_statistics_output_p2_file)
  
  rr_statistics_output_p2 %>%
    select(unique_report_id,
           rr_effect_size_type_reference = rr_effect_size_type_statsteam,
           rr_effect_size_value_reference = rr_effect_size_value_statsteam,
           rr_es_ub_ci_nativeunits,
           rr_es_lb_ci_nativeunits,
           rr_pearsons_r_defined,
           rr_pearsons_r_value,
           rr_es_ub_ci_pearson,
           rr_es_lb_ci_pearson,
           pi_ub_nativeunits = comparison_es_ub_pi_nativeunits,
           pi_lb_nativeunits = comparison_es_lb_pi_nativeunits,
           pi_ub_pearson = comparison_es_ub_pi_pearson,
           pi_lb_pearson = comparison_es_lb_pi_pearson,
           rr_power_small,
           rr_power_medium,
           rr_power_50_original_effect,
           rr_power_75_original_effect,
           rr_power_100_original_effect,
           rr_power_50_original_effect_alpha_.025,
           rr_power_75_original_effect_alpha_.025,
           rr_power_100_original_effect_alpha_.025,
           rr_power_50_original_effect_design,
           rr_power_75_original_effect_design,
           rr_power_100_original_effect_design,
           rr_power_50_original_effect_alpha_.025_design,
           rr_power_75_original_effect_alpha_.025_design,
           rr_power_100_original_effect_alpha_.025_design)
}
