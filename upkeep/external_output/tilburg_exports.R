# Functions for updating data given to the Tilburg University team for
# data enhancement.

# Make RR data inputs for Tilburg delivery
# Selects only replication outcomes from merged RR dataset, organizes columns 
# for export, saves new rr_statistics_input file.
make_tilburg_rr_input <- function(
    orig_dataset,
    repli_export,
    input_gsheet = "1xkbE74CmOJaPdN0Y_Z6upcbPo-GGkiBdKT2PS-VoK9M"
  ) {
  
  orig_available <- orig_dataset %>%
    select(unique_claim_id) %>%
    pull()
  
  # Tilburg needs the row order to be preserved in our uploads to them
  tilburg_order <- input_gsheet %>%
    range_read(sheet = 2) %>%
    select(unique_report_id)
  
  repli <- repli_export %>%
    filter(rr_type != "Hybrid") %>%
    mutate(
      unique_claim_id = paste0(paper_id,
                               "_",
                               claim_id),
      rr_statistic_type_reported = case_match(
        rr_statistic_type_reported,
        "f" ~ "F",
        .default = rr_statistic_type_reported
      )
    ) %>%
    filter(unique_claim_id %in% orig_available)
  
  new_repli <- repli %>%
    filter(!(unique_report_id %in% tilburg_order$unique_report_id))

  rr_statistics_input <- tilburg_order %>%
    left_join(repli, by = "unique_report_id") %>%
    add_row(new_repli) %>%
    select("paper_id", 
           "rr_id", 
           "pdf_filename", 
           "rr_type", 
           "rr_analysis_link",
           "rr_analytic_sample_size_units_reported", 
           "rr_analytic_sample_size_value_reported",
           "rr_analytic_sample_stage", 
           "rr_expected_sample_reached_reported",
           "rr_analytic_sample_cells_reported",
           "rr_analytic_subsample_n1",
           "rr_analytic_subsample_n2",
           "rr_analytic_subsample_a_00", 
           "rr_analytic_subsample_b_01",
           "rr_analytic_subsample_c_10", 
           "rr_analytic_subsample_d_11",
           "rr_statistic_fulltext_reported", 
           "rr_statistic_analysis_type_reported",
           "rr_statistic_interaction_reported",
           "rr_statistic_type_reported", 
           "rr_statistic_value_reported",
           "rr_statistic_df1_reported", 
           "rr_statistic_df2_reported",
           "rr_coefficient_type_reported", 
           "rr_coefficient_value_reported", 
           "rr_coefficient_se_reported",
           "rr_p_value_confirmation_reported",
           "rr_p_value_value_reported",
           "rr_effect_size_fulltext_reported",
           "rr_effect_size_type_reported",
           "rr_effect_size_value_reported", 
           "rr_repl_pattern_criteria_reported",
           "rr_repl_pattern_description_reported", 
           "rr_repl_pattern_replicated_reported",
           "rr_repl_effect_direction_reported", 
           "rr_repl_exact_replicated_reported",
           "rr_repl_subjective_replicated_reported",
           "rr_repl_subjective_description_reported",
           "rr_total_model_parameters",
           "rr_labteam_notes",
           "rr_stat_version",
           "rr_original_data_overlap",
           "claim_id",
           "unique_report_id",
           "unique_claim_id") 
  
  return(rr_statistics_input)
  
}

# Make original data inputs for Tilburg delivery
make_tilburg_orig_input <- function(
    p2_id_key,
    xmlpdfmerge,
    all_metadata_filled,
    tagtable_covid_p1,
    rr_confrontations_prereg,
    repli_primary,
    tagtable_p1,
    tagtable_p2_CES,
    finalized_claim4_table,
    orig_dataset,
    input_gsheet = "1P4RrEUET-jdgbrMyFofEgKlJR1DX7oKxA9azcmcXwFI") {

  # Tilburg needs the row order to be preserved in our uploads to them
  current_order <- input_gsheet %>%
    range_read(sheet = 2) %>%
    select(paper_id,
           c4_claim_id,
           unique_claim_id)

  # Currently "id" in xmlpdfmerge can be either paper_id or ta2_id
  pid_key <- select(p2_id_key,
                    paper_id,
                    id = ta2_pid)

  pdf_links <- xmlpdfmerge %>%
    select(id, pdf) %>%
    left_join(pid_key, by = "id") %>%
    mutate(paper_id = coalesce(paper_id, id)) %>%
    mutate(pdf = str_c(pdf, Sys.getenv("current_vol"))) %>%
    select(paper_id, pdf)
  
  p1_claims <- select(tagtable_p1,
                      paper_id,
                      coded_claim3b,
                      coded_claim4) %>%
    mutate(c4_claim_id = "single-trace")
  
  p2_claims <- select(tagtable_p2_CES,
                      paper_id,
                      coded_claim4) %>%
    mutate(c4_claim_id = "single-trace",
           coded_claim3b = NA)
  
  bushel_claims <- finalized_claim4_table %>%
    select(paper_id,
           c4_claim_id = claim_id,
           coded_claim4 = coded_claim) %>%
    mutate(coded_claim3b = NA)
  
  claims <- rbind(p1_claims, p2_claims, bushel_claims)

  metadata <- all_metadata_filled %>%
    select(paper_id,
           pub_short,
           author_last_CR,
           pub_year_CR) %>%
    mutate(pdf_filename = str_c(author_last_CR,
                                "_",
                                pub_short,
                                "_",
                                pub_year_CR,
                                "_",
                                paper_id))

  # Fill in weird missing pdf_filename due to missing author_last_CR
  metadata[metadata$paper_id == "q3XN",
           "pdf_filename"] <- "Na_Criminology_2012_q3XN"

  covid_pdf_filenames <- select(tagtable_covid_p1,
                                paper_id,
                                pdf_filename)

  pdf_filenames <- metadata %>%
    select(paper_id,
           pdf_filename) %>%
    rbind(covid_pdf_filenames)

  # Prereg check-ins represent one source of expected claims
  repli_prereg <- rr_confrontations_prereg %>%
    select(paper_id,
           confrontation_claim4_id) %>%
    mutate(unique_claim_id = str_c(paper_id,
                                   "_",
                                   confrontation_claim4_id),
           .keep = "none")

  # Otherwise, everything that is already coded is expected
  # Only repli primary is necessary because every claim in repli secondary is
  # represented in there already
  expected_claims <- repli_primary %>%
    select(paper_id,
           claim_id) %>%
    mutate(unique_claim_id = str_c(paper_id,
                                   "_",
                                   claim_id),
           .keep = "none") %>%
    rbind(repli_prereg) %>%
    distinct()
  
  pdf_info <- left_join(pdf_filenames, pdf_links, by = "paper_id")
  
  orig_input <- left_join(current_order, 
                          orig_dataset, 
                          join_by(paper_id,
                                  c4_claim_id == claim_id,
                                  unique_claim_id)) %>%
    full_join(orig_dataset) %>%
    mutate(c4_claim_id = coalesce(c4_claim_id, claim_id)) %>%
    select(-c(claim_id)) %>%
    mutate(
      original_statistic_type_reported = case_match(
        original_statistic_type_reported,
        "f" ~ "F",
        .default = original_statistic_type_reported
      ),
      original_statistic_type_reference = case_match(
        original_statistic_type_reference,
        "f" ~ "F",
        .default = original_statistic_type_reference
      )
    )
  
  # Restrict to just what's needed based on either being checked in at prereg 
  # or reported. Also, due to how Tilburg relies on row order remaining intact,
  # allow everything that has already been sent to stay even if it's useless
  orig_input %>%
    filter(unique_claim_id %in% pull(expected_claims) | 
             unique_claim_id %in% pull(current_order, unique_claim_id)) %>%
    left_join(pdf_info, by = "paper_id") %>%
    mutate(
      original_materials_link = case_when(is.na(original_materials_link) ~ pdf,
                                          .default = original_materials_link)
    ) %>%
    left_join(claims, join_by(paper_id, c4_claim_id)) %>%
    select("pdf_filename",
           "paper_id", 
           "c4_claim_id", 
           "unique_claim_id", 
           "orig_stat_version",
           "original_materials_link",
           "original_poweranalysis_link",
           "coded_claim3b", 
           "coded_claim4",
           "original_statistic_analysis_type",
           "original_analytic_sample_size_units_reported",
           "original_analytic_sample_size_value_reported",
           "original_analytic_subsample_n1", 
           "original_analytic_subsample_n2",
           "original_analytic_subsample_a_00", 
           "original_analytic_subsample_b_01",
           "original_analytic_subsample_c_10", 
           "original_analytic_subsample_d_11",
           "original_statistic_fulltext_reported",
           "original_statistic_effect_type_reported",
           "original_statistic_type_reported",
           "original_statistic_value_reported",
           "original_statistic_df1_reported", 
           "original_statistic_df2_reported",
           "original_coefficient_type_reported",
           "original_coefficient_value_reported",
           "original_coefficient_se_reported",
           "original_total_model_parameters",
           "original_p_value_type_reported", 
           "original_p_value_tails_reported",
           "original_p_value_value_reported",
           "original_effect_size_fulltext_reported",
           "original_effect_size_type_reported",
           "original_effect_size_value_reported",
           "original_samplesize_calculation_contributor",
           "original_analytic_sample_size_value_reference",
           "original_statistic_fulltext_reference",
           "original_statistic_effect_type_reference",
           "original_statistic_type_reference", 
           "original_statistic_value_reference",
           "original_statistic_df1_reference", 
           "original_statistic_df2_reference",
           "original_coefficient_type_reference",
           "original_coefficient_value_reference",
           "original_coefficient_se_reference",
           "original_p_value_tails_reference", 
           "original_p_value_value_reference",
           "original_effect_size_type_reference",
           "original_effect_size_value_reference",
           "rr_threshold_analytic_sample_size", 
           "rr_stage1_analytic_sample_size",
           "rr_stage2_analytic_sample_size",
           "original_poweranalysis_notes")
    
}