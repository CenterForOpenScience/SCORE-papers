merge_claims <- function(tagtable_p1,
                         tagtable_covid_p1,
                         tagtable_p2_CES,
                         stitched_claims,
                         complex_bushel,
                         status,
                         covid_metadata) {
  
  claims_p1 <- transform_p1_ces(tagtable_p1,
                                tagtable_covid_p1)
  
  claims_p2 <- transform_p2_ces(tagtable_p2_CES)
  
  bushel_claims <- transform_bushel(stitched_claims)
  
  complex <- complex_bushel %>%
    mutate(unique_claim_id = str_c(paper_id, "_", claim_id),
           complex = as.logical(complex)) %>%
    select(unique_claim_id,
           bushel_complex = complex)
  
  # List of papers that were actually used in SCORE
  used <- status %>%
    filter(p1_delivery | p2_delivery | bushel_delivery) %>%
    pull(paper_id) %>%
    c(covid_metadata$paper_id)
  
  rbind(claims_p1,
                claims_p2) %>%
    mutate(unique_claim_id = str_c(paper_id, "_single-trace"),
           .before = "paper_id") %>%
    add_column(single_trace_equivalent = NA) %>%
    rbind(bushel_claims) %>%
    filter(!is.na(claim2_abstract)) %>%
    left_join(complex, by = "unique_claim_id") %>%
    filter(paper_id %in% used)
  
}

# Merging Sub-functions ----
# Keep only necessary variables and transform for merging

# Transform Phase 1 extracted claims
transform_p1_ces <- function(tagtable_p1,
                             tagtable_covid_p1) {
  
  tagtable_p1 %>%
    select(-c(pdf_filename,
              coder,
              return_rationale,
              internal_note,
              claim_decision,
              version,
              claim2_internalnote, 
              claim3a_internalnote,
              claim3b_internalnote,
              claim4_internalnote,           
              sample_size_avail,        
              sample_size_internalnote,
              effect_size_avail,        
              effect_size_internalnote,
              p_value_avail,            
              p_value_internalnote,    
              return_pretrace)) %>%
    mutate(phase = case_when(
      paper_id %in% tagtable_covid_p1$paper_id ~ "covid",
      .default = "P1"
      )) %>%
    add_column(claim4_result = NA_character_,
               .before = "coded_claim4") %>%
    add_column(stat_evidence = NA_character_,        
               coded_stat_evidence = NA_character_,
               stat_evidence_pg = NA_character_,      
               stat_evidence_start = NA_character_,
               stat_evidence_box = NA_character_,     
               sample_size_confidence = NA_character_,
               .after = "claim4_box") %>%
    rename(claim3_hyp = claim3a_concretehyp,
           coded_claim3 = coded_claim3a,
           claim3_pg = claim3a_pg,
           claim3_start = claim3a_start,
           claim3_box = claim3a_box)
  
}

# Transform Phase 2 extracted claims
transform_p2_ces <- function(tagtable_p2_CES) {
  
  tagtable_p2_CES %>%
    select(-c(pdf_filename,
              coder,
              article_eligibility,
              return_rationale,
              internal_note,
              claim_decision,
              version)) %>%
    add_column(phase = "P2") %>%
    add_column(coded_claim2 = NA_character_,
               .after = "claim2_abstract") %>%
    add_column(coded_claim3 = NA_character_,
               .after = "claim3_hyp") %>%
    add_column(claim3b_testspec = NA_character_,
               coded_claim3b = NA_character_,
               claim3b_pg = NA_character_,
               claim3b_start = NA_character_,
               claim3b_box = NA_character_,
               claim4_inftest = NA_character_,
               .after = "claim3_box")
  
}

# Transform Bushel Claims
transform_bushel <- function(stitched_claims) {
  
  stitched_claims %>%
    mutate(unique_claim_id = str_c(paper_id, "_", claim4_id),
           .before = "paper_id") %>%
    select(-c(pdf_filename_x,
              pdf_filename_y,
              internal_note,
              claim_decision,
              version,
              trace_n,
              claim2_id,
              ta2_claim2_id,
              ta3_claim2_id,
              claim3_id,
              ta2_claim3_id,
              ta3_claim3_id,
              focal,
              focal_sample,
              claim4_id,
              ta2_claim4_id,
              ta3_claim4_id,
              stat_evidence_id,
              ta2_stat_evidence_id,
              ta3_stat_evidence_id)) %>%
    add_column(phase = "P2-bushel") %>%
    add_column(claim3b_testspec = NA_character_,
               coded_claim3b = NA_character_,
               claim3b_pg = NA_character_,
               claim3b_start = NA_character_,
               claim3b_box = NA_character_,
               claim4_inftest = NA_character_,
               .after = "claim3_box") %>%
    rename(single_trace_equivalent = p1_equivalent) %>%
    relocate(single_trace_equivalent,
            .after = "phase") %>%
    relocate(coded_stat_evidence,
            .after = "stat_evidence") %>%
    relocate(stat_evidence_box,
             .after = "stat_evidence_box") %>%
    add_column(sample_size = NA_character_,
               coded_sample_size = NA_character_, 
               sample_size_pg = NA_character_,       
               sample_size_start = NA_character_,
               sample_size_box = NA_character_,
               sample_size_confidence = NA_character_,
               effect_size = NA_character_,
               coded_effect_size = NA_character_,
               effect_size_pg = NA_character_,        
               effect_size_start = NA_character_,
               effect_size_box = NA_character_,
               p_value = NA_character_,
               coded_p_value = NA_character_,
               p_value_pg = NA_character_,
               p_value_start = NA_character_,         
               p_value_box = NA_character_,
               corr_author = NA_character_,
               corr_email = NA_character_,
               .after = "stat_evidence_start")
  
}
