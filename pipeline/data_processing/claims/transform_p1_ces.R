# Transform Phase 1 extracted claims
# Keep only necessary variables and transform for merging
transform_p1_ces <- function(tagtable_p1) {
  
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
    add_column(claim4_result = NA_character_,
               .before = "coded_claim4") %>%
    add_column(phase = "P1") %>%
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
