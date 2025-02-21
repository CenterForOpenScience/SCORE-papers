# Transform Phase 2 extracted claims
# Keep only necessary variables and transform for merging
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
