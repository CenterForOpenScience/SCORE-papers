##################
# HELPER FUNCTIONS
# Subfunctions that are reused in multiple functions
##################

# QC Check Helpers ----
# Create list of valid IDs
# Generates a list which holds valid paper IDs, claim IDs, and report IDs
extend_rr_projects <- function(rr_projects_raw,
                               repli_cases_projects) {
  
  rr_projects_raw %>%
    rbind(repli_cases_projects)
  
}

create_id_list <- function(status,
                           tagtable_covid_p1,
                           finalized_claim4_table,
                           rr_projects,
                           repli_cases_projects) {

  valid_papers <- rbind(select(status, paper_id),
                        select(tagtable_covid_p1, paper_id))

  bushel_claims <- finalized_claim4_table %>%
    select(paper_id,
           claim_id,
           p1_claim)

  single_trace_claims <- status %>%
    filter(p1_delivery | p2_delivery) %>%
    mutate(claim_id = "single-trace",
           p1_claim = TRUE) %>%
    select(paper_id,
           claim_id,
           p1_claim)

  covid <- tagtable_covid_p1 %>%
    filter(external) %>%
    mutate(claim_id = "single-trace",
           p1_claim = TRUE) %>%
    select(paper_id,
           claim_id,
           p1_claim)

  valid_claims <- rbind(bushel_claims, single_trace_claims, covid)


  # Generates a list of valid rr_ids
  valid_rr <- rr_projects %>%
    select(paper_id,
           rr_id,
           project_guid) %>%
    distinct(paper_id, rr_id, .keep_all = TRUE)
    

  list(valid_papers,
       valid_claims,
       valid_rr)

}


