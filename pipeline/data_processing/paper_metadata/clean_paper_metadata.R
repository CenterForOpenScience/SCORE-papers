
# Creates a single dataset that contains:
# 1. Paper IDs (with TA2 and TA3 equivalents)
# 2. Claim IDs and the claim type (covid, phase 1, etc.)
# 3. RR IDs and their type (DAR, DR, etc.)
# 4. Report IDs
create_id_key <- function(covid_ta2,
                          tagtable_covid_p1,
                          status,
                          p2_id_key,
                          SCORE_CVD2,
                          stitched_claims,
                          replication_qa,
                          reproduction_qa) {

  # Paper IDs ----
  # COVID paper IDs not present in the status table, so pull them here
  # covid_ta2 and tagtable_covid_p1 are needed to get both paper ID and TA2 ID
  covid_ids <- covid_ta2 %>%
    select(ta2_pid,
           DOI = DOI_CR) %>%
    left_join(tagtable_covid_p1 %>%
                select(paper_id, DOI),
              by = "DOI") %>%
    select(-c(DOI)) %>%
    add_column(ta3_pid = NA)

  paper_ids <- status %>%
    # Only keep papers that were used in one of the deliveries to teams
    filter(p1_delivery | p2_delivery | bushel_delivery) %>%
    select(paper_id, p1_delivery) %>%
    left_join(p2_id_key %>% select(-c(index)),
              by = "paper_id") %>%
    add_row(covid_ids)

  # Claim IDs ----
  p1_claim_ids <- paper_ids %>%
    filter(p1_delivery) %>%
    select(paper_id) %>%
    mutate(claim_id = str_c(paper_id, "_single-trace"),
           type = "phase1")

  covid_claim_ids <- tagtable_covid_p1 %>%
    filter(external == TRUE) %>%
    select(paper_id) %>%
    mutate(claim_id = str_c(paper_id, "_single-trace"),
           type = "covid")

  p2_claim_ids <- SCORE_CVD2 %>%
    left_join(paper_ids, by = "ta2_pid") %>%
    mutate(claim_id = str_c(paper_id, "_single-trace"),
           type = "phase2") %>%
    select(paper_id, claim_id, type)

  bushel_claim_ids <- stitched_claims %>%
    select(paper_id, claim4_id, p1_equivalent) %>%
    mutate(claim_id = str_c(paper_id, "_", claim4_id),
           type = case_when(
             p1_equivalent == TRUE ~ "bushel_p1_equivalent",
             .default = "bushel")) %>%
    select(-c(claim4_id, p1_equivalent))

  claim_ids <- rbind(p1_claim_ids,
                     covid_claim_ids,
                     p2_claim_ids,
                     bushel_claim_ids)

  # RR IDs and report IDs ----
  repli_ids <- replication_qa %>%
    select(paper_id,
           claim_id,
           rr_id,
           unique_report_id,
           rr_type_internal) %>%
    mutate(claim_id = str_c(paper_id, "_", claim_id)) %>%
    select(-c(paper_id))

  repro_ids <- reproduction_qa %>%
    select(c4_claim_id,
           asana_ticket_name,
           rr_type_internal) %>%
    mutate(
      # Original row index is used to create unique record id later
      original_index = as.character(row_number()-1),
      rr_id = str_extract(asana_ticket_name, "([^(?= - )]*)$"),
      unique_report_id = str_c(rr_id,
                               "_",
                               original_index,
                               "_1"),
      claim_id = str_c(str_extract(asana_ticket_name,
                                   "(?<=_)[:alnum:]*(?=[:blank:])"),
                       "_",
                       c4_claim_id)
    ) %>%
    select(-c(asana_ticket_name, c4_claim_id, original_index))

  rr_ids <- rbind(repli_ids, repro_ids) %>%
    mutate(
      rr_type = case_match(rr_type_internal,
                           c("Direct Replication",
                             "Data Analytic Replication",
                             "Generalizability",
                             "Undefined") ~ "replication",
                           c("Source Data Reproduction",
                             "Author Data Reproduction",
                             "Extended Push Button Reproduction",
                             "Push Button Reproduction") ~ "reproduction",
                           "Hybrid" ~ "hybrid"),
      rr_subtype = case_match(rr_type_internal,
                              "Direct Replication" ~ "New Data",
                              "Data Analytic Replication" ~ "Secondary Data",
                              "Hybrid" ~ "Original and Secondary data",
                              .default = rr_type_internal)
    ) %>%
    rename(report_id = unique_report_id) %>%
    # Remove redundant fields
    select(-c(rr_type_internal))

  # Compile datasets ----
  paper_ids %>%
    left_join(claim_ids, by = "paper_id") %>%
    left_join(rr_ids, by = "claim_id") %>%
    select(-c(p1_delivery))

}

transform_pubs <- function(publications_raw) {
  publications_raw %>%
    mutate(
      # We want both the 10 field and 6 field versions for supplement and 
      # analysis, respectively
      COS_pub_expanded = COS_pub_category,
      COS_pub_category = case_match(
        COS_pub_category,
        c("marketing/org behavior", "management") ~ "business",
        "economics" ~ "economics and finance",
        c("criminology", "sociology") ~ "sociology and criminology",
        "public administration" ~ "political science",
        c("health", "psychology") ~ "psychology and health",
        .default = COS_pub_category
      )
    ) 
}

merge_paper_metadata <- function(all_metadata_filled,
                                 publications,
                                 covid_metadata,
                                 status) {
  
  # Metadata for covid papers was collected separately
  # Reconcile differences for merging
  covid <- covid_metadata %>%
    filter(external == TRUE) %>%
    mutate(published = ymd(published) %>% 
             format("%b %Y") %>% 
             str_to_upper()) %>%
    add_column(author_address = NA,
               author_email = NA,
               keywords = NA,
               volume = NA,
               issue = NA,
               pg = NA,
               reprint_address = NA,
               type = NA,
               language = NA,
               funding = NA,
               open_access = NA,
               publisher_address = NA,
               eISSN = NA,
               is_covid = TRUE,
               COS_pub_expanded = "health",
               COS_pub_category = "psychology and health") %>%
    select(paper_id,
           title,
           author_last,
           author_first,
           author_address,
           author_email,
           abstract,
           keywords,
           volume,
           issue,
           pg,
           published,
           pub_year,
           DOI,
           reprint_address,
           type,
           language,
           funding,
           open_access,
           publisher,
           publisher_address,
           ISSN,
           publication_standard,
           eISSN,
           COS_pub_expanded,
           COS_pub_category,
           is_covid)
  
  all_metadata_filled %>%
    # Drop WOS entries so there is only one source for paper metadata
    select(paper_id,
           title = title_CR,
           author_last = author_last_CR,
           author_first = author_first_CR,
           author_address,
           author_email,
           abstract,
           keywords,
           volume,
           issue,
           pg,
           published,
           pub_year = pub_year_CR,
           DOI = DOI_CR,
           reprint_address,
           type,
           language,
           funding,
           open_access,
           publisher,
           publisher_address,
           ISSN = ISSN_CR) %>%
    left_join(publications, by = "ISSN") %>%
    # There are 7 more papers in this set than were actually used.
    # Filter out the 7 papers not among p1 and p2 deliverables
    left_join(status %>% select(paper_id, p1_delivery, p2_delivery),
              by = "paper_id") %>%
    filter(p1_delivery | p2_delivery) %>%
    select(-c(p1_delivery, p2_delivery)) %>%
    add_column(is_covid = FALSE) %>%
    add_row(covid) %>%
    # Standardize cases
    mutate(across(c(title, author_last, author_first), str_to_title)) %>%
    # For paper q3XN, Author last name is Na, which gets erased when loaded 
    # into R
    rows_update(tibble(paper_id = "q3XN", author_last = "Na"), 
                by = "paper_id")
  
}