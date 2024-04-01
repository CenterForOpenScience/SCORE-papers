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
           COS_pub_category)
  
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
    ) %>%
    # There are 7 more papers in this set than were actually used.
    # Filter out the 7 papers not among p1 and p2 deliverables
    left_join(status %>% select(paper_id, p1_delivery, p2_delivery),
              by = "paper_id") %>%
    filter(p1_delivery | p2_delivery) %>%
    select(-c(p1_delivery, p2_delivery)) %>%
    add_row(covid) %>%
    # Standardize cases
    mutate(across(c(title, author_last, author_first), str_to_title)) %>%
    # For paper q3XN, Author last name is Na, which gets erased when loaded 
    # into R
    rows_update(tibble(paper_id = "q3XN", author_last = "Na"), 
                by = "paper_id")
  
}