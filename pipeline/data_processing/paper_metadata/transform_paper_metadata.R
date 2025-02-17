transform_paper_metadata <- function(all_metadata_filled, status) {
  
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
    # There are 7 more papers in this set than were actually used.
    # Filter out the 7 papers not among p1 and p2 deliverables
    left_join(status %>% select(paper_id, p1_delivery, p2_delivery),
              by = "paper_id") %>%
    filter(p1_delivery | p2_delivery) %>%
    select(-c(p1_delivery, p2_delivery)) %>%
    add_column(is_covid = FALSE) %>%
    # For paper q3XN, Author last name is Na, which gets erased when loaded 
    # into R
    rows_update(tibble(paper_id = "q3XN", author_last = "Na"), 
                by = "paper_id")
}
