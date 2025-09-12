merge_paper_metadata <- function(non_covid_metadata,
                                 covid_metadata,
                                 paper_cite,
                                 publications) {
  
  non_covid_metadata %>%
    left_join(publications, by = "ISSN") %>%
    add_row(covid_metadata) %>%
    # Standardize cases
    mutate(across(c(title, author_last, author_first), str_to_title)) %>%
    left_join(paper_cite %>% select(paper_id, citation), by = "paper_id")
  
}
