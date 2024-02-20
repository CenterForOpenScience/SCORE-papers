merge_paper_metadata <- function(all_metadata_filled,
                                 publications) {
  
  all_metadata_filled %>%
    select(-c(index, 
              ISSN_WOS, 
              publication_standard, 
              eISSN,
              category,
              WOS_category,
              accession_number,
              pub_year_WOS,
              publication_CR,
              publication_WOS)) %>%
    rename(ISSN = ISSN_CR,
           pub_year = pub_year_CR) %>%
    left_join(publications, by = "ISSN")

}