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
    left_join(publications, by = "ISSN") %>%
    mutate(
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