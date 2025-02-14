# Metadata for covid papers was collected separately from other papers
# Reconcile differences for merging
transform_covid_metadata <- function(covid_metadata_raw) {
  
  covid_metadata_raw %>%
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
  
}
