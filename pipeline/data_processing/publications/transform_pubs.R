# Add changes to the raw publications dataset and recode publication categories
transform_pubs <- function(publications_raw,
                           pub_changelog) {
  
  publications_raw %>%
    add_column(pub_stat_version = 1) %>%
    apply_changelog(changes = pub_changelog,
                    id = "ISSN") %>%
    select(-c(pub_stat_version)) %>%
    mutate(
      # We want both the 10 field and 6 field versions  
      # for supplement and analysis, respectively
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
