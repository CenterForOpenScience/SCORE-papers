create_pr_analytic <- function(pr_data_form) {
  
  pr_outcomes <- pr_data_form %>%
    rename(
      paper_data_type = existing_data,
      paper_link = paper_data_link,
      paper_link_content = paper_data_link_content,
      paper_instructions = paper_data_instructions,
      paper_instructions_content = paper_data_instructions_content,
      paper_request = paper_materials_available_on_request,
      paper_request_content = paper_materials_available_on_request_content,
      pr_notes = other_notes
    ) %>%
    mutate(
      pr_data_location = case_match(
        pr_data_location,
        "Publisher/journal website" ~ "Publisher website",
        .default = pr_data_location
      ),
      pr_data_complete = case_match(
        pr_data_complete,
        "Partial/Incomplete" ~ "Incomplete",
        .default = pr_data_complete
      ),
      restricted_data = case_match(
        restricted_data,
        "Yes - all" ~ "Yes_all",
        "Yes - some" ~ "Yes_some",
        "Unclear/NA" ~ "No",
        .default = restricted_data
      ),
      restricted_data_category = case_match(
        restricted_data_category,
        "Other/Unclear" ~ "Other",
        .default = restricted_data_category
      ),
      restricted_data_instructions = case_match(
        restricted_data_instructions,
        "Somewhat/Unsure" ~ "Somewhat",
        .default = restricted_data_instructions
      ),
      pr_code_location = case_match(
        pr_code_location,
        "Publisher/journal website" ~ "Publisher website",
        .default = pr_code_location
      ),
      pr_code_complete = case_match(
        pr_code_complete,
        "Partial/Incomplete" ~ "Incomplete",
        .default = pr_code_complete
      )
    ) %>%
    select(-c(paper_data_link_functioning,
              pr_code_responses_same_as_data,
              is_vor))
  
}