create_pr_analytic <- function(pr_qc,
                               oa_outreach) {
  
  oa_fields <- oa_outreach %>%
    select(OA_data_shared,
           OA_code_shared,
           OA_source_data_shared,
           paper_id)
  
  pr_qc %>%
    left_join(oa_fields, by = "paper_id") %>%
    mutate(
      # Copy over from load_clean_pr to catch changes post update
      pr_code_location = case_match(
        pr_code_responses_same_as_data,
        "Yes - same responses" ~ pr_data_location,
        .default = pr_code_location
      ),
      pr_code_location_description = case_match(
        pr_code_responses_same_as_data,
        "Yes - same responses" ~ pr_data_location_description,
        .default = pr_code_location_description
      ),
      pr_code_date = case_match(
        pr_code_responses_same_as_data,
        "Yes - same responses" ~ pr_data_date,
        .default = pr_code_date
      ),
      pr_code_notes = case_match(
        pr_code_responses_same_as_data,
        "Yes - same responses" ~ pr_data_notes,
        .default = pr_code_notes
      ),
      paper_data_instructions = case_when(
        is.na(paper_data_instructions) ~ "No",
        .default = paper_data_instructions
      ),
      paper_materials_available_on_request = case_when(
        is.na(paper_materials_available_on_request) ~ "No",
        .default = paper_materials_available_on_request
      ),
      restricted_data = case_when(
        is.na(restricted_data) ~ "Unclear/NA",
        .default = restricted_data
      ),
      pr_data_available = case_match(
        pr_data_available,
        "Yes - data are available" ~ "Yes",
        "No - data were not found/are not available" ~ "No",
        .default = pr_data_available
      ),
      pr_code_available = case_match(
        pr_code_available,
        "Yes - code is publicly available" ~ "Yes",
        "Yes - code is available" ~ "Yes",
        "No - code was not found/is not available" ~ "No",
        .default = pr_code_available
      ),
      # End copy
      pr_code_complete = case_match(
        pr_code_responses_same_as_data,
        "Yes - same responses" ~ pr_data_complete,
        .default = pr_code_complete
      ),
      pr_data_location = str_remove(pr_data_location, "/journal"),
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
      pr_code_location = str_remove(pr_code_location, "/journal"),
      pr_code_complete = case_match(
        pr_code_complete,
        "Partial/Incomplete" ~ "Incomplete",
        .default = pr_code_complete
      ),
      process_reproducible = case_when(
        pr_data_available == "Yes" & pr_code_available == "Yes" ~ "Yes",
        public_source_data_and_code == "Yes" ~ "Yes",
        .default = "No"
      ),
      OA_data_shared = case_when(
        OA_data_shared == "available" ~ "available_online",
        pr_data_available == "Yes" ~ "available_online",
        is.na(OA_data_shared) ~ "no",
        OA_data_shared == "yes_private" ~ "shared_on_request",
        OA_data_shared == "yes_public" ~ "shared_on_request",
        .default = OA_data_shared
      ),
      OA_code_shared = case_when(
        OA_code_shared == "available" ~ "available_online",
        pr_code_available == "Yes" ~ "available_online",
        is.na(OA_code_shared) ~ "no",
        OA_code_shared == "yes_private" ~ "shared_on_request",
        OA_code_shared == "yes_public" ~ "shared_on_request",
        .default = OA_code_shared
      )
    ) %>%
    select(pdf_filename,
           paper_id,
           covid,
           paper_data_type = existing_data,
           paper_link = paper_data_link,
           paper_link_content = paper_data_link_content,
           paper_instructions = paper_data_instructions,
           paper_instructions_content = paper_data_instructions_content,
           paper_request = paper_materials_available_on_request,
           paper_request_content = paper_materials_available_on_request_content,
           data_available = pr_data_available,
           data_location = pr_data_location,
           data_location_description = pr_data_location_description,
           data_complete = pr_data_complete,
           data_date = pr_data_date,
           data_notes = pr_data_notes,
           restricted_data,
           restricted_data_instructions,
           restricted_data_category,
           restricted_data_description,
           code_available = pr_code_available,
           code_location = pr_code_location,
           code_location_description = pr_code_location_description,
           code_complete = pr_code_complete,
           code_date = pr_code_date,
           code_notes = pr_code_notes,
           pr_notes = other_notes,
           public_source_data_and_code,
           process_reproducible,
           OA_data_shared,
           OA_code_shared,
           OA_source_data_shared)
  
}