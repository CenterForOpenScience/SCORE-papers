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
      ),
      public_source_data_and_code = "No",
      process_reproducible = case_when(
        pr_data_available == "Yes" & pr_code_available == "Yes" ~ "Yes",
        public_source_data_and_code == "Yes" ~ "Yes",
        .default = "No"
      ),
      OA_data_shared = case_when(
        OA_data_shared == "available" ~ "available_online",
        pr_data_available == "Yes" ~ "available_online",
        is.na(OA_data_shared) ~ "no",
        .default = OA_data_shared
      ),
      OA_code_shared = case_when(
        OA_code_shared == "available" ~ "available_online",
        pr_code_available == "Yes" ~ "available_online",
        is.na(OA_code_shared) ~ "no",
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
           restricted_data_description,
           code_available = pr_code_available,
           restricted_data_category,
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