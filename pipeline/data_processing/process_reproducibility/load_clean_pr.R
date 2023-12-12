# Load and transform PR data

# Load PR data
load_pr_data <- function(pr_gsheet){
  
  read_sheet(pr_gsheet,
             sheet = "Form Responses 1") %>%
    filter(!is.na(Timestamp)) %>%
    add_count(pdf_filename) %>%
    filter(is_vor | n == 1) %>%
    select(-c(n))
  
}

# Clean/transform PR data
process_pr_data <- function(pr_data_raw,
                            covid_ta2,
                            tagtable_covid_p1) {
  
  # COVID papers use a different ID in their pdf file name than other papers
  # Use this key to insert the correct paper IDs 
  id_key <- covid_ta2 %>%
    select(ta2_pid,
           DOI = DOI_CR) %>%
    left_join(tagtable_covid_p1 %>% 
                select(paper_id, DOI),
              by = "DOI") %>%
    select(-c(DOI))
  
  pr_data_raw %>%
    select(-c(`Email Address`,
              auditor_name,
              paper_data_availability_statement,
              paper_data_availability_statement_content,
              pr_data_available_and_existing,
              restricted_data_,
              pr_data_available...35)) %>% 
    rename(pr_data_available = pr_data_available...11) %>%
    mutate(
      pdf_paper_id = str_extract(pdf_filename,
                                 "[:alnum:]*$"),
      covid = case_when(str_detect(pdf_filename, "covid") ~ TRUE,
                        .default = FALSE),
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
      )
    ) %>%
    left_join(id_key, join_by(pdf_paper_id == ta2_pid)) %>%
    mutate(paper_id = coalesce(paper_id, pdf_paper_id)) %>%
    select(-c(pdf_paper_id))
  
}