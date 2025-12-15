# Load and transform PR data

# Load PR data
load_pr_data <- function(pr_gsheet){
  
  read_sheet(pr_gsheet,
             sheet = "Form Responses 1") %>%
    filter(!is.na(Timestamp)) %>%
    add_count(pdf_filename) %>%
    filter(is_vor | n == 1) %>%
    select(-c(n)) %>%
    # Correct variables that load in as lists
    mutate(pr_data_date = as.character(pr_data_date) %>%
             na_if("NULL"),
           pr_data_notes = as.character(pr_data_notes) %>%
             na_if("NULL"),
           pr_code_date = as.character(pr_code_date) %>%
             na_if("NULL"),
           pr_code_notes = as.character(pr_code_notes) %>%
             na_if("NULL"))
  
}

# Clean/transform PR data
process_pr_data <- function(materials_availability,
                            orig_claims_covid_ta2,
                            orig_claims_p1_covid) {
  
  # COVID papers use a different ID in their pdf file name than other papers
  # Use this key to insert the correct paper IDs 
  id_key <- orig_claims_covid_ta2 %>%
    select(ta2_pid,
           DOI = DOI_CR) %>%
    left_join(orig_claims_p1_covid %>% 
                select(paper_id, DOI),
              by = "DOI") %>%
    select(-c(DOI))
  
  materials_availability %>%
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
      # Should be considered a character field for processing
      pr_code_date = as.character(pr_code_date),
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
      public_source_data_and_code = "No",
      # Some dates are full dates rather than just year
      pr_data_date = case_when(nchar(pr_data_date) > 4 ~ str_sub(pr_data_date,
                                                                 1,
                                                                 4),
                               .default = pr_data_date),
      pr_code_date = case_when(nchar(pr_code_date) > 4 ~ str_sub(pr_code_date,
                                                                 1,
                                                                 4),
                               .default = pr_code_date)
    ) %>%
    left_join(id_key, join_by(pdf_paper_id == ta2_pid)) %>%
    mutate(paper_id = coalesce(paper_id, pdf_paper_id)) %>%
    select(-c(pdf_paper_id))
  
}

update_pr <- function(pr_data_form,
                      pr_input_changelog) {
  
  # We only want to work with the highest version for each claim
  changelog <- pr_input_changelog %>%
    arrange(paper_id,
            col_name,
            desc(pr_stat_version)) %>%
    distinct(paper_id,
             col_name,
             .keep_all = TRUE) 
  
  versions <- changelog %>%
    group_by(paper_id) %>%
    summarise(max_version = max(pr_stat_version))
  
  data_entry <- pr_data_form %>%
    add_column(pr_stat_version = 1)
  
  for(i in 1:nrow(changelog)){
    
    col_name <- changelog[i,]$col_name
    
    version <- versions %>%
      filter(paper_id == changelog[i, ]$paper_id) %>%
      pull(max_version)
    
    change_to <- changelog[i, ]$change_to
    
    change_tbl <- tibble(paper_id = changelog[i, ]$paper_id,
                         pr_stat_version = version,
                         {{ col_name }} := change_to)
    
    data_entry <- data_entry %>%
      rows_update(change_tbl, by = "paper_id")
    
  }
  
  return(data_entry)
  
}
