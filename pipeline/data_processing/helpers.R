##################
# HELPER FUNCTIONS
# Subfunctions that are reused in multiple functions
##################

# QC Check Helpers ----
# Create list of valid IDs
# Generates a list which holds valid paper IDs, claim IDs, and report IDs
create_id_list <- function(status,
                           tagtable_covid_p1,
                           finalized_claim4_table,
                           rr_projects) {
  
  valid_papers <- rbind(select(status, paper_id),
                        select(tagtable_covid_p1, paper_id))
  
  bushel_claims <- finalized_claim4_table %>%
    select(paper_id,
           claim_id,
           p1_claim)
  
  single_trace_claims <- status %>%
    filter(p1_delivery | p2_delivery) %>%
    mutate(claim_id = "single-trace",
           p1_claim = TRUE) %>%
    select(paper_id,
           claim_id,
           p1_claim)
  
  covid <- tagtable_covid_p1 %>%
    filter(external) %>%
    mutate(claim_id = "single-trace",
           p1_claim = TRUE) %>%
    select(paper_id,
           claim_id,
           p1_claim)
  
  valid_claims <- rbind(bushel_claims, single_trace_claims, covid)

  
  # Generates a list of valid rr_ids
  valid_rr <- rr_projects %>%
    select(paper_id,
           rr_id,
           project_guid) %>%
    distinct(paper_id, rr_id, .keep_all = TRUE)
  
  list(valid_papers,
       valid_claims,
       valid_rr)

}

# Modification Date Checks ----
# These functions will trigger targets to pull a new version of a remotely 
# hosted file when it gets modified.

# Get OSF modification date
# Gets the last date that an OSF file was updated
get_osf_mod_date <- function(osf_file) {
  
  osf_file %>%
    osf_retrieve_file() %>%
    pull(meta) %>%
    flatten() %>%
    pluck("attributes") %>%
    pluck("date_modified")

}

# Get Google Drive modification date
# Gets the last date that an item on Google Drive was updated
# Note: for some reason this doesn't update when new form responses add rows
get_google_mod_date <- function(file_id) {
  
  drive_get(id = file_id) %>%
    pull(drive_resource) %>%
    flatten() %>%
    pluck("modifiedTime")
  
}

# Write File Helpers ----
# Generic functions to write files and return file names as expect by targets

# # Write Google sheet
# write_target_gsheet <- function(data, file_id){
#   
#   write_sheet(data,
#               ss = file_id,
#               sheet = 1)
#   
#   file_id %>%
#     googledrive::as_id() %>%
#     drive_link()
#   
# }
# 
# # Write TSV for targets
# write_target_tsv <- function(dataset, file_path) {
#   
#   write_tsv(x = dataset,
#             file = file_path,
#             quote = "needed")
#   
#   return(file_path)
#   
# }

# Read File Helper ----
# Read CSV files from Google Drive
read_google_csv <- function(file_id, mod_date) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop(simpleError("File modification date invalid."))
  }
  
  file_id %>%
    googledrive::as_id() %>%
    drive_read_string() %>%
    read_csv(show_col_types = FALSE,
             na = c("", "na"))
  
}

read_google_tsv <- function(file_id, mod_date) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop(simpleError("File modification date invalid."))
  }
  
  file_id %>%
    googledrive::as_id() %>%
    drive_read_string() %>%
    read_tsv(show_col_types = FALSE,
             na = c("NA", "NC", "N/A", "na", "n/a", ""))
  
}

read_google_sheet <- function(file_id, mod_date) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop(simpleError("File modification date invalid."))
  }
  
  file_id %>%
    read_sheet()
  
}

# Downloads CSV from OSF if it has been updated, stores the output, and 
# deletes the temp downloaded file
load_osf_csv <- function(osf_id,
                         osf_mod_date) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(osf_mod_date))){
    stop(simpleError("File modification date invalid."))
  }
  
  osf_csv_file <- osf_id %>%
    osf_retrieve_file() %>%
    osf_download(path = here("pipeline",
                             "data_processing",
                             "temp"),
                 conflicts = "overwrite") %>%
    pull(local_path)
  
  osf_csv_data <- read_csv(osf_csv_file,
                           show_col_types = FALSE)
  
  file.remove(osf_csv_file)
  
  return(osf_csv_data)
}

load_changelog <- function(orig_input_changelog_file,
                           orig_input_changelog_moddate) {
  
  read_google_sheet(orig_input_changelog_file,
                    orig_input_changelog_moddate) %>%
    mutate(change_to = as.character(change_to),
           change_from = as.character(change_from)) %>%
    mutate(across(c(change_to, change_from), ~na_if(., "na")),
           across(c(change_to, change_from, rationale), ~na_if(., "NA")),
           across(c(change_to, change_from), ~na_if(., "NC")))
  
}

# Customized rounding function that pushes 5 up rather than down
round_off <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z <- posneg * (floor(z + 0.5) / 10 ^ digits)
  return(z)
}
