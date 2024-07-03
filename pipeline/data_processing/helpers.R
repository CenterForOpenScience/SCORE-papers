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
                           rr_projects,
                           repli_cases_projects) {

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
    rbind(repli_cases_projects) %>%
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

# Read File Helper ----
# Read CSV files from Google Drive
read_google_csv <- function(file_id, 
                            mod_date, 
                            na = c("", "na"),
                            col_types = NULL) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop("File modification date invalid.")
  }
  
  file_id %>%
    googledrive::as_id() %>%
    drive_read_string(encoding = "UTF-8") %>%
    read_csv(show_col_types = FALSE,
             na = na,
             col_types = col_types)
  
}

read_google_tsv <- function(file_id, 
                            mod_date,
                            na = c("NA", "NC", "N/A", "na", "n/a", ""),
                            col_types = NULL) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop("File modification date invalid.")
  }
  
  file_id %>%
    googledrive::as_id() %>%
    drive_read_string(encoding = "UTF-8") %>%
    read_tsv(show_col_types = FALSE,
             na = na,
             col_types = col_types)
  
}

read_google_sheet <- function(file_id, 
                              mod_date,
                              sheet = NULL) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop("File modification date invalid.")
  }
  
  file_id %>%
    read_sheet(sheet = sheet) %>%
    mutate(across(where(is.list), as.character))
  
}

# Downloads CSV from OSF if it has been updated, stores the output, and 
# deletes the temp downloaded file
load_osf_csv <- function(osf_id,
                         osf_mod_date) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(osf_mod_date))){
    stop("File modification date invalid.")
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

# Downloads TSV from OSF if it has been updated, stores the output, and 
# deletes the temp downloaded file
load_osf_tsv <- function(osf_id,
                         osf_mod_date,
                         quote = "\"") {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(osf_mod_date))){
    stop("File modification date invalid.")
  }
  
  osf_tsv_file <- osf_id %>%
    osf_retrieve_file() %>%
    osf_download(path = here("pipeline",
                             "data_processing",
                             "temp"),
                 conflicts = "overwrite") %>%
    pull(local_path)
  
  osf_tsv_data <- read_tsv(osf_tsv_file,
                           show_col_types = FALSE,
                           quote = quote)
  
  file.remove(osf_tsv_file)
  
  return(osf_tsv_data)
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

# dat is the dataset to be changed, changes is the changelog dataset, 
# id is the id to use
apply_changelog <- function(dat, changes, id) {
  
  nums <- names(dat)[map_lgl(dat, is.double)]
  
  logs <- names(dat)[map_lgl(dat, is.logical)]
  
  # Create a list of changes that need to be made for each row
  changelog <- changes %>%
    select(-c(change_from,
              date_implemented,
              reported_by,
              rationale)) %>%
    # We only want to work with the highest version for each column changed
    arrange(get({{ id }}),
            col_name,
            desc(across(ends_with("version")))) %>%
    distinct(pick({{ id }}),
             col_name,
             .keep_all = TRUE) %>%
    mutate(across(ends_with("version"), max),
           .by = {{ id }}) %>%
    # Changes for each report ID get condensed into subtables to update the
    # respective rows in the main dataset
    nest(data = c({{ id }},
                  ends_with("version"),
                  col_name,
                  change_to),
         .by = {{ id }}
    ) %>%
    mutate(
      data = map(data,
                 pivot_wider,
                 names_from = col_name,
                 values_from = change_to),
      # For each subtable of data to be updated, make sure columns that are
      # supposed to be numeric in the final data are numeric here
      data = map(data,
                 \(x) mutate(x, across(any_of(nums), as.double))),
      # Same for any boolean
      data = map(data,
                 \(x) mutate(x, across(any_of(logs), as.logical)))
    ) %>%
    select(-c({{ id }})) %>%
    flatten()
  
  # Apply all updates to the dataset
  for (i in seq_along(changelog)) {
    
    dat <- dat %>%
      rows_update(changelog[[i]], by = id)
    
  }
  
  return(dat)
  
}
