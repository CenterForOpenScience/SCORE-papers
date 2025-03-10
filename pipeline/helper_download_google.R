# Get Google Drive modification date
# Gets the last date that an item on Google Drive was updated
# Note: for some reason this doesn't update when new form responses add rows
get_google_mod_date <- function(file_id) {
  
  drive_get(id = file_id) %>%
    pull(drive_resource) %>%
    flatten() %>%
    pluck("modifiedTime")
  
}

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
                              sheet = NULL,
                              drop_cols = NULL) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop("File modification date invalid.")
  }
  
  file_id %>%
    read_sheet(sheet = sheet) %>%
    mutate(across(where(is.list), as.character)) %>%
    select(-any_of(drop_cols))
  
}
