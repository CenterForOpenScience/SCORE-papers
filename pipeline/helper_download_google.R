# Google Drive Helper Functions ----

# Helper functions for reading and downloading files from Google Drive. 
# These functions were used when the project was actively ongoing, but are 
# largely not present in the code of the archived project.

#' Get Google Drive modification date
#'
#' Gets the timestamp of the last modification date for a file on Google Drive. Note: Occasionally the modification date does not properly update when a new form response adds rows to a Google Sheet. This was compensated for during production by requiring targets to re-read the file every time it ran (via cue = tar_cue("always")) rather than just when the modification date was updated.
#'
#' @param file_id An identifier for a file on Google Drive.
#'
#' @returns The date that the file was last modified.
get_google_mod_date <- function(file_id) {
  
  googledrive::drive_get(id = file_id) |>
    dplyr::pull(drive_resource) |>
    purrr::list_flatten() |>
    purrr::pluck("modifiedTime")
  
}

#' Read Google CSV
#' 
#' Reads in CSV files from Google Drive. 
#'
#' @param file_id A file ID for a CSV stored on Google Drive.
#' @param mod_date The date that the file was last modified (see get_google_mod_date()).
#' @param na Character vector of strings to interpret as missing values.
#' @param col_types One of NULL, a cols() specification, or a string. See vignette("readr") for more details.
#'
#' @returns A tibble.
read_google_csv <- function(
    file_id, 
    mod_date, 
    na = c("", "na"),
    col_types = NULL
) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop("File modification date invalid.")
  }
  
  file_id |>
    googledrive::as_id() |>
    googledrive::drive_read_string(encoding = "UTF-8") |>
    readr::read_csv(
      show_col_types = FALSE,
      na = na,
      col_types = col_types
    )
  
}

#' Read Google TSV
#' 
#' Reads in TSV files from Google Drive.
#'
#' @param file_id A file ID for a TSV stored on Google Drive.
#' @param mod_date The date that the file was last modified (see get_google_mod_date()).
#' @param na Character vector of strings to interpret as missing values.
#' @param col_types One of NULL, a cols() specification, or a string. See vignette("readr") for more details.
#'
#' @returns A tibble.
read_google_tsv <- function(
    file_id, 
    mod_date,
    na = c("NA", "NC", "N/A", "na", "n/a", ""),
    col_types = NULL
) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop("File modification date invalid.")
  }
  
  file_id |>
    googledrive::as_id() |>
    googledrive::drive_read_string(encoding = "UTF-8") |>
    readr::read_tsv(
      show_col_types = FALSE,
      na = na,
      col_types = col_types
    )
  
}

#' Read Google Sheet
#' 
#' Reads in a Google Sheet file.
#'
#' @param file_id A file ID for a Google Sheet
#' @param mod_date The date that the file was last modified (see get_google_mod_date()).
#' @param sheet Sheet to read, in the sense of "worksheet" or "tab". 
#' @param skip Minimum number of rows to skip before reading anything.
#' @param col_names TRUE to use the first row as column names, FALSE to get default names, or a character vector to provide column names directly. 
#' @param drop_cols Columns to drop when reading from the original sheet.
#'
#' @returns A tibble.
read_google_sheet <- function(
    file_id, 
    mod_date,
    sheet = NULL,
    skip = 0,
    col_names = TRUE,
    drop_cols = NULL
) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!is.Date(as.Date(mod_date))) {
    stop("File modification date invalid.")
  }
  
  file_id |>
    googlesheets4::read_sheet(
      sheet = sheet,
      col_names = col_names,
      skip = skip
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.list), as.character)) |>
    dplyr::select(-dplyr::any_of(drop_cols))
  
}

#' Read Google RDS
#' 
#' Reads in an RDS file stored on Google Drive. Note: The file will be read into the pipeline/data_processing/temp folder, then subsequently deleted from this folder after being read into R
#'
#' @param file_id A file ID for an RDS file on Google Drive.
#' @param mod_date The date that the file was last modified (see get_google_mod_date()).
#'
#' @returns A tibble.
read_google_rds <- function(file_id, mod_date) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if (!lubridate::is.Date(as.Date(mod_date))) {
    stop("File modification date invalid.")
  }
  
  google_rds_file <- file_id |>
    googledrive::as_id() |>
    googledrive::drive_download(
      path = here::here("pipeline",
                        "data_processing",
                        "temp",
                        "temp_rds.rds"),
      overwrite = TRUE
    ) |>
    dplyr::pull(local_path)
  
  google_rds <- readr::read_rds(google_rds_file)
  
  file.remove(google_rds_file)
  
  return(google_rds)
  
}
