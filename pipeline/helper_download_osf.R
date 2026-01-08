# OSF Helper Functions ----

# Functions for working with files stored on the Open Science Framework

#' Get OSF modification date
#' 
#' Retrieves the last date that an OSF file was modified.
#'
#' @param osf_id The GUID for a file on the OSF.
#'
#' @returns A timestamp.
get_osf_mod_date <- function(osf_id) {
  
  osf_id |>
    osfr::osf_retrieve_file() |>
    dplyr::pull(meta) |>
    purrr::list_flatten() |>
    purrr::pluck("attributes") |>
    purrr::pluck("date_modified")
  
}

#' Read OSF CSV
#' 
#' Downloads and reads a CSV from OSF, then deletes the temp downloaded file.
#'
#' @param osf_id The GUID for a file on the OSF.
#' @param osf_moddate The modification date for a file on the OSF (see get_osf_mod_date()).
#' @param quote Single character used to quote strings.
#' @param col_types One of NULL, a cols() specification, or a string. See vignette("readr") for more details.
#'
#' @returns A tibble.
load_osf_csv <- function(
    osf_id,
    osf_moddate,
    quote = "\"",
    col_types = NULL
) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(osf_moddate))){
    stop("File modification date invalid.")
  }
  
  osf_csv_file <- osf_id |>
    osfr::osf_retrieve_file() |>
    osfr::osf_download(
      path = here::here("pipeline",
                        "data_processing",
                        "temp"),
      conflicts = "overwrite"
    ) |>
    dplyr::pull(local_path)
  
  osf_csv_data <- readr::read_csv(
    osf_csv_file,
    col_types = col_types,
    quote = quote,
    show_col_types = FALSE
  )
  
  file.remove(osf_csv_file)
  
  return(osf_csv_data)
}

#' Download OSF TSV
#' 
#' Downloads and reads a TSV from OSF, then deletes the temp downloaded file.
#'
#' @param osf_id The GUID for a file on the OSF.
#' @param osf_moddate The modification date for a file on the OSF (see get_osf_mod_date()).
#' @param quote Single character used to quote strings.
#'
#' @returns A tibble
load_osf_tsv <- function(osf_id,
                         osf_moddate,
                         quote = "\"") {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(osf_moddate))){
    stop("File modification date invalid.")
  }
  
  osf_tsv_file <- osf_id |>
    osfr::osf_retrieve_file() |>
    osfr::osf_download(
      path = here("pipeline",
                  "data_processing",
                  "temp"),
      onflicts = "overwrite"
    ) |>
    dplyr::pull(local_path)
  
  osf_tsv_data <- readr::read_tsv(
    osf_tsv_file,
    show_col_types = FALSE,
    quote = quote
  )
  
  file.remove(osf_tsv_file)
  
  return(osf_tsv_data)
}