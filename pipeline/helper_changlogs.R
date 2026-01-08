# Change Log Helper Functions ----

# Functions for loading and applying change logs.

#' Load Change Log
#' 
#' Reads change logs in from Google Sheets and performs common transformations. Used in active production, but not the archived workflow.
#'
#' @param changelog_file A spreadsheet ID for the change log
#' @param changelog_moddate The date that the file was last modified (typically obtained through the get_google_mod_date() function).
#'
#' @returns A cleaned-up version of the change log, ready to use when updating raw datasets.
load_changelog <- function(changelog_file, changelog_moddate) {
  
  googlesheets4::read_google_sheet(changelog_file, changelog_moddate) |>
    dplyr::mutate(
      change_to = as.character(change_to),
      change_from = as.character(change_from)
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(change_to, change_from), 
        ~dplyr::na_if(., "na")
      ),
      dplyr::across(
        c(change_to, change_from, rationale),
        ~dplyr::na_if(., "NA")
      ),
      dplyr::across(
        c(change_to, change_from),
        ~dplyr::na_if(., "NC")
      )
    )
  
}

#' Apply Changes
#' 
#' Updates datasets with changes from the change log. In cases where a field was changed multiple times, only the most recent version is applied. 
#'
#' @param dat An original dataset.
#' @param changes A change log dataset.
#' @param id A column name for the key ID field for the dataset being updated.
#'
#' @returns A dataset updated with changes.
apply_changelog <- function(dat, changes, id) {
  
  # Keep list of columns that need to be converted from strings
  nums <- names(dat)[purrr::map_lgl(dat, is.double)]
  
  logs <- names(dat)[purrr::map_lgl(dat, is.logical)]
  
  # Create a list of changes that need to be made for each row
  changelog <- changes |>
    dplyr::select(
      -c(change_from,
         date_implemented,
         reported_by,
         rationale)
    ) |>
    # We only want to work with the highest version for each column changed
    dplyr::arrange(
      get({{ id }}),
      col_name,
      dplyr::desc(dplyr::across(dplyr::ends_with("version")))
    ) |>
    dplyr::distinct(
      dplyr::pick({{ id }}),
      col_name,
      .keep_all = TRUE
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::ends_with("version"), max),
      .by = {{ id }}
    ) |>
    # Changes for each report ID get condensed into subtables to update the
    # respective rows in the main dataset
    tidyr::nest(
      data = c({{ id }},
               dplyr::ends_with("version"),
               col_name,
               change_to),
      .by = {{ id }}
    ) |>
    dplyr::mutate(
      data = purrr::map(
        data, 
        pivot_wider, 
        names_from = col_name,
        values_from = change_to
      ),
      # For each subtable of data to be updated, make sure columns that are
      # supposed to be numeric in the final data are numeric here
      data = purrr::map(
        data,
        \(x) dplyr::mutate(x, dplyr::across(dplyr::any_of(nums), as.double))
      ),
      # Same for any boolean
      data = purrr::map(
        data,
        \(x) dplyr::mutate(x, dplyr::across(dplyr::any_of(logs), as.logical)))
    ) |>
    dplyr::select(-c({{ id }})) |>
    purrr::flatten()
  
  # Apply all updates to the dataset
  for (i in seq_along(changelog)) {
    
    dat <- dat |>
      dplyr::rows_update(changelog[[i]], by = id)
    
  }
  
  return(dat)
  
}
