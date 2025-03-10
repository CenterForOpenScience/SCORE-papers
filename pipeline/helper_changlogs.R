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
