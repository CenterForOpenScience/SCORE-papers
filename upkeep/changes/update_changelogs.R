# Update changelog
# Takes a new entry to the changelog as input, adds date, version number, and
# previous value, reads current changelog, adds new line to reflect new entry,
# and writes updated changelog. Works for original variables and replication 
# outcomes

update_changelog <- function(change_sheet,
                             unique_id_field,
                             source_data_name,
                             version_field) {
  
  function(outcome_id,
           changes_dict,
           reported_by,
           rationale) {
    
    source_data <- do.call(tar_read, list(source_data_name))
    
    if (!(outcome_id %in% pull(source_data, unique_id_field))) {
      stop("Outcome ID does not exist in source data.")
    }
    
    if (any(!(names(changes_dict) %in% names(source_data)))) {
      stop("Invalid column names present in changes.")
    }
    
    changelog <- read_sheet(change_sheet)
    
    # If a change has been made in a particular row/column before, we want to 
    # use that value as the "change from". Otherwise, use what is in the 
    # original data
    get_change_from <- function(idx) {
      if (any(pull(changelog, unique_id_field) == outcome_id &
              pull(changelog, col_name) == idx)) {
        changelog %>%
          filter(get({{ unique_id_field }}) == outcome_id) %>%
          group_by(col_name, {{ version_field }}) %>%
          filter(col_name == idx,
                 {{ version_field }} == max({{ version_field }})) %>%
          pull(change_to) %>%
          as.character()
      } else {
        source_data %>%
          filter(get({{ unique_id_field }}) == outcome_id) %>%
          pull(idx) %>%
          as.character()
      }
    }
    
    make_row <- function(change_row, idx) {

      tibble(reported_by = reported_by,
             date_implemented = as.character(Sys.Date()),
             {{ unique_id_field }} := outcome_id,
             col_name = idx,
             change_to = change_row,
             change_from = get_change_from(idx),
             {{ version_field }} := this_version,
             rationale = rationale)

    }
    
    this_version <- changelog %>%
      filter(get({{ unique_id_field }}) == outcome_id) %>%
      pull(version_field) %>%
      {
        if (is_empty(.)) 2 else max(.) + 1
      }
    
    new_changes <- imap(changes_dict, ~ make_row(.x, .y)) %>%
      reduce(bind_rows) %>%
      mutate(date_implemented = as_date(date_implemented))
    
    sheet_append(change_sheet,
                 new_changes)
    
  }
}

update_repli_changes <- update_changelog(
  "1jbkVYHLinIN9zXBuPTok26bHjDt69-7D_Z7yVOin3mM",
  "unique_report_id",
  "repli_merged",
  "rr_stat_version"
)

update_orig_changes <- update_changelog(
  "1M8H_76ajxwdhVuuSyIo18BxRrJbMF3hXpAkzYY39g2k",
  "unique_claim_id",
  "orig_dataset",
  "orig_stat_version"
)

update_repro_changes <- update_changelog(
  "1c2i_k-RMzTWAC7RD6cqhheToihtS8_C5r5powvRuMDE",
  "unique_report_id",
  "repro_merged",
  "rr_stat_version"
)

update_rr_minted_changes <- update_changelog(
  "1rlVy9A4jBwQzpxTH4my4FUWaZkq7388YfpRmQOZWihw",
  "rr_id",
  "rr_attempts_minted",
  "minted_version"
)

update_pr_changes <- update_changelog(
  "1zek9Lu-s303ZXs3B84h2vy-sJ1avOZSgpZEsna40pdA",
  "paper_id",
  "pr_data_form",
  "pr_stat_version"
)

update_oa_changes <- update_changelog(
  "1mrH4mPCr6VUrOD8S08hhzD3jdcRjOnMPAF9XfZ0_vH4",
  "paper_id",
  "oa_raw",
  "oa_stat_version"
)

update_repo_supplement_changes <- update_changelog(
  "1OzOpJCJCy5OPYISk1UR5tI0ehNaou-ykAeD7GPZ99ug",
  "unique_report_id",
  "repro_supplement_raw",
  "supplement_stat_version"
)


## Old ----
# update_changelog <- function(change_sheet,
#                              source_data,
#                              outcome_id,
#                              changes_dict,
#                              reported_by,
#                              rationale) {
#   
#   changelog <- change_sheet %>%
#     read_sheet()
#   
#   unique_id <- if_else(
#     change_sheet == "1zek9Lu-s303ZXs3B84h2vy-sJ1avOZSgpZEsna40pdA",
#     "paper_id",
#     names(select(changelog, starts_with("unique")))
#   )
#   
#   version_col <- names(select(changelog, ends_with("_stat_version")))
#   
#   new_changes <- tibble(reported_by = as.character(),
#                         date_implemented = as.character(),
#                         {{ unique_id }} := as.character(),
#                         col_name = as.character(),
#                         change_to = as.character(),
#                         change_from = as.character(),
#                         {{ version_col }} := as.numeric(),
#                         rationale = as.character())
#   
#   if (outcome_id %in% changelog[[unique_id]]) {
#     
#     this_outcome <- changelog %>%
#       filter(get({{ unique_id }}) == outcome_id)
#     
#     this_version <- max(this_outcome[[version_col]]) + 1
#     
#     for (i in 1:length(changes_dict)) {
#       
#       col <- names(changes_dict[i])
#       
#       change_to <- changes_dict[[i]]
#       
#       if (col %in% this_outcome$col_name) {
#         
#         change_from <- this_outcome %>%
#           group_by(col_name, {{ version_col }}) %>%
#           filter(col_name == col,
#                  {{ version_col }} == max({{ version_col }})) %>%
#           pull(change_to) %>%
#           as.character()
#         
#       } else {
#         
#         change_from <- source_data %>%
#           filter(get({{ unique_id }}) == outcome_id) %>%
#           pull(col) %>%
#           as.character()
#         
#       }
#       
#       new_changes <- new_changes %>%
#         add_row(reported_by = reported_by,
#                 date_implemented = as.character(Sys.Date()),
#                 {{ unique_id }} := outcome_id,
#                 col_name = col,
#                 change_to = change_to,
#                 change_from = change_from,
#                 {{ version_col }} := this_version,
#                 rationale = rationale)
#     }
#     
#   } else {
#     
#     for (i in 1:length(changes_dict)) {
#       
#       col <- names(changes_dict[i])
#       
#       change_to <- changes_dict[[i]]
#       
#       # Get original value to change from
#       change_from <- source_data %>%
#         filter(get({{ unique_id }}) == outcome_id) %>%
#         pull(col) %>%
#         as.character()
#       
#       new_changes <- new_changes %>%
#         add_row(reported_by = reported_by,
#                 date_implemented = as.character(Sys.Date()),
#                 {{ unique_id }} := outcome_id,
#                 col_name = col,
#                 change_to = change_to,
#                 change_from = change_from,
#                 {{ version_col }} := 2,
#                 rationale = rationale)
#     }
#   }
#   
#   new_changes <- new_changes %>%
#     mutate(date_implemented = as_date(date_implemented))
#   
#   sheet_append(change_sheet,
#                new_changes)
#   
# }
