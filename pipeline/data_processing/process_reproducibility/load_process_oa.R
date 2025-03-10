load_process_oa <- function(oa_sheet) {
  
  # OA data is found across two sheets
  non_repro <- read_sheet(
    oa_sheet,
    sheet = "OA_coding_nonrepro"
  ) %>%
    slice(-(1:2)) %>%
    mutate(paper_id = str_extract(pdf_filename,
                                  "[:alnum:]*$"))
  
  repro <- read_sheet(
    oa_sheet,
    sheet = "OA_coding_repro"
  ) %>%
    slice(-(1:2)) %>%
    mutate(paper_id = as.character(paper_id))
  
  # Merge the two sheets to create a single dataset
  add_row(non_repro, repro) %>%
    # Resolve and remove duplicates
    group_by(paper_id) %>%
    fill(OA_notes_internal, .direction = "up") %>%
    distinct() %>%
    ungroup()
  
}