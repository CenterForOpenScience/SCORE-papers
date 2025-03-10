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