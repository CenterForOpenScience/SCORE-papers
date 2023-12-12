library(googlesheets4)
library(googledrive)
library(targets)
library(tidyverse)
library(here)
source(here("upkeep",
            "external_output",
            "tilburg_exports.R"))
source(here("pipeline",
            "data_processing",
            "helpers.R"))
tar_make()

# Upload rr_statistics_input to Google Sheets ----
# Overwrite 45 columns (A:AS) of COS data, leave Tilburg's internal notes rows 
# alone
tar_load(orig_dataset)

tar_load(repli_export)

input_gsheet <- "1xkbE74CmOJaPdN0Y_Z6upcbPo-GGkiBdKT2PS-VoK9M"

rr_statistics_input <- make_tilburg_rr_input(orig_dataset,
                                             repli_export,
                                             input_gsheet)

# Add extra variables requested by Andrew
rr_stat_reported <- rr_statistics_input %>%
  left_join("1bWi-dOC-VGN253-cbFzb3toj7cDckNS3O9LItdV8wXQ" %>%
              read_sheet(sheet = 1),
            by = "unique_report_id") %>%
  select(rr_statistic_nrow_reported,
         rr_statistic_ncol_reported)

# WARNING: This will overwrite the existing data! Make sure this is what you
# really want because it is inconvenient to roll it back.
range_write(rr_statistics_input,
            ss = "1xkbE74CmOJaPdN0Y_Z6upcbPo-GGkiBdKT2PS-VoK9M",
            sheet = 2,
            range = cell_cols("A:AS"))

range_write(rr_stat_reported,
            ss = "1xkbE74CmOJaPdN0Y_Z6upcbPo-GGkiBdKT2PS-VoK9M",
            sheet = 2,
            range = cell_cols("AZ:BA"))


# Upload orig_statistics_input ----
# NOTE: This is a TEMP fix until a more permanent solution for the power 
# analysis links exists
p2_id_key <- "1E9af5ncbY2gm7CvHjrWVJt4x_F-ZbyS1" %>%
  googledrive::as_id() %>%
  drive_read_string() %>%
  read_csv(show_col_types = FALSE,
           na = c("", "na"))

xmlpdfmerge <- "1iqUSzCcnLkKP0I8GAC2YRBmLJbbrYbyS" %>%
  googledrive::as_id() %>%
  drive_read_string() %>%
  read_csv(show_col_types = FALSE,
           na = c("", "na"))

all_metadata_filled <- "1W-sNolZQegujz82TU2RKe0tfCvofrhm1" %>%
  googledrive::as_id() %>%
  drive_read_string() %>%
  read_tsv(show_col_types = FALSE,
           na = c("NA", "NC", "N/A", "na", "n/a", ""))

tar_load(tagtable_covid_p1)

rr_confrontations_prereg <- "1aU2JWLJAoUafz4i7N-Lsn00e3jjEzpCS" %>%
  googledrive::as_id() %>%
  drive_read_string() %>%
  read_tsv(show_col_types = FALSE,
           na = c("NA", "NC", "N/A", "na", "n/a", ""))

tar_load(repli_primary)

tagtable_p1 <- "1NmhTX53-SWsMpIbgZpm2bWm1SzRq071F" %>%
  googledrive::as_id() %>%
  drive_read_string() %>%
  read_tsv(show_col_types = FALSE,
           na = c("NA", "NC", "N/A", "na", "n/a", ""))

tagtable_p2_CES <- "1Lr2SXHO261GaifluZSZ1kl7OjjbnHrqe" %>%
  googledrive::as_id() %>%
  drive_read_string() %>%
  read_tsv(show_col_types = FALSE,
           na = c("NA", "NC", "N/A", "na", "n/a", ""))

tar_load(finalized_claim4_table)

tar_load(orig_dataset)

input_gsheet <- "1P4RrEUET-jdgbrMyFofEgKlJR1DX7oKxA9azcmcXwFI"

orig_statistics_input <- make_tilburg_orig_input(
    p2_id_key,
    xmlpdfmerge,
    all_metadata_filled,
    tagtable_covid_p1,
    rr_confrontations_prereg,
    repli_primary,
    tagtable_p1,
    tagtable_p2_CES,
    finalized_claim4_table,
    orig_dataset,
    input_gsheet
  )

orig_statistics_input_tilburg <- read_sheet(input_gsheet, sheet = 2)

new_rows <- anti_join(orig_statistics_input, 
                      orig_statistics_input_tilburg,
                      by = "unique_claim_id")

old_rows <- orig_statistics_input %>%
  filter(unique_claim_id %in% orig_statistics_input_tilburg$unique_claim_id) %>%
  mutate(original_poweranalysis_link = orig_statistics_input_tilburg$original_poweranalysis_link)

orig_upload <- rbind(old_rows, new_rows)

# For a few select cases, we want to use the "reported" value as opposed
# to the "reference" value. Void out the "reference" value in these cases
# for process consistency 
report_decision <- "1eKJ6kbM6tZthzbeoghy2XBTIEZueDXB2zqCnZHA8-XE" %>%
  read_sheet()

for (i in 1:nrow(report_decision)) {
  
  ref_col <- report_decision$reported[i] %>%
    str_extract(".*(?=_reported)") %>%
    str_c("_reference")
  
  orig_upload[orig_upload$unique_claim_id == report_decision$unique_claim_id[i],
       ref_col] <- NA

}

# orig_stat_reported <- "" %>%
#   read_sheet() %>%
#   right_join() %>%
#   select(orig_statistic_nrow_reported,
#          orig_statistic_ncol_reported)

range_write(orig_upload,
            ss = "1P4RrEUET-jdgbrMyFofEgKlJR1DX7oKxA9azcmcXwFI",
            sheet = 2,
            range = cell_cols("A:BA"))
