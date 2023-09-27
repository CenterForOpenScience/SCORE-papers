options(tidyverse.quiet = TRUE)

# Gmail authorization for accessing sheets
# Set google_oauth_email="youremail@cos.io" in your .Renviron file to use
# this document without changing the code.
options(gargle_oauth_email = Sys.getenv("google_oauth_email"))
googledrive::drive_auth()
googlesheets4::gs4_auth()

tar_option_set(
  # Add any packages your target needs to run to this list
  packages = c("tibble",
               "readr",
               "tidyr",
               "dplyr",
               "stringr",
               "purrr",
               "lubridate",
               "here",
               "googlesheets4",
               "googledrive",
               "osfr") 
)

tar_source(files = "pipeline")

