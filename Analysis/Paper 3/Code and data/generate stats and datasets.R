
# Initial setup and libraries
{
  library(googledrive)
  library(targets)
  library(googlesheets4)
  library(here)
  library(stringr)
  
  drive_auth(Sys.getenv("google_oauth_email"))
  setwd(here())
  
  paper_folder <- "Paper 3"
  #template_doc_name <- ""
  template_drive_ID <- "1DPRhiyjQac_HG-aZcRa9cpYkL2GQCodP"
  target_drive_ID <- "1W0tVAXFgTbdjeB5wN2yBgp85XCBCHfpG"
  target_drive_name <- "knitted manuscript.docx"
}

# Load full sets of datasets from the targets framework and save into analyst folder
{
  objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes",
                       "paper_metadata","status","stitched_claims",
                       "all_rr_attempts","publications","extracted_claims",
                       "repro_export")
  for(i in 1:length(objects_to_load)){
    assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
  }
  
  save(list=objects_to_load,file="Analysis/Paper 3/Code and data/Analyst package/analyst data.RData")
}

# Assemble all code and datasets into the analyst folder and create a zipped copy
{
  
  folder.analyst.package <- paste0("Analysis/",paper_folder,"/Code and data/Analyst package/")
  
  # Save all data and files into 
  # Common functions
  save(list=objects_to_load,file=paste0("Analysis/",paper_folder,"/Code and data/Analyst package/analyst data.RData"))
  file.copy(from=paste0("Analysis/common functions.R"),to=paste0("Analysis/",paper_folder,"/Code and data/Analyst package/common functions.R"),overwrite=TRUE)
  
  # Analysis code
  folder.analyst.package <- paste0("Analysis/",paper_folder,"/Code and data/Analyst package/")
  file.copy(from=paste0("Analysis/",paper_folder,"/Code and data/tagged stats and figures.R"),to=paste0(folder.analyst.package,"tagged stats and figures.R"),overwrite=TRUE)
  
  # Template document
  drive_download(file=as_id(template_drive_ID), path = paste0("Analysis/",paper_folder,"/Code and data/Analyst package/template manuscript.docx"),overwrite = TRUE)
  
  # zipit <- function(){
  #   setwd(here())
  setwd(paste0(here(),"/",folder.analyst.package))
  
  files2zip <- dir(full.names = TRUE)
  files2zip<- files2zip[!endsWith(files2zip,".zip") &
                          files2zip!=target_drive_name]

  zip(zipfile = "data and code.zip", files = files2zip)
  setwd(here())
  # }
  # zipit()
}

# Run the analysis / knit code

{
  # Change directories to run full analysis/knit code locally in analyst folder
  setwd("Analysis/Paper 3/Code and data/Analyst package/")
  source("tagged stats and figures.R")
  
  # Run locally in folder
  knit_manuscript(
    template_docx_file="template manuscript.docx",
    knitted_docx_file = "knitted manuscript.docx")

}

