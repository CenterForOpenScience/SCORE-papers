# Settings
{
  folder_main_code <- paste0("Analysis/Paper 3/")
  folder_analyst_package <- paste0(folder_main_code,"Analyst package/")
  
  template_drive_ID_internal <- "18U3ElDrhF5PltX_UP1XIN1e6CK8nkfOzjSDZr2zF6lo"
  
  #knitted_drive_ID_internal <- "1W0tVAXFgTbdjeB5wN2yBgp85XCBCHfpG"
  
  template_docx_file_internal <- "template manuscript.docx"
  knitted_docx_file_internal <- "knitted manuscript.docx"
  
  run_knit <- TRUE
  knit_from <- "local" # sets source of the template doc from "google drive" or "local"
  knit_to <- "local" # sets source of the knitted doc from "google drive" or "local"
  
  objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes",
                       "paper_metadata","status","stitched_claims",
                       "all_rr_attempts","publications","extracted_claims",
                       "repro_export","repro_journal_policies")
  
  
  save_figures <- TRUE
  folder_figures <- paste0("Analysis/Paper 3/Figures/")
}

# Initial setup and libraries
{
  library(googledrive)
  library(targets)
  library(here)
  library(stringr)
  
  drive_auth(Sys.getenv("google_oauth_email"))
  setwd(here())
}

# Load full sets of datasets from the targets framework and save into analyst folder
{
  
  for(i in 1:length(objects_to_load)){
    assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
  }
  
  save(list=objects_to_load,file=paste0(folder_analyst_package,"analyst data.RData"))
}

# Assemble all code and datasets into the analyst folder and create a zipped package
{
  # Save all data and files into 
  # Common functions
  save(list=objects_to_load,file=paste0(folder_analyst_package,"analyst data.RData"))
  file.copy(from=paste0("Analysis/common functions.R"),to=paste0(folder_analyst_package,"common functions.R"),overwrite=TRUE)
  
  # Analysis code
  file.copy(from=paste0(folder_main_code,"analysis and knit.R"),to=paste0(folder_analyst_package,"analysis and knit.R"),overwrite=TRUE)
  
  # Template document
  drive_download(file=as_id(template_drive_ID_internal), path = paste0(folder_analyst_package,"template manuscript.docx"),overwrite = TRUE)
  
  # Create a .zip file with the full package
  setwd(paste0(here(),"/",folder_analyst_package))
  
  files2zip <- dir(full.names = TRUE)
  files2zip <- files2zip[!endsWith(files2zip,".zip") &
                          files2zip!=template_docx_file_internal]

  zip(zipfile = "data and code.zip", files = files2zip)
  setwd(here())
}

# Run the analysis / knit code
if(run_knit==TRUE){
  # Change directories to run full analysis/knit code locally in analyst folder
  setwd(folder_analyst_package)
  source("analysis and knit.R")
  
  if (knit_from == "google drive" & knit_to == "google drive"){
    knit_manuscript(
      template_docx_file = NA,
      template_drive_ID = template_drive_ID_internal,
      # knitted_docx_file = NA,
      # knitted_drive_ID = knitted_drive_ID_internal)
      knitted_docx_file = knitted_docx_file_internal,
      knitted_drive_ID = NA)
    drive_update(media=knitted_docx_file_internal,file=as_id(knitted_drive_ID_internal))
  } else if (knit_from ==" google drive" & knit_to == "local") {
    knit_manuscript(
      template_docx_file = NA,
      template_drive_ID = template_drive_ID_internal,
      knitted_docx_file = knitted_docx_file_internal,
      knitted_drive_ID = NA)
  } else if (knit_from == "local" & knit_to == "google drive") {
    knit_manuscript(
      template_docx_file = template_docx_file_internal,
      template_drive_ID = NA,
      # knitted_docx_file = NA,
      # knitted_drive_ID = knitted_drive_ID_internal)
      knitted_docx_file = knitted_docx_file_internal,
      knitted_drive_ID = NA)
    drive_update(media=knitted_docx_file_internal,file=as_id(knitted_drive_ID_internal))
  } else if (knit_from == "local" & knit_to == "local") {
    knit_manuscript(
      template_docx_file = template_docx_file_internal,
      template_drive_ID = NA,
      knitted_docx_file = knitted_docx_file_internal,
      knitted_drive_ID = NA)
  }
  
  if (save_figures == TRUE){
    #library(Cairo)
    setwd(paste0(here(),"/",folder_figures))
    
    for (figure in names(results_figures)){
      export_bundled_ggplot(results_figures[[figure]],paste0(figure,".svg"),device = "svg")
    }
    
  }
  
}

