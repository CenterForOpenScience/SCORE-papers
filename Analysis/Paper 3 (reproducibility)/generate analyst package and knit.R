# Settings
{
  folder_main_code <- paste0("Analysis/Paper 3 (reproducibility)/")
  folder_analyst_package <- paste0(folder_main_code,"Analyst package/")
  
  template_drive_ID_internal <- "18U3ElDrhF5PltX_UP1XIN1e6CK8nkfOzjSDZr2zF6lo"
  
  #knitted_drive_ID_internal <- "1W0tVAXFgTbdjeB5wN2yBgp85XCBCHfpG"
  
  template_docx_file_internal <- "template manuscript.docx"
  knitted_docx_file_internal <- "knitted manuscript.docx"
  
  knit_from <- "local" # sets source of the template doc from "google drive" or "local"
  knit_to <- "local" # sets source of the knitted doc from "google drive" or "local"
  
  objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes",
                       "paper_metadata","paper_status_tracking","claims_bushel_ids",
                       "rr_internal_tracking","extracted_claims",
                       "repro_journal_policies")
  
  save_figures <- TRUE
  folder_figures <- paste0("Analysis/Paper 3 (reproducibility)/Figures/")
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
  setwd(here())
}

# Run the analysis / knit code
{
  # Change directories to run full analysis/knit code locally in analyst folder
  setwd(folder_analyst_package)
  source("analysis and knit.R")
  
  # Remove all packages currently loaded
  packages.loaded <- names(sessionInfo()$otherPkgs)
  packages.loaded <- packages.loaded[packages.loaded!="here"]
  invisible(lapply(paste0('package:', packages.loaded), detach, character.only=TRUE, unload=TRUE))
  
  library(renv)
  
  knit_manuscript()
  
  if (save_figures == TRUE){
    setwd(paste0(here(),"/",folder_figures))
    
    for (figure in names(results_figures)){
      export_bundled_ggplot(results_figures[[figure]],paste0(figure,".svg"),device = "svg")
    }
  }
  
  # Generate a renv lock file
  setwd(folder_analyst_package)
  renv::snapshot(lockfile=paste0(folder_analyst_package,"renv.lock"))
  
}

