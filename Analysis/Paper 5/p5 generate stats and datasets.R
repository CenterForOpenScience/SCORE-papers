# Settings
{
  folder_main_code <- paste0("Analysis/Paper 5/")
  folder_figures <- paste0("Analysis/Paper 5/Figures/")
  folder_analyst_package <- paste0(folder_main_code,"Analyst package/")
  
  template_drive_ID_internal <- "1mauNAwu0eZfvh5-5p44wnKz8NQL-5Tm_bAyZNseTONo"
  
  #knitted_drive_ID_internal <- "1W0tVAXFgTbdjeB5wN2yBgp85XCBCHfpG"
  
  template_docx_file_internal <- "template manuscript.docx"
  knitted_docx_file_internal <- "knitted manuscript.docx"
  
  run_knit <- TRUE
  knit_from <- "local" # sets source of the template doc from "google drive" or "local"
  knit_to <- "local" # sets source of the knitted doc from "google drive" or "local"
  
  objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata",
                       "status","all_rr_attempts","repli_binary","publications",
                       "non_significant_bushels","rr_sourced","repli_export",
                       "repli_case_exclusions","orig_dataset",
                       "full_dates","never_sourced","ser_power",
                       "traditional_power")
  
  save_figures <- TRUE
  
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
  
  # Repli binary code
  file.copy(from=paste0(folder_main_code,"repli_binary.R"),to=paste0(folder_analyst_package,"repli_binary.R"),overwrite=TRUE)
  
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




# 
# 
# # Initial setup and libraries
# {
#   rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
#   
#   library(shiny)
#   library(bslib)
#   library(dplyr)
#   library(ggplot2)
#   library(ggExtra)
#   library(DT)
#   library(tidyr)
#   library(pbapply)
#   library(googledrive)
#   library(stringr)
#   library(Hmisc)
#   library(targets)
#   library(googlesheets4)
#   library(zcurve)
#   library(scales)
#   library(wCorr)
#   library(corrplot)
#   library(cowplot)
#   library(ggridges) #note: using the github version, as the CRAN hasn't been pushed to get the weights functionality
#   library(ggside)
#   library(weights)
#   library(glue)
#   
#   drive_auth(Sys.getenv("google_oauth_email"))
#   #drive_deauth()
# }
# 
# # Load and save data and datasets for public running
# {
#   objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata",
#                        "status","all_rr_attempts","repli_binary","publications",
#                        "non_significant_bushels","rr_sourced","repli_export",
#                        "repli_case_exclusions","orig_dataset",
#                        "full_dates","never_sourced","ser_power",
#                        "traditional_power")
#   for(i in 1:length(objects_to_load)){
#     assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
#   }
#   
#   save(list=objects_to_load,file="Analysis/Paper 5/Code and data/Analyst package/analyst data.RData")
#   file.copy(from="Analysis/common functions.R",to="Analysis/Paper 5/Code and data/Analyst package/common functions.R",overwrite=TRUE)
#   file.copy(from="Analysis/Paper 5/Code and data/tagged stats and figures.R",to="Analysis/Paper 5/Code and data/Analyst package/tagged stats and figures.R",overwrite=TRUE)
#   file.copy(from="pipeline/data_processing/rr/repli_binary.R",to="Analysis/Paper 5/Code and data/Analyst package/repli_binary.R",overwrite=TRUE)
# }
# 
# # Pull source code docs
# {
#   # Common functions
#   #source(file="Analysis/common functions.R")
#   source(file="Analysis/Paper 5/Code and data/tagged stats and figures.R")
# }
# 
# # Generate tagged stats in text
# {
#   # Pull paper to find what tags are in paper
#   {
#     paper_text <- drive_read_string(file=googledrive::as_id("1mauNAwu0eZfvh5-5p44wnKz8NQL-5Tm_bAyZNseTONo"),
#                                     type = "text/plain",encoding="UTF-8")  %>%
#       strsplit(split = "(\r\n|\r|\n)") %>%
#       .[[1]]
#     paper_text <- paste0(paper_text,collapse="  ")
#     
#     # Pull paper to find what tags are calculated
#     tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
#     tags <- tags[tags!=""]
#     tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)
#   }
#   
#   # Generate stats
#   results_tagged_stats <<- tagged_stats(iters = 1000)
#   
#   # Generate list of tags
#   values_text <- do.call(c,lapply(1:length(tags),function(x) {
#     tag_to_find <- tags[x]
#     if(tag_to_find %in% names(results_tagged_stats)){
#       as.character(results_tagged_stats[[tag_to_find]])
#     } else {
#       "MISSING"
#     }
#   }))
#   
#   # Export
#   ss <- "https://docs.google.com/spreadsheets/d/1uaR6vYAMVywk1liQeKRyRCDQcs--teFpRsWzHrvfg3s"
#   range_delete(ss,range="A:H")
#   range_write(ss,data = data.frame(tags=paste0("{",tags,"}"),values_text), range = "A1",col_names=FALSE)
# }
# 
# # Generate figures
# {
#   generated_figures <- figures(iters = 1000)
#   
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure 1.png",
#     plot = generated_figures$figure_1,
#     width = 3000,height = 1000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure 2.png",
#     plot = generated_figures$figure_2,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure 3.png",
#     plot = generated_figures$figure_3,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure 4.png",
#     plot = generated_figures$figure_4,
#     width = 2000,height = 1200,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s1.png",
#     plot = generated_figures$figure_s1,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s2.png",
#     plot = generated_figures$figure_s2,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s3.png",
#     plot = generated_figures$figure_s3,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s4.png",
#     plot = generated_figures$figure_s4,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s5.png",
#     plot = generated_figures$figure_s5,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s6.png",
#     plot = generated_figures$figure_s6,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s7.png",
#     plot = generated_figures$figure_s7,
#     width = 4000,height = 4000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s8.png",
#     plot = generated_figures$figure_s8,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s9.png",
#     plot = generated_figures$figure_s9,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s10.png",
#     plot = generated_figures$figure_s10,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s11.png",
#     plot = generated_figures$figure_s11,
#     width = 2000,height = 2000,units = "px",bg="white"
#   )
#   ggsave(
#     "Analysis/Paper 5/Code and data/Figures/figure s12.png",
#     plot = generated_figures$figure_s12,
#     width = 3000,height = 1000,units = "px",bg="white"
#   )
# }
