
# Run tag generation for testing
if(TRUE){

  # Initial setup and libraries
  {
    #rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now

    library(shiny)
    library(bslib)
    library(dplyr)
    library(ggplot2)
    library(ggExtra)
    library(DT)
    library(tidyr)
    library(pbapply)
    library(googledrive)
    library(stringr)
    library(Hmisc)
    library(targets)
    library(googlesheets4)
    library(zcurve)
    library(scales)

    drive_auth(Sys.getenv("google_oauth_email"))

    
  }


  # Load data
    objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata")
    for(i in 1:length(objects_to_load)){
      assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
      save(list=objects_to_load[i],file=paste0("Analysis/Paper 5/Code and data/Commons/",objects_to_load[i],".RData"))
    }
    
    source(file="Analysis/common functions.R")
    file.copy("Analysis/common functions.R", "Analysis/Paper 5/Code and data/Commons/common functions.R",overwrite = TRUE)
    
    source(file="Analysis/Paper 5/Code and data/tagged stats and figures.R")
    #file.copy("Analysis/Paper 5/Code and data/Commons/tagged stats and figures.R", "Analysis/Data exploration app/tagged stats and figures.R",overwrite = TRUE)
    


    # Pull paper to find what tags are in paper
    paper_text <- drive_read_string(file=googledrive::as_id("1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg"),
                                      type = "text/plain",encoding="UTF-8")  %>%
      strsplit(split = "(\r\n|\r|\n)") %>%
      .[[1]]
    paper_text <- paste0(paper_text,collapse="  ")

    # Pull paper to find what tags are calculated
      tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
      tags <- tags[tags!=""]
      tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)

    # Generate stats
      results_tagged_stats <- tagged_stats(iters = 20,repli_outcomes_default_subset(),orig_outcomes)

    # Generate list of tags
      values_text <- do.call(c,lapply(1:length(tags),function(x) {
        tag_to_find <- tags[x]
        if(tag_to_find %in% names(results_tagged_stats)){
          as.character(results_tagged_stats[[tag_to_find]])
        } else {
          "MISSING"
        }
      }))


  # Export
    sheet_write(data.frame(tags,values_text),
                ss="https://docs.google.com/spreadsheets/d/1iIBhBsbvz89sZCDRFn9wghh17RExMa5XxQPLhlB_Bt8",sheet = "Sheet1")



}