
# Functions (also readable by external sources)
{
  # Create an object that contains the tagged stats
  tagged_stats <- function(iters = 100,repro_outcomes,orig_outcomes,paper_metadata){
  
    # Data preparation
    {
      paper_metadata <- paper_metadata[c("paper_id","publication_standard","COS_pub_category","pub_year")]
      orig_outcomes <- merge(orig_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
      
      repro_outcomes_merged <- merge(repro_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                     by="claim_id",all.x=TRUE,all.y=FALSE)
      orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
      repro_outcomes <- repro_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
      repro_outcomes_merged <- repro_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n())
    }
    
    # Stats
    {
      n_claims <- length(unique(repro_outcomes_merged$claim_id))
      
      n_papers <- length(unique(repro_outcomes_merged$paper_id))
      
      n_journals <- length(unique(repro_outcomes_merged$publication_standard))
    }
    
  
    # Clean up input values and export
    {
      rm(iters,orig_outcomes,repro_outcomes,repro_outcomes_merged)
      return(rev(as.list(environment())))
    }
  }
}

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
    # Common functions
    source(file="Analysis/common functions.R")
  }


  # Load data
    objects_to_load <- c("repro_outcomes","orig_outcomes","paper_metadata")
    for(i in 1:length(objects_to_load)){
      assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
      #save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
    }


    # Pull paper to find what tags are in paper
    paper_text <- drive_read_string(file=googledrive::as_id("1yqMVMzZMmGMyPG4IiD_urFHmBfztFXv-om-Y2M0T7jQ"),
                                      type = "text/plain",encoding="UTF-8")  %>%
      strsplit(split = "(\r\n|\r|\n)") %>%
      .[[1]]
    paper_text <- paste0(paper_text,collapse="  ")

    # Pull paper to find what tags are calculated
      tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
      tags <- tags[tags!=""]
      tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)

    # Generate stats
      results_tagged_stats <- tagged_stats(iters = 20,repro_outcomes,orig_outcomes,paper_metadata)

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
                ss="https://docs.google.com/spreadsheets/d/1iIBhBsbvz89sZCDRFn9wghh17RExMa5XxQPLhlB_Bt8",sheet = "Paper 3")



}
