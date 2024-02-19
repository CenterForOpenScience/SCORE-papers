# Startup and initialization
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  

    bootstrap.clust(data=,
                    FUN=function(x) {
                      x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                      
                    },
                    clustervar = "paper_id",
                    keepvars=,
                    alpha=.05,tails="two-tailed",iters=iters,
                    format.percent=TRUE,digits=1
    )$formatted.text
}

# Find tags in text file
if (FALSE){
  library(stringr)
  library(googledrive)
  paper_5_text <- drive_read_string(file=googledrive::as_id("1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg"),
                      type = "text/plain")  %>%
    strsplit(split = "(\r\n|\r|\n)") %>%
    .[[1]]
  paper_5_text <- paste0(paper_5_text,collapse="  ")
  
  tags <- unique(str_match_all(paper_5_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
  tags <- tags[tags!=""]
  
  
 
}


{
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
  library(zcurve)
  
  # Data loading
  {
    # Check if this is being run from shinyapps.io or from the github folder (in
    # which case the data gets pulled from the targets output)
    if (file.exists("Analysis/common functions.R")) {
      # Being run from github/locally, get raw data and copy data files into
      # same level folder for uploading
      run_location <- "local"
      objects_to_load <- c("repli_outcomes","orig_outcomes","repro_outcomes")
      for(i in 1:length(objects_to_load)){
        assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
        save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
      }
      
      source(file="Analysis/common functions.R")
      file.copy("Analysis/common functions.R", "Analysis/Data exploration app/common functions.R",overwrite = TRUE)
      
      source(file="Analysis/Paper 5/Code/tagged stats and figures.R")
      file.copy("Analysis/Paper 5/Code/tagged stats and figures.R", "Analysis/Data exploration app/tagged stats and figures.R",overwrite = TRUE)
      
    } else {
      # Being run on shinyapps.io; data files already in folder
      run_location <- "server"
      load(file="repli_outcomes.RData")
      load(file="repro_outcomes.RData")
      load(file="orig_outcomes.RData")
      source("common functions.R")
      source("tagged stats and figures.R")
      drive_deauth()
    }
  }
  
  # Data manipulation and other setup
  {
    # RR UI and selection options data
    {
      select_repli_type_set <- c("new data","secondary data")
      select_repli_type_labels <- c("New data","Secondary data")
      select_repli_type_selected_default <- c("new data","secondary data")
      
      select_repli_version_of_record_set  <- c(TRUE,FALSE)
      select_repli_version_of_record_labels  <- c("VoR","Not VoR")
      select_repli_version_of_record_selected_default <- c(TRUE)
      
      select_repli_is_generalizability_set  <- c(FALSE,TRUE)
      select_repli_is_generalizability_labels  <- c("Standard","Generalizability study")
      select_repli_is_generalizability_selected_default <- c(FALSE)
      
      select_manylabs_set <- c("not_manylabs","ml_aggregation","ml_count","ml_instance_primary")
      select_manylabs_labels <- c("Not ManyLabs","ManyLabs Aggregation","ManyLabs Count","ManyLabs Instance Primary")
      select_manylabs_selected_default <- c("not_manylabs","ml_aggregation")
      
      select_power_for_effect_size_set <- c("50% for 100%","90% for 50%","90% for 75%","lab power analysis","not performed")
      select_power_for_effect_size_labels <- c("50% for 100%","90% for 50%","90% for 75%", "lab power analysis","Not performed")
      select_power_for_effect_size_selected_default <- select_power_for_effect_size_set
    }
  }
  
  repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                 by="claim_id",all.x=TRUE,all.y=FALSE)
  orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
  repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
  repli_outcomes_merged <- repli_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n())
  
  repli_outcomes_merged_pvals <- repli_outcomes_merged[!is.na(repli_outcomes_merged$orig_p_value) & !is.na(repli_outcomes_merged$repli_p_value),]
  repli_outcomes_merged_pvals_st <- repli_outcomes_merged_pvals[grepl("single-trace", repli_outcomes_merged_pvals$claim_id, fixed = TRUE),]
  
  zcurve.obj <- zcurve(p = repli_outcomes_merged_pvals$orig_p_value)
  # EDR: "Estimated proportion that the reported statistically significant
  # results constitute from all conducted statistical tests."
  EDR(zcurve.obj)
  
  # ODR: Observed proportion of statistically significant results
  ODR(zcurve.obj)
  
  # ERR:the predicted success rate of exact replication studies based on the
  # mean power after selection for significance.
  ERR(zcurve.obj)
  
  zcurve.obj <- zcurve(p = repli_outcomes_merged_pvals_st$orig_p_value)
  # EDR: "Estimated proportion that the reported statistically significant
  # results constitute from all conducted statistical tests."
  EDR(zcurve.obj)
  
  # ODR: Observed proportion of statistically significant results
  ODR(zcurve.obj)
  
  # ERR:the predicted success rate of exact replication studies based on the
  # mean power after selection for significance.
  ERR(zcurve.obj)
  
}