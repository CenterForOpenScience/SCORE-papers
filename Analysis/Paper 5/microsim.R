# Initial setup and libraries
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  
  
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
  library(googlesheets4)
  library(zcurve)
  
  drive_auth(Sys.getenv("google_oauth_email"))
  
  # Common functions
  source(file="Analysis/common functions.R")
}


# Data preparation
{
  
  objects_to_load <- c("repli_outcomes","orig_outcomes")
  for(i in 1:length(objects_to_load)){
    assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
    save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
  }
  repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                 by="claim_id",all.x=TRUE,all.y=FALSE)
  orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
  repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
  repli_outcomes_merged <- repli_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n())
}

# Generate zcurve 2.0
{
  # Data
  df <- repli_outcomes_merged[!is.na(repli_outcomes_merged$orig_p_value) & !is.na(repli_outcomes_merged$repli_p_value),]
  
  pvals <- c(na.omit(df$orig_p_value))
  zcurve <- summary(zcurve(p = df$orig_p_value))
  test <- summary(zcurve)
  plot(zcurve)
  
  
  # comparison
  p_repli_stat_sig_claims_any_dir <- 
    bootstrap.clust(data=df,
                    FUN=function(x) {
                      mean(x$repli_p_value <= 0.05)
                    },
                    clustervar = "paper_id",
                    keepvars=c("repli_p_value","repli_pattern_direction"),
                    alpha=.05,tails="two-tailed",iters=1000,
                    format.percent=TRUE,digits=1
    )$formatted.text
}
