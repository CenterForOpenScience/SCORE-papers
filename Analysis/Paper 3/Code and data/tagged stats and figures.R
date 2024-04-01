
# Functions (also readable by external sources)
{
  # Create an object that contains the tagged stats
  tagged_stats <- function(iters = 100,repro_outcomes,pr_outcomes,orig_outcomes,paper_metadata){
  
    # Data preparation
    {
      paper_metadata <- paper_metadata[c("paper_id","publication_standard","COS_pub_category","pub_year")]
      orig_outcomes <- merge(orig_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
      
      repro_outcomes_merged <- merge(repro_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                     by="claim_id",all.x=TRUE,all.y=FALSE)
      
      pr_outcomes <- merge(pr_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)

    }
    
    # pr_outcomes
    # binary data_shared
    # categorical data_shared_type
    # binary code_shared
    # categorical code_shared_type
    
    # repro_outcomes
    # binary outcome_reproduced
    # categorical outcome_repro_result_type
    # categorical outcome_repro_analyst_interp_type
    
    # paper_metadata
    # make COS_pub_category the original 10 field designations
    # categorical field, (the collapsed 6 field designation)
    # is_covid (move this here; easier to deal with as paper metadata)
    
    # New key variables
    {
      pr_outcomes_modified <- pr_outcomes %>% 
        mutate(
          data_shared = ifelse(str_detect(OA_data_shared, "yes"), TRUE, FALSE),
          data_shared_type = factor(OA_data_shared,
                                    levels=c("available_online","no","yes_private","yes_public"),
                                    labels=c("available online","not shared","shared by authors privately","shared by authors publicly")),
          code_shared = ifelse(str_detect(OA_code_shared, "yes"), TRUE, FALSE),
          code_shared_type = factor(OA_code_shared,
                                    levels=c("available_online","no","yes_private","yes_public"),
                                    labels=c("available online","not shared","shared by authors privately","shared by authors publicly")),
        )
      
      pr %>% 
        group_by(COS_pub_category) %>% 
        count(data_shared) %>% 
        mutate(tot = sum(n)) %>% 
        ungroup() %>% 
        mutate(prop = n/tot) %>% 
        select(-n, -tot) %>% 
        pivot_wider(names_from = data_shared, values_from = prop) %>% 
        mutate(across(.cols = everything(), .fns = function(x) ifelse(is.na(x), 0, x))) %>% 
        arrange(desc(no)) %>% 
        mutate(idx = row_number()) %>% 
        pivot_longer(cols = -c(COS_pub_category, idx), names_to = "data_shared", values_to = "prop") %>% 
        mutate(
          data_shared = as_factor(data_shared) %>% fct_relevel(., "no", "shared", "available_online"),
        )
    }
    
    # Stats
    {
      # note: this isn't right
      n_claims <- length(unique(orig_outcomes$claim_id))
      
      n_papers <- length(unique(pr_outcomes$paper_id))
      
      n_journals <- length(unique(pr_outcomes$publication_standard))
      
      n_papers_passed_process_repro_assess <- sum(pr_outcomes$process_reproducible=="Yes")
      
      p_papers_passed_process_repro_assess <- 
        bootstrap.clust(data=pr_outcomes,
                        FUN=function(x) {
                          mean(x$process_reproducible=="Yes",
                                        na.rm=TRUE)},
                        clustervar = "paper_id",keepvars=c("process_reproducible"),
                        alpha=.05,tails="two-tailed",iters=iters,format.percent=TRUE,digits=1
        )$formatted.text
        
        p_data_available <- 
          bootstrap.clust(data=pr_outcomes,
                          FUN=function(x) {
                            mean(x$data_available=="Yes",
                                 na.rm=TRUE)},
                          clustervar = "paper_id",keepvars=c("data_available"),
                          alpha=.05,tails="two-tailed",iters=iters,format.percent=TRUE,digits=1
          )$formatted.text

        for (i in 2008:2018){
          assign(paste0("p_data_available_",i),{
            bootstrap.clust(data=pr_outcomes[pr_outcomes$pub_year==i,],
                            FUN=function(x) {
                              mean(x$data_available=="Yes",
                                   na.rm=TRUE)},
                            clustervar = "paper_id",keepvars=c("data_available"),
                            alpha=.05,tails="two-tailed",iters=iters,format.percent=TRUE,digits=1
            )$formatted.text
          })
        }

        df.fields <- do.call(rbind,lapply(na.omit(unique(pr_outcomes$COS_pub_category)),function(field){
          process_reproducible <- bootstrap.clust(data=pr_outcomes[pr_outcomes$COS_pub_category==field,],
                                            FUN=function(x) {
                                              mean(x$process_reproducible=="Yes",
                                                   na.rm=TRUE)},
                                            clustervar = "paper_id",keepvars=c("process_reproducible"),
                                            alpha=.05,tails="two-tailed",iters=iters,format.percent=TRUE,digits=1
          )
          p_process_reproducible <- process_reproducible$point.estimate
          formatted_text_process_reproducible <- process_reproducible$formatted.text

          data.frame(field,p_process_reproducible,formatted_text_process_reproducible)
        }))

        field_most_passing_process_repro <- df.fields$field[which.max(df.fields$p_process_reproducible)]

        p_field_most_passing_process_repro <- df.fields$formatted_text_process_reproducible[which.max(df.fields$p_process_reproducible)]
        
        field_least_passing_process_repro <- df.fields$field[which.min(df.fields$p_process_reproducible)]
        
        p_field_least_passing_process_repro <- df.fields$formatted_text_process_reproducible[which.min(df.fields$p_process_reproducible)]
        
        n_claims_passed_outcome_repro <- "Missing outcome repro variable?"
        
        n_claims_passed_process_repro <- sum(pr_outcomes$process_reproducible=="Yes")
        
        
    }
    
  
    # Clean up input values and export
    {
      rm(iters,orig_outcomes,repro_outcomes,repro_outcomes_merged)
      return(rev(as.list(environment())))
    }
  }
}

# Run tag generation for testing. Note: comment out this section before deploying
# if(TRUE){
# 
#   # Initial setup and libraries
#   {
#     #rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
# 
#     library(shiny)
#     library(bslib)
#     library(dplyr)
#     library(ggplot2)
#     library(ggExtra)
#     library(DT)
#     library(tidyr)
#     library(pbapply)
#     library(googledrive)
#     library(stringr)
#     library(Hmisc)
#     library(targets)
#     library(googlesheets4)
#     library(zcurve)
#     library(scales)
# 
#     drive_auth(Sys.getenv("google_oauth_email"))
#     #drive_deauth()
#     # Common functions
#     source(file="Analysis/common functions.R")
#   }
# 
# 
#   # Load data
#     objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes","paper_metadata")
#     for(i in 1:length(objects_to_load)){
#       assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
#       #save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
#     }
# 
# 
#     # Pull paper to find what tags are in paper
#     paper_text <- drive_read_string(file=googledrive::as_id("1yqMVMzZMmGMyPG4IiD_urFHmBfztFXv-om-Y2M0T7jQ"),
#                                       type = "text/plain",encoding="UTF-8")  %>%
#       strsplit(split = "(\r\n|\r|\n)") %>%
#       .[[1]]
#     paper_text <- paste0(paper_text,collapse="  ")
# 
#     # Pull paper to find what tags are calculated
#       tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
#       tags <- tags[tags!=""]
#       tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)
# 
#     # Generate stats
#       results_tagged_stats <- tagged_stats(iters = 20,
#                                            repro_outcomes=repro_outcomes,
#                                            pr_outcomes=pr_outcomes,
#                                            orig_outcomes=orig_outcomes,
#                                            paper_metadata=paper_metadata)
# 
#     # Generate list of tags
#       values_text <- do.call(c,lapply(1:length(tags),function(x) {
#         tag_to_find <- tags[x]
#         if(tag_to_find %in% names(results_tagged_stats)){
#           as.character(results_tagged_stats[[tag_to_find]])
#         } else {
#           "MISSING"
#         }
#       }))
# 
# 
#   # Export
#     sheet_write(data.frame(tags,values_text),
#                 ss="https://docs.google.com/spreadsheets/d/1iIBhBsbvz89sZCDRFn9wghh17RExMa5XxQPLhlB_Bt8",sheet = "Paper 3")
# 
# 
# 
# }
