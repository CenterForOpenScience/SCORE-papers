
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
      
      repro_papers <- pr_outcomes[pr_outcomes$covid==FALSE,][c(
        "paper_id"
      )]
      repro_papers <- merge(repro_papers,paper_metadata[c("paper_id","publication_standard","COS_pub_category","pub_year")],
                                     by="paper_id",all.x=TRUE,all.y=FALSE)
      
      # Trim data to remove COVID papers
      {
        orig_outcomes <- orig_outcomes[!orig_outcomes$is_covid,]
        repro_outcomes <- repro_outcomes[!repro_outcomes$is_covid,]
        pr_outcomes <- pr_outcomes[!pr_outcomes$covid,]
      }
      
    }

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
      
      repro_outcomes_merged$field <- str_to_title(repro_outcomes_merged$COS_pub_category)
      repro_outcomes_merged$field <- str_replace_all(repro_outcomes_merged$field,"And","and")
      repro_outcomes_merged$field <- ordered(repro_outcomes_merged$field,
                                             levels=sort(unique(repro_outcomes_merged$field)),labels=sort(unique(repro_outcomes_merged$field)))
      
      repro_papers$field <- str_to_title(repro_papers$COS_pub_category)
      repro_papers$field <- str_replace_all(repro_papers$field,"And","and")
      repro_papers$field <- ordered(repro_papers$field,
                                             levels=sort(unique(repro_papers$field)),labels=sort(unique(repro_papers$field)))
      
      pr_outcomes_modified$field <- str_to_title(pr_outcomes_modified$COS_pub_category)
      pr_outcomes_modified$field <- str_replace_all(pr_outcomes_modified$field,"And","and")
      pr_outcomes_modified$field <- ordered(pr_outcomes_modified$field,
                                    levels=sort(unique(pr_outcomes_modified$field)),labels=sort(unique(pr_outcomes_modified$field)))
    }
    
    # Generate OR outcomes data frame
    {
      repro_outcomes_expanded <- status %>%
        filter(RR) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      repro_outcomes_expanded$paper_id <- do.call(c,lapply(1:nrow(repro_outcomes_expanded),function(x)
        strsplit(repro_outcomes_expanded$claim_id[x],"_")[[1]][1]))
      
      test <- merge(repro_outcomes_expanded,repro_outcomes[c("claim_id","repro_outcome_overall")],
                    by="claim_id",all.x=TRUE,all.y=FALSE)
      
      
      
      repro_outcomes_expanded <- merge(repro_outcomes_expanded,repro_outcomes[c("claim_id","repro_outcome_overall")],
                                       by="claim_id",all.x=TRUE,all.y=FALSE)
      repro_outcomes_expanded <- merge(repro_outcomes_expanded,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                                       by="paper_id",all.x =TRUE,all.y=FALSE)
      
      repro_outcomes_expanded<-repro_outcomes_expanded[repro_outcomes_expanded$is_covid==FALSE,]
      repro_outcomes_expanded$is_covid <- NULL
      repro_outcomes_expanded$repro_outcome_overall <- ifelse(is.na(repro_outcomes_expanded$repro_outcome_overall),
                                                              "not attempted",
                                                              repro_outcomes_expanded$repro_outcome_overall)
      repro_outcomes_expanded <- repro_outcomes_expanded[repro_outcomes_expanded$repro_outcome_overall!="none",]
      repro_outcomes_expanded <- repro_outcomes_expanded %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())
      repro_outcomes_expanded$field <- str_to_title(repro_outcomes_expanded$COS_pub_category)
    }
    
    
    # Stats
    {
      # Abstract
      {
        # note: this isn't right
        n_claims <- length(unique(orig_outcomes$claim_id))
        
        n_papers <- length(unique(pr_outcomes$paper_id))
        
        n_journals <- length(unique(
          paper_metadata[paper_metadata$paper_id %in% pr_outcomes$paper_id,]$publication_standard
        ))
        
        n_papers_passed_process_repro_assess <- sum(pr_outcomes$process_reproducible=="Yes")
        
        p_papers_passed_process_repro_assess <- format.text.percent(n_papers_passed_process_repro_assess,nrow(pr_outcomes))
        
        p_data_available <- format.text.percent(sum(pr_outcomes$data_available=="Yes"),
                                                nrow(pr_outcomes))
        
        for (i in 2009:2018){
          assign(paste0("p_data_available_",i),{
            format.text.percent(sum(pr_outcomes[pr_outcomes$pub_year==i,]$data_available=="Yes"),
                                nrow(pr_outcomes[pr_outcomes$pub_year==i,]))
          })
        }
        
        df.fields <- do.call(rbind,lapply(na.omit(unique(pr_outcomes$COS_pub_category)),function(field){
          p_process_reproducible <- sum(pr_outcomes[pr_outcomes$COS_pub_category==field,]$data_available=="Yes")/
            nrow(pr_outcomes[pr_outcomes$COS_pub_category==field,])
          formatted_text_process_reproducible <- 
            format.text.percent(sum(pr_outcomes[pr_outcomes$COS_pub_category==field,]$data_available=="Yes"),
                                nrow(pr_outcomes[pr_outcomes$COS_pub_category==field,]))
          
          data.frame(field,p_process_reproducible,formatted_text_process_reproducible)
        }))
        
        field_most_passing_process_repro <- df.fields$field[which.max(df.fields$p_process_reproducible)]
        
        p_field_most_passing_process_repro <- df.fields$formatted_text_process_reproducible[which.max(df.fields$p_process_reproducible)]
        
        field_least_passing_process_repro <- df.fields$field[which.min(df.fields$p_process_reproducible)]
        
        p_field_least_passing_process_repro <- df.fields$formatted_text_process_reproducible[which.min(df.fields$p_process_reproducible)]
        
        length(unique(repro_outcomes_expanded$paper_id))
        length(unique(repro_outcomes_expanded$claim_id))
        
        
        
        
        
        
        n_claims_passed_outcome_repro <- sum(repro_outcomes$repro_pattern_criteria_met=="yes" & repro_outcomes$repro_p_value<=.05,na.rm=TRUE)
        
        n_claims_passed_process_repro <- sum(pr_outcomes$process_reproducible=="Yes")
      }
      
      
      
      # Table 2
      {
        data.table_2_col_1 <- pr_outcomes_modified[pr_outcomes_modified$covid==FALSE,]
        table_2_col_1 <- c(do.call(c,lapply(levels(data.table_2_col_1$field),function(field) {
          n <- length(unique(data.table_2_col_1[data.table_2_col_1$field==field,]$publication_standard))
          paste0(n," (",format.round(100*n/length(unique(data.table_2_col_1$publication_standard)),1),"%)")
        })),paste0(length(unique(data.table_2_col_1$publication_standard))," (100%)")
        )
        for(x in 1:length(table_2_col_1)) {assign(paste0("table_2_",x,"_1"),table_2_col_1[x])}
        
        data.table_2_col_2 <- pr_outcomes_modified[pr_outcomes_modified$covid==FALSE,]
        table_2_col_2 <- c(do.call(c,lapply(levels(data.table_2_col_2$field),function(field) {
          n <- length(unique(data.table_2_col_2[data.table_2_col_2$field==field,]$paper_id))
          paste0(n," (",format.round(100*n/length(unique(data.table_2_col_2$paper_id)),1),"%)")
        })),paste0(length(unique(data.table_2_col_2$paper_id))," (100%)")
        )
        for(x in 1:length(table_2_col_2)) {assign(paste0("table_2_",x,"_2"),table_2_col_2[x])}
        
        data.table_2_col_3 <- pr_outcomes_modified[pr_outcomes_modified$covid==FALSE & pr_outcomes_modified$OA_data_shared!="no",]
        table_2_col_3 <- c(do.call(c,lapply(levels(data.table_2_col_3$field),function(field) {
          n <- length(unique(data.table_2_col_3[data.table_2_col_3$field==field,]$paper_id))
          paste0(n," (",format.round(100*n/length(unique(data.table_2_col_3$paper_id)),1),"%)")
        })),paste0(length(unique(data.table_2_col_3$paper_id))," (100%)")
        )
        for(x in 1:length(table_2_col_3)) {assign(paste0("table_2_",x,"_3"),table_2_col_3[x])}
        
        data.table_2_col_4 <- pr_outcomes_modified[pr_outcomes_modified$covid==FALSE & pr_outcomes_modified$paper_id %in% repro_outcomes$paper_id,]
        table_2_col_4 <- c(do.call(c,lapply(levels(data.table_2_col_4$field),function(field) {
          n <- length(unique(data.table_2_col_4[data.table_2_col_4$field==field,]$paper_id))
          paste0(n," (",format.round(100*n/length(unique(data.table_2_col_4$paper_id)),1),"%)")
        })),paste0(length(unique(data.table_2_col_4$paper_id))," (100%)")
        )
        for(x in 1:length(table_2_col_4)) {assign(paste0("table_2_",x,"_4"),table_2_col_4[x])}
      }
      
      # Table 3
      {
        data.table_3_col_1 <- pr_outcomes_modified[pr_outcomes_modified$covid==FALSE,]
        table_3_col_1 <- c(do.call(c,lapply(sort(unique(data.table_3_col_1$pub_year)),function(pub_year) {
          n <- length(unique(data.table_3_col_1[data.table_3_col_1$pub_year==pub_year,]$publication_standard))
          paste0(n," (",format.round(100*n/length(unique(data.table_3_col_1$publication_standard)),1),"%)")
        })),paste0(length(unique(data.table_3_col_1$publication_standard))," (100%)")
        )
        for(x in 1:length(table_3_col_1)) {assign(paste0("table_3_",x,"_1"),table_3_col_1[x])}
        
        data.table_3_col_2 <- pr_outcomes_modified[pr_outcomes_modified$covid==FALSE,]
        table_3_col_2 <- c(do.call(c,lapply(sort(unique(data.table_3_col_2$pub_year)),function(pub_year) {
          n <- length(unique(data.table_3_col_2[data.table_3_col_2$pub_year==pub_year,]$paper_id))
          paste0(n," (",format.round(100*n/length(unique(data.table_3_col_2$paper_id)),1),"%)")
        })),paste0(length(unique(data.table_3_col_2$paper_id))," (100%)")
        )
        for(x in 1:length(table_3_col_2)) {assign(paste0("table_3_",x,"_2"),table_3_col_2[x])}
        
        data.table_3_col_3 <- pr_outcomes_modified[pr_outcomes_modified$covid==FALSE & pr_outcomes_modified$OA_data_shared!="no",]
        table_3_col_3 <- c(do.call(c,lapply(sort(unique(data.table_3_col_3$pub_year)),function(pub_year) {
          n <- length(unique(data.table_3_col_3[data.table_3_col_3$pub_year==pub_year,]$paper_id))
          paste0(n," (",format.round(100*n/length(unique(data.table_3_col_3$paper_id)),1),"%)")
        })),paste0(length(unique(data.table_3_col_3$paper_id))," (100%)")
        )
        for(x in 1:length(table_3_col_3)) {assign(paste0("table_3_",x,"_3"),table_3_col_3[x])}
        
        data.table_3_col_4 <- pr_outcomes_modified[pr_outcomes_modified$covid==FALSE & pr_outcomes_modified$paper_id %in% repro_outcomes$paper_id,]
        table_3_col_4 <- c(do.call(c,lapply(sort(unique(data.table_3_col_4$pub_year)),function(pub_year) {
          n <- length(unique(data.table_3_col_4[data.table_3_col_4$pub_year==pub_year,]$paper_id))
          paste0(n," (",format.round(100*n/length(unique(data.table_3_col_4$paper_id)),1),"%)")
        })),paste0(length(unique(data.table_3_col_4$paper_id))," (100%)")
        )
        for(x in 1:length(table_3_col_4)) {assign(paste0("table_3_",x,"_4"),table_3_col_4[x])}
      }
        
    }
    
  
    # Clean up input values and export
    {
      rm(iters,orig_outcomes,repro_outcomes,repro_outcomes_merged)
      return(rev(as.list(environment())))
    }
  }
  
  }
  
  


# Run tag generation for testing. Note: comment out this section before deploying
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
    #drive_deauth()
    # Common functions
    source(file="Analysis/common functions.R")
  }


  # Load data
    objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes","paper_metadata","status","stitched_claims")
    for(i in 1:length(objects_to_load)){
      assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
      #save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
    }


    # Pull paper to find what tags are in paper
    paper_text <- drive_read_string(file=googledrive::as_id("18U3ElDrhF5PltX_UP1XIN1e6CK8nkfOzjSDZr2zF6lo"),
                                      type = "text/plain",encoding="UTF-8")  %>%
      strsplit(split = "(\r\n|\r|\n)") %>%
      .[[1]]
    paper_text <- paste0(paper_text,collapse="  ")

    # Pull paper to find what tags are calculated
      tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
      tags <- tags[tags!=""]
      tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)

    # Generate stats
      results_tagged_stats <- tagged_stats(iters = 20,
                                           repro_outcomes=repro_outcomes,
                                           pr_outcomes=pr_outcomes,
                                           orig_outcomes=orig_outcomes,
                                           paper_metadata=paper_metadata)

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
      ss <- "https://docs.google.com/spreadsheets/d/1qs8Ap3wfw-t5SqlDbCa1sQvvtVY4LS38xwxd3rro0po"
      range_delete(ss,range="A:H")
      range_write(ss,data = data.frame(tags=paste0("{",tags,"}"),values_text), range = "A1",col_names=FALSE)
      
    # sheet_write(data.frame(tags=paste0("{",tags,"}"),values_text),
    #             ss=ss,sheet = "Sheet1")

}
