# Run knit_manuscript() to
# 1) Generate all placeholder stats
# 2) Generate all placeholder figures
# 3) Read the template MS Word doc
# 4) Replace all placeholder stats and figures and knit them together into a
# knitted Microsoft Word document

# The resulting objects (results_tagged_stats and results_figures) contain
# listed outputs for all analysis tags contained within the publication,
# accessed with $ (e.g. results_figures$figure_1)

# Note: packages are current CRAN versions as of Jul 10, 2025

knit_manuscript <- function(template_docx_file="template manuscript.docx",
                            template_drive_ID=NA,
                            knitted_docx_file = "knitted manuscript.docx",
                            knitted_drive_ID=NA)
{
  source("common functions.R")
  set.seed(1565079)
  # Place all numeric and character tagged placeholders into the current function environment
  print("Generating placeholder text stats")
  results_placeholder_stats <<- placeholder_stats(iters = 1000)
  print("Generating figures")
  results_figures <<- figures(iters = 1000)
  
  results_all <- append(results_placeholder_stats, results_figures)
  
  print("Knitting manuscript from template")
  knit_docx(template_docx_file=template_docx_file,
            template_drive_ID=template_drive_ID,
            knitted_docx_file = knitted_docx_file,
            knitted_drive_ID=knitted_drive_ID,
            placeholder_object_source = results_all)
}

# Generate placeholder text and statistics
placeholder_stats <- function(iters=100){
  options(tidyverse.quiet = TRUE)
  # Load libraries
  {
    library(officer)
    library(pandoc)
    library(dplyr)
    library(ggplot2)
    library(ggExtra)
    library(DT)
    library(tidyr)
    library(stringr)
    library(Hmisc)
    library(scales)
    library(wCorr)
    library(corrplot)
    library(cowplot)
    library(ggside)
    library(weights)
    library(glue)
    library(DescTools)
  }
  
  # Load data and common functions
  {
    load("analyst data.RData")
    source("common functions.R")
  }
  
  # Data preparation
  {
    # Set defaults for convenience
    {
      if (!exists("iters")){ iters <- 100}
    }
    
    # Preserve original untrimmed datasets
      orig_outcomes_orig <- orig_outcomes
      repro_outcomes_orig <- repro_outcomes
      pr_outcomes_orig <- pr_outcomes
    
    # Trim data to remove COVID papers
      orig_outcomes <- orig_outcomes[!orig_outcomes$is_covid,]
      repro_outcomes <- repro_outcomes[!repro_outcomes$is_covid,]
      pr_outcomes <- pr_outcomes[!pr_outcomes$covid,]
      
    # Modify repro outcomes overall result to make "not attempted" = "not"
      repro_outcomes$repro_outcome_overall <- 
        ifelse(repro_outcomes$repro_outcome_overall=="not attemptable",
               "not",repro_outcomes$repro_outcome_overall)
    
    # Trim out non-version of record entries (and save original version)
      repro_outcomes_inc_vor <- repro_outcomes
      repro_outcomes <- repro_outcomes[!is.na(repro_outcomes$repro_version_of_record)&
                                         repro_outcomes$repro_version_of_record=="T",]
    
    # Merge in paper metadata
      paper_metadata <- paper_metadata[c("paper_id","publication_standard","COS_pub_category","COS_pub_expanded","pub_year","is_covid")]
      orig_outcomes <- merge(orig_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
      
      repro_outcomes_merged <- merge(repro_outcomes,orig_outcomes_orig[,!(names(orig_outcomes_orig) %in% c("paper_id"))],
                                     by="claim_id",all.x=TRUE,all.y=FALSE)
      repro_outcomes_merged <- merge(repro_outcomes_merged,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
      
      pr_outcomes <- merge(pr_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
      
      # Key variables
      {
        pr_outcomes_modified <- pr_outcomes %>% 
          mutate(
            data_shared = ifelse(str_detect(OA_data_shared, "yes"), TRUE, FALSE),
            data_shared_type = factor(OA_data_shared,
                                      levels=c("available_online","no","shared_on_request"),
                                      labels=c("available online","not shared","shared on request")),
            code_shared = ifelse(str_detect(OA_code_shared, "yes"), TRUE, FALSE),
            code_shared_type = factor(OA_code_shared,
                                      levels=c("available_online","no","shared_on_request"),
                                      labels=c("available online","not shared","shared on request")),
          )
        
        pr_outcomes_modified$data_available_or_shared <- pr_outcomes_modified$data_available=="Yes" | pr_outcomes_modified$OA_data_shared!="no"
        pr_outcomes_modified$code_available_or_shared <- pr_outcomes_modified$code_available=="Yes" | pr_outcomes_modified$OA_code_shared!="no"
        
        repro_outcomes_merged$field <- str_to_title(repro_outcomes_merged$COS_pub_category)
        repro_outcomes_merged$field <- str_replace_all(repro_outcomes_merged$field,"And","and")
        repro_outcomes_merged$field <- ordered(repro_outcomes_merged$field,
                                               levels=sort(unique(repro_outcomes_merged$field)),labels=sort(unique(repro_outcomes_merged$field)))
        
        paper_metadata$field <- str_to_title(paper_metadata$COS_pub_category)
        paper_metadata$field <- str_replace_all(paper_metadata$field,"And","and")
        paper_metadata$field <- ordered(paper_metadata$field,
                                        levels=sort(unique(paper_metadata$field)),labels=sort(unique(repro_outcomes_merged$field)))
        
        pr_outcomes_modified$field <- str_to_title(pr_outcomes_modified$COS_pub_category)
        pr_outcomes_modified$field <- str_replace_all(pr_outcomes_modified$field,"And","and")
        pr_outcomes_modified$field <- ordered(pr_outcomes_modified$field,
                                              levels=sort(unique(pr_outcomes_modified$field)),labels=sort(unique(pr_outcomes_modified$field)))
        
        pr_outcomes_modified$pub_year <- ordered(pr_outcomes_modified$pub_year,
                                                 levels=c(2009:2018),
                                                 labels=c(2009:2018))
        
        repro_outcomes_merged$pub_year <- ordered(repro_outcomes_merged$pub_year,
                                                  levels=c(2009:2018),
                                                  labels=c(2009:2018))
        
        repro_outcomes <- repro_outcomes %>%
          group_by(paper_id) %>%
          mutate(weight=1/n())
        
        repro_outcomes_merged <- repro_outcomes_merged %>%
          group_by(paper_id) %>%
          mutate(weight=1/n())
        
        repro_outcomes$repro_type_consolidated <- 
          ordered(repro_outcomes$repro_type,
                  labels=c("Data and code available","Data and code available",
                           "Only data available","Data reconstructed from source"),
                  levels=c("Push Button Reproduction","Extended Push Button Reproduction",
                           "Author Data Reproduction","Source Data Reproduction")
          )
        
        repro_outcomes_merged$repro_type_consolidated <- 
          ordered(repro_outcomes_merged$repro_type,
                  labels=c("Data and code available","Data and code available",
                           "Only data available","Data reconstructed from source"),
                  levels=c("Push Button Reproduction","Extended Push Button Reproduction",
                           "Author Data Reproduction","Source Data Reproduction")
          )
        
        repro_outcomes$repro_outcome_overall_consolidated <- 
          ordered(repro_outcomes$repro_outcome_overall,
                  labels=c("Precisely\nReproduced","Precisely\nReproduced","Approximately\nReproduced","Not\nReproduced"),
                  levels=c("push button","precise","approximate","not")
          )
        
        repro_outcomes_merged$repro_outcome_overall_consolidated <- 
          ordered(repro_outcomes_merged$repro_outcome_overall,
                  labels=c("Precisely\nReproduced","Precisely\nReproduced","Approximately\nReproduced","Not\nReproduced"),
                  levels=c("push button","precise","approximate","not")
          )
        
      }
      
      # Generate expanded OR outcomes data frame
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
        
        repro_outcomes_expanded <- merge(repro_outcomes_expanded,repro_outcomes[c("claim_id","repro_outcome_overall")],
                                         by="claim_id",all.x=TRUE,all.y=FALSE)
        repro_outcomes_expanded <- merge(repro_outcomes_expanded,paper_metadata[c("paper_id","pub_year","COS_pub_category")],
                                         by="paper_id",all.x =TRUE,all.y=FALSE)
        
        repro_outcomes_expanded$repro_outcome_overall <- ifelse(is.na(repro_outcomes_expanded$repro_outcome_overall),
                                                                "not attempted",
                                                                repro_outcomes_expanded$repro_outcome_overall)

        repro_outcomes_expanded <- repro_outcomes_expanded %>%
          group_by(paper_id) %>%
          mutate(weight=1/n())
        repro_outcomes_expanded$field <- str_to_title(repro_outcomes_expanded$COS_pub_category)
      }

  }

  # Stats
  {
    # Table 2
    {
      fields.order <- c("Business","Economics and Finance","Education",
                        "Political Science","Psychology and Health","Sociology and Criminology")
      
      format.row <- function(data){
        data <- data %>% 
          select(paper_id) %>% 
          left_join(paper_metadata %>% select(paper_id, field), by = "paper_id") %>% 
          group_by(field) %>%
          dplyr::summarize(n=n())
        
        total <- sum(data$n)
        field <- "Total"
        n <- paste0(total,"\n(100%)")
        data$n <- paste0(data$n,"\n(",
                         format.round(100*data$n/sum(data$n),1),
                         "%)")
        data <- rbind(data,data.frame(field,n))
        colnames <- data$field
        data <- data.frame(t(data[,-1]))
        #data <- data.frame(t(rbind(data,data.frame(field,n))[,-1]))
        colnames(data) <- colnames
        data[c(fields.order,"Total")]
      }
      
      # Papers with claims
      r1 <- format.row(status %>% filter(p1_delivery))
      # Papers eligible for reproduction
      r2 <- format.row(status %>% filter(RR))
      # Papers with multiple claims
      r3 <- format.row(status %>% filter(bushel))
      # Papers with single claim
      r4 <- format.row(status %>% filter(RR & !bushel))
      # Papers with data available
      r5 <- format.row(all_rr_attempts %>%
                         semi_join(status %>% filter(RR), by = "paper_id") %>%
                         filter(str_detect(type, "Source Data Reproduction")) %>%
                         select(paper_id) %>%
                         bind_rows(
                           pr_outcomes %>%
                             filter(!covid & OA_data_shared != "no") %>%
                             select(paper_id)
                         ) %>%
                         distinct())
      
      r6 <- format.row(all_rr_attempts %>%
        filter(field != "covid") %>%
        filter(str_detect(type, "Reproduction")) %>%
        select(paper_id, rr_id) %>%
        bind_rows(
          repro_outcomes %>% filter(!is_covid) %>%
            select(paper_id, rr_id)
        ) %>%
        select(paper_id) %>%
        distinct() %>%
        left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id")
      )
      
      # Papers with reproduction completed
      r7 <- format.row(repro_outcomes %>%
                         group_by(paper_id) %>%
                         dplyr::summarize(i=1))
      # Total reproductions of claims
      r8 <- format.row(repro_outcomes_inc_vor %>%
                         semi_join(status %>% filter(RR), by = "paper_id") %>% 
                         select(paper_id, claim_id, rr_id) %>% 
                         distinct())
      
      # Reproductions of unique claims
      # r9 <- format.row(repro_outcomes_inc_vor %>%
      #                    semi_join(status %>% filter(RR), by = "paper_id") %>%
      #                    select(paper_id, claim_id) %>%
      #                    distinct())
      r9 <- format.row(repro_outcomes_orig %>%
        filter(!is_covid) %>%
        left_join(extracted_claims %>% select(unique_claim_id, p1 = single_trace_equivalent),
                  by = c("claim_id" = "unique_claim_id")) %>%
        mutate(all_st = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        mutate(alt_id = ifelse(p1 | is.na(p1), all_st, claim_id)) %>%
        mutate(alt_id = ifelse(claim_id %in% c("0a3Z_mqy444", "a2Yx_3lxxq3", "a2Yx_single-trace"), claim_id, alt_id)) %>%
        select("paper_id","alt_id") %>%
        unique())
      
      table_2 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9)
      for (row in 1:nrow(table_2)){
        for (col in 1:ncol(table_2)){
          assign(paste0("table_2_",row,"_",col),
                 table_2[row,col])
        }
      }
      rm(r1,r2,r3,r4,r5,r6,r7,r8)
    }
    
    # Table S2
    {
      table_s2 <- all_rr_attempts %>%
        filter(field != "covid") %>%
        filter(str_detect(all_types, "Reproduction")) %>% 
        mutate(`Completed & reported` = ifelse(rr_id %in% repro_outcomes$rr_id, "Yes", "No")) %>%
        select(`Paper ID` = paper_id, `Project ID` = rr_id, `OSF` = project_guid, `Completed & reported`) %>%
        arrange(`Paper ID`)
      
      for (row in 1:nrow(table_s2)){
        for (col in 1:ncol(table_s2)){
          assign(paste0("table_s2_",row,"_",col),
                 table_s2[row,col])
        }
      }
    }
    
    # Table S3
    {
      years.order <- 2009:2018
      
      format.row <- function(data){
        data <- data %>% 
          select(paper_id) %>% 
          left_join(paper_metadata %>% select(paper_id, pub_year), by = "paper_id") %>% 
          group_by(pub_year) %>%
          dplyr::summarize(n=n())
        
        total <- sum(data$n)
        pub_year <- "Total"
        n <- paste0(total,"\n(100%)")
        data$n <- paste0(data$n,"\n(",
                         format.round(100*data$n/sum(data$n),1),
                         "%)")
        data <- rbind(data,data.frame(pub_year,n))
        colnames <- data$pub_year
        data <- data.frame(t(data[,-1]))
        #data <- data.frame(t(rbind(data,data.frame(field,n))[,-1]))
        colnames(data) <- colnames
        data[c(years.order,"Total")]
      }
      
      # Papers with claims
      r1 <- format.row(status %>% filter(p1_delivery))
      # Papers eligible for reproduction
      r2 <- format.row(status %>% filter(RR))
      # Papers with multiple claims
      r3 <- format.row(status %>% filter(bushel))
      # Papers with single claim
      r4 <- format.row(status %>% filter(RR & !bushel))
      # Papers with data available
      r5 <- format.row(all_rr_attempts %>%
                         semi_join(status %>% filter(RR), by = "paper_id") %>%
                         filter(str_detect(type, "Source Data Reproduction")) %>%
                         select(paper_id) %>%
                         bind_rows(
                           pr_outcomes %>%
                             filter(!covid & OA_data_shared != "no") %>%
                             select(paper_id)
                         ) %>%
                         distinct())
      # Papers with reproduction started
      # r6 <- format.row(all_rr_attempts  %>%
      #                    filter(str_detect(type, "Reproduction")) %>%
      #                    select(paper_id) %>%
      #                    distinct() %>%
      #                    semi_join(status %>% filter(RR), by = "paper_id"))
      r6 <- format.row(all_rr_attempts %>%
        filter(field != "covid") %>%
        filter(str_detect(type, "Reproduction")) %>%
        select(paper_id, rr_id) %>%
        bind_rows(
          repro_outcomes %>% filter(!is_covid) %>%
            select(paper_id, rr_id)
        ) %>%
        select(paper_id) %>%
        distinct() %>%
        left_join(paper_metadata %>% select(paper_id, year = pub_year), by = "paper_id") 
      )
      # Papers with reproduction completed
      r7 <- format.row(repro_outcomes %>%
                         group_by(paper_id) %>%
                         dplyr::summarize(i=1))
      # Total reproductions of claims
      r8 <- format.row(repro_outcomes_inc_vor %>%
                         semi_join(status %>% filter(RR), by = "paper_id") %>% 
                         select(paper_id, claim_id, rr_id) %>% 
                         distinct())
      
      # Reproductions of unique claims
      r9 <- format.row(repro_outcomes_orig %>%
                         filter(!is_covid) %>%
                         left_join(extracted_claims %>% select(unique_claim_id, p1 = single_trace_equivalent),
                                   by = c("claim_id" = "unique_claim_id")) %>%
                         mutate(all_st = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
                         mutate(alt_id = ifelse(p1 | is.na(p1), all_st, claim_id)) %>%
                         mutate(alt_id = ifelse(claim_id %in% c("0a3Z_mqy444", "a2Yx_3lxxq3", "a2Yx_single-trace"), claim_id, alt_id)) %>%
                         select("paper_id","alt_id") %>%
                         unique())
      
      
      table_s3 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9)
      for (row in 1:nrow(table_s3)){
        for (col in 1:ncol(table_s3)){
          assign(paste0("table_s3_",row,"_",col),
                 table_s3[row,col])
        }
      }
      rm(r1,r2,r3,r4,r5,r6,r7,r8)
    }
    
    # Table S4
    {
      table_s4 <- pr_outcomes %>% 
        filter(!covid) %>% 
        rename(d = OA_data_shared, c = OA_code_shared) %>% 
        mutate(
          pr = case_when(
            d == "available_online" & c != "no" ~ "Open data, code available",
            (restricted_data == "Yes_all" | restricted_data == "Yes_some") & c != "no" ~ "Data restricted, code available",
            d == "shared_on_request" & c != "no" ~ "Data shared directly, code available",
            d == "available_online" & c == "no" ~ "Open data, code unavailable",
            (restricted_data == "Yes_all" | restricted_data == "Yes_some") & c == "no" ~ "Data restricted, code unavailable",
            d == "shared_on_request" & c == "no" ~ "Data shared directly, code unavailable",
            d == "no" & c != "no" ~ "Data unavailable, code available",
            d == "no" & c == "no" ~ "Neither data nor code available"
          )
        ) %>% 
        left_join(paper_metadata %>% select(paper_id, journal = publication_standard, field = COS_pub_category), by = "paper_id") %>% 
        select(paper_id, field, journal, pr) %>% 
        group_by(journal, pr, field) %>% 
        dplyr::summarize(t = n()) %>% 
        pivot_wider(names_from = pr, values_from = t) %>% 
        ungroup() %>% 
        mutate(across(everything(), function(x) ifelse(is.na(x), 0, x))) %>% 
        select(field, journal, "Open data, code available", "Data restricted, code available", "Data shared directly, code available",
               "Open data, code unavailable", "Data restricted, code unavailable",
               "Data shared directly, code unavailable", "Data unavailable, code available", "Neither data nor code available") %>% 
        mutate(total = select(., -c(field, journal)) %>% apply(1, sum)) %>% 
        arrange(field) %>% 
        select(-field)
      
      for (row in 1:nrow(table_s4)){
        for (col in 1:ncol(table_s4)){
          assign(paste0("table_s4_",row,"_",col),
                 table_s4[row,col])
        }
      }
    }
    
    # Table S5
    {
      table_s5_out <- repro_outcomes %>%
        filter(!is_covid & repro_version_of_record == "T") %>% 
        mutate(
          repro_outcome_overall = case_match(
            repro_outcome_overall,
            "not attemptable" ~ "not",
            "push button" ~ "precise",
            "none" ~ "excluded",
            .default = repro_outcome_overall
          )
        ) %>% 
        group_by(paper_id) %>%
        dplyr::summarize(
          precise = all(repro_outcome_overall == "precise"),
          approximate = any(repro_outcome_overall == "approximate") & !any(repro_outcome_overall == "not"),
          not = any(repro_outcome_overall == "not"),
          excluded = any(repro_outcome_overall == "excluded")
        ) %>% 
        pivot_longer(
          cols = -paper_id,
          names_to = "outcome",
          values_to = "value"
        ) %>% 
        filter(value) %>% 
        select(-value)
      
        table_s5 <- table_s5_out %>% 
          bind_rows(
          pr_outcomes %>% filter(!covid) %>% 
            anti_join(table_s5_out, by = "paper_id") %>% 
            select(paper_id) %>% mutate(outcome = "not attempted")
        ) %>% 
        left_join(paper_metadata %>% select(paper_id, journal = publication_standard, field = COS_pub_category), by = "paper_id") %>% 
        select(paper_id, field, journal, outcome) %>% 
        group_by(journal, outcome, field) %>% 
        dplyr::summarize(t = n()) %>% 
        pivot_wider(names_from = outcome, values_from = t) %>% 
        ungroup() %>% 
        mutate(across(everything(), function(x) ifelse(is.na(x), 0, x))) %>% 
        mutate(total = select(., -c(field, journal)) %>% apply(1, sum)) %>% 
        arrange(field) %>% 
        select(-field) %>% 
          select(` ` = journal,`Not attempted` = `not attempted`, Excluded = excluded, Not = not,
               Approximate = approximate, Precise = precise, Total = total) %>% 
        as.data.frame()
        
      rm(table_s5_out)
      
      for (row in 1:nrow(table_s5)){
        for (col in 1:ncol(table_s5)){
          assign(paste0("table_s5_",row,"_",col),
                 table_s5[row,col])
        }
      }
    }
    
    # Abstract
    {
      n_papers <- length(unique(pr_outcomes$paper_id))
      
      n_claims <- length(unique(repro_outcomes$claim_id))
      
      n_journals <- length(unique(
        paper_metadata[paper_metadata$paper_id %in% pr_outcomes$paper_id,]$publication_standard
      ))
      
      # n_papers_passed_process_repro_assess <- sum(pr_outcomes$process_reproducible=="Yes")
      # 
      # p_papers_passed_process_repro_assess <- format.text.percent(n_papers_passed_process_repro_assess,nrow(pr_outcomes))
      # 
      # p_data_available <- format.text.percent(sum(pr_outcomes$data_available=="Yes"),
      #                                         nrow(pr_outcomes))
      
      # for (i in 2009:2018){
      #   assign(paste0("p_data_available_",i),{
      #     format.text.percent(sum(pr_outcomes_modified[pr_outcomes_modified$pub_year==i,]$data_available=="Yes"),
      #                         nrow(pr_outcomes_modified[pr_outcomes_modified$pub_year==i,]))
      #   })
      # }
      
      # df.fields <- do.call(rbind,lapply(na.omit(unique(pr_outcomes_modified$COS_pub_category)),function(field){
      #   p_process_reproducible <- sum(pr_outcomes_modified[pr_outcomes_modified$COS_pub_category==field,]$data_available=="Yes")/
      #     nrow(pr_outcomes_modified[pr_outcomes_modified$COS_pub_category==field,])
      #   formatted_text_process_reproducible <- 
      #     format.text.percent(sum(pr_outcomes_modified[pr_outcomes_modified$COS_pub_category==field,]$data_available=="Yes"),
      #                         nrow(pr_outcomes_modified[pr_outcomes_modified$COS_pub_category==field,]))
      #   
      #   data.frame(field,p_process_reproducible,formatted_text_process_reproducible)
      # }))
      # 
      # field_most_passing_process_repro <- df.fields$field[which.max(df.fields$p_process_reproducible)]
      # 
      # p_field_most_passing_process_repro <- df.fields$formatted_text_process_reproducible[which.max(df.fields$p_process_reproducible)]
      # 
      # field_least_passing_process_repro <- df.fields$field[which.min(df.fields$p_process_reproducible)]
      # 
      # p_field_least_passing_process_repro <- df.fields$formatted_text_process_reproducible[which.min(df.fields$p_process_reproducible)]
      
    }
    
    # Results: Process reproducibility
    {
      n_papers_assess_process_repro <- nrow(pr_outcomes_modified)
      
      p_papers_assess_process_repro <- format.text.percent(nrow(pr_outcomes_modified),
                                                           nrow(pr_outcomes_modified))
      
      n_papers_data_available <- sum(pr_outcomes_modified$data_available_or_shared==TRUE)
      p_papers_data_available <- format.text.percent(n_papers_data_available,n_papers_assess_process_repro)
      p_claims_data_available <- p_papers_data_available # Identical because process repro was was assessed one claim per paper
      
      n_papers_code_available <- sum(pr_outcomes_modified$code_available_or_shared==TRUE)
      p_papers_code_available <- format.text.percent(n_papers_code_available,n_papers_assess_process_repro)
      
      n_papers_data_unavailable <- sum(pr_outcomes_modified$data_available_or_shared==FALSE)
      p_papers_data_unavailable <- format.text.percent(n_papers_data_unavailable,n_papers_assess_process_repro)
      
      n_papers_code_unavailable <- sum(pr_outcomes_modified$code_available_or_shared==FALSE)
      p_papers_code_unavailable <- format.text.percent(n_papers_code_unavailable,n_papers_assess_process_repro)
      
      n_papers_data_and_code_available <- sum(pr_outcomes_modified$data_available_or_shared==TRUE & pr_outcomes_modified$code_available_or_shared==TRUE)
      p_papers_data_and_code_available <- format.text.percent(n_papers_data_and_code_available,n_papers_assess_process_repro)
      
      n_papers_data_or_code_available <- sum(pr_outcomes_modified$data_available_or_shared==TRUE | pr_outcomes_modified$code_available_or_shared==TRUE)
      p_papers_data_or_code_available <- format.text.percent(n_papers_data_or_code_available,n_papers_assess_process_repro)
      
      n_papers_data_only_available <- sum(pr_outcomes_modified$data_available_or_shared==TRUE & pr_outcomes_modified$code_available_or_shared==FALSE)
      p_papers_data_only_available <- format.text.percent(n_papers_data_only_available,n_papers_assess_process_repro)
      
      n_papers_code_only_available <- sum(pr_outcomes_modified$data_available_or_shared==FALSE & pr_outcomes_modified$code_available_or_shared==TRUE)
      p_papers_code_only_available <- format.text.percent(n_papers_code_only_available,n_papers_assess_process_repro)
      
      n_papers_neither_code_nor_data_available <- sum(pr_outcomes_modified$data_available_or_shared==FALSE & pr_outcomes_modified$code_available_or_shared==FALSE)
      p_papers_neither_code_nor_data_available <- format.text.percent(n_papers_neither_code_nor_data_available,n_papers_assess_process_repro)
      
    }
    
    # Results: Process reproducibility by year of original publication
    {
      rho <- SpearmanRho(x=as.numeric(pr_outcomes_modified$pub_year),
               y=as.numeric(pr_outcomes_modified$data_available_or_shared==TRUE),
               conf.level=.95)
      rho_data_avail_v_year <- format.text.CI(rho[1],rho[2],rho[3],digits=2)
      
      rho <- SpearmanRho(x=as.numeric(pr_outcomes_modified$pub_year),
                         y=as.numeric(pr_outcomes_modified$code_available_or_shared==TRUE),
                         conf.level=.95)
      rho_code_avail_v_year <- format.text.CI(rho[1],rho[2],rho[3],digits=2)
      
      rho <- SpearmanRho(x=as.numeric(pr_outcomes_modified$pub_year),
                         y=as.numeric(pr_outcomes_modified$code_available_or_shared==TRUE & pr_outcomes_modified$data_available_or_shared==TRUE ),
                         conf.level=.95)
      rho_code_and_data_avail_v_year <- format.text.CI(rho[1],rho[2],rho[3],digits=2)
      
      rho <- SpearmanRho(x=as.numeric(pr_outcomes_modified[pr_outcomes_modified$data_available_or_shared==TRUE | pr_outcomes_modified$code_available_or_shared==TRUE,]$pub_year),
                         y=as.numeric(pr_outcomes_modified[pr_outcomes_modified$data_available_or_shared==TRUE | pr_outcomes_modified$code_available_or_shared==TRUE,]$code_available_or_shared==TRUE &
                                        pr_outcomes_modified[pr_outcomes_modified$data_available_or_shared==TRUE | pr_outcomes_modified$code_available_or_shared==TRUE,]$data_available_or_shared==TRUE ),
                         conf.level=.95)
      rho_code_and_data_avail_v_year_among_either <- format.text.CI(rho[1],rho[2],rho[3],digits=2)
      
      rm(rho)
      
      p_papers_data_or_code_available_among_either<- 
        format.text.percent(n_papers_data_and_code_available,n_papers_data_or_code_available)
      
    }
    
    # Process reproducibility by discipline
    {
      n_papers_data_available_econ <- sum(pr_outcomes_modified[pr_outcomes_modified$field=="Economics and Finance",]$data_available_or_shared)
      n_papers_econ <- sum(pr_outcomes_modified$field=="Economics and Finance")
      p_papers_data_available_econ <- format.text.percent(n_papers_data_available_econ,n_papers_econ)
      
      n_papers_data_available_polisci <- sum(pr_outcomes_modified[pr_outcomes_modified$field=="Political Science",]$data_available_or_shared)
      n_papers_polisci <- sum(pr_outcomes_modified$field=="Political Science")
      p_papers_data_available_polisci <- format.text.percent(n_papers_data_available_polisci,n_papers_polisci)
      
      n_papers_data_available_non_econ_polisci <- sum(pr_outcomes_modified[pr_outcomes_modified$field!="Political Science" & pr_outcomes_modified$field!="Economics and Finance",]$data_available_or_shared)
      n_papers_non_econ_polisci <- sum(pr_outcomes_modified$field!="Political Science" & pr_outcomes_modified$field!="Economics and Finance")
      p_papers_data_available_non_econ_polisci <- format.text.percent(n_papers_data_available_non_econ_polisci,n_papers_non_econ_polisci)
      
      n_papers_data_available_edu <- sum(pr_outcomes_modified[pr_outcomes_modified$field=="Education",]$data_available_or_shared)
      n_papers_edu <- sum(pr_outcomes_modified$field=="Education")
      p_papers_data_available_edu <- format.text.percent(n_papers_data_available_edu,n_papers_edu)
      
      n_papers_open_and_code_data_econ_polisci <- 
        sum(pr_outcomes_modified[pr_outcomes_modified$field=="Political Science" | pr_outcomes_modified$field=="Economics and Finance",]$data_shared_type=="available online" &
            pr_outcomes_modified[pr_outcomes_modified$field=="Political Science" | pr_outcomes_modified$field=="Economics and Finance",]$code_shared_type=="available online")
      n_papers_econ_polisci <- sum(pr_outcomes_modified$field=="Political Science" | pr_outcomes_modified$field=="Economics and Finance")
      p_papers_open_data_and_code_econ_polisci <- format.text.percent(n_papers_open_and_code_data_econ_polisci,n_papers_econ_polisci)
      
      n_papers_open_data_and_code_non_econ_polisci <- 
        sum(pr_outcomes_modified[pr_outcomes_modified$field!="Political Science" & pr_outcomes_modified$field!="Economics and Finance",]$data_shared_type=="available online" &
              pr_outcomes_modified[pr_outcomes_modified$field!="Political Science" & pr_outcomes_modified$field!="Economics and Finance",]$code_shared_type=="available online")
      n_papers_non_econ_polisci <- sum(pr_outcomes_modified$field=="Political Science" | pr_outcomes_modified$field=="Economics and Finance")
      p_papers_open_data_and_code_non_econ_polisci <- format.text.percent(n_papers_open_data_and_code_non_econ_polisci,n_papers_non_econ_polisci)
      
      
      p_ratio_papers_open_data_and_code_econ_polisci_vs_other <- bootstrap.clust(data=pr_outcomes_modified,FUN=function(x) {
        n_papers_open_data_and_code_econ_polisci <- 
          sum(x[x$field=="Political Science" | x$field=="Economics and Finance",]$data_shared_type=="available online" &
                x[x$field=="Political Science" | x$field=="Economics and Finance",]$code_shared_type=="available online")
        n_papers_econ_polisci <- sum(x$field=="Political Science" | x$field=="Economics and Finance")
        n_papers_open_data_and_code_non_econ_polisci <- 
          sum(x[x$field!="Political Science" & x$field!="Economics and Finance",]$data_shared_type=="available online" &
                x[x$field!="Political Science" & x$field!="Economics and Finance",]$code_shared_type=="available online")
        n_papers_non_econ_polisci <- sum(x$field=="Political Science" | x$field=="Economics and Finance")
        
        
        (n_papers_open_data_and_code_econ_polisci/n_papers_econ_polisci)/
          (n_papers_open_data_and_code_non_econ_polisci/n_papers_non_econ_polisci)
      },clustervar="paper_id",iters=iters)$formatted.text
      
      n_min_pr_edu_by_year <- min(table(pr_outcomes_modified[pr_outcomes_modified$field=="Education",]$pub_year))
      n_max_pr_edu_by_year <- max(table(pr_outcomes_modified[pr_outcomes_modified$field=="Education",]$pub_year))
    
      exp_fields <- paper_metadata %>% 
        select(paper_id, pub = publication_standard, field = COS_pub_expanded) %>% 
        mutate(
          field2 = case_when(
            str_detect(pub, "financ|Financ") ~ "finance",
            str_detect(pub, "organization|Organization") ~ "org. behavior",
            !str_detect(pub, "organization|Organization") & field == "marketing/org behavior"  ~ "marketing",
            !str_detect(pub, "financ|Financ") & field == "economics"  ~ "economics",
            .default = field
          )
        ) %>%
        mutate(field2 = str_to_title(field2)) %>%
        select(paper_id,field2)
      pr_outcomes_subfields <- merge(pr_outcomes_modified,exp_fields,by="paper_id",all.x=TRUE,all.y=FALSE)
      
      n_min_pr_edu_alone_by_year <- min(table(pr_outcomes_subfields[pr_outcomes_subfields$field2=="Education",]$pub_year))
      n_max_pr_edu_alone_by_year <- max(table(pr_outcomes_subfields[pr_outcomes_subfields$field2=="Education",]$pub_year))
      
      n_pr_crim_PA_alone_each_year <- max(table(pr_outcomes_subfields[pr_outcomes_subfields$field2=="Criminology"| pr_outcomes_subfields$field2=="Public Administration ",]$pub_year))
      
      
      p_papers_data_available_econ_sub <- format.text.percent(
        sum(pr_outcomes_subfields[pr_outcomes_subfields$field2=="Economics",]$data_available_or_shared),
        sum(pr_outcomes_subfields$field2=="Economics"))
      p_papers_data_available_polisci_sub <- format.text.percent(
        sum(pr_outcomes_subfields[pr_outcomes_subfields$field2=="Political Science",]$data_available_or_shared),
        sum(pr_outcomes_subfields$field2=="Political Science"))
      p_papers_data_available_fin_sub <- format.text.percent(
        sum(pr_outcomes_subfields[pr_outcomes_subfields$field2=="Finance",]$data_available_or_shared),
        sum(pr_outcomes_subfields$field2=="Finance"))
      p_papers_data_available_padmin_sub <- format.text.percent(
        sum(pr_outcomes_subfields[pr_outcomes_subfields$field2=="Public Administration",]$data_available_or_shared),
        sum(pr_outcomes_subfields$field2=="Public Administration"))

      
      rm(exp_fields,pr_outcomes_subfields)

      
    }
    
    # Assessing outcome reproducibility
    {
      repro_outcomes_OR <- repro_outcomes %>% filter(!repro_outcome_overall=="none")
      
      n_papers_OR_at_least_one <- length(unique(repro_outcomes$paper_id))
      n_papers_at_exc_no_elig <- format.round(n_papers_OR_at_least_one-sum(repro_outcomes_OR$weight),1)
      
      n_claims_OR_at_least_one <- length(unique(repro_outcomes$claim_id))
      n_claims_at_exc_no_elig <- n_claims_OR_at_least_one-nrow(repro_outcomes_OR)
      
      n_papers_not_attemptable <- repro_outcomes_orig %>%
        filter(!is_covid & repro_version_of_record == "T") %>%
        filter(repro_outcome_overall == "not attemptable") %>%
        nrow()
      
      n_claims_not_attemptable <- repro_outcomes_orig %>%
        filter(!is_covid & repro_version_of_record == "T") %>%
        filter(repro_outcome_overall == "not attemptable") %>%
        nrow()
      
      paper_ids_NA <- repro_outcomes_orig %>%
        filter(!is_covid & repro_version_of_record == "T") %>%
        filter(repro_outcome_overall == "not attemptable") %>%
        select(paper_id)
      
      n_papers_OR <- sum(repro_outcomes_OR$weight)
      
      n_claims_OR <- length(unique(repro_outcomes_OR$claim_id))
      
      claims_per_paper <- repro_outcomes_OR %>%
        group_by(paper_id) %>%
        dplyr::summarise(count = n(),
                         multiple = as.numeric(n()>1))
      
      n_papers_OR_multiple_claims <- sum(claims_per_paper$multiple)
      p_papers_OR_multiple_claims <- paste0(format.round(100*n_papers_OR_multiple_claims/n_papers_OR,1),"%")
      
      mean_claims_per_paper <- format.round(mean(claims_per_paper$count),1)
      SD_claims_per_paper <- format.round(SD(claims_per_paper$count),1)
      range_claims_per_paper <- paste0(min(claims_per_paper$count),"-",max(claims_per_paper$count))
      
      n_papers_OR_data_available <- 
        nrow(all_rr_attempts %>%
               semi_join(status %>% filter(RR), by = "paper_id") %>%
               filter(str_detect(all_types, "Source Data Reproduction")) %>%
               select(paper_id) %>%
               bind_rows(
                 pr_outcomes %>%
                   filter(!covid & OA_data_shared != "no") %>%
                   select(paper_id)
                 ) %>%
               distinct())
      
      rm(claims_per_paper)
        
    }
    
    # Outcome reproducibility assessments in comparison with the sampling frame
    {
      n_pr_papers_author_gen_data <- nrow(all_rr_attempts %>%
                                            semi_join(status %>% filter(RR), by = "paper_id") %>%
                                            filter(str_detect(all_types, "Source Data Reproduction")) %>%
                                            select(paper_id) %>%
                                            bind_rows(
                                              pr_outcomes %>%
                                                filter(!covid & OA_data_shared != "no") %>%
                                                select(paper_id)
                                            ) %>%
                                            distinct())
      
      repro_outcomes_OR <- repro_outcomes %>% filter(!repro_outcome_overall=="none")
      
      #n_papers_completed_outcome_test <- length(unique(repro_outcomes_OR$paper_id))
      #p_papers_completed_outcome_test <- format.text.percent(n_papers_completed_outcome_test,n_papers_OR_data_available)
      
      data <- status %>% filter(RR) %>% 
        select(paper_id) %>% 
        left_join(paper_metadata %>% select(paper_id, COS_pub_category), by = "paper_id") %>% 
        mutate(econ_polisci = COS_pub_category=="economics and finance"|COS_pub_category=="political science")
      
      p_sample_econ_polisci_eligible <- paste0(format.round(100*sum(data$econ_polisci)/nrow(data),1),"%")
      
      p_sample_not_econ_polisci_eligible <- paste0(format.round(100*sum(1-data$econ_polisci)/nrow(data),1),"%")

      data <- all_rr_attempts %>%
        semi_join(status %>% filter(RR), by = "paper_id") %>%
        filter(str_detect(all_types, "Source Data Reproduction")) %>%
        select(paper_id) %>%
        bind_rows(
          pr_outcomes %>%
            filter(!covid & OA_data_shared != "no") %>%
            select(paper_id)
        ) %>%
        distinct() %>%
        select(paper_id) %>% 
        left_join(paper_metadata %>% select(paper_id, COS_pub_category), by = "paper_id") %>% 
        mutate(econ_polisci = COS_pub_category=="economics and finance"|COS_pub_category=="political science")
      
      p_sample_econ_polisci_data_available <- paste0(format.round(100*sum(data$econ_polisci)/nrow(data),1),"%")
      
      p_sample_not_econ_polisci_available <- paste0(format.round(100*sum(1-data$econ_polisci)/nrow(data),1),"%")
      
      data <- status %>% filter(RR) %>% 
        select(paper_id) %>% 
        left_join(paper_metadata %>% select(paper_id, pub_year), by = "paper_id") %>% 
        mutate(pub_year_2014_to_2018 = pub_year>=2014)
      
      p_sample_pub_year_2014_to_2018_eligible <- paste0(format.round(100*sum(data$pub_year_2014_to_2018)/nrow(data),1),"%")
      
      p_sample_pub_year_2009_to_2013_eligible <- paste0(format.round(100*sum(1-data$pub_year_2014_to_2018)/nrow(data),1),"%")
      
      data <- all_rr_attempts %>%
        semi_join(status %>% filter(RR), by = "paper_id") %>%
        filter(str_detect(all_types, "Source Data Reproduction")) %>%
        select(paper_id) %>%
        bind_rows(
          pr_outcomes %>%
            filter(!covid & OA_data_shared != "no") %>%
            select(paper_id)
        ) %>%
        distinct() %>%
        select(paper_id) %>% 
        left_join(paper_metadata %>% select(paper_id, COS_pub_category,pub_year), by = "paper_id") %>% 
        mutate(pub_year_2014_to_2018 = pub_year>=2014)
      
      p_sample_pub_year_2014_to_2018_data_available <- paste0(format.round(100*sum(data$pub_year_2014_to_2018)/nrow(data),1),"%")
      
      p_sample_pub_year_2009_to_2013_data_available <- paste0(format.round(100*sum(1-data$pub_year_2014_to_2018)/nrow(data),1),"%")
      
    }
    
    # Outcome reproducibility assessments in comparison with the sample
    {
      repro_outcomes_OR <- repro_outcomes %>% filter(!repro_outcome_overall=="none")
      
      n_claims_exc_no_elig <- nrow(repro_outcomes)-nrow(repro_outcomes_OR)
      
      n_papers_OR_added_SDR <- format.round(
        sum(repro_outcomes_OR %>%
        filter(repro_type=="Source Data Reproduction") %>%
        dplyr::summarise(n = sum(weight)) %>%
        pull(n)),1)

      n_papers_OR_of_all_precise <- format.round(sum(repro_outcomes_expanded$weight *
                                                       (repro_outcomes_expanded$repro_outcome_overall=="precise" |
                                                          repro_outcomes_expanded$repro_outcome_overall=="push button")),1)

      p_papers_OR_of_all_precise <- cw.proportion(
        repro_outcomes_expanded$repro_outcome_overall=="precise" |
          repro_outcomes_expanded$repro_outcome_overall=="push button",
        weights=repro_outcomes_expanded$weight,
        clusters = repro_outcomes_expanded$paper_id,iters)$formatted.text
      
      n_papers_OR_approx_or_precise <- format.round(
        sum(repro_outcomes_OR$weight *
              (repro_outcomes_OR$repro_outcome_overall=="approximate" |
                 repro_outcomes_OR$repro_outcome_overall=="precise" | 
                 repro_outcomes_OR$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_approx_or_precise <- cw.proportion(
        repro_outcomes_OR$repro_outcome_overall=="approximate" | 
          repro_outcomes_OR$repro_outcome_overall=="precise" | 
          repro_outcomes_OR$repro_outcome_overall=="push button",
        weights=repro_outcomes_OR$weight,
        clusters = repro_outcomes_OR$paper_id,iters)$formatted.text
      
      n_papers_OR_precise <- format.round(sum(repro_outcomes_OR$weight *
                                                       (repro_outcomes_OR$repro_outcome_overall=="precise" |
                                                          repro_outcomes_OR$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_precise <- cw.proportion(
        repro_outcomes_OR$repro_outcome_overall=="precise" | 
          repro_outcomes_OR$repro_outcome_overall=="push button",
        weights=repro_outcomes_OR$weight,
        clusters = repro_outcomes_OR$paper_id,iters)$formatted.text
      
      n_papers_OR_precise_pushbutton <- format.round(sum(repro_outcomes_OR$weight *
                                                ((repro_outcomes_OR$repro_outcome_overall=="precise" | 
                                                    repro_outcomes_OR$repro_outcome_overall=="push button") & 
                                                   repro_outcomes_OR$repro_type=="Push Button Reproduction")),1)
      
      p_papers_OR_precise_pushbutton <- cw.proportion(
        (repro_outcomes_OR$repro_outcome_overall=="precise" |
           repro_outcomes_OR$repro_outcome_overall=="push button") &
          repro_outcomes_OR$repro_type=="Push Button Reproduction",
        weights=repro_outcomes_OR$weight,
        clusters = repro_outcomes_OR$paper_id,iters)$formatted.text
      
      repro_outcomes_expanded_no_none <- repro_outcomes_expanded[repro_outcomes_expanded$repro_outcome_overall!="none",]
      
      n_papers_of_all_exc_no_elig <- format.round(
        sum(repro_outcomes_expanded$weight)-sum(repro_outcomes_expanded_no_none$weight),1)
      
      n_claims_of_all_exc_no_elig <- nrow(repro_outcomes_expanded)-nrow(repro_outcomes_expanded_no_none)
      
      n_papers_of_all_denom <- format.round(sum(repro_outcomes_expanded_no_none$weight),1)
      
      n_papers_OR_of_all_approx_or_precise <- format.round(sum(repro_outcomes_expanded_no_none$weight *
                                                                 (repro_outcomes_expanded_no_none$repro_outcome_overall=="approximate" |
                                                                    repro_outcomes_expanded_no_none$repro_outcome_overall=="precise" |
                                                                    repro_outcomes_expanded_no_none$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_of_all_approx_or_precise <- cw.proportion(
        repro_outcomes_expanded_no_none$repro_outcome_overall=="approximate" |
          repro_outcomes_expanded_no_none$repro_outcome_overall=="precise" |
          repro_outcomes_expanded_no_none$repro_outcome_overall=="push button",
        weights=repro_outcomes_expanded_no_none$weight,
        clusters = repro_outcomes_expanded_no_none$paper_id,iters)$formatted.text
      
      repro_outcomes_dc <- repro_outcomes_OR[repro_outcomes_OR$repro_type_consolidated=="Data and code available",]
      repro_outcomes_dc <- repro_outcomes_dc
      repro_outcomes_dc <- repro_outcomes_dc %>%
        filter(!repro_outcome_overall=="none")
      
      n_papers_OR_data_and_code <- format.round(sum(repro_outcomes_dc$weight),1)
      
      n_papers_OR_data_and_code_approx_or_precise <- format.round(
        sum(repro_outcomes_dc$weight *
              (repro_outcomes_dc$repro_outcome_overall=="approximate" |
                 repro_outcomes_dc$repro_outcome_overall=="precise" | 
                 repro_outcomes_dc$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_data_and_code_approx_or_precise <- cw.proportion(
        repro_outcomes_dc$repro_outcome_overall=="approximate" | 
          repro_outcomes_dc$repro_outcome_overall=="precise" | 
          repro_outcomes_dc$repro_outcome_overall=="push button",
        weights=repro_outcomes_dc$weight,
        clusters = repro_outcomes_dc$paper_id,iters)$formatted.text
      
      n_papers_OR_data_and_code_precise <- format.round(sum(repro_outcomes_dc$weight *
                                                (repro_outcomes_dc$repro_outcome_overall=="precise" |
                                                   repro_outcomes_dc$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_data_and_code_precise <- cw.proportion(
        repro_outcomes_dc$repro_outcome_overall=="precise" | 
          repro_outcomes_dc$repro_outcome_overall=="push button",
        weights=repro_outcomes_dc$weight,
        clusters = repro_outcomes_dc$paper_id,iters)$formatted.text
      
      n_papers_OR_data_and_code_pushbutton <- format.round(sum(repro_outcomes_dc$weight *
                                                             (repro_outcomes_dc$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_data_and_code_pushbutton <- cw.proportion(
        repro_outcomes_dc$repro_outcome_overall=="push button",
        weights=repro_outcomes_dc$weight,
        clusters = repro_outcomes_dc$paper_id,iters)$formatted.text
      
      repro_outcomes_do <- repro_outcomes_OR[repro_outcomes_OR$repro_type_consolidated=="Only data available",]

      repro_outcomes_do <- repro_outcomes_do %>%
        filter(!repro_outcome_overall=="none")
      
      n_papers_OR_data_only <- format.round(sum(repro_outcomes_do$weight),1)
      
      n_papers_OR_data_only_approx_or_precise <- format.round(
        sum(repro_outcomes_do$weight *
              (repro_outcomes_do$repro_outcome_overall=="approximate" |
                 repro_outcomes_do$repro_outcome_overall=="precise" | 
                 repro_outcomes_do$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_data_only_approx_or_precise <- cw.proportion(
        repro_outcomes_do$repro_outcome_overall=="approximate" | 
          repro_outcomes_do$repro_outcome_overall=="precise" | 
          repro_outcomes_do$repro_outcome_overall=="push button",
        weights=repro_outcomes_do$weight,
        clusters = repro_outcomes_do$paper_id,iters)$formatted.text
      
      n_papers_OR_data_only_precise <- format.round(sum(repro_outcomes_do$weight *
                                                              (repro_outcomes_do$repro_outcome_overall=="precise" |
                                                                 repro_outcomes_do$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_data_only_precise <- cw.proportion(
        repro_outcomes_do$repro_outcome_overall=="precise" | 
          repro_outcomes_do$repro_outcome_overall=="push button",
        weights=repro_outcomes_do$weight,
        clusters = repro_outcomes_do$paper_id,iters)$formatted.text
      
      n_papers_OR_data_only_pushbutton <- format.round(sum(repro_outcomes_do$weight *
                                                          (repro_outcomes_do$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_data_only_pushbutton <- cw.proportion(
          repro_outcomes_do$repro_outcome_overall=="push button",
        weights=repro_outcomes_do$weight,
        clusters = repro_outcomes_do$paper_id,iters)$formatted.text
      
      repro_outcomes_sd <- repro_outcomes_OR[repro_outcomes_OR$repro_type_consolidated=="Data reconstructed from source",]
      repro_outcomes_sd <- repro_outcomes_sd %>%
        filter(!repro_outcome_overall=="none")
      
      n_papers_OR_source_data_of_all <- all_rr_attempts %>%
        filter(field != "covid") %>%
        filter(str_detect(type, "Source Data Reproduction")) %>%
        anti_join(
          pr_outcomes %>%
            filter(!covid) %>%
            filter(OA_data_shared != "no"),
          by = "paper_id"
        ) %>%
        count(paper_id) %>%
        nrow()
      
      n_papers_OR_source_data <- format.round(sum(repro_outcomes_sd$weight),1)
      
      n_papers_OR_source_data_approx_or_precise <- format.round(
        sum(repro_outcomes_sd$weight *
              (repro_outcomes_sd$repro_outcome_overall=="approximate" |
                 repro_outcomes_sd$repro_outcome_overall=="precise" | 
                 repro_outcomes_sd$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_source_data_approx_or_precise <- cw.proportion(
        repro_outcomes_sd$repro_outcome_overall=="approximate" | 
          repro_outcomes_sd$repro_outcome_overall=="precise" | 
          repro_outcomes_sd$repro_outcome_overall=="push button",
        weights=repro_outcomes_sd$weight,
        clusters = repro_outcomes_sd$paper_id,iters)$formatted.text
      
      n_papers_OR_source_data_precise <- format.round(sum(repro_outcomes_sd$weight *
                                                          (repro_outcomes_sd$repro_outcome_overall=="precise" |
                                                             repro_outcomes_sd$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_source_data_precise <- cw.proportion(
        repro_outcomes_sd$repro_outcome_overall=="precise" | 
          repro_outcomes_sd$repro_outcome_overall=="push button",
        weights=repro_outcomes_sd$weight,
        clusters = repro_outcomes_sd$paper_id,iters)$formatted.text
      
      n_papers_OR_source_data_pushbutton <- format.round(sum(repro_outcomes_sd$weight *
                                                             (repro_outcomes_sd$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_source_data_pushbutton <- cw.proportion(
        repro_outcomes_sd$repro_outcome_overall=="push button",
        weights=repro_outcomes_sd$weight,
        clusters = repro_outcomes_sd$paper_id,iters)$formatted.text
      
      as.numeric(n_papers_OR_data_and_code) + as.numeric(n_papers_OR_source_data)+as.numeric(n_papers_OR_data_only)
      
      n_papers_analyst_report_success <- repro_outcomes %>%
        rename(analyst = repro_interpret_supported) %>%
        mutate(analyst = ifelse(analyst %in% c("complicated", "undefined"), NA, analyst)) %>%
        drop_na(analyst) %>%
        group_by(paper_id) %>%
        dplyr::summarize(all_yes = all(analyst == "yes")) %>%
        filter(all_yes) %>%
        nrow()
      
      n_papers_analyst_report <- repro_outcomes %>%
        rename(analyst = repro_interpret_supported) %>%
        mutate(analyst = ifelse(analyst %in% c("complicated", "undefined"), NA, analyst)) %>%
        drop_na(analyst) %>%
        group_by(paper_id) %>%
        dplyr::summarize(all_yes = all(analyst == "yes")) %>%
        nrow()
      
      p_papers_analyst_report_success <- 
        format.text.percent(n_papers_analyst_report_success,n_papers_analyst_report)
      
      n_claims_analyst_report_success <- repro_outcomes %>%
        rename(analyst = repro_interpret_supported) %>%
        mutate(analyst = ifelse(analyst %in% c("complicated", "undefined"), NA, analyst)) %>%
        drop_na(analyst) %>%
        group_by(claim_id) %>%
        dplyr::summarize(all_yes = all(analyst == "yes")) %>%
        filter(all_yes) %>%
        nrow()
      
      n_claims_analyst_report <- repro_outcomes %>%
        rename(analyst = repro_interpret_supported) %>%
        mutate(analyst = ifelse(analyst %in% c("complicated", "undefined"), NA, analyst)) %>%
        drop_na(analyst) %>%
        group_by(claim_id) %>%
        dplyr::summarize(all_yes = all(analyst == "yes")) %>%
        nrow()
      
      p_claims_analyst_report_success <-
        format.text.percent(n_claims_analyst_report_success,n_claims_analyst_report)
      

    }
    
    # Implied overall outcome reproducibility
    {
      p_papers_OR_approx_or_precise_overall <- 
        bootstrap.clust(data=pr_outcomes,
                      FUN=function(x) {
                        paper_ids <- x$paper_id
                        pr_outcomes_modified_int <- pr_outcomes_modified[pr_outcomes_modified$paper_id %in% paper_ids,]

                        p_data <- sum(pr_outcomes_modified_int$data_available_or_shared==TRUE)/
                          nrow(pr_outcomes_modified_int)
                        
                        repro_outcomes_OR <- repro_outcomes[repro_outcomes$paper_id %in% paper_ids,] %>%
                          filter(!repro_outcome_overall=="none")
                        
                        p_papers_OR_approx_or_precise <- sum(repro_outcomes_OR$weight*
                              (repro_outcomes_OR$repro_outcome_overall=="approximate" | 
                                 repro_outcomes_OR$repro_outcome_overall=="precise" | 
                                 repro_outcomes_OR$repro_outcome_overall=="push button"))/
                          sum(repro_outcomes_OR$weight)
                        
                        p_papers_OR_approx_or_precise_overall <- p_papers_OR_approx_or_precise*p_data
                        p_papers_OR_approx_or_precise_overall
                        },
                      clustervar = "paper_id",
                      keepvars=c("paper_id"),
                      alpha=.05,tails="two-tailed",iters=iters,
                      format.percent=TRUE,digits=1
      )$formatted.text
      
      p_papers_OR_precise_overall <- 
        bootstrap.clust(data=pr_outcomes,
                        FUN=function(x) {
                          paper_ids <- x$paper_id
                          pr_outcomes_modified_int <- pr_outcomes_modified[pr_outcomes_modified$paper_id %in% paper_ids,]
                          
                          p_data <- sum(pr_outcomes_modified_int$data_available_or_shared==TRUE)/
                            nrow(pr_outcomes_modified_int)
                          
                          repro_outcomes_OR <- repro_outcomes[repro_outcomes$paper_id %in% paper_ids,] %>%
                            filter(!repro_outcome_overall=="none")
                          
                          p_papers_OR_approx_or_precise <- sum(repro_outcomes_OR$weight*
                                                                 (
                                                                    repro_outcomes_OR$repro_outcome_overall=="precise" | 
                                                                    repro_outcomes_OR$repro_outcome_overall=="push button"))/
                            sum(repro_outcomes_OR$weight)
                          
                          p_papers_OR_approx_or_precise_overall <- p_papers_OR_approx_or_precise*p_data
                          p_papers_OR_approx_or_precise_overall
                        },
                        clustervar = "paper_id",
                        keepvars=c("paper_id"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        format.percent=TRUE,digits=1
        )$formatted.text
      
    }
    
    # Outcome reproducibility by year of original publication
    {
      #rho_of_all_precise_v_year <- bootstrap.clust(data=repro_outcomes_expanded,
      rho_OR_precise_v_year <- bootstrap.clust(data=repro_outcomes_merged,
                      FUN=function(x) {
                        SpearmanRho(x=as.numeric(x$pub_year),
                                    y=as.numeric(x$repro_outcome_overall=="precise" |
                                                   x$repro_outcome_overall=="push button"),
                                    conf.level=.95)[1]
                      },
                      clustervar = "paper_id",
                      keepvars=c("paper_id","pub_year","repro_outcome_overall"),
                      alpha=.05,tails="two-tailed",iters=iters,
                      format.percent=FALSE,digits=3
      )$formatted.text
      
      rho_OR_precise_or_approx_v_year <- bootstrap.clust(data=repro_outcomes_merged,
                                                   FUN=function(x) {
                                                     SpearmanRho(x=as.numeric(x$pub_year),
                                                                 y=as.numeric(x$repro_outcome_overall=="precise" |
                                                                                x$repro_outcome_overall=="push button" |
                                                                                x$repro_outcome_overall=="approximate"),
                                                                 conf.level=.95)[1]
                                                   },
                                                   clustervar = "paper_id",
                                                   keepvars=c("paper_id","pub_year","repro_outcome_overall"),
                                                   alpha=.05,tails="two-tailed",iters=iters,
                                                   format.percent=FALSE,digits=3
      )$formatted.text
    }
    
    # Outcome reproducibility by discipline
    {
      repro_outcomes_merged_econ <- repro_outcomes_merged[!is.na(repro_outcomes_merged$field) & repro_outcomes_merged$field=="Economics and Finance",]
      repro_outcomes_merged_econ <- repro_outcomes_merged_econ %>% filter(!repro_outcome_overall=="none")
      
      #n_papers_OR_econ <- length(unique(repro_outcomes_merged_econ$paper_id))
      n_papers_OR_econ <- format.round(sum(repro_outcomes_merged_econ$weight),1)
      
      n_papers_OR_approx_or_precise_econ <- format.round(
        sum(repro_outcomes_merged_econ$weight *
              (repro_outcomes_merged_econ$repro_outcome_overall=="approximate" |
                 repro_outcomes_merged_econ$repro_outcome_overall=="precise" |
                 repro_outcomes_merged_econ$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_approx_or_precise_econ <- cw.proportion(
        repro_outcomes_merged_econ$repro_outcome_overall=="approximate" | 
          repro_outcomes_merged_econ$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_econ$repro_outcome_overall=="push button",
        weights=repro_outcomes_merged_econ$weight,
        clusters = repro_outcomes_merged_econ$paper_id,iters)$formatted.text
      
      n_papers_OR_precise_econ <- format.round(sum(repro_outcomes_merged_econ$weight *
                                                (repro_outcomes_merged_econ$repro_outcome_overall=="precise" | 
                                                   repro_outcomes_merged_econ$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_precise_econ <- cw.proportion(
        repro_outcomes_merged_econ$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_econ$repro_outcome_overall=="push button",
        weights=repro_outcomes_merged_econ$weight,
        clusters = repro_outcomes_merged_econ$paper_id,iters)$formatted.text
      
      repro_outcomes_merged_polisci <- repro_outcomes_merged[!is.na(repro_outcomes_merged$field) & repro_outcomes_merged$field=="Political Science",]
      repro_outcomes_merged_polisci <- repro_outcomes_merged_polisci %>% filter(!repro_outcome_overall=="none")
      
      #n_papers_OR_polisci <- length(unique(repro_outcomes_merged_polisci$paper_id))
      n_papers_OR_polisci <- format.round(sum(repro_outcomes_merged_polisci$weight),1)
      
      n_papers_OR_approx_or_precise_polisci <- format.round(
        sum(repro_outcomes_merged_polisci$weight *
              (repro_outcomes_merged_polisci$repro_outcome_overall=="approximate" |
                 repro_outcomes_merged_polisci$repro_outcome_overall=="precise" | 
                 repro_outcomes_merged_polisci$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_approx_or_precise_polisci <- cw.proportion(
        repro_outcomes_merged_polisci$repro_outcome_overall=="approximate" | 
          repro_outcomes_merged_polisci$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_polisci$repro_outcome_overall=="push button",
        weights=repro_outcomes_merged_polisci$weight,
        clusters = repro_outcomes_merged_polisci$paper_id,iters)$formatted.text
      
      n_papers_OR_precise_polisci <- format.round(sum(repro_outcomes_merged_polisci$weight *
                                                     (repro_outcomes_merged_polisci$repro_outcome_overall=="precise"|
                                                        repro_outcomes_merged_polisci$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_precise_polisci <- cw.proportion(
        repro_outcomes_merged_polisci$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_polisci$repro_outcome_overall=="push button",
        weights=repro_outcomes_merged_polisci$weight,
        clusters = repro_outcomes_merged_polisci$paper_id,iters)$formatted.text
      

      repro_outcomes_merged_other <- repro_outcomes_merged[repro_outcomes_merged$field!="Economics and Finance" & 
                                                             repro_outcomes_merged$field!="Political Science",]
      repro_outcomes_merged_other <- repro_outcomes_merged_other %>% filter(!repro_outcome_overall=="none")
      
      #n_papers_OR_other <- length(unique(repro_outcomes_merged_other$paper_id))
      n_papers_OR_other <- format.round(sum(repro_outcomes_merged_other$weight),1)
      
      n_papers_OR_approx_or_precise_other <- format.round(
        sum(repro_outcomes_merged_other$weight *
              (repro_outcomes_merged_other$repro_outcome_overall=="approximate" |
                 repro_outcomes_merged_other$repro_outcome_overall=="precise" | 
                 repro_outcomes_merged_other$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_approx_or_precise_other <- cw.proportion(
        repro_outcomes_merged_other$repro_outcome_overall=="approximate" |
          repro_outcomes_merged_other$repro_outcome_overall=="precise"|
          repro_outcomes_merged_other$repro_outcome_overall=="push button",
        weights=repro_outcomes_merged_other$weight,
        clusters = repro_outcomes_merged_other$paper_id,iters)$formatted.text
      
      n_papers_OR_precise_other <- format.round(sum(repro_outcomes_merged_other$weight *
                                                     (repro_outcomes_merged_other$repro_outcome_overall=="precise" | 
                                                        repro_outcomes_merged_other$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_precise_other <- cw.proportion(
        repro_outcomes_merged_other$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_other$repro_outcome_overall=="push button",
        weights=repro_outcomes_merged_other$weight,
        clusters = repro_outcomes_merged_other$paper_id,iters)$formatted.text
      
    }
    
    # Discussion
    {
      p_short_papers_OR_approx_or_precise <- 
        paste0(format.round(100*sum((repro_outcomes$repro_outcome_overall=="approximate" | 
                                       repro_outcomes$repro_outcome_overall=="precise" | 
                                       repro_outcomes$repro_outcome_overall=="push button")*
                                      repro_outcomes$weight
        )/sum(repro_outcomes$weight),1),"%")
      
        #format.round(sum(repro_outcomes$weight),1)
      
      p_raw_papers_data_unavailable <- n_papers_data_unavailable/n_papers_assess_process_repro
      p_raw_papers_OR_approx_or_precise <- cw.proportion(
        repro_outcomes_OR$repro_outcome_overall=="approximate" | 
          repro_outcomes_OR$repro_outcome_overall=="precise" | 
          repro_outcomes_OR$repro_outcome_overall=="push button",
        weights=repro_outcomes_OR$weight,
        clusters = repro_outcomes_OR$paper_id,iters)$point.estimate
      
      p_short_papers_OR_approx_or_precise <- 
        paste0(format.round(100*p_raw_papers_OR_approx_or_precise,0),"%")
      
      p_short_OR_min_plausible <- 
        paste0(format.round(
          100*(0*(p_raw_papers_data_unavailable)+p_raw_papers_OR_approx_or_precise*(1-p_raw_papers_data_unavailable))
          ,0),"%")
      
      p_short_papers_OR_approx_or_precise_overall <- paste0(format.round(100*
          (sum(pr_outcomes_modified$data_available_or_shared==TRUE)/
            nrow(pr_outcomes_modified))*
          (sum(repro_outcomes[repro_outcomes$repro_outcome_overall!="none",]$weight*
                (repro_outcomes[repro_outcomes$repro_outcome_overall!="none",]$repro_outcome_overall=="approximate" | 
                   repro_outcomes[repro_outcomes$repro_outcome_overall!="none",]$repro_outcome_overall=="precise" | 
                   repro_outcomes[repro_outcomes$repro_outcome_overall!="none",]$repro_outcome_overall=="push button"))/
            sum(repro_outcomes[repro_outcomes$repro_outcome_overall!="none",]$weight)),
          1),"%")
    }
    
    # Sampling frame and selection of claims for reproduction
    {
      n_papers_initial_sample_p1 <- status %>% filter(p1_delivery) %>% nrow()
      
      n_papers_bushel <- (nrow(status  %>% filter(bushel)))
    }
    
    # Attrition of reproductions that started but were not completed
    {
      n_papers_repro_completed <- all_rr_attempts %>%
        filter(field != "covid" & str_detect(type, "Reproduction")) %>%
        count(paper_id) %>%
        nrow()
      
      p_papers_repro_completed <- 
        paste0(format.round(100*n_papers_OR_at_least_one/n_papers_repro_completed,1),"%")
    }
    
    # Claims-level Summary of Outcome Reproducibility 
    {
      repro_outcomes_OR <- repro_outcomes %>% filter(!repro_outcome_overall=="none")
      
      n_claims_exc_no_elig <- nrow(repro_outcomes)-nrow(repro_outcomes_OR)
      
      n_claims_OR_added_SDR <- nrow(repro_outcomes_OR %>%
        filter(repro_type=="Source Data Reproduction"))

      n_claims_OR_of_all_precise <- format.round(
        sum(1 *
              (repro_outcomes_expanded$repro_outcome_overall=="precise" |
                 repro_outcomes_expanded$repro_outcome_overall=="push button")),0)
      
      p_claims_OR_of_all_precise <- format.text.percent(
        sum(repro_outcomes_expanded$repro_outcome_overall=="precise" |
          repro_outcomes_expanded$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_expanded))
      
      n_claims_OR_approx_or_precise <- format.round(
        sum(1 *
              (repro_outcomes_OR$repro_outcome_overall=="approximate" |
                 repro_outcomes_OR$repro_outcome_overall=="precise" | 
                 repro_outcomes_OR$repro_outcome_overall=="push button")),0)
      
      p_claims_OR_approx_or_precise <- format.text.percent(
        sum(repro_outcomes_OR$repro_outcome_overall=="approximate" | 
          repro_outcomes_OR$repro_outcome_overall=="precise" | 
          repro_outcomes_OR$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_OR))
      
      n_claims_OR_precise <- format.round(
        sum(1 *
              (repro_outcomes_OR$repro_outcome_overall=="precise" |
                 repro_outcomes_OR$repro_outcome_overall=="push button")),0)
      
      p_claims_OR_precise <- format.text.percent(
        sum(repro_outcomes_OR$repro_outcome_overall=="precise" | 
          repro_outcomes_OR$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_OR))
      
      n_claims_OR_precise_pushbutton <- format.round(
        sum(repro_outcomes_OR$weight *
              ((repro_outcomes_OR$repro_outcome_overall=="precise" | 
                  repro_outcomes_OR$repro_outcome_overall=="push button") & 
                 repro_outcomes_OR$repro_type=="Push Button Reproduction")),0)
      
      p_claims_OR_precise_pushbutton <- format.text.percent(
        sum((repro_outcomes_OR$repro_outcome_overall=="precise" |
           repro_outcomes_OR$repro_outcome_overall=="push button") &
          repro_outcomes_OR$repro_type=="Push Button Reproduction"),
        nrow(repro_outcomes_OR))
      
      repro_outcomes_expanded_no_none <- repro_outcomes_expanded[repro_outcomes_expanded$repro_outcome_overall!="none",]
      
      n_claims_of_all_denom <- nrow(repro_outcomes_expanded_no_none)
      
      n_claims_OR_of_all_approx_or_precise <- 
        sum(repro_outcomes_expanded_no_none$repro_outcome_overall=="approximate" |
                repro_outcomes_expanded_no_none$repro_outcome_overall=="precise" |
              repro_outcomes_expanded_no_none$repro_outcome_overall=="push button")
      
      p_claims_OR_of_all_approx_or_precise <- format.text.percent(
        sum(repro_outcomes_expanded_no_none$repro_outcome_overall=="approximate" |
          repro_outcomes_expanded_no_none$repro_outcome_overall=="precise" |
          repro_outcomes_expanded_no_none$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_expanded_no_none))
      
      repro_outcomes_dc <- repro_outcomes_OR[repro_outcomes_OR$repro_type_consolidated=="Data and code available",]
      repro_outcomes_dc <- repro_outcomes_dc %>%
        filter(!repro_outcome_overall=="none")
      
      n_claims_OR_data_and_code <- nrow(repro_outcomes_dc)
      
      n_claims_OR_data_and_code_approx_or_precise <- 
        sum(repro_outcomes_dc$repro_outcome_overall=="approximate" |
               repro_outcomes_dc$repro_outcome_overall=="precise" | 
               repro_outcomes_dc$repro_outcome_overall=="push button")
      
      p_claims_OR_data_and_code_approx_or_precise <- format.text.percent(
        sum(repro_outcomes_dc$repro_outcome_overall=="approximate" |
              repro_outcomes_dc$repro_outcome_overall=="precise" | 
              repro_outcomes_dc$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_dc),
        digits=1,confint = FALSE)
      
      n_claims_OR_data_and_code_precise <- 
        sum(repro_outcomes_dc$repro_outcome_overall=="precise" |
                           repro_outcomes_dc$repro_outcome_overall=="push button")
      
      p_claims_OR_data_and_code_precise <- format.text.percent(
        sum(repro_outcomes_dc$repro_outcome_overall=="precise" | 
          repro_outcomes_dc$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_dc),
        digits=1,confint = FALSE)
      
      n_claims_OR_data_and_code_pushbutton <- 
        sum(repro_outcomes_dc$repro_outcome_overall=="push button")
      
      p_claims_OR_data_and_code_pushbutton <- format.text.percent(
        sum(repro_outcomes_dc$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_dc),
        digits=1,confint = FALSE)
      
      repro_outcomes_do <- repro_outcomes_OR[repro_outcomes_OR$repro_type_consolidated=="Only data available",]
      
      repro_outcomes_do <- repro_outcomes_do %>%
        filter(!repro_outcome_overall=="none")
      
      n_claims_OR_data_only <- nrow(repro_outcomes_do)
      
      n_claims_OR_data_only_approx_or_precise <- 
        sum(repro_outcomes_do$repro_outcome_overall=="approximate" |
                 repro_outcomes_do$repro_outcome_overall=="precise" | 
                 repro_outcomes_do$repro_outcome_overall=="push button")
      
      p_claims_OR_data_only_approx_or_precise <- format.text.percent(
        sum(repro_outcomes_do$repro_outcome_overall=="approximate" | 
          repro_outcomes_do$repro_outcome_overall=="precise" | 
          repro_outcomes_do$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_do),
        digits=1,confint = FALSE)
      
      n_claims_OR_data_only_precise <- 
        sum(repro_outcomes_do$repro_outcome_overall=="precise" |
              repro_outcomes_do$repro_outcome_overall=="push button")
      
      p_claims_OR_data_only_precise <- format.text.percent(
        sum(repro_outcomes_do$repro_outcome_overall=="precise" | 
          repro_outcomes_do$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_do),
        digits=1,confint = FALSE)
      
      n_claims_OR_data_only_pushbutton <- 
        sum(repro_outcomes_do$repro_outcome_overall=="push button")
      
      p_claims_OR_data_only_pushbutton <- format.text.percent(
        sum(repro_outcomes_do$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_do),
        digits=1,confint = FALSE)
      
      repro_outcomes_sd <- repro_outcomes_OR[repro_outcomes_OR$repro_type_consolidated=="Data reconstructed from source",]
      repro_outcomes_sd <- repro_outcomes_sd %>%
        filter(!repro_outcome_overall=="none")
      
      # all_rr_attempts_claims <- merge(all_rr_attempts,repro_outcomes_expanded[c("claim")])
      
      n_claims_OR_source_data <- nrow(repro_outcomes_sd)
      
      n_claims_OR_source_data_approx_or_precise <- 
        sum(
          repro_outcomes_sd$repro_outcome_overall=="approximate" |
            repro_outcomes_sd$repro_outcome_overall=="precise" | 
            repro_outcomes_sd$repro_outcome_overall=="push button")
      
      p_claims_OR_source_data_approx_or_precise <- format.text.percent(
        sum(repro_outcomes_sd$repro_outcome_overall=="approximate" | 
          repro_outcomes_sd$repro_outcome_overall=="precise" | 
          repro_outcomes_sd$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_sd),
        digits=1,confint = FALSE)
      
      n_claims_OR_source_data_precise <- 
        sum(
          repro_outcomes_sd$repro_outcome_overall=="precise" |
            repro_outcomes_sd$repro_outcome_overall=="push button")
      
      p_claims_OR_source_data_precise <- format.text.percent(
        sum(repro_outcomes_sd$repro_outcome_overall=="precise" | 
          repro_outcomes_sd$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_sd),
        digits=1,confint = FALSE)
      
      n_claims_OR_source_data_pushbutton <- sum(
        repro_outcomes_sd$repro_outcome_overall=="push button")
      
      p_claims_OR_source_data_pushbutton <- format.text.percent(
        sum(repro_outcomes_sd$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_sd),
        digits=1,confint = FALSE)
      
      p_claims_OR_approx_or_precise_overall <- paste0(format.round(
        100*(sum((repro_outcomes_OR$repro_outcome_overall=="approximate" | 
                repro_outcomes_OR$repro_outcome_overall=="precise" | 
                repro_outcomes_OR$repro_outcome_overall=="push button"))/
        nrow(repro_outcomes_OR))*
        (sum(pr_outcomes_modified$data_available_or_shared==TRUE)/
        nrow(pr_outcomes_modified)),1),"%")
        
      
      p_claims_OR_precise_overall <- paste0(format.round(
        100*(sum((
                    repro_outcomes_OR$repro_outcome_overall=="precise" | 
                    repro_outcomes_OR$repro_outcome_overall=="push button"))/
               nrow(repro_outcomes_OR))*
          (sum(pr_outcomes_modified$data_available_or_shared==TRUE)/
             nrow(pr_outcomes_modified)),1),"%")

      rho_OR_precise_v_year_claims <- bootstrap.clust(data=repro_outcomes_merged,
                                               FUN=function(x) {
                                                 SpearmanRho(x=as.numeric(x$pub_year),
                                                             y=as.numeric(x$repro_outcome_overall=="precise" |
                                                                            x$repro_outcome_overall=="push button"),
                                                             conf.level=.95)[1]
                                               },
                                               clustervar = "claim_id",
                                               keepvars=c("claim_id","pub_year","repro_outcome_overall"),
                                               alpha=.05,tails="two-tailed",iters=iters,
                                               format.percent=FALSE,digits=3
      )$formatted.text
      
      rho_OR_approx_or_precise_v_year_claims <- bootstrap.clust(data=repro_outcomes_merged,
                                                         FUN=function(x) {
                                                           SpearmanRho(x=as.numeric(x$pub_year),
                                                                       y=as.numeric(x$repro_outcome_overall=="precise" |
                                                                                      x$repro_outcome_overall=="push button" |
                                                                                      x$repro_outcome_overall=="approximate"),
                                                                       conf.level=.95)[1]
                                                         },
                                                         clustervar = "claim_id",
                                                         keepvars=c("claim_id","pub_year","repro_outcome_overall"),
                                                         alpha=.05,tails="two-tailed",iters=iters,
                                                         format.percent=FALSE,digits=3
      )$formatted.text

      repro_outcomes_merged_econ <- repro_outcomes_merged[!is.na(repro_outcomes_merged$field) & repro_outcomes_merged$field=="Economics and Finance",]
      repro_outcomes_merged_econ <- repro_outcomes_merged_econ %>% filter(!repro_outcome_overall=="none")
      
      #n_papers_OR_econ <- length(unique(repro_outcomes_merged_econ$paper_id))
      n_claims_OR_econ <- nrow(repro_outcomes_merged_econ)
      
      n_claims_OR_approx_or_precise_econ <- 
        sum(repro_outcomes_merged_econ$repro_outcome_overall=="approximate" |
                 repro_outcomes_merged_econ$repro_outcome_overall=="precise" |
                 repro_outcomes_merged_econ$repro_outcome_overall=="push button")
      
      p_claims_OR_approx_or_precise_econ <- format.text.percent(
        sum(repro_outcomes_merged_econ$repro_outcome_overall=="approximate" | 
          repro_outcomes_merged_econ$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_econ$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_merged_econ),
        digits=1,confint = FALSE)
      
      n_claims_OR_precise_econ <- sum(
        repro_outcomes_merged_econ$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_econ$repro_outcome_overall=="push button")
      
      p_claims_OR_precise_econ <- format.text.percent(
        sum(repro_outcomes_merged_econ$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_econ$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_merged_econ),
        digits=1,confint = FALSE)
      
      repro_outcomes_merged_polisci <- repro_outcomes_merged[!is.na(repro_outcomes_merged$field) & repro_outcomes_merged$field=="Political Science",]
      repro_outcomes_merged_polisci <- repro_outcomes_merged_polisci %>% filter(!repro_outcome_overall=="none")
      
      #n_papers_OR_polisci <- length(unique(repro_outcomes_merged_polisci$paper_id))
      n_claims_OR_polisci <- nrow(repro_outcomes_merged_polisci)
      
      n_claims_OR_approx_or_precise_polisci <- 
        sum(repro_outcomes_merged_polisci$repro_outcome_overall=="approximate" |
                 repro_outcomes_merged_polisci$repro_outcome_overall=="precise" | 
                 repro_outcomes_merged_polisci$repro_outcome_overall=="push button")
      
      p_claims_OR_approx_or_precise_polisci <- format.text.percent(
        sum(repro_outcomes_merged_polisci$repro_outcome_overall=="approximate" | 
          repro_outcomes_merged_polisci$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_polisci$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_merged_polisci),
        digits=1,confint = FALSE)
      
      n_claims_OR_precise_polisci <- sum(
        repro_outcomes_merged_polisci$repro_outcome_overall=="precise"|
          repro_outcomes_merged_polisci$repro_outcome_overall=="push button")
      
      p_claims_OR_precise_polisci <- format.text.percent(
        sum(repro_outcomes_merged_polisci$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_polisci$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_merged_polisci),
        digits=1,confint = FALSE)
      
      repro_outcomes_merged_other <- repro_outcomes_merged[!is.na(repro_outcomes_merged$field) & 
                                                             repro_outcomes_merged$field!="Economics and Finance" & 
                                                             repro_outcomes_merged$field!="Political Science",]
      repro_outcomes_merged_other <- repro_outcomes_merged_other %>% filter(!repro_outcome_overall=="none")
      
      #n_papers_OR_other <- length(unique(repro_outcomes_merged_other$paper_id))
      n_claims_OR_other <- nrow(repro_outcomes_merged_other)
      
      n_claims_OR_approx_or_precise_other <- 
        sum(repro_outcomes_merged_other$repro_outcome_overall=="approximate" |
                 repro_outcomes_merged_other$repro_outcome_overall=="precise" | 
                 repro_outcomes_merged_other$repro_outcome_overall=="push button")
      
      p_claims_OR_approx_or_precise_other <- format.text.percent(
        sum(repro_outcomes_merged_other$repro_outcome_overall=="approximate" |
          repro_outcomes_merged_other$repro_outcome_overall=="precise"|
          repro_outcomes_merged_other$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_merged_other),
        digits=1,confint = FALSE)
      
      n_claims_OR_precise_other <- 
        sum(repro_outcomes_merged_other$repro_outcome_overall=="precise" |
              repro_outcomes_merged_other$repro_outcome_overall=="push button")
      
      p_claims_OR_precise_other <- format.text.percent(
        sum(repro_outcomes_merged_other$repro_outcome_overall=="precise" | 
          repro_outcomes_merged_other$repro_outcome_overall=="push button"),
        nrow(repro_outcomes_merged_other),
        digits=1,confint = FALSE)
      
    }
    
    # Outcome reproducibility
    {
      n_papers_eligible_both_data_code <- pr_outcomes %>%
        filter(!covid) %>%
        filter(OA_data_shared != "no") %>%
        select(paper_id) %>%
        bind_rows(
          all_rr_attempts %>%
            filter(field != "covid") %>%
            filter(str_detect(all_types, "Source Data Reproduction")) %>%
            select(paper_id)
        ) %>%
        distinct() %>% nrow()
    }
    
    # Data aggregation
    {
      n_claims_multi_analyst <- repro_outcomes_orig %>%
        filter(!is_covid & repro_version_of_record == "F") %>%
        left_join(extracted_claims %>% select(unique_claim_id, p1 = single_trace_equivalent),
                  by = c("claim_id" = "unique_claim_id")) %>%
        mutate(all_st = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        mutate(alt_id = ifelse(p1 | is.na(p1), all_st, claim_id)) %>%
        mutate(alt_id = ifelse(claim_id %in% c("0a3Z_mqy444", "a2Yx_3lxxq3", "a2Yx_single-trace"), claim_id, alt_id)) %>%
        count(alt_id) %>%
        nrow()
      
      n_papers_multi_analyst <- repro_outcomes_orig %>%
        filter(!is_covid & repro_version_of_record == "F") %>%
        left_join(extracted_claims %>% select(unique_claim_id, p1 = single_trace_equivalent),
                  by = c("claim_id" = "unique_claim_id")) %>%
        mutate(all_st = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        mutate(alt_id = ifelse(p1 | is.na(p1), all_st, claim_id)) %>%
        mutate(alt_id = ifelse(claim_id %in% c("0a3Z_mqy444", "a2Yx_3lxxq3", "a2Yx_single-trace"), claim_id, alt_id)) %>%
        count(paper_id) %>%
        nrow()
      
      n_claims_repro_closest_to_orig <- repro_outcomes_orig %>%
          filter(!is_covid) %>%
          left_join(extracted_claims %>% select(unique_claim_id, p1 = single_trace_equivalent),
                    by = c("claim_id" = "unique_claim_id")) %>%
          mutate(all_st = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
          mutate(alt_id = ifelse(p1 | is.na(p1), all_st, claim_id)) %>%
          mutate(alt_id = ifelse(claim_id %in% c("0a3Z_mqy444", "a2Yx_3lxxq3", "a2Yx_single-trace"), claim_id, alt_id)) %>%
          group_by(alt_id) %>%
          mutate(n = n()) %>%
          ungroup() %>%
          mutate(repro_outcome_overall = ifelse(repro_outcome_overall == "not attemptable", "not", repro_outcome_overall)) %>%
          mutate(repro_outcome_overall = ifelse(repro_outcome_overall == "push button", "precise", repro_outcome_overall)) %>%
          group_by(alt_id) %>%
          mutate(ct = length(unique(repro_outcome_overall))) %>%
          ungroup() %>%
          filter(n > 1 & ct > 1) %>%
          count(alt_id) %>%
          nrow()
      
      n_claims_most_author_materials <- repro_outcomes_orig %>%
        filter(!is_covid) %>%
        left_join(extracted_claims %>% select(unique_claim_id, p1 = single_trace_equivalent),
                  by = c("claim_id" = "unique_claim_id")) %>%
        mutate(all_st = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        mutate(alt_id = ifelse(p1 | is.na(p1), all_st, claim_id)) %>%
        mutate(alt_id = ifelse(claim_id %in% c("0a3Z_mqy444", "a2Yx_3lxxq3", "a2Yx_single-trace"), claim_id, alt_id)) %>%
        group_by(alt_id) %>%
        mutate(n = n()) %>%
        ungroup() %>%
        mutate(repro_outcome_overall = ifelse(repro_outcome_overall == "not attemptable", "not", repro_outcome_overall)) %>%
        mutate(repro_outcome_overall = ifelse(repro_outcome_overall == "push button", "precise", repro_outcome_overall)) %>%
        group_by(alt_id) %>%
        mutate(ct = length(unique(repro_outcome_overall))) %>%
        ungroup() %>%
        filter(n > 1 & ct == 1) %>%
        select(claim_id, alt_id, repro_outcome_overall, repro_type) %>%
        group_by(alt_id, repro_outcome_overall) %>%
        mutate(method_ct = length(unique(repro_type))) %>%
        ungroup() %>%
        filter(method_ct == 2) %>%
        count(alt_id) %>%
        nrow()
      
      n_claims_part_of_multi <- repro_outcomes_orig %>%
        filter(!is_covid) %>%
        left_join(extracted_claims %>% select(unique_claim_id, p1 = single_trace_equivalent),
                  by = c("claim_id" = "unique_claim_id")) %>%
        mutate(all_st = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        mutate(alt_id = ifelse(p1 | is.na(p1), all_st, claim_id)) %>%
        mutate(alt_id = ifelse(claim_id %in% c("0a3Z_mqy444", "a2Yx_3lxxq3", "a2Yx_single-trace"), claim_id, alt_id)) %>%
        group_by(alt_id) %>%
        mutate(n = n()) %>%
        ungroup() %>%
        mutate(repro_outcome_overall = ifelse(repro_outcome_overall == "not attemptable", "not", repro_outcome_overall)) %>%
        mutate(repro_outcome_overall = ifelse(repro_outcome_overall == "push button", "precise", repro_outcome_overall)) %>%
        group_by(alt_id) %>%
        mutate(ct = length(unique(repro_outcome_overall))) %>%
        ungroup() %>%
        filter(n > 1 & ct == 1) %>%
        select(rr_id, claim_id, alt_id, repro_outcome_overall, repro_type) %>%
        group_by(alt_id, repro_outcome_overall) %>%
        mutate(method_ct = length(unique(repro_type))) %>%
        ungroup() %>%
        filter(method_ct == 1) %>%
        mutate(
          in_bushel = rr_id %in% (repro_outcomes_orig %>%
                                    filter(!is_covid) %>%
                                    group_by(rr_id) %>%
                                    dplyr::summarize(n = n()) %>%
                                    filter(n > 1) %>%
                                    pull(rr_id))
        ) %>%
        group_by(alt_id) %>%
        mutate(bushel_ct = length(unique(in_bushel))) %>%
        ungroup() %>%
        filter(bushel_ct > 1) %>%
        count(alt_id) %>%
        nrow()
      
      n_claims_repro_randomly_selected <- repro_outcomes_orig %>%
        filter(!is_covid) %>%
        left_join(extracted_claims %>% select(unique_claim_id, p1 = single_trace_equivalent),
                  by = c("claim_id" = "unique_claim_id")) %>%
        mutate(all_st = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        mutate(alt_id = ifelse(p1 | is.na(p1), all_st, claim_id)) %>%
        mutate(alt_id = ifelse(claim_id %in% c("0a3Z_mqy444", "a2Yx_3lxxq3", "a2Yx_single-trace"), claim_id, alt_id)) %>%
        group_by(alt_id) %>%
        mutate(n = n()) %>%
        ungroup() %>%
        mutate(repro_outcome_overall = ifelse(repro_outcome_overall == "not attemptable", "not", repro_outcome_overall)) %>%
        mutate(repro_outcome_overall = ifelse(repro_outcome_overall == "push button", "precise", repro_outcome_overall)) %>%
        group_by(alt_id) %>%
        mutate(ct = length(unique(repro_outcome_overall))) %>%
        ungroup() %>%
        filter(n > 1 & ct == 1) %>%
        select(rr_id, claim_id, alt_id, repro_outcome_overall, repro_type) %>%
        group_by(alt_id, repro_outcome_overall) %>%
        mutate(method_ct = length(unique(repro_type))) %>%
        ungroup() %>%
        filter(method_ct == 1) %>%
        mutate(
          in_bushel = rr_id %in% (repro_outcomes_orig %>%
                                    filter(!is_covid) %>%
                                    group_by(rr_id) %>%
                                    dplyr::summarize(n = n()) %>%
                                    filter(n > 1) %>%
                                    pull(rr_id))
        ) %>%
        group_by(alt_id) %>%
        mutate(bushel_ct = length(unique(in_bushel))) %>%
        ungroup() %>%
        filter(bushel_ct == 1) %>%
        count(alt_id) %>%
        nrow()
      
    }
    
    # Attrition of reproductions that started but were not completed
    {
      n_papers_repro_tyner_workflow <- 
        repro_export %>%
        filter(!is_covid) %>%
        select(paper_id) %>%
        anti_join(
          all_rr_attempts %>%
            filter(field != "covid") %>%
            filter(str_detect(type, "Reproduction")) %>%
            select(paper_id),
          by = "paper_id"
        ) %>%
        distinct() %>%
        nrow()
    }
  }

  # Clean up input values and export
  {
    rm(iters,orig_outcomes,repro_outcomes,repro_outcomes_merged)
    return(rev(as.list(environment())))
  }
}

# Figures
figures <- function(iters=100){
  # Setup and initialization
  {
    options(tidyverse.quiet = TRUE)
    # Libraries
    {
      library(dplyr)
      library(ggplot2)
      library(ggExtra)
      library(tidyr)
      library(pbapply)
      library(stringr)
      library(Hmisc)
      library(zcurve)
      library(funkyheatmap)
      library(tidyverse)
      library(cowplot)
      library(colorspace)
      
    }
    
    # Set defaults for convenience
    {
      if (!exists("iters")){ iters <- 100}
    }
    
    # Load data and common functions
    {
      # Check if loading locally
      if(file.exists("common functions.R") & file.exists("analyst data.RData")){
        load("analyst data.RData")
        source("common functions.R")
      } else {
        load("Analysis/Paper 3/Code and data/Analyst package/analyst data.RData")
        source("Analysis/Paper 3/Code and data/Analyst package/common functions.R")
      }
    }
    
    # Generate / modify key variables
    {
      # Fields order
      fields.raw <-
        c("political science",
          "economics and finance",
          "sociology and criminology",
          "psychology and health",
          "business",
          "education")
      fields.format.2.row <- 
        c("Political\nScience",
          "Economics\nand Finance",
          "Sociology and\nCriminology",
          "Psychology\nand Health",
          "Business",
          "Education")
      fields.format.3.row <- 
        c("Political\nScience",
          "Economics\nand\nFinance",
          "Sociology\nand\nCriminology",
          "Psychology\nand\nHealth",
          "Business",
          "Education")
      fields.abbreviated <- 
        c("Political Science",
          "Economics",
          "Sociology",
          "Psychology",
          "Business",
          "Education")
      
      # Modify repro outcomes overall result to make "not attempted" = "not"
      repro_outcomes$repro_outcome_overall <- 
        ifelse(repro_outcomes$repro_outcome_overall=="not attemptable",
               "not",repro_outcomes$repro_outcome_overall)
      
      repro_outcomes$repro_type_consolidated <- 
        ordered(repro_outcomes$repro_type,
                labels=c("Data and code available","Data and code available",
                         "Only data available","Data reconstructed from source"),
                levels=c("Push Button Reproduction","Extended Push Button Reproduction",
                         "Author Data Reproduction","Source Data Reproduction")
        )
      
      repro_outcomes$repro_outcome_overall_consolidated <- 
        ordered(repro_outcomes$repro_outcome_overall,
                labels=c("Precisely\nReproduced","Precisely\nReproduced","Approximately\nReproduced","Not\nReproduced"),
                levels=c("push button","precise","approximate","not")
        )
    }
    
  }
  
  # Global aesthetic options
  {
    # palette_process_repro_charts <- 
    #   c(palette_score_charts[5],
    #     lighten(palette_score_charts[5],amount=.3),
    #     lighten(palette_score_charts[5],amount=.6),
    #     palette_score_charts[2],
    #     lighten(palette_score_charts[2],amount=.3),
    #     palette_score_charts[1],
    #     "grey90"
    #   )
    
    palette_process_repro_charts <- 
      c(darken(palette_score_charts[5],amount=.2),
        lighten(palette_score_charts[5],amount=0.25),
        lighten(palette_score_charts[5],amount=.5),
        darken(palette_score_charts[2],amount=.25),
        lighten(palette_score_charts[2],amount=0),
        lighten(palette_score_charts[2],amount=.35),
        darken(palette_score_charts[1],amount=.1),
        "grey90"
      )
    
    palette_outcome_repro_charts <-
      c(palette_score_charts[7],
        lighten(palette_score_charts[7],.8),
        palette_score_charts[8],
        "grey90")
    palette_outcome_repro_charts_attempts <-
      c(lighten(palette_score_charts[7],.25),
        palette_outcome_repro_charts[length(palette_outcome_repro_charts)])

    palette_journal_TOP_factor_charts <-
      c(palette_score_charts[6],
        lighten(palette_score_charts[6],.4),
        lighten(palette_score_charts[6],.8),
        palette_score_charts[8])
  }
  
  # Figure 1. Process reproducibility success rates by year of publication
  {
    # Data wrangling
    {
      data <- pr_outcomes %>%  filter(!covid) 
      data$OA_data_shared <- data$OA_data_shared
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data!="No",
                                    "restricted",data$OA_data_shared)
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data=="No",
                                    "no data available",data$OA_data_shared)
      data$OA_code_shared_simple <- ifelse(data$OA_code_shared!="no","code available","code unavailable")
      
      data <- data %>%
        mutate(d_open_c_avail = OA_data_shared=="available_online" & OA_code_shared_simple == "code available",
               d_restricted_c_avail = OA_data_shared=="restricted" & OA_code_shared_simple == "code available",
               d_shared_c_avail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code available",
               
               d_open_c_unavail = OA_data_shared=="available_online" & OA_code_shared_simple == "code unavailable",
               d_restricted_c_unavail = OA_data_shared=="restricted" & OA_code_shared_simple == "code unavailable",
               d_shared_c_unavail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code unavailable",
               
               d_not_avail_c_avail = OA_data_shared=="no data available" & OA_code_shared_simple == "code available",
               
               d_not_avail_c_unavail = OA_data_shared=="no data available" & OA_code_shared_simple == "code unavailable") %>%
        select(paper_id,d_open_c_avail,d_restricted_c_avail,d_shared_c_avail,d_open_c_unavail,d_restricted_c_unavail,d_shared_c_unavail,d_not_avail_c_avail,d_not_avail_c_unavail) %>% 
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>%
        filter(response)
      
      data <- merge(data,paper_metadata[c("paper_id","pub_year")],by="paper_id",all.x =TRUE,all.y=FALSE)
      
      
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
      
      data$cat <- ordered(data$cat,
                          levels = c("d_open_c_avail",
                                     "d_restricted_c_avail",
                                     "d_shared_c_avail",
                                     "d_open_c_unavail",
                                     "d_restricted_c_unavail",
                                     "d_shared_c_unavail",
                                     "d_not_avail_c_avail",
                                     "d_not_avail_c_unavail"),
                          labels = c("Data open,\ncode available",
                                     "Data restricted,\ncode available", 
                                     "Data shared directly,\ncode available",
                                     "Data open,\ncode unavailable",
                                     "Data restricted,\ncode unavailable", 
                                     "Data shared directly,\ncode unavailable",
                                     "Data not available,\ncode available",
                                     "Neither data nor\ncode available")
      )
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat
      
      # Trim data for speed
      data <- data %>% select(pub_year,cat,group,paper_id)
      
    }
    
    # Aesthetic setup
    {
      bars_range <- c(0,1)
      col_widths <- c(0.8,8,1.5)
      n_bins_max <- 80
      y_axis_text_size <- 8
      x_axis_text_size <- 12
      legend_text_size <- 4
      
      chart.palette <- palette_process_repro_charts
    }
    
    # Plots
    {
      # Group by group plots
      {
        plotlist <- lapply(1:length(group_order),function(x) {
          group_label <- ggplot()+theme_nothing()+
            annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)
          
          rounded.bars_plot <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot
          
          snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                      chart.palette = chart.palette,
                                      n_bins_max=n_bins_max,
                                      display_axis = FALSE,
                                      collapsevar="paper_id")$plot
          plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
        })
      }
      # Blank / right axis
      {
        group_label <- ggplot()+theme_nothing()
        
        rounded.bars_plot <- ggplot()+theme_nothing()
        
        snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                    chart.palette = "white",
                                    n_bins_max=n_bins_max,
                                    axis_only = TRUE)$plot+
          theme(axis.text.x= element_text(size=x_axis_text_size))
        
        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All years",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)
        
        rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot
        
        snakebins_plot <- ggplot()+theme_nothing()
        
        plotlist[[length(plotlist)+1]] <- 
          plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
      # Axis row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                          chart.palette = rep("white",length(chart.palette)),
                                          axis_only = TRUE,)$plot+
          scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
          theme(axis.text.x= element_text(size=x_axis_text_size))
        
        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot+
          xlim(0,n_bins_max)
        
        plotlist[[length(plotlist)+1]] <- 
          plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
      
      # Spacer row
      {
        plotlist[[length(plotlist)+1]] <- ggplot()+theme_nothing()
      }
      # Legend row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        
        data.legend <- data %>% group_by(cat) %>% summarise(n=n())
        cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                          chart.palette = chart.palette,
                                          display_axis=FALSE,legend=FALSE)$cats_rects
        rounded.bars_plot <- rounded.bars(data.legend,nesting.structure,
                                          chart.palette = chart.palette,
                                          display_axis=FALSE,legend=FALSE)$plot+
          geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","white","black","white","white","black","black","black"),fontface="bold")+
          geom_segment(x=3/8,xend=3/8,y=1,yend=1.6,linetype=3)+
          geom_segment(x=7/8,xend=7/8,y=1,yend=1.6,linetype=3)+
          ylim(0,1.2)+
          annotate("text",x=1.5/8,y=1.05,label="Both Code and Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
          annotate("text",x=5/8,y=1.05,label="Either Code or Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
          annotate("text",x=7.5/8,y=1.05,label="Neither",color="black",vjust=0,size=legend_text_size+2,fontface="bold")
          
        
        snakebins_plot <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        
        plotlist[[length(plotlist)+1]] <- 
          plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
    }
    # Display plots
    figure_1 <- bundle_ggplot(
      plot = plot_grid(plotlist=plotlist,ncol=1,align = "v",
                       rel_heights = c(rep(1,length(plotlist)-5),0.5,1.2,0.6,0.6,3)),
      width = 6000,height = 2500,units = "px",bg="white")
  }
  
  # Figure 2. Process reproducibility success rates by field
  {
    # Data wrangling
    {
      data <- pr_outcomes %>%  filter(!covid)
      data$OA_data_shared <- data$OA_data_shared
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data!="No",
                                    "restricted",data$OA_data_shared)
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data=="No",
                                    "no data available",data$OA_data_shared)
      data$OA_code_shared_simple <- ifelse(data$OA_code_shared!="no","code available","code unavailable")

      data <- data %>%
        mutate(d_open_c_avail = OA_data_shared=="available_online" & OA_code_shared_simple == "code available",
               d_restricted_c_avail = OA_data_shared=="restricted" & OA_code_shared_simple == "code available",
               d_shared_c_avail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code available",

               d_open_c_unavail = OA_data_shared=="available_online" & OA_code_shared_simple == "code unavailable",
               d_restricted_c_unavail = OA_data_shared=="restricted" & OA_code_shared_simple == "code unavailable",
               d_shared_c_unavail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code unavailable",

               d_not_avail_c_avail = OA_data_shared=="no data available" & OA_code_shared_simple == "code available",

               d_not_avail_c_unavail = OA_data_shared=="no data available" & OA_code_shared_simple == "code unavailable") %>%
        select(paper_id,d_open_c_avail,d_restricted_c_avail,d_shared_c_avail,d_open_c_unavail,d_restricted_c_unavail,d_shared_c_unavail,d_not_avail_c_avail,d_not_avail_c_unavail) %>%
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>%
        filter(response)

      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)

      data$field <- str_to_title(data$COS_pub_category)

      group_order <- fields.abbreviated
      data$group <- ordered(data$COS_pub_category,
                            levels=fields.raw,
                            labels=fields.abbreviated)
                            #labels=fields.format.2.row)

      data$cat <- ordered(data$cat,
                          levels = c("d_open_c_avail",
                                     "d_restricted_c_avail",
                                     "d_shared_c_avail",
                                     "d_open_c_unavail",
                                     "d_restricted_c_unavail",
                                     "d_shared_c_unavail",
                                     "d_not_avail_c_avail",
                                     "d_not_avail_c_unavail"),
                          labels = c("Data open,\ncode available",
                                     "Data restricted,\ncode available",
                                     "Data shared directly,\ncode available",
                                     "Data open,\ncode unavailable",
                                     "Data restricted,\ncode unavailable",
                                     "Data shared directly,\ncode unavailable",
                                     "Data not available,\ncode available",
                                     "Neither data nor\ncode available")
      )
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      # Trim data for speed
      data <- data %>% select(field,cat,group,paper_id)

    }

    # Aesthetic setup
    {
      bars_range <- c(0,1)
      col_widths <- c(1.5,8,1.5)
      n_bins_max <- 150
      y_axis_text_size <- 8
      x_axis_text_size <- 12
      legend_text_size <- 4

      chart.palette <- palette_process_repro_charts
    }

    # Group by group plots
    {
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        rounded.bars_plot <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot

        snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      })
    }
    # Blank / right axis
    {
      group_label <- ggplot()+theme_nothing()

      rounded.bars_plot <- ggplot()+theme_nothing()

      snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                  chart.palette = "white",
                                  n_bins_max=n_bins_max,
                                  axis_only = TRUE)$plot+
        theme(axis.text.x= element_text(size=x_axis_text_size))

      plotlist[[length(plotlist)+1]] <-
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
    # Totals row
    {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=1,y=1,label="All fields",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

      rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot

      snakebins_plot <- ggplot()+theme_nothing()

      plotlist[[length(plotlist)+1]] <-
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    }
    # Axis row
    {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label="")
      rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
        scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
        theme(axis.text.x= element_text(size=x_axis_text_size))

      snakebins_plot <- snakebins(data,nesting.structure,
                                  chart.palette = rep("white",length(chart.palette)),
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot+
        xlim(0,n_bins_max)

      plotlist[[length(plotlist)+1]] <-
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    }
    # Spacer row
    {
      plotlist[[length(plotlist)+1]] <- ggplot()+theme_nothing()
    }
    # Legend row
    {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label="")

      data.legend <- data %>% group_by(cat) %>% summarise(n=n())
      cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE,legend=FALSE)$cats_rects
      rounded.bars_plot <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE,legend=FALSE)$plot+
        geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                  color=c("white","white","black","white","white","black","black","black"),fontface="bold")+
        geom_segment(x=3/8,xend=3/8,y=1,yend=1.6,linetype=3)+
        geom_segment(x=7/8,xend=7/8,y=1,yend=1.6,linetype=3)+
        ylim(0,1.2)+
        annotate("text",x=1.5/8,y=1.05,label="Both Code and Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
        annotate("text",x=5/8,y=1.05,label="Either Code or Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
        annotate("text",x=7.5/8,y=1.05,label="Neither",color="black",vjust=0,size=legend_text_size+2,fontface="bold")

      snakebins_plot <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label="")

      plotlist[[length(plotlist)+1]] <-
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    }
    # Display plots
    figure_2 <- bundle_ggplot(
      plot = plot_grid(plotlist=plotlist,ncol=1,align = "v",
                       rel_heights = c(rep(1,length(plotlist)-5),0.5,1.2,0.6,0.6,2)),
      width = 6000,height = 2000,units = "px",bg="white"
    )
      
  }

  # Figure 3. Outcome reproducibility by data and code availability
  {
    # Data wrangling
    {
      data <- repro_outcomes
      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE & !is.na(data$repro_version_of_record) &
                   data$repro_version_of_record=="T",]

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      data$field <- str_to_title(data$COS_pub_category)

      group_order <- c("Data and code\navailable",
                       "Only data\navailable","Data reconstructed\nfrom source")

      data$group <-
        ordered(data$repro_type,
                labels=c("Data and code\navailable","Data and code\navailable",
                         "Only data\navailable","Data reconstructed\nfrom source"),
                levels=c("Push Button Reproduction","Extended Push Button Reproduction",
                         "Author Data Reproduction","Source Data Reproduction")
        )

      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)

      data$cat <- data$repro_outcome_overall_consolidated

      # Drop not attempteds / missing repro types
      data <- data[!is.na(data$repro_type) & !is.na(data$cat),]

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
    }

    # Aesthetic options
    {
      chart.palette <- palette_outcome_repro_charts

      # Aesthetic setup
      bars_range <- c(0,1)
      col_widths <- c(1,4,1)
      n_bins_max <- 100
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 5
    }

    # Plots
    {
      # Group by group plots
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        data.group <- data %>%
          filter(group==group_order[x])

        cats_rects <- rounded.bars(data.group,nesting.structure,
                                   weightvar="weight",
                                   chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects

        cats_rects_final <- cats_rects[nrow(cats_rects),]
        rounded.bars.cutoff <- rounded.bars(data.group,nesting.structure,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)

        snakebins_plot <- snakebins(data.group,nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot
        plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      })
      # Blank / right axis
      {
        group_label <- ggplot()+theme_nothing()

        rounded.bars.cutoff <- ggplot()+theme_nothing()

        snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                    chart.palette = "white",
                                    n_bins_max=n_bins_max,
                                    axis_only = TRUE,
                                    collapsevar="paper_id")$plot+
        theme(axis.text.x=element_text(size=x_axis_text_size))

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All types",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        cats_rects <- rounded.bars(data,nesting.structure,
                                   weightvar="weight",
                                   #chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects

        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)+
          geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","black","white"),size=legend_text_size,fontface="bold")

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }

      # Axis row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            chart.palette = rep("white",length(chart.palette)),
                                            axis_only = TRUE,)$plot+
          scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot+
          xlim(0,n_bins_max)

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
    }
    # Plot outputs
    figure_3 <- bundle_ggplot(
      plot = plot_grid(plotlist=plotlist,ncol=1,align = "v",
                       rel_heights = c(rep(1,length(plotlist)-3),0.5,1.2,0.5)),
      width = 6000,height = 2000,units = "px",bg="white"
    )
  }

  # Figure 4. Outcome reproducibility by year of publication
  {
    # Data with attempteds in
    {
      data <- status %>%
        filter(RR) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))

      repro_outcomes.trimmed <-  repro_outcomes %>%
        filter(repro_version_of_record=="T") %>%
        select(c("claim_id","repro_outcome_overall_consolidated"))
      data <- merge(data,repro_outcomes.trimmed,
                    by="claim_id",all.x=TRUE,all.y=FALSE)

      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL

      #data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())
      data$field <- str_to_title(data$COS_pub_category)

      data$cat <- ifelse(is.na(data$repro_outcome_overall_consolidated),
                               "Not\nAttempted",
                               as.character(data$repro_outcome_overall_consolidated))

      data$cat <- ordered(data$cat,
                          labels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"),
                          levels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"))
      # Assign group
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)

      data.largecat <- data
      levels(data.largecat$cat) <- c(rep(" Attempted",3),"Not attempted ")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)

      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat

      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      # Trim data for speed
      data <- data %>% select(pub_year,cat,group,weight)
    }

    # Data with the attempteds out
    {
      data.trimmed <- data[data$cat!=as.character(data$cat[length(levels(data$cat))]),]
      data.trimmed$cat <- ordered(data.trimmed$cat,
                                  labels=levels(repro_outcomes$repro_outcome_overall_consolidated),
                                  levels=levels(repro_outcomes$repro_outcome_overall_consolidated))

      # Define nesting structure
      cat <- levels(data.trimmed$cat)
      nesting.structure.trimmed <- data.frame(cat)

      nesting.structure.trimmed$cat2 <- nesting.structure.trimmed$cat
      nesting.structure.trimmed$cat3 <- nesting.structure.trimmed$cat

      nesting.structure.trimmed$cat <- ordered(nesting.structure.trimmed$cat)
      nesting.structure.trimmed$cat2 <- ordered(nesting.structure.trimmed$cat2)
      nesting.structure.trimmed$cat3 <- ordered(nesting.structure.trimmed$cat3)

      data.trimmed <- data.trimmed #%>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

      # Trim data for speed
      data.trimmed <- data.trimmed %>% select(pub_year,cat,group,weight)
    }

    # Aesthetic setup
    {
      chart.palette <- palette_outcome_repro_charts

      chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(.4,1.2,4,1)
      n_bins_max <- 80
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
    }

    # Plots
    {

      # Group by group plots
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        data.group <- data %>%
          filter(group==group_order[x])# %>%
          # group_by(paper_id) %>%
          # mutate(weight=1/n())

        data.trimmed.group <- data.trimmed %>%
          filter(group==group_order[x]) # %>%
          # group_by(paper_id) %>%
          # mutate(weight=1/n())

        data.largecat.group <- data.largecat %>%
          filter(group==group_order[x]) # %>%
          # group_by(paper_id) %>%
          # mutate(weight=1/n())

        rounded.bars.cutoff <- rounded.bars(data.trimmed.group,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)

        rounded.bars.largecat <- rounded.bars(data.largecat.group,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot

        snakebins_plot <- snakebins(data.group,nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot
        plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      })
      # Blank / right axis
      {
        group_label <- ggplot()+theme_nothing()

        rounded.bars.cutoff <- ggplot()+theme_nothing()

        rounded.bars.largecat <- ggplot()+theme_nothing()

        snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                    chart.palette = "white",
                                    n_bins_max=n_bins_max,
                                    axis_only = TRUE,
                                    collapsevar="paper_id")$plot+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }
      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All years",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        cats_rects <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$cats_rects
        rounded.bars.cutoff <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)+
          geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","black","white"),size=legend_text_size,fontface="bold")

        cats_rects <- rounded.bars(data.largecat,nesting.structure.largecat,
                                   weightvar="weight",
                                   chart.palette = chart.palette.largecat,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot+
          geom_text(data=cats_rects,aes(x=xmax,y=c(0.90,.1),label=cat),
                    color=c("black","black"),size=legend_text_size,
                    hjust=c(0,1),vjust=c(1,0),fontface="bold")
        snakebins_plot <- ggplot()+theme_nothing()

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }

      # Axis row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            chart.palette = rep("white",length(chart.palette)),
                                            axis_only = TRUE,)$plot+
          scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
          theme(axis.text.x=element_text(size=x_axis_text_size))


        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              chart.palette = rep("white",length(chart.palette.largecat)),
                                              axis_only = TRUE)$plot+
          scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot+
          xlim(0,n_bins_max)

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }


    }
    # Export plots
    figure_4 <- bundle_ggplot(
      plot = plot_grid(plotlist=plotlist,ncol=1,align = "v",
                       rel_heights = c(rep(1,length(plotlist)-3),0.5,1.5,0.5)),
      width = 6000,height = 2000,units = "px",bg="white"
    )
  }
  # Figure 5. Outcome reproducibility by discipline
  {
    # Data with attempteds in
    {
      data <- status %>%
        filter(RR) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
      repro_outcomes.trimmed <-  repro_outcomes %>%
        filter(repro_version_of_record=="T") %>%
        select(c("claim_id","repro_outcome_overall_consolidated"))

      data <- merge(data,repro_outcomes.trimmed,
                    by="claim_id",all.x=TRUE,all.y=FALSE)
      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL

      #data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      data$cat <- ifelse(is.na(data$repro_outcome_overall_consolidated),
                         "Not\nAttempted",
                         as.character(data$repro_outcome_overall_consolidated))

      data$cat <- ordered(data$cat,
                          labels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"),
                          levels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"))
      # Assign group
      group_order <- fields.abbreviated
      data$group <- ordered(data$COS_pub_category,
                            levels=fields.raw,
                            labels=fields.abbreviated)

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)

      data.largecat <- data
      levels(data.largecat$cat) <- c(rep(" Attempted",3),"Not attempted ")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)

      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat

      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      # Trim data for speed
      data <- data %>% select(pub_year,cat,group,weight)
    }

    # Data with the attempteds out
    {
      data.trimmed <- data[data$cat!=as.character(data$cat[length(levels(data$cat))]),]
      data.trimmed$cat <- ordered(data.trimmed$cat,
                                  labels=levels(repro_outcomes$repro_outcome_overall_consolidated),
                                  levels=levels(repro_outcomes$repro_outcome_overall_consolidated))

      # Define nesting structure
      cat <- levels(data.trimmed$cat)
      nesting.structure.trimmed <- data.frame(cat)

      nesting.structure.trimmed$cat2 <- nesting.structure.trimmed$cat
      nesting.structure.trimmed$cat3 <- nesting.structure.trimmed$cat

      nesting.structure.trimmed$cat <- ordered(nesting.structure.trimmed$cat)
      nesting.structure.trimmed$cat2 <- ordered(nesting.structure.trimmed$cat2)
      nesting.structure.trimmed$cat3 <- ordered(nesting.structure.trimmed$cat3)

      # data.trimmed <- data.trimmed %>%
      #   group_by(paper_id) %>%
      #   mutate(weight=1/n())

      # Trim data for speed
      #data.trimmed <- data.trimmed %>% select(field,pub_year,cat,group,weight)
    }

    # Aesthetic setup
    {
      chart.palette <- palette_outcome_repro_charts

      chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(.8,1.2,4,1)
      n_bins_max <- 150
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
    }

    # Plots
    {
      # Group by group plots
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        data.group <- data %>%
          filter(group==group_order[x]) #%>%
          # group_by(paper_id) %>%
          # mutate(weight=1/n())

        data.trimmed.group <- data.trimmed %>%
          filter(group==group_order[x]) #%>%
          # group_by(paper_id) %>%
          # mutate(weight=1/n())

        data.largecat.group <- data.largecat %>%
          filter(group==group_order[x])# %>%
          # group_by(paper_id) %>%
          # mutate(weight=1/n())

        rounded.bars.cutoff <- rounded.bars(data.trimmed.group,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)

        rounded.bars.largecat <- rounded.bars(data.largecat.group,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot

        snakebins_plot <- snakebins(data.group,nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot
        plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      })
      # Blank / right axis
      {
        group_label <- ggplot()+theme_nothing()

        rounded.bars.cutoff <- ggplot()+theme_nothing()

        rounded.bars.largecat <- ggplot()+theme_nothing()

        snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                    chart.palette = "white",
                                    n_bins_max=n_bins_max,
                                    axis_only = TRUE,
                                    collapsevar="paper_id")$plot+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }

      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All fields",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        cats_rects <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                   weightvar="weight",
                                   chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.cutoff <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)+
          geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","black","white"),size=legend_text_size,fontface="bold")+
          geom_text(data=cats_rects,aes(x=c(0,0.5,1),y=c(1.35,0.5,-.35),label=c("","","")),
                    hjust=c(0,0.5,1),vjust=c(1,0.5,0))

        cats_rects <- rounded.bars(data.largecat,nesting.structure.largecat,
                                   weightvar="weight",
                                   chart.palette = chart.palette.largecat,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot+
          #geom_text(data=cats_rects,aes(x=xmax,y=c(0.90,.1),label=cat),
          geom_text(data=cats_rects,aes(x=c(0,1),y=c(1.35,-.35),label=cat),
                    color=c("black","black"),size=legend_text_size,fontface="bold",
                    hjust=c(0,1),vjust=c(1,0))
        snakebins_plot <- ggplot()+theme_nothing()

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }

      # Axis row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            chart.palette = rep("white",length(chart.palette)),
                                            axis_only = TRUE,)$plot+
          scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
          theme(axis.text.x=element_text(size=x_axis_text_size))


        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              chart.palette = rep("white",length(chart.palette.largecat)),
                                              axis_only = TRUE)$plot+
          scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot+
          xlim(0,n_bins_max)

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }
    }
    # Display plots
    
    figure_5 <- bundle_ggplot(
      plot = plot_grid(plotlist=plotlist,ncol=1,align = "v",
                rel_heights = c(rep(1,length(plotlist)-3),0.5,1.5,0.5)),
      width = 6000,height = 2000,units = "px",bg="white"
    )

  }

  # Figure 6: Data and code sharing policies for 62 social and behavioral science journals by discipline in 2024
  {
    # Data wrangling
    {
      publications <- na.omit(publications)

      publications$field <- ordered(publications$COS_pub_category,
                                    labels=c(fields.abbreviated),
                                    levels=c(fields.raw))
      publications$data_sharing <- ordered(publications$`Data Transparency`,
                                          levels=c(3,2,1,0),
                                          labels=c("Verify","Require","Disclose","None"))
      publications$code_sharing <- ordered(publications$`Analysis Code Transparency`,
                                           levels=c(3,2,1,0),
                                           labels=c("Verify","Require","Disclose","None"))
      pubs_data_sharing <- publications[c("field","data_sharing")]
      colnames(pubs_data_sharing) <- c("group","cat")
      pubs_code_sharing <- publications[c("field","code_sharing")]
      colnames(pubs_code_sharing) <- c("group","cat")
      group_order <- fields.abbreviated

      cat <- levels(pubs_data_sharing$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
    }

    # Aesthetic options
    {
      chart.palette <- palette_journal_TOP_factor_charts

      bars_range <- c(0,1)
      col_widths <- c(0.4,1,0.5,1,0.5)
      n_bins_max <- 20
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
      n_bins_vert <- 4
    }

    # Plots
    {
      # Main plots by field
      plotlist <- lapply(0:length(group_order),function(x) {

        if(x==0){
          left_label <-ggplot()+theme_nothing()+xlim(0,1)+ylim(0,1)+
            annotate("text",x=0.5,y=0.5,label="Data Sharing Policy",size=y_axis_text_size,fontface="bold",hjust=0.5)+
            theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
          right_label <-ggplot()+theme_nothing()+xlim(0,1)+ylim(0,1)+
            annotate("text",x=0.5,y=0.5,label="Code Sharing Policy",size=y_axis_text_size,fontface="bold",hjust=0.5)+
            theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
          plot_grid(ggplot()+theme_nothing(),left_label,right_label,
                    nrow=1,rel_widths = c(col_widths[1],(sum(col_widths)-col_widths[1])/2,(sum(col_widths)-col_widths[1])/2))
        } else{

          group_label <- ggplot()+theme_nothing()+
            annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

          rounded.bars.data.sharing <- rounded.bars(pubs_data_sharing[pubs_data_sharing$group==group_order[x],],nesting.structure,
                                     chart.palette = chart.palette,
                                     display_axis = FALSE)$plot
          rounded.bars.code.sharing <- rounded.bars(pubs_code_sharing[pubs_code_sharing$group==group_order[x],],nesting.structure,
                                                    chart.palette = chart.palette,
                                                    display_axis = FALSE)$plot

          snakebins.data.sharing <- snakebins(pubs_data_sharing[pubs_data_sharing$group==group_order[x],],nesting.structure,
                                      chart.palette = chart.palette,
                                      n_bins=n_bins_vert,
                                      n_bins_max=n_bins_max,
                                      display_axis = FALSE)$plot
          snakebins.code.sharing <- snakebins(pubs_code_sharing[pubs_code_sharing$group==group_order[x],],nesting.structure,
                                              chart.palette = chart.palette,
                                              n_bins=n_bins_vert,
                                              n_bins_max=n_bins_max,
                                              display_axis = FALSE)$plot
          plot_grid(group_label,rounded.bars.data.sharing,snakebins.data.sharing,
                    rounded.bars.code.sharing,snakebins.code.sharing,
                    nrow=1,rel_widths = col_widths)

        }
      })
      # Blank / Axis row
      {
        group_label <- ggplot()+theme_nothing()
        rounded.bars.data.sharing <- ggplot()+theme_nothing()
        rounded.bars.code.sharing <- ggplot()+theme_nothing()
        snakebins.data.sharing <- snakebins(pubs_data_sharing,nesting.structure,
                                            chart.palette = rep("white",nrow(nesting.structure)),
                                            n_bins=n_bins_vert,
                                            n_bins_max=n_bins_max,
                                            display_axis = TRUE)$plot
        snakebins.code.sharing <- snakebins(pubs_code_sharing,nesting.structure,
                                            chart.palette = rep("white",nrow(nesting.structure)),
                                            n_bins=n_bins_vert,
                                            n_bins_max=n_bins_max,
                                            display_axis = TRUE)$plot
        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.data.sharing,snakebins.data.sharing,
                    rounded.bars.code.sharing,snakebins.code.sharing,
                    nrow=1,rel_widths = col_widths)

      }
      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All fields",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        rounded.bars.data.sharing <- rounded.bars(pubs_data_sharing,nesting.structure,
                                                  chart.palette = chart.palette,
                                                  display_axis = TRUE)$plot
        rounded.bars.code.sharing <- rounded.bars(pubs_code_sharing,nesting.structure,
                                                  chart.palette = chart.palette,
                                                  display_axis = TRUE)$plot

        snakebins.data.sharing <- ggplot()+theme_nothing()
        snakebins.code.sharing <- ggplot()+theme_nothing()

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.data.sharing,snakebins.data.sharing,
                  rounded.bars.code.sharing,snakebins.code.sharing,
                  nrow=1,rel_widths = col_widths)
      }
      # Spacer
        plotlist[[length(plotlist)+1]] <- ggplot()+theme_nothing()

      # Legend
      {
        data.legend <- rbind(pubs_data_sharing,pubs_code_sharing) %>% group_by(cat) %>% summarise(n=1)
        cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                          chart.palette = chart.palette,
                                          display_axis=FALSE,legend=FALSE)$cats_rects
        legend <- rounded.bars(data.legend,nesting.structure,
                               chart.palette = chart.palette,
                               display_axis=FALSE)$plot+
          theme_nothing()+
          geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","white","black","white"),size=legend_text_size,fontface="bold")

        plotlist[[length(plotlist)+1]] <-
          plot_grid(ggplot()+theme_nothing(),legend,
                    nrow=1,rel_widths = c(col_widths[1],sum(col_widths)-col_widths[1]))
      }
    }
    figure_6 <- bundle_ggplot(
      plot_grid(plotlist=plotlist,ncol=1,rel_heights = c(1.5,rep(1,length(group_order)),.5,1.5,0.5,1.5)),
      width = 6000,height = 2000,units = "px",bg="white"
    )
  }

  # Figure S1. Outcome reproducibility by data and code availability, claims level
  {
    # Data wrangling
    {
      data <- repro_outcomes
      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE & !is.na(data$repro_version_of_record) & data$repro_version_of_record=="T",]

      data$field <- str_to_title(data$COS_pub_category)

      group_order <- c("Data and code\navailable",
                       "Only data\navailable","Data reconstructed\nfrom source")

      data$group <-
        ordered(data$repro_type,
                labels=c("Data and code\navailable","Data and code\navailable",
                         "Only data\navailable","Data reconstructed\nfrom source"),
                levels=c("Push Button Reproduction","Extended Push Button Reproduction",
                         "Author Data Reproduction","Source Data Reproduction")
        )

      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)


      data$cat <- data$repro_outcome_overall_consolidated

      # Drop not attempteds / missing repro types
      data <- data[!is.na(data$repro_type) & !is.na(data$cat),]

      data <- data %>%
        group_by(paper_id) %>%
        #mutate(weight=1/n())
        mutate(weight=1)

      # # Drop un needed variables (for speed)
      # data <- data[c("paper_id","claim_id","group","weight","cat","field","pub_year")]

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
    }

    # Aesthetic options
    {
      chart.palette <- palette_outcome_repro_charts

      # Aesthetic setup
      bars_range <- c(0,1)
      col_widths <- c(1,4,1)
      n_bins_max <- 400
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 5
    }

    # Plots
    {
      # Group by group plots
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        data.group <- data %>%
          filter(group==group_order[x]) #%>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        cats_rects <- rounded.bars(data.group,nesting.structure,
                                   weightvar="weight",
                                   chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects

        cats_rects_final <- cats_rects[nrow(cats_rects),]
        rounded.bars.cutoff <- rounded.bars(data.group,nesting.structure,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)

        snakebins_plot <- snakebins(data.group,nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE)$plot
                                    #collapsevar="paper_id")$plot

        plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      })
      # Blank / right axis
      {
        group_label <- ggplot()+theme_nothing()

        rounded.bars.cutoff <- ggplot()+theme_nothing()

        snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                    chart.palette = "white",
                                    n_bins_max=n_bins_max,
                                    axis_only = TRUE,
                                    collapsevar="paper_id")$plot+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All types",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        cats_rects <- rounded.bars(data,nesting.structure,
                                   weightvar="weight",
                                   #chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects

        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)+
          geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","black","white"),size=legend_text_size,fontface="bold")

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot


        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }

      # Axis row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            chart.palette = rep("white",length(chart.palette)),
                                            axis_only = TRUE,)$plot+
          scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot+
          xlim(0,n_bins_max)

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      }
    }
    # Plot outputs
    figure_s1 <- bundle_ggplot(
      plot=plot_grid(plotlist=plotlist,ncol=1,align = "v",
                          rel_heights = c(rep(1,length(plotlist)-3),0.5,1.2,0.5)),
      width = 6000,height = 1200,units = "px",bg="white"
    )
  }
  # width = 6000,height = 1200,units = "px",bg="white"

  # Figure S2. Outcome reproducibility by year of publication, claims level
  {
    # Data with attempteds in
    {
      data <- status %>%
        filter(RR) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))

      repro_outcomes.trimmed <-  repro_outcomes %>%
        filter(repro_version_of_record=="T") %>%
        select(c("claim_id","repro_outcome_overall_consolidated"))
      data <- merge(data,repro_outcomes.trimmed,
                    by="claim_id",all.x=TRUE,all.y=FALSE)

      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL

      #data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1)
        #mutate(weight=1/n())
      data$field <- str_to_title(data$COS_pub_category)

      data$cat <- ifelse(is.na(data$repro_outcome_overall_consolidated),
                         "Not\nAttempted",
                         as.character(data$repro_outcome_overall_consolidated))

      data$cat <- ordered(data$cat,
                          labels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"),
                          levels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"))
      # Assign group
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)

      data.largecat <- data
      levels(data.largecat$cat) <- c(rep(" Attempted",3),"Not attempted ")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)

      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat

      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      # Trim data for speed
      data <- data %>% select(pub_year,cat,group,weight)
    }

    # Data with the attempteds out
    {
      data.trimmed <- data[data$cat!=as.character(data$cat[length(levels(data$cat))]),]
      data.trimmed$cat <- ordered(data.trimmed$cat,
                                  labels=levels(repro_outcomes$repro_outcome_overall_consolidated),
                                  levels=levels(repro_outcomes$repro_outcome_overall_consolidated))

      # Define nesting structure
      cat <- levels(data.trimmed$cat)
      nesting.structure.trimmed <- data.frame(cat)

      nesting.structure.trimmed$cat2 <- nesting.structure.trimmed$cat
      nesting.structure.trimmed$cat3 <- nesting.structure.trimmed$cat

      nesting.structure.trimmed$cat <- ordered(nesting.structure.trimmed$cat)
      nesting.structure.trimmed$cat2 <- ordered(nesting.structure.trimmed$cat2)
      nesting.structure.trimmed$cat3 <- ordered(nesting.structure.trimmed$cat3)

      data.trimmed <- data.trimmed #%>%
      # group_by(paper_id) %>%
      # mutate(weight=1/n())

      # Trim data for speed
      data.trimmed <- data.trimmed %>% select(pub_year,cat,group,weight)
    }

    # Aesthetic setup
    {
      chart.palette <- palette_outcome_repro_charts

      chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(.4,1.2,4,1)
      n_bins_max <- 600
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
    }

    # Plots
    {

      # Group by group plots
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        data.group <- data %>%
          filter(group==group_order[x])# %>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        data.trimmed.group <- data.trimmed %>%
          filter(group==group_order[x]) # %>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        data.largecat.group <- data.largecat %>%
          filter(group==group_order[x]) # %>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        rounded.bars.cutoff <- rounded.bars(data.trimmed.group,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)

        rounded.bars.largecat <- rounded.bars(data.largecat.group,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot

        snakebins_plot <- snakebins(data.group,nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE)$plot
        plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      })
      # Blank / right axis
      {
        group_label <- ggplot()+theme_nothing()

        rounded.bars.cutoff <- ggplot()+theme_nothing()

        rounded.bars.largecat <- ggplot()+theme_nothing()

        snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                    chart.palette = "white",
                                    n_bins_max=n_bins_max,
                                    axis_only = TRUE,
                                    collapsevar="paper_id")$plot+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }
      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All years",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        cats_rects <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                   weightvar="weight",
                                   chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.cutoff <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)+
          geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","black","white"),size=legend_text_size,fontface="bold")

        cats_rects <- rounded.bars(data.largecat,nesting.structure.largecat,
                                   weightvar="weight",
                                   chart.palette = chart.palette.largecat,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot+
          geom_text(data=cats_rects,aes(x=xmax,y=c(0.90,.1),label=cat),
                    color=c("black","black"),size=legend_text_size,
                    hjust=c(0,1),vjust=c(1,0),fontface="bold")
        snakebins_plot <- ggplot()+theme_nothing()

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }

      # Axis row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            chart.palette = rep("white",length(chart.palette)),
                                            axis_only = TRUE,)$plot+
          scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
          theme(axis.text.x=element_text(size=x_axis_text_size))


        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              chart.palette = rep("white",length(chart.palette.largecat)),
                                              axis_only = TRUE)$plot+
          scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot+
          xlim(0,n_bins_max)

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }
    }
    # Display plots
    figure_s2 <- bundle_ggplot(
      plot=plot_grid(plotlist=plotlist,ncol=1,align = "v",
                     rel_heights = c(rep(1,length(plotlist)-3),0.5,1.5,0.5)),
      width = 10000,height = 4000,units = "px",bg="white"
    )
  }

  # Figure S3. Outcome reproducibility by discipline, claims level
  {
    # Data with attempteds in
    {
      data <- status %>%
        filter(RR) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
      repro_outcomes.trimmed <-  repro_outcomes %>%
        filter(repro_version_of_record=="T") %>%
        select(c("claim_id","repro_outcome_overall_consolidated"))

      data <- merge(data,repro_outcomes.trimmed,
                    by="claim_id",all.x=TRUE,all.y=FALSE)
      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL

      #data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        #mutate(weight=1/n())
        mutate(weight=1)

      data$cat <- ifelse(is.na(data$repro_outcome_overall_consolidated),
                         "Not\nAttempted",
                         as.character(data$repro_outcome_overall_consolidated))

      data$cat <- ordered(data$cat,
                          labels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"),
                          levels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"))
      # Assign group
      group_order <- fields.abbreviated
      data$group <- ordered(data$COS_pub_category,
                            levels=fields.raw,
                            labels=fields.abbreviated)

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)

      data.largecat <- data
      levels(data.largecat$cat) <- c(rep(" Attempted",3),"Not attempted ")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)

      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat

      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      # Trim data for speed
      data <- data %>% select(pub_year,cat,group,weight)
    }

    # Data with the attempteds out
    {
      data.trimmed <- data[data$cat!=as.character(data$cat[length(levels(data$cat))]),]
      data.trimmed$cat <- ordered(data.trimmed$cat,
                                  labels=levels(repro_outcomes$repro_outcome_overall_consolidated),
                                  levels=levels(repro_outcomes$repro_outcome_overall_consolidated))

      # Define nesting structure
      cat <- levels(data.trimmed$cat)
      nesting.structure.trimmed <- data.frame(cat)

      nesting.structure.trimmed$cat2 <- nesting.structure.trimmed$cat
      nesting.structure.trimmed$cat3 <- nesting.structure.trimmed$cat

      nesting.structure.trimmed$cat <- ordered(nesting.structure.trimmed$cat)
      nesting.structure.trimmed$cat2 <- ordered(nesting.structure.trimmed$cat2)
      nesting.structure.trimmed$cat3 <- ordered(nesting.structure.trimmed$cat3)

      # data.trimmed <- data.trimmed %>%
      #   group_by(paper_id) %>%
      #   mutate(weight=1/n())

      # Trim data for speed
      #data.trimmed <- data.trimmed %>% select(field,pub_year,cat,group,weight)
    }

    # Aesthetic setup
    {
      chart.palette <- palette_outcome_repro_charts

      chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(.8,1.2,4,1)
      n_bins_max <- 1000
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
    }

    # Plots
    {
      # Group by group plots
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        data.group <- data %>%
          filter(group==group_order[x]) #%>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        data.trimmed.group <- data.trimmed %>%
          filter(group==group_order[x]) #%>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        data.largecat.group <- data.largecat %>%
          filter(group==group_order[x])# %>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        rounded.bars.cutoff <- rounded.bars(data.trimmed.group,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)

        rounded.bars.largecat <- rounded.bars(data.largecat.group,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot

        snakebins_plot <- snakebins(data.group,nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE)$plot
        plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      })
      # Blank / right axis
      {
        group_label <- ggplot()+theme_nothing()

        rounded.bars.cutoff <- ggplot()+theme_nothing()

        rounded.bars.largecat <- ggplot()+theme_nothing()

        snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                    chart.palette = "white",
                                    n_bins_max=n_bins_max,
                                    axis_only = TRUE,
                                    collapsevar="paper_id")$plot+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }

      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All fields",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        cats_rects <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                   weightvar="weight",
                                   chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.cutoff <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)+
          geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","black","white"),size=legend_text_size,fontface="bold")+
          geom_text(data=cats_rects,aes(x=c(0,0.5,1),y=c(1.35,0.5,-.35),label=c("","","")),
                    hjust=c(0,0.5,1),vjust=c(1,0.5,0))

        cats_rects <- rounded.bars(data.largecat,nesting.structure.largecat,
                                   weightvar="weight",
                                   chart.palette = chart.palette.largecat,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot+
          #geom_text(data=cats_rects,aes(x=xmax,y=c(0.90,.1),label=cat),
          geom_text(data=cats_rects,aes(x=c(0,1),y=c(1.35,-.35),label=cat),
                    color=c("black","black"),size=legend_text_size,fontface="bold",
                    hjust=c(0,1),vjust=c(1,0))
        snakebins_plot <- ggplot()+theme_nothing()

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }

      # Axis row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            chart.palette = rep("white",length(chart.palette)),
                                            axis_only = TRUE,)$plot+
          scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
          theme(axis.text.x=element_text(size=x_axis_text_size))


        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              chart.palette = rep("white",length(chart.palette.largecat)),
                                              axis_only = TRUE)$plot+
          scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot+
          xlim(0,n_bins_max)

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }
    }
    # Display plots
    figure_s3 <- bundle_ggplot(
      plot =  plot_grid(plotlist=plotlist,ncol=1,align = "v",
                rel_heights = c(rep(1,length(plotlist)-3),0.5,1.5,0.5)),
      width = 10000,height = 2000,units = "px",bg="white"
    )
  }

  # Figure S4. Outcome reproducibility by discipline and year
  {
    # Data manipulation
    {
      data <- status %>%
        filter(RR) %>%
        filter(p1_delivery | p2_delivery) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
      # data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
      #               by="claim_id",all.x=TRUE,all.y=FALSE)
      repro_outcomes.trimmed <-  repro_outcomes %>%
        filter(repro_version_of_record=="T") %>%
        select(c("claim_id","repro_outcome_overall"))
      data <- merge(data,repro_outcomes.trimmed,
                    by="claim_id",all.x=TRUE,all.y=FALSE)

      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL
      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)
      data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      field_order <- fields.abbreviated
      data$field <- ordered(data$COS_pub_category,
                            levels=fields.raw,
                            labels=fields.abbreviated)

      # Assign group
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)

      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)

      data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)

      data$cat <- ordered(data$repro_outcome_overall,
                          labels=c( "Precisely\nReproduced",
                                    "Precisely\nReproduced",
                                    "Approximately\nReproduced",
                                    "Not\nReproduced",
                                    "Not\nAttempted"),
                          levels=c( "Push Button",
                                    "Precise",
                                    "Approximate",
                                    "Not",
                                    "Not Attempted"))

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)


      data.largecat <- data
      levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)

      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat

      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)

      #chart.palette.largecat <- c(palette_score_charts[6],chart.palette[5])

      # Trim data for speed
      data <- data %>% select(field,pub_year,cat,group,weight)
    }

    # Aesthetic setup
    {
      chart.palette <- palette_outcome_repro_charts

      chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(.4,1.2,4,1)
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
      fields_text_size <- 8
    }

    # Plots
    {
      # Group by field plots
      plotlist_field <- lapply(levels(data$field),function(field) {
        data_field <- data[data$field==field,]

        plotlist <- lapply(1:length(group_order),function(x) {
          data.field.group <- data_field %>%
            filter(group==group_order[x]) %>%
            group_by(paper_id) %>%
            mutate(weight=1/n())

          cats_rects <- rounded.bars(data.field.group,nesting.structure,
                                     weightvar="weight",
                                     chart.palette = chart.palette,
                                     display_axis = FALSE)$cats_rects
          cats_rects_final <- cats_rects[nrow(cats_rects),]
          rounded.bars.cutoff <- rounded.bars(data.field.group,nesting.structure,
                                              weightvar="weight",
                                              chart.palette = chart.palette,
                                              display_axis = FALSE)$plot+
            funkyheatmap::geom_rounded_rect(data=cats_rects_final,
                                            aes(xmin=xmin,
                                                ymin=ymin,
                                                xmax=bars_range[2],
                                                ymax=ymax),
                                            radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
            xlim(bars_range)
          rounded.bars.cutoff
        })

        plotlist <- append(list(ggplot() + theme_nothing()+
                                  annotate("text",x=0.5,y=1,label=field,size=fields_text_size,fontface="bold")),
                           plotlist)

        plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
      })
      plots_field <- plot_grid(plotlist=plotlist_field,nrow=1)

      # Year labels
      plotlist_yearlabels <- lapply(1:length(group_order),function(x) {
        ggplot() + theme_nothing()+
          annotate("text",x=0.5,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold")
      })
      plotlist_yearlabels <- append(list(ggplot() + theme_nothing()+
                                           annotate("text",x=0.5,y=1,label="")),
                                    plotlist_yearlabels)
      year_labels <- plot_grid(plotlist=plotlist_yearlabels,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))


      main_plots <- plot_grid(year_labels,plots_field,nrow=1,rel_widths=c(0.5,length(levels(data$field))))

      # Legend
      data.legend <- data %>% group_by(cat) %>% summarise(n=n())
      cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$cats_rects
      legend <- rounded.bars(data.legend,nesting.structure,
                             chart.palette = chart.palette,
                             display_axis=TRUE)$plot+
        geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                  color=c("white","black","white","black"),size=legend_text_size,fontface="bold")+
        theme(axis.text.x=element_text(size=x_axis_text_size))+
        scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))
      legend_bar <- plot_grid(ggplot()+theme_nothing()+
                                annotate("text",x=0.5,y=1,label=""),
                              legend,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
    }
    figure_s4 <- bundle_ggplot(
      plot = plot_grid(main_plots,legend_bar,ncol=1,rel_heights = c(1+length(unique(data$pub_year)),1.8)),
      width = 6000,height = 2000,units = "px",bg="white"
    )
  }

  # Figure S5. Outcome reproducibility by discipline and year for all claims
  {
    # Data manipulation
    {
      data <- status %>%
        filter(RR) %>%
        filter(p1_delivery | p2_delivery) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
      # data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
      #               by="claim_id",all.x=TRUE,all.y=FALSE)
      repro_outcomes.trimmed <-  repro_outcomes %>%
        filter(repro_version_of_record=="T") %>%
        select(c("claim_id","repro_outcome_overall"))
      data <- merge(data,repro_outcomes.trimmed,
                    by="claim_id",all.x=TRUE,all.y=FALSE)

      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL
      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)
      data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        # mutate(weight=1/n())
        mutate(weight=1)

      field_order <- fields.abbreviated
      data$field <- ordered(data$COS_pub_category,
                            levels=fields.raw,
                            labels=fields.abbreviated)

      # Assign group
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)

      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)

      data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)

      data$cat <- ordered(data$repro_outcome_overall,
                          labels=c( "Precisely\nReproduced",
                                    "Precisely\nReproduced",
                                    "Approximately\nReproduced",
                                    "Not\nReproduced",
                                    "Not\nAttempted"),
                          levels=c( "Push Button",
                                    "Precise",
                                    "Approximate",
                                    "Not",
                                    "Not Attempted"))

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)


      data.largecat <- data
      levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)

      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat

      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)

      #chart.palette.largecat <- c(palette_score_charts[6],chart.palette[5])

      # Trim data for speed
      data <- data %>% select(field,pub_year,cat,group,weight)
    }

    # Aesthetic setup
    {
      chart.palette <- palette_outcome_repro_charts

      chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(.4,1.2,4,1)
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
      fields_text_size <- 8
    }

    # Plots
    {
      # Group by field plots
      plotlist_field <- lapply(levels(data$field),function(field) {
        data_field <- data[data$field==field,]

        plotlist <- lapply(1:length(group_order),function(x) {
          data.field.group <- data_field %>%
            filter(group==group_order[x]) %>%
            group_by(paper_id) %>%
            #mutate(weight=1/n())
            mutate(weight=1)

          cats_rects <- rounded.bars(data.field.group,nesting.structure,
                                     weightvar="weight",
                                     chart.palette = chart.palette,
                                     display_axis = FALSE)$cats_rects
          cats_rects_final <- cats_rects[nrow(cats_rects),]
          rounded.bars.cutoff <- rounded.bars(data.field.group,nesting.structure,
                                              weightvar="weight",
                                              chart.palette = chart.palette,
                                              display_axis = FALSE)$plot+
            funkyheatmap::geom_rounded_rect(data=cats_rects_final,
                                            aes(xmin=xmin,
                                                ymin=ymin,
                                                xmax=bars_range[2],
                                                ymax=ymax),
                                            radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
            xlim(bars_range)
          rounded.bars.cutoff
        })

        plotlist <- append(list(ggplot() + theme_nothing()+
                                  annotate("text",x=0.5,y=1,label=field,size=fields_text_size,fontface="bold")),
                           plotlist)

        plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
      })
      plots_field <- plot_grid(plotlist=plotlist_field,nrow=1)

      # Year labels
      plotlist_yearlabels <- lapply(1:length(group_order),function(x) {
        ggplot() + theme_nothing()+
          annotate("text",x=0.5,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold")
      })
      plotlist_yearlabels <- append(list(ggplot() + theme_nothing()+
                                           annotate("text",x=0.5,y=1,label="")),
                                    plotlist_yearlabels)
      year_labels <- plot_grid(plotlist=plotlist_yearlabels,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))


      main_plots <- plot_grid(year_labels,plots_field,nrow=1,rel_widths=c(0.5,length(levels(data$field))))

      # Legend
      data.legend <- data %>% group_by(cat) %>% summarise(n=n())
      cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$cats_rects
      legend <- rounded.bars(data.legend,nesting.structure,
                             chart.palette = chart.palette,
                             display_axis=TRUE)$plot+
        geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                  color=c("white","black","white","black"),size=legend_text_size,fontface="bold")+
        theme(axis.text.x=element_text(size=x_axis_text_size))+
        scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))
      legend_bar <- plot_grid(ggplot()+theme_nothing()+
                                annotate("text",x=0.5,y=1,label=""),
                              legend,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
    }
    figure_s5 <- bundle_ggplot(
      plot=plot_grid(main_plots,legend_bar,ncol=1,rel_heights = c(1+length(unique(data$pub_year)),1.8)),
      width = 6000,height = 2000,units = "px",bg="white"
    )
      
  }
  
  # Figure S6. Process reproducibility success rates by 12 subfields
  {
    # Data wrangling
    {
      data <- pr_outcomes %>%  filter(!covid)
      data$OA_data_shared <- data$OA_data_shared
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data!="No",
                                    "restricted",data$OA_data_shared)
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data=="No",
                                    "no data available",data$OA_data_shared)
      data$OA_code_shared_simple <- ifelse(data$OA_code_shared!="no","code available","code unavailable")

      data <- data %>%
        mutate(d_open_c_avail = OA_data_shared=="available_online" & OA_code_shared_simple == "code available",
               d_restricted_c_avail = OA_data_shared=="restricted" & OA_code_shared_simple == "code available",
               d_shared_c_avail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code available",

               d_open_c_unavail = OA_data_shared=="available_online" & OA_code_shared_simple == "code unavailable",
               d_restricted_c_unavail = OA_data_shared=="restricted" & OA_code_shared_simple == "code unavailable",
               d_shared_c_unavail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code unavailable",

               d_not_avail_c_avail = OA_data_shared=="no data available" & OA_code_shared_simple == "code available",

               d_not_avail_c_unavail = OA_data_shared=="no data available" & OA_code_shared_simple == "code unavailable") %>%
        select(paper_id,d_open_c_avail,d_restricted_c_avail,d_shared_c_avail,d_open_c_unavail,d_restricted_c_unavail,d_shared_c_unavail,d_not_avail_c_avail,d_not_avail_c_unavail) %>%
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>%
        filter(response)

      exp_fields <- paper_metadata %>%
        select(paper_id, pub = publication_standard, field = COS_pub_expanded) %>%
        mutate(
          field = case_when(
            str_detect(pub, "financ|Financ") ~ "finance",
            str_detect(pub, "organization|Organization") ~ "org. behavior",
            !str_detect(pub, "organization|Organization") & field == "marketing/org behavior"  ~ "marketing",
            !str_detect(pub, "financ|Financ") & field == "economics"  ~ "economics",
            .default = field
          )
        )
      exp_fields$field <- str_to_title(exp_fields$field)

      data <- merge(data,exp_fields[c("paper_id","field")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)
      group_order <- sort(unique(exp_fields$field))
      data$group <- ordered(data$field,
                            levels=sort(unique(exp_fields$field)),
                            labels=sort(unique(exp_fields$field)))

      data$cat <- ordered(data$cat,
                          levels = c("d_open_c_avail",
                                     "d_restricted_c_avail",
                                     "d_shared_c_avail",
                                     "d_open_c_unavail",
                                     "d_restricted_c_unavail",
                                     "d_shared_c_unavail",
                                     "d_not_avail_c_avail",
                                     "d_not_avail_c_unavail"),
                          labels = c("Data open,\ncode available",
                                     "Data restricted,\ncode available",
                                     "Data shared directly,\ncode available",
                                     "Data open,\ncode unavailable",
                                     "Data restricted,\ncode unavailable",
                                     "Data shared directly,\ncode unavailable",
                                     "Data not available,\ncode available",
                                     "Neither data nor\ncode available")
      )
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      # Trim data for speed
      data <- data %>% select(field,cat,group,paper_id)

    }

    # Aesthetic setup
    {
      bars_range <- c(0,1)
      col_widths <- c(2.5,7,1.5)
      n_bins_max <- 150
      y_axis_text_size <- 8
      x_axis_text_size <- 12
      legend_text_size <- 4

      chart.palette <- palette_process_repro_charts
    }
    # Plots
    {
      # Group by group plots
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        rounded.bars_plot <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot

        snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      })
      # Blank / right axis
      group_label <- ggplot()+theme_nothing()

      rounded.bars_plot <- ggplot()+theme_nothing()

      snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                  chart.palette = "white",
                                  n_bins_max=n_bins_max,
                                  axis_only = TRUE)$plot+
        theme(axis.text.x= element_text(size=x_axis_text_size))

      plotlist[[length(plotlist)+1]] <-
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
      # Totals row
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=1,y=1,label="All fields",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

      rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot

      snakebins_plot <- ggplot()+theme_nothing()

      plotlist[[length(plotlist)+1]] <-
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")

      # Axis row
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label="")
      rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
        scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
        theme(axis.text.x= element_text(size=x_axis_text_size))

      snakebins_plot <- snakebins(data,nesting.structure,
                                  chart.palette = rep("white",length(chart.palette)),
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot+
        xlim(0,n_bins_max)

      plotlist[[length(plotlist)+1]] <-
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")

      # Spacer row
      plotlist[[length(plotlist)+1]] <- ggplot()+theme_nothing()

      # Legend row
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label="")

      data.legend <- data %>% group_by(cat) %>% summarise(n=n())
      cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE,legend=FALSE)$cats_rects
      rounded.bars_plot <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE,legend=FALSE)$plot+
        geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                  color=c("white","white","black","white","white","black","black","black"),fontface="bold")+
        geom_segment(x=3/8,xend=3/8,y=1,yend=1.6,linetype=3)+
        geom_segment(x=7/8,xend=7/8,y=1,yend=1.6,linetype=3)+

        #geom_segment(x=2/3,xend=2/3+.05,y=.5,yend=0.5,linetype=3)+
        ylim(0,1.2)+
        annotate("text",x=1.5/8,y=1.05,label="Both Code and Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
        annotate("text",x=5/8,y=1.05,label="Either Code or Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
        annotate("text",x=7.5/8,y=1.05,label="Neither",color="black",vjust=0,size=legend_text_size+2,fontface="bold")

      snakebins_plot <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label="")

      plotlist[[length(plotlist)+1]] <-
        plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    }
    # Display plots
    figure_s6 <- bundle_ggplot(
      plot = plot_grid(plotlist=plotlist,ncol=1,align = "v",
                       rel_heights = c(rep(1,length(plotlist)-5),0.5,1.2,0.6,0.6,2)),
      width = 6000,height = 4000,units = "px",bg="white"
    )
      
  }
  
  # Figure S7. Process reproducibility success rates by year of publication for all disciplines
  {
    # Data wrangling
    {
      data <- pr_outcomes %>%  filter(!covid)
      data$OA_data_shared <- data$OA_data_shared
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data!="No",
                                    "restricted",data$OA_data_shared)
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data=="No",
                                    "no data available",data$OA_data_shared)
      data$OA_code_shared_simple <- ifelse(data$OA_code_shared!="no","code available","code unavailable")

      data <- data %>%
        mutate(d_open_c_avail = OA_data_shared=="available_online" & OA_code_shared_simple == "code available",
               d_restricted_c_avail = OA_data_shared=="restricted" & OA_code_shared_simple == "code available",
               d_shared_c_avail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code available",

               d_open_c_unavail = OA_data_shared=="available_online" & OA_code_shared_simple == "code unavailable",
               d_restricted_c_unavail = OA_data_shared=="restricted" & OA_code_shared_simple == "code unavailable",
               d_shared_c_unavail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code unavailable",

               d_not_avail_c_avail = OA_data_shared=="no data available" & OA_code_shared_simple == "code available",

               d_not_avail_c_unavail = OA_data_shared=="no data available" & OA_code_shared_simple == "code unavailable") %>%
        select(paper_id,d_open_c_avail,d_restricted_c_avail,d_shared_c_avail,d_open_c_unavail,d_restricted_c_unavail,d_shared_c_unavail,d_not_avail_c_avail,d_not_avail_c_unavail) %>%
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>%
        filter(response)

      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)

      data$cat <- ordered(data$cat,
                          levels = c("d_open_c_avail",
                                     "d_restricted_c_avail",
                                     "d_shared_c_avail",
                                     "d_open_c_unavail",
                                     "d_restricted_c_unavail",
                                     "d_shared_c_unavail",
                                     "d_not_avail_c_avail",
                                     "d_not_avail_c_unavail"),
                          labels = c("Data open,\ncode available",
                                     "Data restricted,\ncode available",
                                     "Data shared directly,\ncode available",
                                     "Data open,\ncode unavailable",
                                     "Data restricted,\ncode unavailable",
                                     "Data shared directly,\ncode unavailable",
                                     "Data not available,\ncode available",
                                     "Neither data nor\ncode available")
      )
      field_order <- fields.abbreviated
      data$field <- ordered(data$COS_pub_category,
                            levels=fields.raw,
                            labels=fields.abbreviated)

      data.econ.poli <- data[data$COS_pub_category=="economics and finance" | data$COS_pub_category=="political science",]
      data.other <- data[!data$COS_pub_category=="economics and finance" & !data$COS_pub_category=="political science",]
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

    }
    # Aesthetic setup
    {
      chart.palette <- palette_process_repro_charts

      bars_range <- c(0,1)
      col_widths <- c(.4,1.2,4,1)
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
      fields_text_size <- 8
    }
    # Plots
    {    
    # Group by field plots
      plotlist_field <- lapply(levels(data$field),function(field) {
        data_field <- data[data$field==field,]
  
        plotlist <- lapply(1:length(group_order),function(x) {
          data.field.group <- data_field %>%
            filter(group==group_order[x]) %>%
            group_by(paper_id) %>%
            mutate(weight=1/n())
  
          cats_rects <- rounded.bars(data.field.group,nesting.structure,
                                     weightvar="weight",
                                     chart.palette = chart.palette,
                                     display_axis = FALSE)$cats_rects
          cats_rects_final <- cats_rects[nrow(cats_rects),]
          rounded.bars.cutoff <- rounded.bars(data.field.group,nesting.structure,
                                              weightvar="weight",
                                              chart.palette = chart.palette,
                                              display_axis = FALSE)$plot+
            funkyheatmap::geom_rounded_rect(data=cats_rects_final,
                                            aes(xmin=xmin,
                                                ymin=ymin,
                                                xmax=bars_range[2],
                                                ymax=ymax),
                                            radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
            xlim(bars_range)
          rounded.bars.cutoff
        })
  
        plotlist <- append(list(ggplot() + theme_nothing()+
                                  annotate("text",x=0.5,y=1,label=field,size=fields_text_size,fontface="bold")),
                           plotlist)
  
        plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
      })
      plots_field <- plot_grid(plotlist=plotlist_field,nrow=1)
  
      # Year labels
      plotlist_yearlabels <- lapply(1:length(group_order),function(x) {
        ggplot() + theme_nothing()+
          annotate("text",x=0.5,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold")
      })
      plotlist_yearlabels <- append(list(ggplot() + theme_nothing()+
                                           annotate("text",x=0.5,y=1,label="")),
                                    plotlist_yearlabels)
      year_labels <- plot_grid(plotlist=plotlist_yearlabels,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
  
  
      main_plots <- plot_grid(year_labels,plots_field,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
  
      # Legend
      data.legend <- data %>% group_by(cat) %>% summarise(n=n())
      cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE,legend=FALSE)$cats_rects
      legend <- rounded.bars(data.legend,nesting.structure,
                             chart.palette = chart.palette,
                             display_axis=TRUE,legend=FALSE)$plot+
        geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                  color=c("white","white","black","white","white","black","black","black"),fontface="bold")+
        geom_segment(x=3/8,xend=3/8,y=1,yend=1.6,linetype=3)+
        geom_segment(x=7/8,xend=7/8,y=1,yend=1.6,linetype=3)+
        ylim(0,1.6)+
        annotate("text",x=1.5/8,y=1.05,label="Both Code and Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
        annotate("text",x=5/8,y=1.05,label="Either Code or Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
        annotate("text",x=7.5/8,y=1.05,label="Neither",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
        theme(axis.text.x=element_text(size=x_axis_text_size))+
        scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))
  
      legend_bar <- plot_grid(ggplot()+theme_nothing()+
                                annotate("text",x=0.5,y=1,label=""),
                              legend,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
      }

    figure_s7 <- bundle_ggplot(
      plot = plot_grid(main_plots,ggplot()+theme_nothing(),legend_bar,ncol=1,rel_heights = c(1+length(unique(data$pub_year)),0.4,2.5)),
      width = 6000,height = 2000,units = "px",bg="white"
    )
      
  }

  # Figure S8. Process reproducibility success rates by year of publication by 12 subfields
  {
    # Data wrangling
    {
      data <- pr_outcomes %>%  filter(!covid)
      data$OA_data_shared <- data$OA_data_shared
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data!="No",
                                    "restricted",data$OA_data_shared)
      data$OA_data_shared <- ifelse(data$OA_data_shared=="no" & data$restricted_data=="No",
                                    "no data available",data$OA_data_shared)
      data$OA_code_shared_simple <- ifelse(data$OA_code_shared!="no","code available","code unavailable")

      data <- data %>%
        mutate(d_open_c_avail = OA_data_shared=="available_online" & OA_code_shared_simple == "code available",
               d_restricted_c_avail = OA_data_shared=="restricted" & OA_code_shared_simple == "code available",
               d_shared_c_avail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code available",

               d_open_c_unavail = OA_data_shared=="available_online" & OA_code_shared_simple == "code unavailable",
               d_restricted_c_unavail = OA_data_shared=="restricted" & OA_code_shared_simple == "code unavailable",
               d_shared_c_unavail = OA_data_shared=="shared_on_request" & OA_code_shared_simple == "code unavailable",

               d_not_avail_c_avail = OA_data_shared=="no data available" & OA_code_shared_simple == "code available",

               d_not_avail_c_unavail = OA_data_shared=="no data available" & OA_code_shared_simple == "code unavailable") %>%
        select(paper_id,d_open_c_avail,d_restricted_c_avail,d_shared_c_avail,d_open_c_unavail,d_restricted_c_unavail,d_shared_c_unavail,d_not_avail_c_avail,d_not_avail_c_unavail) %>%
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>%
        filter(response)

      data <- merge(data,paper_metadata[c("paper_id","pub_year")],by="paper_id",all.x =TRUE,all.y=FALSE)
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)

      data$cat <- ordered(data$cat,
                          levels = c("d_open_c_avail",
                                     "d_restricted_c_avail",
                                     "d_shared_c_avail",
                                     "d_open_c_unavail",
                                     "d_restricted_c_unavail",
                                     "d_shared_c_unavail",
                                     "d_not_avail_c_avail",
                                     "d_not_avail_c_unavail"),
                          labels = c("Data open,\ncode available",
                                     "Data restricted,\ncode available",
                                     "Data shared directly,\ncode available",
                                     "Data open,\ncode unavailable",
                                     "Data restricted,\ncode unavailable",
                                     "Data shared directly,\ncode unavailable",
                                     "Data not available,\ncode available",
                                     "Neither data nor\ncode available")
      )

      exp_fields <- paper_metadata %>%
        select(paper_id, pub = publication_standard, field = COS_pub_expanded) %>%
        mutate(
          field = case_when(
            str_detect(pub, "financ|Financ") ~ "finance",
            str_detect(pub, "organization|Organization") ~ "org. behavior",
            !str_detect(pub, "organization|Organization") & field == "marketing/org behavior"  ~ "marketing",
            !str_detect(pub, "financ|Financ") & field == "economics"  ~ "economics",
            .default = field
          )
        )
      exp_fields$field <- str_to_title(exp_fields$field)

      data <- merge(data,exp_fields[c("paper_id","field")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)
      field_order <- sort(unique(exp_fields$field))

      data$field <- ordered(data$field,
                            levels=sort(unique(exp_fields$field)),
                            labels=sort(unique(exp_fields$field)))

      data.econ.poli <- data[data$COS_pub_category=="economics and finance" | data$COS_pub_category=="political science",]
      data.other <- data[!data$COS_pub_category=="economics and finance" & !data$COS_pub_category=="political science",]
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

    }
    # Aesthetic setup
    {
      chart.palette <- palette_process_repro_charts

      #chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(.4,1.2,4,1)
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
      fields_text_size <- 8
    }

    # Plots
    {
      # Top half
      {
        # Group by field plots
        {
          plotlist_field <- lapply(levels(data$field)[1:6],function(field) {
            data_field <- data[data$field==field,]

            plotlist <- lapply(1:length(group_order),function(x) {
              data.field.group <- data_field %>%
                filter(group==group_order[x]) %>%
                group_by(paper_id) %>%
                mutate(weight=1/n())

              cats_rects <- rounded.bars(data.field.group,nesting.structure,
                                         weightvar="weight",
                                         chart.palette = chart.palette,
                                         display_axis = FALSE)$cats_rects
              cats_rects_final <- cats_rects[nrow(cats_rects),]
              rounded.bars.cutoff <- rounded.bars(data.field.group,nesting.structure,
                                                  weightvar="weight",
                                                  chart.palette = chart.palette,
                                                  display_axis = FALSE)$plot+
                funkyheatmap::geom_rounded_rect(data=cats_rects_final,
                                                aes(xmin=xmin,
                                                    ymin=ymin,
                                                    xmax=bars_range[2],
                                                    ymax=ymax),
                                                radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
                xlim(bars_range)
              rounded.bars.cutoff
            })

            plotlist <- append(list(ggplot() + theme_nothing()+
                                      annotate("text",x=0.5,y=1,label=field,size=fields_text_size,fontface="bold")),
                               plotlist)

            plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
          })
          plots_field <- plot_grid(plotlist=plotlist_field,nrow=1)
        }
        # Year labels
        {
          plotlist_yearlabels <- lapply(1:length(group_order),function(x) {
            ggplot() + theme_nothing()+
              annotate("text",x=0.5,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold")
          })
          plotlist_yearlabels <- append(list(ggplot() + theme_nothing()+
                                               annotate("text",x=0.5,y=1,label="")),
                                        plotlist_yearlabels)
          year_labels <- plot_grid(plotlist=plotlist_yearlabels,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))


          main_plots_top <- plot_grid(year_labels,plots_field,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
        }
      }
      # Bottom half
      {
        # Group by field plots
        {
          plotlist_field <- lapply(levels(data$field)[7:12],function(field) {
            data_field <- data[data$field==field,]

            plotlist <- lapply(1:length(group_order),function(x) {
              data.field.group <- data_field %>%
                filter(group==group_order[x]) %>%
                group_by(paper_id) %>%
                mutate(weight=1/n())

              cats_rects <- rounded.bars(data.field.group,nesting.structure,
                                         weightvar="weight",
                                         chart.palette = chart.palette,
                                         display_axis = FALSE)$cats_rects
              cats_rects_final <- cats_rects[nrow(cats_rects),]
              rounded.bars.cutoff <- rounded.bars(data.field.group,nesting.structure,
                                                  weightvar="weight",
                                                  chart.palette = chart.palette,
                                                  display_axis = FALSE)$plot+
                funkyheatmap::geom_rounded_rect(data=cats_rects_final,
                                                aes(xmin=xmin,
                                                    ymin=ymin,
                                                    xmax=bars_range[2],
                                                    ymax=ymax),
                                                radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
                xlim(bars_range)
              rounded.bars.cutoff
            })

            plotlist <- append(list(ggplot() + theme_nothing()+
                                      annotate("text",x=0.5,y=1,label=field,size=fields_text_size,fontface="bold")),
                               plotlist)

            plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
          })
          plots_field <- plot_grid(plotlist=plotlist_field,nrow=1)
        }
        # Year labels
        {
          plotlist_yearlabels <- lapply(1:length(group_order),function(x) {
            ggplot() + theme_nothing()+
              annotate("text",x=0.5,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold")
          })
          plotlist_yearlabels <- append(list(ggplot() + theme_nothing()+
                                               annotate("text",x=0.5,y=1,label="")),
                                        plotlist_yearlabels)
          year_labels <- plot_grid(plotlist=plotlist_yearlabels,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))


          main_plots_bottom <- plot_grid(year_labels,plots_field,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
        }
      }
      # Legend
      {
        data.legend <- data %>% group_by(cat) %>% summarise(n=n())
        cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                          chart.palette = chart.palette,
                                          display_axis=FALSE,legend=FALSE)$cats_rects
        legend <- rounded.bars(data.legend,nesting.structure,
                               chart.palette = chart.palette,
                               display_axis=TRUE,legend=FALSE)$plot+
          geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","white","black","white","white","black","black","black"),fontface="bold")+
          geom_segment(x=3/8,xend=3/8,y=1,yend=1.6,linetype=3)+
          geom_segment(x=7/8,xend=7/8,y=1,yend=1.6,linetype=3)+
          ylim(0,1.6)+
          annotate("text",x=1.5/8,y=1.05,label="Both Code and Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
          annotate("text",x=5/8,y=1.05,label="Either Code or Data",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
          annotate("text",x=7.5/8,y=1.05,label="Neither",color="black",vjust=0,size=legend_text_size+2,fontface="bold")+
          theme(axis.text.x=element_text(size=x_axis_text_size))+
          scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))

        legend_bar <- plot_grid(ggplot()+theme_nothing()+
                                  annotate("text",x=0.5,y=1,label=""),
                                legend,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
      }
    }

    figure_s8 <- bundle_ggplot(
      plot = plot_grid(main_plots_top,
                       main_plots_bottom,
                       ggplot()+theme_nothing(),
                       legend_bar,
                       ncol=1,
                       rel_heights = c(1+length(unique(data$pub_year)),1+length(unique(data$pub_year)),0.4,2.5)),
      width = 6000,height = 5000,units = "px",bg="white"
    )
      
  }

  # Figure S9. Outcome reproducibility by discipline
  {
    # Data with attempteds in
    {
      data <- status %>%
        filter(RR) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
      repro_outcomes.trimmed <-  repro_outcomes %>%
        filter(repro_version_of_record=="T") %>%
        select(c("claim_id","repro_outcome_overall_consolidated"))

      data <- merge(data,repro_outcomes.trimmed,
                    by="claim_id",all.x=TRUE,all.y=FALSE)
      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL

      #data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      data$cat <- ifelse(is.na(data$repro_outcome_overall_consolidated),
                         "Not\nAttempted",
                         as.character(data$repro_outcome_overall_consolidated))

      data$cat <- ordered(data$cat,
                          labels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"),
                          levels=c(levels(data$repro_outcome_overall_consolidated),
                                   "Not\nAttempted"))
      # # Assign group
      # group_order <- fields.abbreviated
      # data$group <- ordered(data$COS_pub_category,
      #                       levels=fields.raw,
      #                       labels=fields.abbreviated)

      exp_fields <- paper_metadata %>%
        select(paper_id, pub = publication_standard, field = COS_pub_expanded) %>%
        mutate(
          field = case_when(
            str_detect(pub, "financ|Financ") ~ "finance",
            str_detect(pub, "organization|Organization") ~ "org. behavior",
            !str_detect(pub, "organization|Organization") & field == "marketing/org behavior"  ~ "marketing",
            !str_detect(pub, "financ|Financ") & field == "economics"  ~ "economics",
            .default = field
          )
        )
      exp_fields$field <- str_to_title(exp_fields$field)

      data <- merge(data,exp_fields[c("paper_id","field")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)
      group_order <- sort(unique(exp_fields$field))
      data$group <- ordered(data$field,
                            levels=sort(unique(exp_fields$field)),
                            labels=sort(unique(exp_fields$field)))

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)

      data.largecat <- data
      levels(data.largecat$cat) <- c(rep(" Attempted",3),"Not attempted ")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)

      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat

      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      # Trim data for speed
      data <- data %>% select(pub_year,cat,group,weight)
    }

    # Data with the attempteds out
    {
      data.trimmed <- data[data$cat!=as.character(data$cat[length(levels(data$cat))]),]
      data.trimmed$cat <- ordered(data.trimmed$cat,
                                  labels=levels(repro_outcomes$repro_outcome_overall_consolidated),
                                  levels=levels(repro_outcomes$repro_outcome_overall_consolidated))

      # Define nesting structure
      cat <- levels(data.trimmed$cat)
      nesting.structure.trimmed <- data.frame(cat)

      nesting.structure.trimmed$cat2 <- nesting.structure.trimmed$cat
      nesting.structure.trimmed$cat3 <- nesting.structure.trimmed$cat

      nesting.structure.trimmed$cat <- ordered(nesting.structure.trimmed$cat)
      nesting.structure.trimmed$cat2 <- ordered(nesting.structure.trimmed$cat2)
      nesting.structure.trimmed$cat3 <- ordered(nesting.structure.trimmed$cat3)

      # data.trimmed <- data.trimmed %>%
      #   group_by(paper_id) %>%
      #   mutate(weight=1/n())

      # Trim data for speed
      #data.trimmed <- data.trimmed %>% select(field,pub_year,cat,group,weight)
    }

    # Aesthetic setup
    {
      chart.palette <- palette_outcome_repro_charts

      chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(1.2,1.2,3.2,1)
      n_bins_max <- 150
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
    }

    # Plots
    {
      # Group by group plots
      plotlist <- lapply(1:length(group_order),function(x) {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        data.group <- data %>%
          filter(group==group_order[x]) #%>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        data.trimmed.group <- data.trimmed %>%
          filter(group==group_order[x]) #%>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        data.largecat.group <- data.largecat %>%
          filter(group==group_order[x])# %>%
        # group_by(paper_id) %>%
        # mutate(weight=1/n())

        rounded.bars.cutoff <- rounded.bars(data.trimmed.group,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)

        rounded.bars.largecat <- rounded.bars(data.largecat.group,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot

        snakebins_plot <- snakebins(data.group,nesting.structure,
                                    chart.palette = chart.palette,
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot
        plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      })
      # Blank / right axis
      {
        group_label <- ggplot()+theme_nothing()

        rounded.bars.cutoff <- ggplot()+theme_nothing()

        rounded.bars.largecat <- ggplot()+theme_nothing()

        snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                    chart.palette = "white",
                                    n_bins_max=n_bins_max,
                                    axis_only = TRUE,
                                    collapsevar="paper_id")$plot+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }

      # Totals row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=1,y=1,label="All fields",size=y_axis_text_size,fontface="bold",hjust=1)+xlim(0,1)

        cats_rects <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                   weightvar="weight",
                                   chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.cutoff <- rounded.bars(data.trimmed,nesting.structure.trimmed,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          xlim(bars_range)+
          geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=cat),
                    color=c("white","black","white"),size=legend_text_size,fontface="bold")+
          geom_text(data=cats_rects,aes(x=c(0,0.5,1),y=c(1.35,0.5,-.35),label=c("","","")),
                    hjust=c(0,0.5,1),vjust=c(1,0.5,0))

        cats_rects <- rounded.bars(data.largecat,nesting.structure.largecat,
                                   weightvar="weight",
                                   chart.palette = chart.palette.largecat,
                                   display_axis = FALSE)$cats_rects
        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              weightvar="weight",
                                              chart.palette = chart.palette.largecat,
                                              display_axis = FALSE)$plot+
          #geom_text(data=cats_rects,aes(x=xmax,y=c(0.90,.1),label=cat),
          geom_text(data=cats_rects,aes(x=c(0,1),y=c(1.35,-.35),label=cat),
                    color=c("black","black"),size=legend_text_size,fontface="bold",
                    hjust=c(0,1),vjust=c(1,0))
        snakebins_plot <- ggplot()+theme_nothing()

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
      }

      # Axis row
      {
        group_label <- ggplot()+theme_nothing()+
          annotate("text",x=0.5,y=1,label="")
        rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                            chart.palette = rep("white",length(chart.palette)),
                                            axis_only = TRUE,)$plot+
          scale_x_continuous(limits=bars_range,labels=scales::percent_format())+
          theme(axis.text.x=element_text(size=x_axis_text_size))


        rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                              chart.palette = rep("white",length(chart.palette.largecat)),
                                              axis_only = TRUE)$plot+
          scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))+
          theme(axis.text.x=element_text(size=x_axis_text_size))

        snakebins_plot <- snakebins(data,nesting.structure,
                                    chart.palette = rep("white",length(chart.palette)),
                                    n_bins_max=n_bins_max,
                                    display_axis = FALSE,
                                    collapsevar="paper_id")$plot+
          xlim(0,n_bins_max)

        plotlist[[length(plotlist)+1]] <-
          plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,
                    ncol=4,rel_widths = col_widths,align="v")
      }
    }
    # Display plots
    figure_s9 <- bundle_ggplot(
      plot = plot_grid(plotlist=plotlist,ncol=1,align = "v",
                       rel_heights = c(rep(1,length(plotlist)-3),0.5,3,0.5)),
      width = 6000,height = 2000,units = "px",bg="white"
    )
      

  }
  # Figure S10. Outcome reproducibility by 12 subfields and by year
  {
    # Data manipulation
    {
      data <- status %>%
        filter(RR) %>%
        filter(p1_delivery | p2_delivery) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
      # data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
      #               by="claim_id",all.x=TRUE,all.y=FALSE)
      repro_outcomes.trimmed <-  repro_outcomes %>%
        filter(repro_version_of_record=="T") %>%
        select(c("claim_id","repro_outcome_overall"))
      data <- merge(data,repro_outcomes.trimmed,
                    by="claim_id",all.x=TRUE,all.y=FALSE)

      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)

      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL
      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)
      data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        # mutate(weight=1/n())
        mutate(weight=1)

      exp_fields <- paper_metadata %>%
        select(paper_id, pub = publication_standard, field = COS_pub_expanded) %>%
        mutate(
          field = case_when(
            str_detect(pub, "financ|Financ") ~ "finance",
            str_detect(pub, "organization|Organization") ~ "org. behavior",
            !str_detect(pub, "organization|Organization") & field == "marketing/org behavior"  ~ "marketing",
            !str_detect(pub, "financ|Financ") & field == "economics"  ~ "economics",
            .default = field
          )
        )
      exp_fields$field <- str_to_title(exp_fields$field)

      data <- merge(data,exp_fields[c("paper_id","field")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)
      field_order <- sort(unique(exp_fields$field))

      data$field <- ordered(data$field,
                            levels=sort(unique(exp_fields$field)),
                            labels=sort(unique(exp_fields$field)))


      # Assign group
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)

      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)

      data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)

      data$cat <- ordered(data$repro_outcome_overall,
                          labels=c( "Precisely\nReproduced",
                                    "Precisely\nReproduced",
                                    "Approximately\nReproduced",
                                    "Not\nReproduced",
                                    "Not\nAttempted"),
                          levels=c( "Push Button",
                                    "Precise",
                                    "Approximate",
                                    "Not",
                                    "Not Attempted"))

      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())

      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)

      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat

      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)


      data.largecat <- data
      levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)

      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat

      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)

      #chart.palette.largecat <- c(palette_score_charts[6],chart.palette[5])

      # Trim data for speed
      data <- data %>% select(field,pub_year,cat,group,weight)
    }

    # Aesthetic setup
    {
      chart.palette <- palette_outcome_repro_charts

      chart.palette.largecat <- palette_outcome_repro_charts_attempts

      bars_range <- c(0,1)
      col_widths <- c(.4,1.2,4,1)
      y_axis_text_size <- 6
      x_axis_text_size <- 12
      legend_text_size <- 6
      fields_text_size <- 8
    }

    # Plots
    {
      # Main plots top half
      {
        # Group by field plots
        {
          plotlist_field <- lapply(levels(data$field)[1:6],function(field) {
            data_field <- data[data$field==field,]

            plotlist <- lapply(1:length(group_order),function(x) {
              data.field.group <- data_field %>%
                filter(group==group_order[x]) %>%
                group_by(paper_id) %>%
                #mutate(weight=1/n())
                mutate(weight=1)

              cats_rects <- rounded.bars(data.field.group,nesting.structure,
                                         weightvar="weight",
                                         chart.palette = chart.palette,
                                         display_axis = FALSE)$cats_rects
              cats_rects_final <- cats_rects[nrow(cats_rects),]
              rounded.bars.cutoff <- rounded.bars(data.field.group,nesting.structure,
                                                  weightvar="weight",
                                                  chart.palette = chart.palette,
                                                  display_axis = FALSE)$plot+
                funkyheatmap::geom_rounded_rect(data=cats_rects_final,
                                                aes(xmin=xmin,
                                                    ymin=ymin,
                                                    xmax=bars_range[2],
                                                    ymax=ymax),
                                                radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
                xlim(bars_range)
              rounded.bars.cutoff
            })

            plotlist <- append(list(ggplot() + theme_nothing()+
                                      annotate("text",x=0.5,y=1,label=field,size=fields_text_size,fontface="bold")),
                               plotlist)

            plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
          })
          plots_field <- plot_grid(plotlist=plotlist_field,nrow=1)
        }
        # Year labels
        {
          plotlist_yearlabels <- lapply(1:length(group_order),function(x) {
            ggplot() + theme_nothing()+
              annotate("text",x=0.5,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold")
          })
          plotlist_yearlabels <- append(list(ggplot() + theme_nothing()+
                                               annotate("text",x=0.5,y=1,label="")),
                                        plotlist_yearlabels)
          year_labels <- plot_grid(plotlist=plotlist_yearlabels,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))

        }

        main_plots_top <- plot_grid(year_labels,plots_field,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
      }
      # Main plots bottom half
      {
        # Group by field plots
        {
          plotlist_field <- lapply(levels(data$field)[7:12],function(field) {
            data_field <- data[data$field==field,]

            plotlist <- lapply(1:length(group_order),function(x) {
              data.field.group <- data_field %>%
                filter(group==group_order[x]) %>%
                group_by(paper_id) %>%
                #mutate(weight=1/n())
                mutate(weight=1)

              cats_rects <- rounded.bars(data.field.group,nesting.structure,
                                         weightvar="weight",
                                         chart.palette = chart.palette,
                                         display_axis = FALSE)$cats_rects
              cats_rects_final <- cats_rects[nrow(cats_rects),]
              rounded.bars.cutoff <- rounded.bars(data.field.group,nesting.structure,
                                                  weightvar="weight",
                                                  chart.palette = chart.palette,
                                                  display_axis = FALSE)$plot+
                funkyheatmap::geom_rounded_rect(data=cats_rects_final,
                                                aes(xmin=xmin,
                                                    ymin=ymin,
                                                    xmax=bars_range[2],
                                                    ymax=ymax),
                                                radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
                xlim(bars_range)
              rounded.bars.cutoff
            })

            plotlist <- append(list(ggplot() + theme_nothing()+
                                      annotate("text",x=0.5,y=1,label=field,size=fields_text_size,fontface="bold")),
                               plotlist)

            plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
          })
          plots_field <- plot_grid(plotlist=plotlist_field,nrow=1)
        }
        # Year labels
        {
          plotlist_yearlabels <- lapply(1:length(group_order),function(x) {
            ggplot() + theme_nothing()+
              annotate("text",x=0.5,y=1,label=group_order[x],size=y_axis_text_size,fontface="bold")
          })
          plotlist_yearlabels <- append(list(ggplot() + theme_nothing()+
                                               annotate("text",x=0.5,y=1,label="")),
                                        plotlist_yearlabels)
          year_labels <- plot_grid(plotlist=plotlist_yearlabels,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))

        }

        main_plots_bottom <- plot_grid(year_labels,plots_field,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
      }
      # Legend
      data.legend <- data %>% group_by(cat) %>% summarise(n=n())
      cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$cats_rects
      legend <- rounded.bars(data.legend,nesting.structure,
                             chart.palette = chart.palette,
                             display_axis=TRUE)$plot+
        geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                  color=c("white","black","white","black"),size=legend_text_size,fontface="bold")+
        theme(axis.text.x=element_text(size=x_axis_text_size))+
        scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))
      legend_bar <- plot_grid(ggplot()+theme_nothing()+
                                annotate("text",x=0.5,y=1,label=""),
                              legend,nrow=1,rel_widths=c(0.5,length(levels(data$field))))
    }
    figure_s10 <- bundle_ggplot(
      plot=plot_grid(main_plots_top,main_plots_bottom,legend_bar,ncol=1,
                     rel_heights = c(1+length(unique(data$pub_year)),1+length(unique(data$pub_year)),1.8)),
      width = 6000,height = 5000,units = "px",bg="white"
    )
      
  }

  # Export
  {
    return(list(
      "figure_1"=figure_1,
      "figure_2"=figure_2,
      "figure_3"=figure_3,
      "figure_4"=figure_4,
      "figure_5"=figure_5,
      "figure_6"=figure_6,
      "figure_s1"=figure_s1,
      "figure_s2"=figure_s2,
      "figure_s3"=figure_s3,
      "figure_s4"=figure_s4,
      "figure_s5"=figure_s5,
      "figure_s6"=figure_s6,
      "figure_s7"=figure_s7,
      "figure_s8"=figure_s8,
      "figure_s9"=figure_s9,
      "figure_s10"=figure_s10))
  }
}
