
# Create an object that contains the tagged stats
tagged_stats <- function(iters = 100,repro_outcomes,pr_outcomes,orig_outcomes,paper_metadata){

  # Data preparation
  {
    # Trim data to remove COVID papers
      orig_outcomes <- orig_outcomes[!orig_outcomes$is_covid,]
      repro_outcomes <- repro_outcomes[!repro_outcomes$is_covid,]
      pr_outcomes <- pr_outcomes[!pr_outcomes$covid,]
    
    # Trim out non-version of record entries (and save original version)
      repro_outcomes_inc_vor <- repro_outcomes
      repro_outcomes <- repro_outcomes[!is.na(repro_outcomes$repro_version_of_record)&repro_outcomes$repro_version_of_record=="T",]
    
    # Merge in paper metadata
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

  }

  # Key variables
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
    
    paper_metadata$field <- str_to_title(paper_metadata$COS_pub_category)
    paper_metadata$field <- str_replace_all(paper_metadata$field,"And","and")
    paper_metadata$field <- ordered(paper_metadata$field,
                                           levels=sort(unique(paper_metadata$field)),labels=sort(unique(repro_outcomes_merged$field)))

    repro_papers$field <- str_to_title(repro_papers$COS_pub_category)
    repro_papers$field <- str_replace_all(repro_papers$field,"And","and")
    repro_papers$field <- ordered(repro_papers$field,
                                           levels=sort(unique(repro_papers$field)),labels=sort(unique(repro_papers$field)))
    
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
    
    repro_outcomes_expanded <- merge(repro_outcomes_expanded,repro_outcomes[c("claim_id","repro_outcome_overall")],
                  by="claim_id",all.x=TRUE,all.y=FALSE)
    repro_outcomes_expanded <- merge(repro_outcomes_expanded,paper_metadata[c("paper_id","pub_year","COS_pub_category")],
                                     by="paper_id",all.x =TRUE,all.y=FALSE)
    
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
        n <- paste0(total," (100%)")
        data$n <- paste0(data$n," (",
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
                         filter(str_detect(all_types, "Source Data Reproduction")) %>%
                         select(paper_id) %>%
                         bind_rows(
                           pr_outcomes %>%
                             filter(!covid & OA_data_shared != "no") %>%
                             select(paper_id)
                         ) %>%
                         distinct())
      # Papers with reproduction started
      r6 <- format.row(all_rr_attempts  %>%
                         filter(str_detect(type, "Reproduction")) %>%
                         select(paper_id) %>%
                         distinct() %>%
                         semi_join(status %>% filter(RR), by = "paper_id"))
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
      r9 <- format.row(repro_outcomes %>%
                         semi_join(status %>% filter(RR), by = "paper_id") %>% 
                         select(paper_id, claim_id) %>% 
                         distinct()) 
      
      
      table_2 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9)
      for (row in 1:nrow(table_2)){
        for (col in 1:ncol(table_2)){
          assign(paste0("table_2_",row,"_",col),
                 table_2[row,col])
        }
      }
      rm(r1,r2,r3,r4,r5,r6,r7,r8)
    }
    
    # Table 3
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
        n <- paste0(total," (100%)")
        data$n <- paste0(data$n," (",
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
                         filter(str_detect(all_types, "Source Data Reproduction")) %>%
                         select(paper_id) %>%
                         bind_rows(
                           pr_outcomes %>%
                             filter(!covid & OA_data_shared != "no") %>%
                             select(paper_id)
                         ) %>%
                         distinct())
      # Papers with reproduction started
      r6 <- format.row(all_rr_attempts  %>%
                         filter(str_detect(type, "Reproduction")) %>%
                         select(paper_id) %>%
                         distinct() %>%
                         semi_join(status %>% filter(RR), by = "paper_id"))
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
      r9 <- format.row(repro_outcomes %>%
                         semi_join(status %>% filter(RR), by = "paper_id") %>% 
                         select(paper_id, claim_id) %>% 
                         distinct()) 
      
      
      table_3 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9)
      for (row in 1:nrow(table_3)){
        for (col in 1:ncol(table_3)){
          assign(paste0("table_3_",row,"_",col),
                 table_3[row,col])
        }
      }
      rm(r1,r2,r3,r4,r5,r6,r7,r8)
    }
    
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
      
    }
    
    # Results: Process reproducibility
    {
      n_papers_assess_process_repro <- nrow(pr_outcomes_modified)
      
      p_papers_assess_process_repro <- format.text.percent(nrow(pr_outcomes_modified),
                                                           nrow(pr_outcomes_modified))
      
      n_papers_data_available <- sum(pr_outcomes_modified$data_available=="Yes")
      p_papers_data_available <- format.text.percent(n_papers_data_available,n_papers_assess_process_repro)
      
      n_papers_code_available <- sum(pr_outcomes_modified$code_available=="Yes")
      p_papers_code_available <- format.text.percent(n_papers_code_available,n_papers_assess_process_repro)
      
      n_papers_data_unavailable <- sum(pr_outcomes_modified$data_available=="No")
      p_papers_data_unavailable <- format.text.percent(n_papers_data_unavailable,n_papers_assess_process_repro)
      
      n_papers_code_unavailable <- sum(pr_outcomes_modified$code_available=="No")
      p_papers_code_unavailable <- format.text.percent(n_papers_code_unavailable,n_papers_assess_process_repro)
      
      n_papers_data_and_code_available <- sum(pr_outcomes_modified$data_available=="Yes" & pr_outcomes_modified$code_available=="Yes")
      p_papers_data_and_code_available <- format.text.percent(n_papers_data_and_code_available,n_papers_assess_process_repro)
      
      n_papers_data_or_code_available <- sum(pr_outcomes_modified$data_available=="Yes" | pr_outcomes_modified$code_available=="Yes")
      p_papers_data_or_code_available <- format.text.percent(n_papers_data_or_code_available,n_papers_assess_process_repro)
      
      n_papers_data_only_available <- sum(pr_outcomes_modified$data_available=="Yes" & pr_outcomes_modified$code_available=="No")
      p_papers_data_only_available <- format.text.percent(n_papers_data_only_available,n_papers_assess_process_repro)
      
      n_papers_code_only_available <- sum(pr_outcomes_modified$data_available=="No" & pr_outcomes_modified$code_available=="Yes")
      p_papers_code_only_available <- format.text.percent(n_papers_code_only_available,n_papers_assess_process_repro)
      
      n_papers_neither_code_nor_data_available <- sum(pr_outcomes_modified$data_available=="No" & pr_outcomes_modified$code_available=="No")
      p_papers_neither_code_nor_data_available <- format.text.percent(n_papers_neither_code_nor_data_available,n_papers_assess_process_repro)
      
    }
    
    # Results: Process reproducibility by year of original publication
    {
      rho <- SpearmanRho(x=as.numeric(pr_outcomes_modified$pub_year),
               y=as.numeric(pr_outcomes_modified$data_available=="Yes"),
               conf.level=.95)
      rho_data_avail_v_year <- format.text.CI(rho[1],rho[2],rho[3],digits=2)
      
      rho <- SpearmanRho(x=as.numeric(pr_outcomes_modified$pub_year),
                         y=as.numeric(pr_outcomes_modified$code_available=="Yes"),
                         conf.level=.95)
      rho_data_avail_v_year <- format.text.CI(rho[1],rho[2],rho[3],digits=2)
      
      rho <- SpearmanRho(x=as.numeric(pr_outcomes_modified$pub_year),
                         y=as.numeric(pr_outcomes_modified$code_available=="Yes" & pr_outcomes_modified$data_available=="Yes" ),
                         conf.level=.95)
      rho_code_and_data_avail_v_year <- format.text.CI(rho[1],rho[2],rho[3],digits=2)
      
      rho <- SpearmanRho(x=as.numeric(pr_outcomes_modified[pr_outcomes_modified$data_available=="Yes" | pr_outcomes_modified$code_available=="Yes",]$pub_year),
                         y=as.numeric(pr_outcomes_modified[pr_outcomes_modified$data_available=="Yes" | pr_outcomes_modified$code_available=="Yes",]$code_available=="Yes" &
                                        pr_outcomes_modified[pr_outcomes_modified$data_available=="Yes" | pr_outcomes_modified$code_available=="Yes",]$data_available=="Yes" ),
                         conf.level=.95)
      rho_code_and_data_avail_v_year_among_either <- format.text.CI(rho[1],rho[2],rho[3],digits=2)
      
      rm(rho)
      
      p_papers_data_or_code_available_among_either<- 
        format.text.percent(n_papers_data_and_code_available,n_papers_data_or_code_available)
      
    }
    
    # Process reproducibility by discipline
    {
      n_papers_data_available_econ <- sum(pr_outcomes_modified[pr_outcomes_modified$field=="Economics and Finance",]$data_available=="Yes")
      n_papers_econ <- sum(pr_outcomes_modified$field=="Economics and Finance")
      p_papers_data_available_econ <- format.text.percent(n_papers_data_available_econ,n_papers_econ)
      
      n_papers_data_available_polisci <- sum(pr_outcomes_modified[pr_outcomes_modified$field=="Political Science",]$data_available=="Yes")
      n_papers_polisci <- sum(pr_outcomes_modified$field=="Political Science")
      p_papers_data_available_polisci <- format.text.percent(n_papers_data_available_polisci,n_papers_polisci)
      
      n_papers_data_available_non_econ_polisci <- sum(pr_outcomes_modified[pr_outcomes_modified$field!="Political Science" & pr_outcomes_modified$field!="Economics and Finance",]$data_available=="Yes")
      n_papers_non_econ_polisci <- sum(pr_outcomes_modified$field!="Political Science" & pr_outcomes_modified$field!="Economics and Finance")
      p_papers_data_available_non_econ_polisci <- format.text.percent(n_papers_data_available_non_econ_polisci,n_papers_non_econ_polisci)
      
      n_papers_data_available_edu <- sum(pr_outcomes_modified[pr_outcomes_modified$field=="Education",]$data_available=="Yes")
      n_papers_edu <- sum(pr_outcomes_modified$field=="Education")
      p_papers_data_available_edu <- format.text.percent(n_papers_data_available_edu,n_papers_edu)
      
      n_papers_open_data_econ_polisci <- 
        sum(pr_outcomes_modified[pr_outcomes_modified$field=="Political Science" | pr_outcomes_modified$field=="Economics and Finance",]$data_shared_type=="available online" &
            pr_outcomes_modified[pr_outcomes_modified$field=="Political Science" | pr_outcomes_modified$field=="Economics and Finance",]$code_shared_type=="available online")
      n_papers_econ_polisci <- sum(pr_outcomes_modified$field=="Political Science" | pr_outcomes_modified$field=="Economics and Finance")
      n_papers_open_data_econ_polisci <- format.text.percent(n_papers_open_data_econ_polisci,n_papers_econ_polisci)
      
      n_papers_open_data_non_econ_polisci <- 
        sum(pr_outcomes_modified[pr_outcomes_modified$field!="Political Science" & pr_outcomes_modified$field!="Economics and Finance",]$data_shared_type=="available online" &
              pr_outcomes_modified[pr_outcomes_modified$field!="Political Science" & pr_outcomes_modified$field!="Economics and Finance",]$code_shared_type=="available online")
      n_papers_non_econ_polisci <- sum(pr_outcomes_modified$field=="Political Science" | pr_outcomes_modified$field=="Economics and Finance")
      n_papers_open_data_non_econ_polisci <- format.text.percent(n_papers_open_data_non_econ_polisci,n_papers_non_econ_polisci)
    }
    
    # Assessing outcome reproducibility
    {
      n_papers_OR <- length(unique(repro_outcomes$paper_id))
      
      n_claims_OR <- length(unique(repro_outcomes$claim_id))
      
      claims_per_paper <- repro_outcomes %>%
        group_by(paper_id) %>%
        dplyr::summarise(count = n(),
                         multiple = as.numeric(n()>1))
      
      n_papers_OR_multiple_claims <- sum(claims_per_paper$multiple)
      p_papers_OR_multiple_claims <- paste0(format.round(100*n_papers_OR_multiple_claims/n_papers_OR,1),"%")
      
      mean_claims_per_paper <- format.round(mean(claims_per_paper$count),1)
      SD_claims_per_paper <- format.round(SD(claims_per_paper$count),1)
      range_claims_per_paper <- paste0(min(claims_per_paper$count),"-",max(claims_per_paper$count))
      
      rm(claims_per_paper)
        
    }
    
    # Outcome reproducibility assessments in comparison with the sampling frame
    {
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
      
        r2 <- format.row(status %>% filter(RR))
        # Papers with data available
        r5 <- format.row(all_rr_attempts %>%
                           semi_join(status %>% filter(RR), by = "paper_id") %>%
                           filter(str_detect(all_types, "Source Data Reproduction")) %>%
                           select(paper_id) %>%
                           bind_rows(
                             pr_outcomes %>%
                               filter(!covid & OA_data_shared != "no") %>%
                               select(paper_id)
                           ) %>%
                           distinct())

    }
    
    # Outcome reproducibility assessments
    {
      n_papers_OR_of_all <- length(unique(repro_outcomes_expanded$paper_id))
      
      n_papers_OR_of_all_approx_or_precise <- format.round(sum(repro_outcomes_expanded$weight *
        (repro_outcomes_expanded$repro_outcome_overall=="approximate" | 
           repro_outcomes_expanded$repro_outcome_overall=="precise" | 
           repro_outcomes_expanded$repro_outcome_overall=="push button")),1)

      p_papers_OR_of_all_approx_or_precise <- cw.proportion(
        repro_outcomes_expanded$repro_outcome_overall=="approximate" | 
          repro_outcomes_expanded$repro_outcome_overall=="precise" | 
          repro_outcomes_expanded$repro_outcome_overall=="push button",
        weights=repro_outcomes_expanded$weight,
        clusters = repro_outcomes_expanded$paper_id,iters)$formatted.text
      
      n_papers_OR_of_all_precise <- format.round(sum(repro_outcomes_expanded$weight *
                                                       (repro_outcomes_expanded$repro_outcome_overall=="precise" | 
                                                          repro_outcomes_expanded$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_of_all_precise <- cw.proportion(
        repro_outcomes_expanded$repro_outcome_overall=="precise" | 
          repro_outcomes_expanded$repro_outcome_overall=="push button",
        weights=repro_outcomes_expanded$weight,
        clusters = repro_outcomes_expanded$paper_id,iters)$formatted.text
      
      n_papers_OR_approx_or_precise <- format.round(
        sum(repro_outcomes$weight *
              (repro_outcomes$repro_outcome_overall=="approximate" |
                 repro_outcomes$repro_outcome_overall=="precise" | 
                 repro_outcomes$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_approx_or_precise <- cw.proportion(
        repro_outcomes$repro_outcome_overall=="approximate" | 
          repro_outcomes$repro_outcome_overall=="precise" | 
          repro_outcomes$repro_outcome_overall=="push button",
        weights=repro_outcomes$weight,
        clusters = repro_outcomes$paper_id,iters)$formatted.text
      
      n_papers_OR_precise <- format.round(sum(repro_outcomes$weight *
                                                       (repro_outcomes$repro_outcome_overall=="precise" |
                                                          repro_outcomes$repro_outcome_overall=="push button")),1)
      
      p_papers_OR_precise <- cw.proportion(
        repro_outcomes$repro_outcome_overall=="precise" | 
          repro_outcomes$repro_outcome_overall=="push button",
        weights=repro_outcomes$weight,
        clusters = repro_outcomes$paper_id,iters)$formatted.text
      
      n_papers_OR_precise_pushbutton <- format.round(sum(repro_outcomes$weight *
                                                ((repro_outcomes$repro_outcome_overall=="precise" | 
                                                    repro_outcomes$repro_outcome_overall=="push button") & 
                                                   repro_outcomes$repro_type=="Push Button Reproduction")),1)
      
      p_papers_OR_precise_pushbutton <- cw.proportion(
        (repro_outcomes$repro_outcome_overall=="precise" |
          repro_outcomes$repro_outcome_overall=="push button") &
          repro_outcomes$repro_type=="Push Button Reproduction",
        weights=repro_outcomes$weight,
        clusters = repro_outcomes$paper_id,iters)$formatted.text
      
      repro_outcomes_dc <- repro_outcomes[repro_outcomes$repro_type_consolidated=="Data and code available",]
      repro_outcomes_dc <- repro_outcomes_dc %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())
      
      n_papers_OR_data_and_code <- nrow(repro_outcomes_dc)
      
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
      
      repro_outcomes_do <- repro_outcomes[repro_outcomes$repro_type_consolidated=="Only data available",]
      repro_outcomes_do <- repro_outcomes_do %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())
      
      n_papers_OR_data_only <- nrow(repro_outcomes_do)
      
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
      
      
      
      repro_outcomes_sd <- repro_outcomes[repro_outcomes$repro_type_consolidated=="Data reconstructed from source",]
      n_papers_OR_source_data <- nrow(repro_outcomes_sd)
      
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
      
    }
    
    # Outcome reproducibility by year of original publication
    {
      
      rho_of_all_precise_v_year <- bootstrap.clust(data=repro_outcomes_expanded,
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
      
      rho_of_all_precise_or_approx_v_year <- bootstrap.clust(data=repro_outcomes_expanded,
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
      
      n_papers_OR_econ <- length(unique(repro_outcomes_merged_econ$paper_id))
      
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
      
      n_papers_OR_polisci <- length(unique(repro_outcomes_merged_polisci$paper_id))
      
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
      

      repro_outcomes_merged_other <- repro_outcomes_merged[!is.na(repro_outcomes_merged$field) & 
                                                             repro_outcomes_merged$field!="Economics and Finance" & 
                                                             repro_outcomes_merged$field!="Political Science",]
      
      n_papers_OR_other <- length(unique(repro_outcomes_merged_other$paper_id))
      
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
    p_short_papers_OR_approx_or_precise <- 
      paste0(format.round(100*sum((repro_outcomes$repro_outcome_overall=="approximate" | 
                                     repro_outcomes$repro_outcome_overall=="precise" | 
                                     repro_outcomes$repro_outcome_overall=="push button")*
                                    repro_outcomes$weight
      )/(length(unique(repro_outcomes$paper_id))),1),"%")
    
    p_short_papers_data_unavailable <- 
      format.text.percent(n_papers_data_unavailable,n_papers_assess_process_repro)

    
    
      
  }

  # Clean up input values and export
  {
    rm(iters,orig_outcomes,repro_outcomes,repro_outcomes_merged)
    return(rev(as.list(environment())))
  }
}

# Figures
figures <- function(repro_outcomes,pr_outcomes,orig_outcomes,paper_metadata){
  # Setup and initialization
  {
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
    
  }
  
  # Figure 1. Process reproducibility success rates by year of publication
  {
    # Data wrangling
    {
      data <- pr_outcomes %>% 
        filter(!covid) %>% 
        mutate(d_c_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared != "no") %>% 
        mutate(d_c_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared != "no") %>% 
        mutate(d_c_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared != "no") %>% 
        mutate(d_only_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared == "no") %>% 
        mutate(d_only_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared == "no") %>% 
        mutate(d_only_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared == "no") %>% 
        mutate(no_data_code_available = OA_data_shared == "no" & OA_code_shared != "no") %>% 
        mutate(no_data_no_code = OA_data_shared == "no" & OA_code_shared == "no") %>% 
        select(paper_id, d_c_open, d_c_protect, d_c_sharing, d_only_open, d_only_sharing, no_data_code_available, no_data_no_code) %>% 
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>% 
        filter(response)
      
      data <- merge(data,paper_metadata[c("paper_id","pub_year")],by="paper_id",all.x =TRUE,all.y=FALSE)
      
      
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
      
      data$cat <- ordered(data$cat,
                          levels = c("d_c_open","d_c_protect","d_c_sharing","d_only_open",
                                     "d_only_sharing","no_data_code_available","no_data_no_code"),
                          labels = c("Open data,\ncode available", "Data restricted,\ncode available", 
                                     "Data shared directly,\ncode available", "Open data,\ncode unavailable",
                                     "Data shared directly,\ncode unavailable",
                                     "Data unavailable,\ncode available",
                                     "Neither data nor\ncode available")
      )
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      
      nesting.structure$cat2 <- c("Both","Both","Both",
                                  "Either",
                                  "Either",
                                  "Either",
                                  "Neither")
      nesting.structure$cat3 <- c(NA, NA, NA,"Data only","Data only","Code only",NA)
      
      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
      
    }
    
    # Aesthetic setup
    {
      bars_range <- c(0,1)
      col_widths <- c(0.8,8,1.5)
      n_bins_max <- 80
      
      chart.palette <- c(palette_score_charts[5],
                         lighten(palette_score_charts[5],amount=.25),
                         lighten(palette_score_charts[5],amount=.5),
                         palette_score_charts[2],
                         lighten(palette_score_charts[2],amount=.25),
                         palette_score_charts[1],
                         "grey80"
      )
    }
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
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
                                axis_only = TRUE)$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All years")
    
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
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$cats_rects
    rounded.bars_plot <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","white","black","white","black","black","black"))+
      geom_segment(x=1/3,xend=1/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3+.05,y=.5,yend=0.5,linetype=3)+
      ylim(0,1.2)+
      annotate("text",x=1/6,y=1.05,label="Both Code and Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=1/2,y=1.05,label="Either Code or Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=5/6,y=1.05,label="Neither Code nor Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=2/3+.02,
               y=.25,label="Data only",
               color="black",size=3.7,hjust=0)+
      annotate("text",x=2/3+.02,
               y=.75,label="Code only",
               color="black",size=3.7,hjust=0)
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Display plots
    figure_1 <- 
      plot_grid(plotlist=plotlist,ncol=1,align = "v",
                rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.7,3))
  }
  
  # Figure 2. Process reproducibility success rates by field
  {
    # Data wrangling
    {
      data <- pr_outcomes %>% 
        filter(!covid) %>% 
        mutate(d_c_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared != "no") %>% 
        mutate(d_c_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared != "no") %>% 
        mutate(d_c_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared != "no") %>% 
        mutate(d_only_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared == "no") %>% 
        mutate(d_only_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared == "no") %>% 
        mutate(d_only_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared == "no") %>% 
        mutate(no_data_code_available = OA_data_shared == "no" & OA_code_shared != "no") %>% 
        mutate(no_data_no_code = OA_data_shared == "no" & OA_code_shared == "no") %>% 
        select(paper_id, d_c_open, d_c_protect, d_c_sharing, d_only_open, d_only_sharing, no_data_code_available, no_data_no_code) %>% 
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>% 
        filter(response)
      
      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
      
      data$field <- str_to_title(data$COS_pub_category)
      
      # Assign group
      # data$field <- str_to_title(data$COS_pub_category)
      # data$field <- str_replace_all(data$field," ","\n")
      # data$field <- str_replace_all(data$field,"And","and")
      
      group_order <- fields.format.2.row
      data$group <- ordered(data$COS_pub_category,
                            levels=fields.raw,
                            labels=fields.format.2.row)
      
      data$cat <- ordered(data$cat,
                          levels = c("d_c_open","d_c_protect","d_c_sharing","d_only_open",
                                     "d_only_sharing","no_data_code_available","no_data_no_code"),
                          labels = c("Open data,\ncode available", "Data restricted,\ncode available", 
                                     "Data shared directly,\ncode available", "Open data,\ncode unavailable",
                                     "Data shared directly,\ncode unavailable",
                                     "Data unavailable,\ncode available",
                                     "Neither data nor\ncode available")
      )
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      
      nesting.structure$cat2 <- c("Both","Both","Both",
                                  "Either",
                                  "Either",
                                  "Either",
                                  "Neither")
      nesting.structure$cat3 <- c(NA, NA, NA,"Data only","Data only","Code only",NA)
      
      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
      
    }
    
    # Aesthetic setup
    bars_range <- c(0,1)
    col_widths <- c(0.8,8,1.5)
    n_bins_max <- 80
    
    chart.palette <- c(palette_score_charts[5],
                       lighten(palette_score_charts[5],amount=.25),
                       lighten(palette_score_charts[5],amount=.5),
                       palette_score_charts[2],
                       lighten(palette_score_charts[2],amount=.25),
                       palette_score_charts[1],
                       "grey80"
    )
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
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
                                axis_only = TRUE)$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All fields")
    
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
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$cats_rects
    rounded.bars_plot <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","white","black","white","black","black","black"))+
      geom_segment(x=1/3,xend=1/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3+.05,y=.5,yend=0.5,linetype=3)+
      ylim(0,1.2)+
      annotate("text",x=1/6,y=1.05,label="Both Code and Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=1/2,y=1.05,label="Either Code or Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=5/6,y=1.05,label="Neither Code nor Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=2/3+.02,
               y=.25,label="Data only",
               color="black",size=3.7,hjust=0)+
      annotate("text",x=2/3+.02,
               y=.75,label="Code only",
               color="black",size=3.7,hjust=0)
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Display plots
    figure_2 <- 
      plot_grid(plotlist=plotlist,ncol=1,align = "v",
                rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.7,2))
  }
  
  # Figure 3. Process reproducibility success rates by year of publication for economics and political science versus other disciplines
  {
    data <- pr_outcomes %>% 
      filter(!covid) %>% 
      mutate(d_c_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared != "no") %>% 
      mutate(d_c_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared != "no") %>% 
      mutate(d_c_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared != "no") %>% 
      mutate(d_only_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared == "no") %>% 
      mutate(d_only_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared == "no") %>% 
      mutate(d_only_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared == "no") %>% 
      mutate(no_data_code_available = OA_data_shared == "no" & OA_code_shared != "no") %>% 
      mutate(no_data_no_code = OA_data_shared == "no" & OA_code_shared == "no") %>% 
      select(paper_id, d_c_open, d_c_protect, d_c_sharing, d_only_open, d_only_sharing, no_data_code_available, no_data_no_code) %>% 
      pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>% 
      filter(response)
    
    data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
    group_order <- seq(min(data$pub_year),max(data$pub_year,1))
    #data$pub_year <- ordered(data$pub_year,levels=group_order,labels=group_order)
    data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
    
    data$cat <- ordered(data$cat,
                        levels = c("d_c_open","d_c_protect","d_c_sharing","d_only_open",
                                   "d_only_sharing","no_data_code_available","no_data_no_code"),
                        labels = c("Open data,\ncode available", "Data restricted,\ncode available", 
                                   "Data shared directly,\ncode available", "Open data,\ncode unavailable",
                                   "Data shared directly,\ncode unavailable",
                                   "Data unavailable,\ncode available",
                                   "Neither data nor\ncode available")
    )
    data.econ.poli <- data[data$COS_pub_category=="economics and finance" | data$COS_pub_category=="political science",]
    data.other <- data[!data$COS_pub_category=="economics and finance" & !data$COS_pub_category=="political science",]
    # Define nesting structure
    cat <- levels(data$cat)
    nesting.structure <- data.frame(cat)
    
    nesting.structure$cat2 <- c("Both","Both","Both",
                                "Either",
                                "Either",
                                "Either",
                                "Neither")
    nesting.structure$cat3 <- c(NA, NA, NA,"Data only","Data only","Code only",NA)
    
    nesting.structure$cat <- ordered(nesting.structure$cat)
    nesting.structure$cat2 <- ordered(nesting.structure$cat2)
    nesting.structure$cat3 <- ordered(nesting.structure$cat3)
    
    chart.palette <- c(palette_score_charts[5],
                       lighten(palette_score_charts[5],amount=.25),
                       lighten(palette_score_charts[5],amount=.5),
                       palette_score_charts[2],
                       lighten(palette_score_charts[2],amount=.25),
                       palette_score_charts[1],
                       "grey80"
    )
    
    plotlist.left <- lapply(1:length(group_order),function(x) 
      rounded.bars(data.econ.poli[data.econ.poli$pub_year==group_order[x],],nesting.structure,
                   chart.palette = chart.palette,
                   display_axis = FALSE,
                   flip_x=TRUE)$plot+
        theme(axis.title.y=element_blank())+
        ylab(group_order[x])
    )
    plotlist.left[[length(plotlist.left)+1]] <-
      rounded.bars(data[data$pub_year==group_order[1],],nesting.structure,
                   chart.palette = rep("white",length(group_order)),
                   axis_only = TRUE,flip_x=TRUE)$plot
    plot.left <- plot_grid(plotlist=plotlist.left,ncol=1,align = "v")
    
    plotlist.right <- lapply(1:length(group_order),function(x) 
      rounded.bars(data.other[data.other$pub_year==group_order[x],],nesting.structure,
                   chart.palette = chart.palette,
                   display_axis = FALSE,)$plot+
        theme(axis.title.y=element_blank())+
        ylab(group_order[x])
    )
    plotlist.right[[length(plotlist.right)+1]] <-
      rounded.bars(data[data$pub_year==group_order[1],],nesting.structure,
                   chart.palette = rep("white",length(group_order)),
                   axis_only = TRUE)$plot
    plot.right <- plot_grid(plotlist=plotlist.right,ncol=1,align = "v")
    
    plotlist.center <- lapply(1:length(group_order),function(x) 
      #ggplot()+theme_blank() +
      ggplot()+theme_minimal() +
        theme(legend.position = "none",
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.title=element_blank(),
              axis.text = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
        lims(x= c(0,1), y = c(0,1))+
        annotate("text",x=0.5,y=0.5,
                 label=group_order[x])
    )
    plotlist.center[[length(plotlist.center)+1]] <-
      ggplot()+theme_nothing() +
      theme(legend.position = "none",
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.title=element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
      scale_x_continuous(limits=c(0,1),expand=c(0,0))+
      scale_y_continuous(limits=c(0,1),expand=c(0,0))+
      annotate("text",x=0.5,y=0.5,
               label=" ",hjust=0.5)
    
    plot.center <- plot_grid(plotlist=plotlist.center,ncol=1,align = "v",rel_heights = c(rep(1,length(plotlist.center)-1),0.5))
    
    width.ratio <- 8
    
    plot.top.labels <-
      plot_grid(
        ggplot()+theme_nothing() +
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title=element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
          scale_x_continuous(limits=c(-1,0),expand=c(0,0))+
          scale_y_continuous(limits=c(0,1),expand=c(0,0))+
          annotate("text",x=-.5,y=0.5,
                   label="Economics and Political Science",hjust=0.5),
        ggplot()+theme_nothing() +
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title=element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
          scale_x_continuous(limits=c(0,1),expand=c(0,0))+
          scale_y_continuous(limits=c(0,1),expand=c(0,0))+
          annotate("text",x=0.5,y=0.5,
                   label=" "),
        ggplot()+theme_nothing() +
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title=element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
          scale_x_continuous(limits=c(0,1),expand=c(0,0))+
          scale_y_continuous(limits=c(0,1),expand=c(0,0))+
          annotate("text",x=0.5,y=0.5,
                   label="All other fields",hjust=0.5),
        ncol=3,rel_widths = c(width.ratio,1,width.ratio))
    
    plot.main <- plot_grid(plot.left,plot.center,plot.right,ncol=3,rel_widths = c(width.ratio,1,width.ratio))
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$cats_rects
    legend <- rounded.bars(data.legend,nesting.structure,
                           chart.palette = chart.palette,
                           display_axis=FALSE,legend=TRUE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","white","black","white","black","black","black"))+
      geom_segment(x=1/3,xend=1/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3+.05,y=.5,yend=0.5,linetype=3)+
      ylim(0,1.2)+
      annotate("text",x=1/6,y=1.05,label="Both Code and Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=1/2,y=1.05,label="Either Code or Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=5/6,y=1.05,label="Neither Code nor Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=2/3+.02,
               y=.25,label="Data only",
               color="black",size=3.7,hjust=0)+
      annotate("text",x=2/3+.02,
               y=.75,label="Code only",
               color="black",size=3.7,hjust=0)
    
    
    figure_3 <- 
      plot_grid(plot.top.labels,plot.main,legend,
              ncol=1,rel_heights = c(1/length(plotlist.left),1,3/length(plotlist.left)))
  }
  
  # Figure 4. Outcome reproducibility by whether data and code were available, only data was available, and data was reconstructed from source data was available
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
    data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall","repro_type")],
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
    data$field <- str_to_title(data$COS_pub_category)
    
    # # Assign group
    # group_order <- c("Source Data Reproduction",
    #                  "Author Data Reproduction",
    #                  "Extended Push Button Reproduction",
    #                  "Push Button Reproduction"
    # )
    # data$group <- ordered(data$repro_type,levels=group_order,labels=group_order)
    
    group_order <- c("Data and code available",
                     "Only data available","Data reconstructed from source")
    
    data$group <- 
      ordered(data$repro_type,
              labels=c("Data and code available","Data and code available",
                       "Only data available","Data reconstructed from source"),
              levels=c("Push Button Reproduction","Extended Push Button Reproduction",
                       "Author Data Reproduction","Source Data Reproduction")
      )
    
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    
    data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
    
    data$cat <- ordered(data$repro_outcome_overall,
                        labels=c( "Push Button\nReproduced",
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
    
    chart.palette <- c(palette_score_charts[1],
                       palette_score_charts[4],
                       palette_score_charts[3],
                       palette_score_charts[2],
                       "grey90")
    
    data.largecat <- data
    levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
    cat <- levels(data.largecat$cat)
    nesting.structure.largecat <- data.frame(cat)
    
    nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
    nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat
    
    nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
    nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
    nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)
    
    chart.palette.largecat <- c(palette_score_charts[5],chart.palette[5])
    
    # Aesthetic setup
    bars_range <- c(0,1)
    col_widths <- c(1,4,1)
    n_bins_max <- 80
    
    # Drop not attempteds / missing repro types
    data <- data[!is.na(data$repro_type),]
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
      cats_rects <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                 weightvar="weight",
                                 chart.palette = chart.palette,
                                 display_axis = FALSE)$cats_rects
      cats_rects_final <- cats_rects[nrow(cats_rects),]
      rounded.bars.cutoff <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          weightvar="weight",
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot+
        xlim(bars_range)
      
      snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                  chart.palette = chart.palette,
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    })
    # Blank / right axis
    group_label <- ggplot()+theme_nothing()
    
    rounded.bars.cutoff <- ggplot()+theme_nothing()
    
    snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                chart.palette = "white",
                                n_bins_max=n_bins_max,
                                axis_only = TRUE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All types")
    
    cats_rects <- rounded.bars(data,nesting.structure,
                               weightvar="weight",
                               chart.palette = chart.palette,
                               display_axis = FALSE)$cats_rects
    cats_rects_final <- cats_rects[nrow(cats_rects),]
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        weightvar="weight",
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot+
      xlim(bars_range)
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Axis row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.cutoff <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("black","black","black","white"))
    
    data.legend <- data.largecat %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure.largecat,
                                      chart.palette = chart.palette.largecat,
                                      display_axis=FALSE)$cats_rects
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Display plots
    figure_4 <- plot_grid(plotlist=plotlist,ncol=1,align = "v",
              rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.5,.8))
    
  }
  
  # Figure 5. Outcome reproducibility by year of publication
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
    data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
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
    data$field <- str_to_title(data$COS_pub_category)
    
    # Assign group
    group_order <- seq(min(data$pub_year),max(data$pub_year,1))
    data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
    
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    
    data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
    
    data$cat <- ordered(data$repro_outcome_overall,
                        labels=c( "Push Button\nReproduced",
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
    
    chart.palette <- c(palette_score_charts[1],
                       palette_score_charts[4],
                       palette_score_charts[3],
                       palette_score_charts[2],
                       "grey90")
    
    data.largecat <- data
    levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
    cat <- levels(data.largecat$cat)
    nesting.structure.largecat <- data.frame(cat)
    
    nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
    nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat
    
    nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
    nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
    nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)
    
    chart.palette.largecat <- c(palette_score_charts[5],chart.palette[5])
    
    # Aesthetic setup
    bars_range <- c(0,.25)
    col_widths <- c(.4,1.2,4,1)
    n_bins_max <- 80
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
      cats_rects <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                 weightvar="weight",
                                 chart.palette = chart.palette,
                                 display_axis = FALSE)$cats_rects
      cats_rects_final <- cats_rects[nrow(cats_rects),]
      rounded.bars.cutoff <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          weightvar="weight",
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot+
        funkyheatmap::geom_rounded_rect(data=cats_rects_final,aes(xmin=cats_rects_final$xmin,
                                                                  ymin=cats_rects_final$ymin,
                                                                  xmax=bars_range[2],
                                                                  ymax=cats_rects_final$ymax),
                                        radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
        xlim(bars_range)+
        ggpattern::geom_rect_pattern(data=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
                                                               ymin=cats_rects_final$ymin,
                                                               xmax=bars_range[2],
                                                               ymax=cats_rects_final$ymax),
                                     pattern="gradient",pattern_orientation = "horizontal",
                                     pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
      
      rounded.bars.largecat <- rounded.bars(data.largecat[data.largecat$group==group_order[x],],nesting.structure.largecat,
                                            weightvar="weight",
                                            chart.palette = chart.palette.largecat,
                                            display_axis = FALSE)$plot
      
      snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                  chart.palette = chart.palette,
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    })
    # Blank / right axis
    group_label <- ggplot()+theme_nothing()
    
    rounded.bars.cutoff <- ggplot()+theme_nothing()
    
    rounded.bars.largecat <- ggplot()+theme_nothing()
    
    snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                chart.palette = "white",
                                n_bins_max=n_bins_max,
                                axis_only = TRUE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All years")
    
    cats_rects <- rounded.bars(data,nesting.structure,
                               weightvar="weight",
                               chart.palette = chart.palette,
                               display_axis = FALSE)$cats_rects
    cats_rects_final <- cats_rects[nrow(cats_rects),]
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        weightvar="weight",
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot+
      funkyheatmap::geom_rounded_rect(aes(xmin=cats_rects_final$xmin,
                                          ymin=cats_rects_final$ymin,
                                          xmax=bars_range[2],
                                          ymax=cats_rects_final$ymax),
                                      radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
      xlim(bars_range)+
      ggpattern::geom_rect_pattern(data=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
                                                             ymin=cats_rects_final$ymin,
                                                             xmax=bars_range[2],
                                                             ymax=cats_rects_final$ymax),
                                   pattern="gradient",pattern_orientation = "horizontal",
                                   pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
    
    rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                          weightvar="weight",
                                          chart.palette = chart.palette.largecat,
                                          display_axis = FALSE)$plot
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Axis row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    
    rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                          chart.palette = rep("white",length(chart.palette.largecat)),
                                          axis_only = TRUE)$plot+
      scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.cutoff <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("black","black","black","white","black"))
    
    data.legend <- data.largecat %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure.largecat,
                                      chart.palette = chart.palette.largecat,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.largecat <- rounded.bars(data.legend,nesting.structure.largecat,
                                          chart.palette = chart.palette.largecat,
                                          display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","black"))
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    # Display plots
    figure_5 <- 
      plot_grid(plotlist=plotlist,ncol=1,align = "v",
                rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.5,1.5))
    
  }
  
  # Figure 6. Outcome reproducibility by discipline
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
    data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
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
    
    group_order <- fields.format.2.row
    data$group <- ordered(data$COS_pub_category,
                          levels=fields.raw,
                          labels=fields.format.2.row)
    
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    
    data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
    
    data$cat <- ordered(data$repro_outcome_overall,
                        labels=c( "Push Button\nReproduced",
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
    
    chart.palette <- c(palette_score_charts[1],
                       palette_score_charts[4],
                       palette_score_charts[3],
                       palette_score_charts[2],
                       "grey90")
    
    data.largecat <- data
    levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
    cat <- levels(data.largecat$cat)
    nesting.structure.largecat <- data.frame(cat)
    
    nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
    nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat
    
    nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
    nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
    nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)
    
    chart.palette.largecat <- c(palette_score_charts[5],chart.palette[5])
    
    # Aesthetic setup
    bars_range <- c(0,.4)
    col_widths <- c(.4,1.2,4,1)
    n_bins_max <- 180
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
      cats_rects <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                 weightvar="weight",
                                 chart.palette = chart.palette,
                                 display_axis = FALSE)$cats_rects
      cats_rects_final <- cats_rects[nrow(cats_rects),]
      rounded.bars.cutoff <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          weightvar="weight",
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot+
        funkyheatmap::geom_rounded_rect(data=cats_rects_final,aes(xmin=cats_rects_final$xmin,
                                                                  ymin=cats_rects_final$ymin,
                                                                  xmax=bars_range[2],
                                                                  ymax=cats_rects_final$ymax),
                                        radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
        xlim(bars_range)+
        ggpattern::geom_rect_pattern(data=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
                                                               ymin=cats_rects_final$ymin,
                                                               xmax=bars_range[2],
                                                               ymax=cats_rects_final$ymax),
                                     pattern="gradient",pattern_orientation = "horizontal",
                                     pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
      
      rounded.bars.largecat <- rounded.bars(data.largecat[data.largecat$group==group_order[x],],nesting.structure.largecat,
                                            weightvar="weight",
                                            chart.palette = chart.palette.largecat,
                                            display_axis = FALSE)$plot
      
      snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                  chart.palette = chart.palette,
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    })
    # Blank / right axis
    group_label <- ggplot()+theme_nothing()
    
    rounded.bars.cutoff <- ggplot()+theme_nothing()
    
    rounded.bars.largecat <- ggplot()+theme_nothing()
    
    snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                chart.palette = "white",
                                n_bins_max=n_bins_max,
                                axis_only = TRUE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All fields")
    
    cats_rects <- rounded.bars(data,nesting.structure,
                               weightvar="weight",
                               chart.palette = chart.palette,
                               display_axis = FALSE)$cats_rects
    cats_rects_final <- cats_rects[nrow(cats_rects),]
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        weightvar="weight",
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot+
      funkyheatmap::geom_rounded_rect(aes(xmin=cats_rects_final$xmin,
                                          ymin=cats_rects_final$ymin,
                                          xmax=bars_range[2],
                                          ymax=cats_rects_final$ymax),
                                      radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
      xlim(bars_range)+
      ggpattern::geom_rect_pattern(data=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
                                                             ymin=cats_rects_final$ymin,
                                                             xmax=bars_range[2],
                                                             ymax=cats_rects_final$ymax),
                                   pattern="gradient",pattern_orientation = "horizontal",
                                   pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
    
    rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                          weightvar="weight",
                                          chart.palette = chart.palette.largecat,
                                          display_axis = FALSE)$plot
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Axis row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    
    rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                          chart.palette = rep("white",length(chart.palette.largecat)),
                                          axis_only = TRUE)$plot+
      scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.cutoff <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("black","black","black","white","black"))
    
    data.legend <- data.largecat %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure.largecat,
                                      chart.palette = chart.palette.largecat,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.largecat <- rounded.bars(data.legend,nesting.structure.largecat,
                                          chart.palette = chart.palette.largecat,
                                          display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","black"))
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Display plots
    figure_6 <- 
      plot_grid(plotlist=plotlist,ncol=1,align = "v",
                rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.5,1))
    
  }
  
  # Figure 7. Outcome reproducibility by discipline and year
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
      data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
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
      
      
      # 
      # data$field <- str_to_title(data$COS_pub_category)
      # 
      # data$field <- str_to_title(data$COS_pub_category)
      # data$field <- str_replace_all(data$field," ","\n")
      # data$field <- str_replace_all(data$field,"And","and")
      # field_order <- unique(data$field)
      # 
      # data$field <- ordered(data$field,levels=field_order,labels=field_order)
      
      field_order <- fields.format.2.row
      data$field <- ordered(data$COS_pub_category,
                            levels=fields.raw,
                            labels=fields.format.2.row)
      
      # Assign group
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
      
      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)
      
      data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
      
      data$cat <- ordered(data$repro_outcome_overall,
                          labels=c( "Push Button\nReproduced",
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
      
      chart.palette <- c(palette_score_charts[1],
                         palette_score_charts[4],
                         palette_score_charts[3],
                         palette_score_charts[2],
                         "grey90")
      
      data.largecat <- data
      levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)
      
      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat
      
      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)
      
      chart.palette.largecat <- c(palette_score_charts[5],chart.palette[5])
    }
    
    # Aesthetic setup
    bars_range <- c(0,1)
    col_widths <- c(.4,1.2,4,1)
    #n_bins_max <- 80
    
    # Group by field plots
    plotlist_field <- lapply(levels(data$field),function(field) {
      print(field)
      data_field <- data[data$field==field,]
      
      plotlist <- lapply(1:length(group_order),function(x) {
        cats_rects <- rounded.bars(data_field[data_field$group==group_order[x],],nesting.structure,
                                   weightvar="weight",
                                   chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects
        cats_rects_final <- cats_rects[nrow(cats_rects),]
        rounded.bars.cutoff <- rounded.bars(data_field[data_field$group==group_order[x],],nesting.structure,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          funkyheatmap::geom_rounded_rect(data_field=cats_rects_final,aes(xmin=cats_rects_final$xmin,
                                                                          ymin=cats_rects_final$ymin,
                                                                          xmax=bars_range[2],
                                                                          ymax=cats_rects_final$ymax),
                                          radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
          xlim(bars_range)
        # ggpattern::geom_rect_pattern(data_field=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
        #                                                        ymin=cats_rects_final$ymin,
        #                                                        xmax=bars_range[2],
        #                                                        ymax=cats_rects_final$ymax),
        #                              pattern="gradient",pattern_orientation = "horizontal",
        #                              pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
        rounded.bars.cutoff
      })
      
      plotlist <- append(list(ggplot() + theme_nothing()+
                                annotate("text",x=0.5,y=1,label=field)),
                         plotlist)
      
      plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
    })
    
    # Display plots
    
    year_labels <- lapply(c("",levels(data$group)),function(label) {
      ggplot() + theme_nothing()+
        annotate("text",x=0.5,y=1,label=label)
    })
    year_labels_plot <- plot_grid(plotlist=year_labels,ncol=1,align = "v")
    
    total_plot <- append(list(year_labels_plot),
                         plotlist_field)
    
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE)$cats_rects
    legend <- rounded.bars(data.legend,nesting.structure,
                           chart.palette = chart.palette,
                           display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("black","black","black","white","black"))
    legend_bar <- plot_grid(ggplot()+theme_nothing()+
                              annotate("text",x=0.5,y=1,label=""),
                            legend,nrow=1,rel_widths = c(1,length(levels(data$cat))))
    
    main_plots <- plot_grid(plotlist=total_plot,nrow=1)
    
    figure_7 <- 
      plot_grid(main_plots,legend_bar,ncol=1,rel_heights = c(1+length(unique(data$pub_year)),1.2))
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
      "figure_7"=figure_7))
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
    #library(MASS)
    library(DescTools)

    drive_auth(Sys.getenv("google_oauth_email"))
    #drive_deauth()
    # Common functions
    source(file="Analysis/common functions.R")
  }


  # Load data
    objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes",
                         "paper_metadata","status","stitched_claims",
                         "all_rr_attempts")
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
      
  # Generate figures
  # {
  #   generated_figures <- figures(repro_outcomes,pr_outcomes,orig_outcomes,paper_metadata)
  #   
  #   ggsave(
  #     "Analysis/Paper 3/Code and data/Figures/figure 1.png",
  #     plot = generated_figures$figure_1,
  #     width = 6000,height = 2000,units = "px",bg="white"
  #   )
  #   ggsave(
  #     "Analysis/Paper 3/Code and data/Figures/figure 2.png",
  #     plot = generated_figures$figure_2,
  #     width = 6000,height = 2000,units = "px",bg="white"
  #   )
  #   ggsave(
  #     "Analysis/Paper 3/Code and data/Figures/figure 3.png",
  #     plot = generated_figures$figure_3,
  #     width = 6000,height = 2000,units = "px",bg="white"
  #   )
  #   ggsave(
  #     "Analysis/Paper 3/Code and data/Figures/figure 4.png",
  #     plot = generated_figures$figure_4,
  #     width = 6000,height = 1200,units = "px",bg="white"
  #   )
  #   ggsave(
  #     "Analysis/Paper 3/Code and data/Figures/figure 5.png",
  #     plot = generated_figures$figure_5,
  #     width = 6000,height = 2000,units = "px",bg="white"
  #   )
  #   ggsave(
  #     "Analysis/Paper 3/Code and data/Figures/figure 6.png",
  #     plot = generated_figures$figure_6,
  #     width = 6000,height = 2000,units = "px",bg="white"
  #   )
  #   ggsave(
  #     "Analysis/Paper 3/Code and data/Figures/figure 7.png",
  #     plot = generated_figures$figure_7,
  #     width = 6000,height = 2000,units = "px",bg="white"
  #   )
  #   
  # }

}
