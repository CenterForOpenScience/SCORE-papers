
# Functions (also readable by external sources)
{
  # Create an object that contains the tagged stats
  tagged_stats <- function(iters = 100,
                           repli_outcomes,orig_outcomes,paper_metadata,
                           all_rr_attempts,repli_binary,status,publications,
                           non_significant_bushels,rr_sourced,repli_export,
                           generate_binary_data=FALSE){
  
    # Temporary load data for convenience
    {
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
        library(wCorr)
        library(corrplot)
        library(cowplot)
        library(ggridges) #note: using the github version, as the CRAN hasn't been pushed to get the weights functionality
        library(ggside)
        library(weights)
        
        drive_auth(Sys.getenv("google_oauth_email"))
        #drive_deauth()
        # Common functions
        source(file="Analysis/common functions.R")
      }
      
      # Load data
      objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata",
                           "status","all_rr_attempts","repli_binary","publications",
                           "non_significant_bushels","rr_sourced","repli_export",
                           "repli_case_exclusions","orig_dataset",
                           "full_dates")
      for(i in 1:length(objects_to_load)){
        assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
        #save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
      }
    }
    
    # Data preparation
    {
      # Set defaults for convenience
      {
        if (!exists("iters")){ iters <- 100}
        if (!exists("generate_binary_data")){ generate_binary_data <- FALSE}
      }
      
      # Trim out non-version of record lines and non-COVID entries
      {
        repli_outcomes_orig <- repli_outcomes
        
        repli_outcomes <- repli_outcomes[repli_outcomes$repli_version_of_record,]
        repli_outcomes <- repli_outcomes[!repli_outcomes$is_covid,]
      }
      
      # Get and trim paper metadata information
      {
        paper_metadata_orig <- paper_metadata
        paper_metadata <- paper_metadata[c("paper_id","publication_standard","COS_pub_category","pub_year")]
        paper_metadata$field <- str_to_title(paper_metadata$COS_pub_category)
        paper_metadata$field <- str_replace(paper_metadata$field,"And","and")
        
        orig_outcomes <- merge(orig_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
        
        repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                       by="claim_id",all.x=TRUE,all.y=FALSE)
        orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
        repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
        repli_outcomes_merged <- repli_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
      }
      
      # Generate binary data. Note: generate_binary_data flag by default is set 
      # to FALSE, which pulls the binary data precalculated from the data pipeline.
      # If set to TRUE, the code will run to generate the binary success measures
      # from scratch. This is time consuming, but will produce identical results)
      {
        if(generate_binary_data==TRUE ){
          # FILL IN WITH code from Andrew
          
        } else {
          # Switch to paper_id / claim_id coding
          repli_binary <- merge(repli_outcomes[c("paper_id","claim_id","report_id","repli_score_criteria_met")],
                                repli_binary,all.x = FALSE,all.y=TRUE,by="report_id")
          repli_binary$report_id <- NULL
        }
        
      }
      
      # Generate names for binary measures
      {
        binvars.raw <- c("repli_score_criteria_met",
                         "repli_binary_analyst",
                         "repli_binary_orig_wthn",
                         "repli_binary_rep_wthn",
                         "repli_binary_wthn_pi",
                         "repli_binary_meta_success",
                         "repli_binary_sum_p",
                         "repli_binary_telescopes",
                         "repli_binary_bf_result",
                         "repli_binary_bayes_rep",
                         "repli_binary_bma_result",
                         "repli_binary_skep_p",
                         "repli_binary_correspondence")
        binvars.order <- binvars.raw
        
        binvars.full.text <- c("Statistical signifance + pattern",
                           "Subjective interpretation",
                           "Original effect in replication's CI",
                           "Replication effect in original's CI",
                           "Replication effect in prediction interval",
                           "Meta-analysis",
                           "Sum of p-values",
                           "Small telescopes",
                           "Bayes factor",
                           "Replication Bayes factor",
                           "Bayesian meta-analysis",
                           "Skeptical p-value",
                           "Correspondence test")
        binvars.group <- c("Statistic significance / p-value based measures",
                           "SCORE specific",
                           "Contained within interval measures",
                           "Contained within interval measures",
                           "Contained within interval measures",
                           "Meta analysis based measures",
                           "Statistic significance / p-value based measures",
                           "Combined effect magnitude and uncertainty measures",
                           "Combined effect magnitude and uncertainty measures",
                           "Combined effect magnitude and uncertainty measures",
                           "Meta analysis based measures",
                           "Statistic significance / p-value based measures",
                           "Combined effect magnitude and uncertainty measures")
        
        binvars.order <- c("repli_binary_analyst",
                           "repli_score_criteria_met",
                           "repli_binary_sum_p",
                           "repli_binary_skep_p",
                           "repli_binary_orig_wthn",
                           "repli_binary_rep_wthn",
                           "repli_binary_wthn_pi",
                           "repli_binary_meta_success",
                           "repli_binary_bma_result",
                           "repli_binary_telescopes",
                           "repli_binary_bf_result",
                           "repli_binary_bayes_rep",
                           "repli_binary_correspondence")
        
        df.binvars <- data.frame(binvars.raw,binvars.full.text,binvars.group,binvars.order)
        df.binvars <- do.call(rbind,lapply(binvars.order,function(x) {
          df.binvars[df.binvars$binvars.raw==x,]
        }))
      }
    }
    
    # Main section stats
    {
      # Tables
      {
        # Table 2
        {
          fields.order <- sort(c("Psychology and Health","Business","Sociology and Criminology",
                            "Economics and Finance","Political Science","Education"))
          
          format.row <- function(data){
            data <- data %>% 
              select(paper_id) %>% 
              left_join(paper_metadata %>% select(paper_id, field), by = "paper_id") %>% 
              count(field)
            total <- sum(data$n)
            field <- "Total"
            n <- paste0(total," (100%)")
            data$n <- paste0(data$n," (",
                             format.round(100*data$n/sum(data$n),1),
                             "%)")
            data <- rbind(data,data.frame(field,n))
            colnames <- data$field
            data <- data.frame(t(rbind(data,data.frame(field,n))[,-1]))
            colnames(data) <- colnames
            data[c(fields.order,"Total")]
          }
          
          p.row <- function(data){
            data <- data %>% 
              select(paper_id) %>% 
              left_join(paper_metadata %>% select(paper_id, field), by = "paper_id") %>% 
              count(field)
            total <- sum(data$n)
            field <- "Total"
            n <- 1
            data$n <- data$n/sum(data$n)
            data <- rbind(data,data.frame(field,n))
            colnames <- data$field
            data <- data.frame(t(rbind(data,data.frame(field,n))[,-1]))
            colnames(data) <- colnames
            data[c(fields.order,"Total")]
          }
          
          n.row <- function(data){
            data <- data %>% 
              select(paper_id) %>% 
              left_join(paper_metadata %>% select(paper_id, field), by = "paper_id") %>% 
              count(field)
            total <- sum(data$n)
            field <- "Total"
            n <- total
            data <- rbind(data,data.frame(field,n))
            colnames <- data$field
            data <- data.frame(t(rbind(data,data.frame(field,n))[,-1]))
            colnames(data) <- colnames
            data[c(fields.order,"Total")]
          }
          
          r1.data <- status %>% filter(p1_delivery | p2_delivery)
          r2.data <- status %>% filter(RR | p2_delivery)
          r3.data <- status %>% filter(bushel)
          r4.data <- status %>% filter((p2_delivery | RR) & !bushel)
          r5.data <- all_rr_attempts %>% 
            filter(str_detect(type, "Replication")) %>%
            select(paper_id) %>% 
            distinct() %>% 
            semi_join(status %>% filter(p1_delivery | p2_delivery), by = "paper_id")
          r6.data <-  repli_outcomes_orig %>% 
            filter(repli_type != "original and secondary data") %>% 
            filter(!is_covid) %>% 
            select(paper_id) %>% 
            distinct()
          r7.data <- repli_outcomes_orig %>%
            filter(repli_type != "original and secondary data") %>% 
            filter(!is_covid) %>% 
            filter(is.na(manylabs_type) | manylabs_type != "aggregation") %>% 
            select(paper_id, claim_id, rr_id) %>% 
            distinct() 
          r8.data <-  repli_outcomes_orig %>%
            filter(repli_type != "original and secondary data") %>% 
            filter(!is_covid) %>% 
            select(paper_id, claim_id) %>% 
            distinct()
          
          for (i in 1:8){
            assign(paste0("r",i),format.row(get(paste0("r",i,".data"))))
            assign(paste0("r",i,".n"),n.row(get(paste0("r",i,".data"))))
            assign(paste0("r",i,".p"),p.row(get(paste0("r",i,".data"))))
            assign(paste0("r",i,".p.formatted"),
                   paste0(format.round(100*p.row(get(paste0("r",i,".data"))),0),"%")
                   )
          }
          
          table_2 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8)
          table_2_n <- rbind(r1.n,r2.n,r3.n,r4.n,r5.n,r6.n,r7.n,r8.n)
          table_2_p <- rbind(r1.p,r2.p,r3.p,r4.p,r5.p,r6.p,r7.p,r8.p)
          table_2_p_formatted <- rbind(r1.p.formatted,r2.p.formatted,r3.p.formatted,r4.p.formatted,r5.p.formatted,r6.p.formatted,r7.p.formatted,r8.p.formatted)
          for (row in 1:nrow(table_2)){
            for (col in 1:ncol(table_2)){
              assign(paste0("table_2_",row,"_",col),
                     table_2[row,col])
              assign(paste0("table_2_p_",row,"_",col),
                     table_2_p[row,col])
              assign(paste0("table_2_p_formatted_",row,"_",col),
                     table_2_p_formatted[row,col])
            }
          }
          rm(r1,r2,r3,r4,r5,r6,r7,r8,
             r1.p,r2.p,r3.p,r4.p,r5.p,r6.p,r7.p,r8.p,
             r1.data,r2.data,r3.data,r4.data,r5.data,r6.data,r7.data,r8.data,
             r1.p.formatted,r2.p.formatted,r3.p.formatted,r4.p.formatted,r5.p.formatted,r6.p.formatted,r7.p.formatted,r8.p.formatted)
        }
        
        # Table 3
        {
          fields.order <- sort(c("Psychology And Health","Business","Sociology And Criminology",
                            "Economics And Finance","Political Science","Education"))
          
          format.row <- function(data){
            data <- data %>% 
              select(paper_id) %>% 
              left_join(paper_metadata %>% select(paper_id, pub_year), by = "paper_id") %>% 
              count(pub_year)
            total <- sum(data$n)
            pub_year <- "Total"
            n <- paste0(total," (100%)")
            data$n <- paste0(data$n," (",
                             format.round(100*data$n/sum(data$n),1),
                             "%)")
            data <- rbind(data,data.frame(pub_year,n))
            colnames <- data$pub_year
            data <- data.frame(t(rbind(data,data.frame(pub_year,n))[,-1]))
            colnames(data) <- colnames
            data[c(as.character(2009:2018),"Total")]
          }
          n.row <- function(data){
            data <- data %>% 
              select(paper_id) %>% 
              left_join(paper_metadata %>% select(paper_id, pub_year), by = "paper_id") %>% 
              count(pub_year)
            total <- sum(data$n)
            pub_year <- "Total"
            n <- total
            data <- rbind(data,data.frame(pub_year,n))
            colnames <- data$pub_year
            data <- data.frame(t(rbind(data,data.frame(pub_year,n))[,-1]))
            colnames(data) <- colnames
            data[c(as.character(2009:2018),"Total")]
          }
          p.row <- function(data){
            data <- data %>% 
              select(paper_id) %>% 
              left_join(paper_metadata %>% select(paper_id, pub_year), by = "paper_id") %>% 
              count(pub_year)
            total <- sum(data$n)
            pub_year <- "Total"
            n <- 1
            data$n <- data$n/sum(data$n)
            data <- rbind(data,data.frame(pub_year,n))
            colnames <- data$pub_year
            data <- data.frame(t(rbind(data,data.frame(pub_year,n))[,-1]))
            colnames(data) <- colnames
            data[c(as.character(2009:2018),"Total")]
          }
          
          r1.data <- status %>% filter(p1_delivery | p2_delivery)
          r2.data <- status %>% filter(RR | p2_delivery)
          r3.data <- status %>% filter(bushel)
          r4.data <- status %>% filter((p2_delivery | RR) & !bushel)
          r5.data <- all_rr_attempts %>% 
            filter(str_detect(type, "Replication")) %>%
            select(paper_id) %>% 
            distinct() %>% 
            semi_join(status %>% filter(p1_delivery | p2_delivery), by = "paper_id")
          r6.data <-  repli_outcomes_orig %>% 
            filter(repli_type != "original and secondary data") %>% 
            filter(!is_covid) %>% 
            select(paper_id) %>% 
            distinct()
          r7.data <- repli_outcomes_orig %>%
            filter(repli_type != "original and secondary data") %>% 
            filter(!is_covid) %>% 
            filter(is.na(manylabs_type) | manylabs_type != "aggregation") %>% 
            select(paper_id, claim_id, rr_id) %>% 
            distinct() 
          r8.data <-  repli_outcomes_orig %>%
            filter(repli_type != "original and secondary data") %>% 
            filter(!is_covid) %>% 
            select(paper_id, claim_id) %>% 
            distinct()
          
          for (i in 1:8){
            assign(paste0("r",i),format.row(get(paste0("r",i,".data"))))
            assign(paste0("r",i,".n"),n.row(get(paste0("r",i,".data"))))
            assign(paste0("r",i,".p"),p.row(get(paste0("r",i,".data"))))
          }
          
          table_3 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8)
          table_3_n <- rbind(r1.n,r2.n,r3.n,r4.n,r5.n,r6.n,r7.n,r8.n)
          table_3_p <- rbind(r1.p,r2.p,r3.p,r4.p,r5.p,r6.p,r7.p,r8.p)
          for (row in 1:nrow(table_3)){
            for (col in 1:ncol(table_3)){
              assign(paste0("table_3_",row,"_",col),
                     table_3[row,col])
            }
          }
          rm(r1,r2,r3,r4,r5,r6,r7,r8)
        }
        
        # Table 4
        {
          repli_effects <- repli_outcomes %>% 
            filter(!is_covid) %>% 
            filter(repli_version_of_record) %>% 
            select(report_id, claim_id, repli_sample_size_value, success = repli_score_criteria_met, repli_type,
                   repli_conv_r, repli_conv_r_lb, repli_conv_r_ub, repli_effect_size_type, repli_effect_size_value) %>%
            mutate(ser_effect = ifelse(str_detect(repli_effect_size_type, "ser_"), abs(repli_effect_size_value), NA)) %>% 
            mutate(across(contains("conv"), abs))
          
          orig_effects <- orig_outcomes %>% 
            select(claim_id, paper_id,orig_sample_size_value, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub,
                   orig_effect_size_type_repli, orig_effect_size_value_repli) %>%
            semi_join(repli_effects, by = "claim_id") %>% 
            mutate(ser_effect = ifelse(str_detect(orig_effect_size_type_repli, "ser_"), abs(orig_effect_size_value_repli), NA)) %>% 
            mutate(across(contains("conv"), abs))
          
          effects_combined <- merge(repli_effects,orig_effects,by="claim_id",all.x = TRUE,all.y = FALSE)
          
          effects_combined <- effects_combined %>%
            filter(!is.na(repli_conv_r)) %>%
            filter(!is.na(orig_conv_r)) %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n())
            
          
          # Number of papers and claims
          r1 <- c(length(unique(effects_combined$paper_id)),length(unique(effects_combined$paper_id)),
                  length(unique(effects_combined$claim_id)),length(unique(effects_combined$claim_id)))
          
          # Number of effect sizes
          r2 <- c(sum(as.numeric(!is.na(effects_combined$orig_conv_r))*effects_combined$weight),
                  sum(as.numeric(!is.na(effects_combined$repli_conv_r))*effects_combined$weight),
                  sum(!is.na(effects_combined$orig_conv_r)),
                  sum(!is.na(effects_combined$repli_conv_r))
          )
          
          # Median/IQR of sample sizes
          r3.median <- c(weighted.quantile(effects_combined$orig_sample_size_value,effects_combined$weight,.5),
                         weighted.quantile(effects_combined$repli_sample_size_value,effects_combined$weight,.5),
                         quantile(effects_combined$orig_sample_size_value,.5,na.rm=TRUE),
                         quantile(effects_combined$repli_sample_size_value,.5,na.rm=TRUE)
          )
          
          r3.IQR <- c(format.round(weighted.quantile(effects_combined$orig_sample_size_value,effects_combined$weight,.75) - weighted.quantile(effects_combined$orig_sample_size_value,effects_combined$weight,.25),1),
                      format.round(weighted.quantile(effects_combined$repli_sample_size_value,effects_combined$weight,.75) - weighted.quantile(effects_combined$repli_sample_size_value,effects_combined$weight,.25),1),
                      format.round(IQR(effects_combined$orig_sample_size_value,na.rm=TRUE),1),
                      format.round(IQR(as.integer(effects_combined$repli_sample_size_value),na.rm=TRUE),1)
          )

          r3 <- paste0(r3.median,"\n[",r3.IQR,"]")
          
          # Pearson's R effect size
          r4.median <- c(weighted.quantile(effects_combined$orig_conv_r,effects_combined$weight,.5),
                  weighted.quantile(effects_combined$repli_conv_r,effects_combined$weight,.5),
                  quantile(effects_combined$orig_conv_r,.5),
                  quantile(effects_combined$repli_conv_r,.5)
          )
          
          r4.median.formatted <- format.round(r4.median,2)
          
          r4.SD <- c(sqrt(wtd.var(effects_combined$orig_conv_r,effects_combined$weight)),
                  sqrt(wtd.var(effects_combined$repli_conv_r,effects_combined$weight)),
                  sd(effects_combined$orig_conv_r,1),
                  sd(effects_combined$repli_conv_r,1)
          )
          r4.SD.formatted <- format.round(r4.SD,2)
          
          r4 <- paste0(r4.median.formatted,"\n(",r4.SD.formatted,")")
          
          table_4 <- rbind(r1,r2,r3,r4)
          for (row in 1:nrow(table_4)){
            for (col in 1:ncol(table_4)){
              assign(paste0("table_4_",row,"_",col),
                     table_4[row,col])
              if(row==4){
                assign(paste0("table_4_median_",row,"_",col),
                       r4.median[col])
                assign(paste0("table_4_SD_",row,"_",col),
                       r4.SD[col])
                assign(paste0("table_4_median_formatted_",row,"_",col),
                       r4.median.formatted[col])
                assign(paste0("table_4_SD_formatted_",row,"_",col),
                       r4.SD.formatted[col])
              }
            }
          }
          rm(r1,r2,r3,r4)
        }
        
        # Table 5
        {
          fields.order <- c("business","economics and finance","education","political science",
                            "psychology and health","sociology and criminology")

          repli_effects <- repli_outcomes %>%
            filter(!is_covid) %>%
            filter(repli_version_of_record) %>%
            select(report_id, claim_id,repli_sample_size_value, success = repli_score_criteria_met, repli_type,
                   repli_conv_r, repli_conv_r_lb, repli_conv_r_ub, repli_effect_size_type, repli_effect_size_value) %>%
            mutate(ser_effect = ifelse(str_detect(repli_effect_size_type, "ser_"), abs(repli_effect_size_value), NA)) %>%
            mutate(across(contains("conv"), abs))

          orig_effects <- orig_outcomes %>%
            semi_join(repli_effects, by = "claim_id") %>%
            select(claim_id, paper_id,orig_sample_size_value, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub,
                   orig_effect_size_type_repli, orig_effect_size_value_repli) %>%
            mutate(ser_effect = ifelse(str_detect(orig_effect_size_type_repli, "ser_"), abs(orig_effect_size_value_repli), NA)) %>%
            mutate(across(contains("conv"), abs)) %>%
            mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>%
            left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id")

          effects_combined <- merge(orig_effects,repli_effects,by="claim_id",all.x = TRUE,all.y = FALSE)

          effects_combined <- effects_combined %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()

          table_5 <- do.call(rbind,lapply(fields.order, function(field) {
            data <- effects_combined[effects_combined$field==field,]
            
            c1.numerator <- sum(as.numeric(data$success)*data$weight)
            c1.numerator.formatted <- format.round(c1.numerator,1)
            c1.denom <- length(unique(data$paper_id))
            c1.denom.formatted <- format.round(c1.denom,0)
            c1.p <- sum(as.numeric(data$success)*data$weight)/length(unique(data$paper_id))
            c1.p.formatted <- paste0(format.round(100*c1.p,1),"%")
            c1 <- paste0(c1.numerator.formatted," / ",c1.denom.formatted," (",
                         c1.p.formatted,")")
            
            data_orig <- data %>%
              filter(!is.na(orig_conv_r))%>%
              group_by(paper_id) %>%
              mutate(weight = 1/n()) %>%
              ungroup()
            
            data_repli <- data %>%
              filter(!is.na(repli_conv_r))%>%
              group_by(paper_id) %>%
              mutate(weight = 1/n()) %>%
              ungroup()
            
            data_repli_orig <- data %>%
              filter(!is.na(orig_conv_r))%>%
              filter(!is.na(repli_conv_r))%>%
              group_by(paper_id) %>%
              mutate(weight = 1/n()) %>%
              ungroup()
            
            c2.median <- weighted.median(data_repli_orig$orig_conv_r,data_repli_orig$weight)
            c2.SD <- sqrt(wtd.var(data_repli_orig$orig_conv_r,data_repli_orig$weight))
            c2 <- paste0(format.round(c2.median,2)," (",format.round(c2.SD,2),")")
            
            c3.median <- weighted.median(data_repli_orig$repli_conv_r,data_repli_orig$weight)
            c3.SD <- sqrt(wtd.var(data_repli_orig$repli_conv_r,data_repli_orig$weight))
            c3 <- paste0(format.round(c3.median,2)," (",format.round(c3.SD,2),")")
            
            data.frame(field,c1,c2,c3)
          }))
          
          table_5 <- table_5[c("c1","c2","c3")]

          for (row in 1:nrow(table_5)){
            for (col in 1:ncol(table_5)){
              assign(paste0("table_5_",row,"_",col),
                     table_5[row,col])
            }
          }
          rm(orig_effects,repli_effects,effects_combined)
        }
        
        # Table 6
        {
          repli_effects <- repli_outcomes %>%
            filter(!is_covid) %>%
            filter(repli_version_of_record) %>%
            select(report_id, claim_id,repli_sample_size_value, success = repli_score_criteria_met, repli_type,
                   repli_conv_r, repli_conv_r_lb, repli_conv_r_ub, repli_effect_size_type, repli_effect_size_value,
                   rr_power_100_original_effect,rr_power_100_original_effect_design,
                   repli_power_for_75_effect,rr_power_75_original_effect_design,) %>%
            mutate(ser_effect = ifelse(str_detect(repli_effect_size_type, "ser_"), abs(repli_effect_size_value), NA)) %>%
            mutate(across(contains("conv"), abs))
          
          orig_effects <- orig_outcomes %>%
            semi_join(repli_effects, by = "claim_id") %>%
            select(claim_id, paper_id,orig_sample_size_value, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub,
                   orig_effect_size_type_repli, orig_effect_size_value_repli,
                   orig_power_for_75_effect) %>%
            mutate(ser_effect = ifelse(str_detect(orig_effect_size_type_repli, "ser_"), abs(orig_effect_size_value_repli), NA)) %>%
            mutate(across(contains("conv"), abs)) %>%
            mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>%
            left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id")
          
          effects_combined <- merge(orig_effects,repli_effects,by="claim_id",all.x = TRUE,all.y = FALSE)
          
          effects_combined <- effects_combined %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
          
          r1.data <- effects_combined[effects_combined$repli_type=="new data",]
          r1.numerator <- c(length(unique(r1.data$paper_id)),sum(r1.data$success*r1.data$weight),nrow(r1.data),sum(r1.data$success))
          r1.numerator.formatted <- c(format.round(r1.numerator[1],0),format.round(r1.numerator[2],1),format.round(r1.numerator[3:4],0))
          r1.denom <- c(length(unique(r1.data$paper_id)),length(unique(r1.data$paper_id)),nrow(r1.data),nrow(r1.data))
          r1.denom.formatted <- format.round(r1.denom,0)
          r1.p <- r1.numerator/r1.denom
          r1.p.formatted <- paste0(format.round(100*r1.p,1),"%")
          r1 <- paste0(r1.numerator.formatted," / ",r1.denom.formatted,"\n(",r1.p.formatted,")")
          
          r2.data <- effects_combined[effects_combined$repli_type=="secondary data",]
          r2.numerator <- c(length(unique(r2.data$paper_id)),sum(r2.data$success*r2.data$weight),nrow(r2.data),sum(r2.data$success))
          r2.numerator.formatted <- c(format.round(r2.numerator[1],0),format.round(r2.numerator[2],1),format.round(r2.numerator[3:4],0))
          r2.denom <- c(length(unique(r2.data$paper_id)),length(unique(r2.data$paper_id)),nrow(r2.data),nrow(r2.data))
          r2.denom.formatted <- format.round(r2.denom,0)
          r2.p <- r2.numerator/r2.denom
          r2.p.formatted <- paste0(format.round(100*r2.p,1),"%")
          r2 <- paste0(r2.numerator.formatted," / ",r2.denom.formatted,"\n(",r2.p.formatted,")")
          
          r3.data <- r1.data %>%
            filter(!is.na(orig_conv_r)) %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
          r3.median <- c(weighted.quantile(r3.data$orig_conv_r,r3.data$weight,.5),
                         weighted.quantile(r3.data$repli_conv_r,r3.data$weight,.5),
                         quantile(r3.data$orig_conv_r,.5,na.rm=TRUE),
                         quantile(r3.data$repli_conv_r,.5,na.rm=TRUE))
          r3.median.formatted <- format.round(r3.median,2)
          r3.SD <- c(sqrt(wtd.var(r3.data$orig_conv_r,r3.data$weight)),
                     sqrt(wtd.var(r3.data$repli_conv_r,r3.data$weight)),
                     sd(r3.data$orig_conv_r,1),
                     sd(r3.data$repli_conv_r,1))
          r3.SD.formatted <- format.round(r3.SD,2)
          r3 <- paste0(r3.median.formatted," (",r3.SD.formatted,")")
          
          r4.data <- r2.data %>%
            filter(!is.na(orig_conv_r)) %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
          r4.median <- c(weighted.quantile(r4.data$orig_conv_r,r4.data$weight,.5),
                         weighted.quantile(r4.data$repli_conv_r,r4.data$weight,.5),
                         quantile(r4.data$orig_conv_r,.5,na.rm=TRUE),
                         quantile(r4.data$repli_conv_r,.5,na.rm=TRUE))
          r4.median.formatted <- format.round(r4.median,2)
          r4.SD <- c(sqrt(wtd.var(r4.data$orig_conv_r,r4.data$weight)),
                     sqrt(wtd.var(r4.data$repli_conv_r,r4.data$weight)),
                     sd(r4.data$orig_conv_r,1),
                     sd(r4.data$repli_conv_r,1))
          r4.SD.formatted <- format.round(r4.SD,2)
          r4 <- paste0(r4.median.formatted," (",r4.SD.formatted,")")
          
          table_6 <- rbind(r1,r2,r3,r4)
          table_6_numerator <- rbind(r1.numerator,r2.numerator,rep(NA,ncol(table_6)),rep(NA,ncol(table_6)))
          table_6_numerator_formatted <- rbind(r1.numerator.formatted,r2.numerator.formatted,rep(NA,ncol(table_6)),rep(NA,ncol(table_6)))
          table_6_denom <- rbind(r1.denom,r2.denom,rep(NA,ncol(table_6)),rep(NA,ncol(table_6)))
          table_6_denom_formatted <- rbind(r1.denom.formatted,r2.denom.formatted,rep(NA,ncol(table_6)),rep(NA,ncol(table_6)))
          table_6_p <- rbind(r1.p,r2.p,rep(NA,ncol(table_6)),rep(NA,ncol(table_6)))
          table_6_p_formatted <- rbind(r1.p.formatted,r2.p.formatted,rep(NA,ncol(table_6)),rep(NA,ncol(table_6)))
          
          table_6_median <- rbind(rep(NA,ncol(table_6)),rep(NA,ncol(table_6)),r3.median,r4.median)
          table_6_median_formatted <- rbind(rep(NA,ncol(table_6)),rep(NA,ncol(table_6)),r3.median.formatted,r4.median.formatted)
          table_6_SD <- rbind(rep(NA,ncol(table_6)),rep(NA,ncol(table_6)),r3.SD,r4.SD)
          table_6_SD_formatted <- rbind(rep(NA,ncol(table_6)),rep(NA,ncol(table_6)),r3.SD.formatted,r4.SD.formatted)
          
          for (row in 1:nrow(table_6)){
            for (col in 1:ncol(table_6)){
              assign(paste0("table_6_",row,"_",col),
                     table_6[row,col])
              
              assign(paste0("table_6_numerator_",row,"_",col),
                     table_6_numerator[row,col])
              assign(paste0("table_6_numerator_formatted_",row,"_",col),
                     table_6_numerator_formatted[row,col])
              
              assign(paste0("table_6_denom_",row,"_",col),
                     table_6_denom[row,col])
              assign(paste0("table_6_denom_formatted_",row,"_",col),
                     table_6_denom_formatted[row,col])
              
              assign(paste0("table_6_p_",row,"_",col),
                     table_6_p[row,col])
              assign(paste0("table_6_p_formatted_",row,"_",col),
                     table_6_p_formatted[row,col])
              
              assign(paste0("table_6_median_",row,"_",col),
                     table_6_median[row,col])
              assign(paste0("table_6_median_formatted_",row,"_",col),
                     table_6_median_formatted[row,col])
              
              assign(paste0("table_6_SD_",row,"_",col),
                     table_6_SD[row,col])
              assign(paste0("table_6_SD_formatted_",row,"_",col),
                     table_6_SD_formatted[row,col])
            }
          }
          rm(r1,r2,r3,r4)
          
          # Add rows 5 and 6
          
          table_6_5_13.data <- repli_outcomes %>%
            filter(!is.na(repli_power_for_75_effect),
                   repli_type=="new data") %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
          
          table_6_5_1 <- format.round(weighted.median(table_6_5_13.data$repli_power_for_75_effect,table_6_5_13.data$weight),2)
          table_6_5_3 <- format.round(median(table_6_5_13.data$repli_power_for_75_effect),2)
          
          table_6_5_24.data <- repli_outcomes %>%
            filter(!is.na(rr_power_75_original_effect_design),
                   repli_type=="new data") %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
          
          table_6_5_2 <- format.round(weighted.median(table_6_5_24.data$rr_power_75_original_effect_design,table_6_5_24.data$weight),2)
          table_6_5_4 <- format.round(median(table_6_5_24.data$rr_power_75_original_effect_design),2)
          
          table_6_6_13.data <- repli_outcomes %>%
            filter(!is.na(repli_power_for_75_effect),
                   repli_type=="secondary data") %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
          
          table_6_6_1 <- format.round(weighted.median(table_6_6_13.data$repli_power_for_75_effect,table_6_6_13.data$weight),2)
          table_6_6_3 <- format.round(median(table_6_6_13.data$repli_power_for_75_effect),2)
          
          table_6_6_24.data <- repli_outcomes %>%
            filter(!is.na(rr_power_75_original_effect_design),
                   repli_type=="secondary data") %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
          
          table_6_6_2 <- format.round(weighted.median(table_6_6_24.data$rr_power_75_original_effect_design,table_6_6_24.data$weight),2)
          table_6_6_4 <- format.round(median(table_6_6_24.data$rr_power_75_original_effect_design),2)
        }
        
        # Table 7
        {
          fields.order <- sort(c("Psychology and Health","Business","Sociology and Criminology",
                                 "Economics and Finance","Political Science","Education"))
          
          table_7 <- do.call(rbind,lapply(fields.order,function(field_selected) {
            #field_selected <- fields.order[1]
            
            table.data <- repli_outcomes_merged %>%
              filter(field==field_selected) %>%
              group_by(paper_id) %>%
              mutate(weight = 1/n())
            
            c1 <- paste0(format.round(
              100*sum(as.integer(table.data$repli_type=="new data")*table.data$weight)/
                sum(table.data$weight),1),"%")
            
            c2 <- paste0(format.round(
              100*sum(as.integer(table.data$repli_type=="secondary data")*table.data$weight)/
                sum(table.data$weight),1),"%")
            
            table.data <- repli_outcomes_merged %>%
              filter(field==field_selected,
                     !is.na(repli_power_for_75_effect)) %>%
              group_by(paper_id) %>%
              mutate(weight = 1/n())
            c3 <- paste0(format.round(
              100*weighted.median(table.data$repli_power_for_75_effect,table.data$weight),1),"%")
            
            table.data <- repli_outcomes_merged %>%
              filter(field==field_selected,
                     !is.na(rr_power_75_original_effect_design)) %>%
              group_by(paper_id) %>%
              mutate(weight = 1/n())
            c4 <- paste0(format.round(
              100*weighted.median(table.data$rr_power_75_original_effect_design,table.data$weight),1),"%")
            
            data.frame(c1,c2,c3,c4)
          }))
          
          for (row in 1:nrow(table_7)){
            for (col in 1:ncol(table_7)){
              assign(paste0("table_7_",row,"_",col),
                     table_7[row,col])
            }}
        }
      }
      
      # Introduction
      {
        n_journals_business <- length(unique(
          repli_outcomes_merged[repli_outcomes_merged$field=="Business",]$publication_standard
        ))
        n_journals_econ <- length(unique(
          repli_outcomes_merged[repli_outcomes_merged$field=="Economics and Finance",]$publication_standard
        ))
        n_journals_edu <- length(unique(
          repli_outcomes_merged[repli_outcomes_merged$field=="Education",]$publication_standard
        ))
        n_journals_polisci <- length(unique(
          repli_outcomes_merged[repli_outcomes_merged$field=="Political Science",]$publication_standard
        ))
        n_journals_psych <- length(unique(
          repli_outcomes_merged[repli_outcomes_merged$field=="Psychology and Health",]$publication_standard
        ))
        n_journals_soc <- length(unique(
          repli_outcomes_merged[repli_outcomes_merged$field=="Sociology and Criminology",]$publication_standard
        ))
                                             
      }
      
      # Results: Replications completed by discipline and year in comparison with the sampling frame
      {
        p_rep_edu_initial <- 
          paste0(format.round(100*table_2_n$Education[1]/table_2_n$Total[1],1),"% [n = ",table_2_n$Education[1],"/",table_2_n$Total[1],"]")
        p_rep_edu_replis <- 
          paste0(format.round(100*table_2_n$Education[6]/table_2_n$Total[6],1),"% [n = ",table_2_n$Education[6],"/",table_2_n$Total[6],"]")
        
        p_rep_polisci_initial <- 
          paste0(format.round(100*table_2_n$`Political Science`[1]/table_2_n$Total[1],1),"% [n = ",table_2_n$`Political Science`[1],"/",table_2_n$Total[1],"]")
        p_rep_polisci_replis <- 
          paste0(format.round(100*table_2_n$`Political Science`[6]/table_2_n$Total[6],1),"% [n = ",table_2_n$`Political Science`[6],"/",table_2_n$Total[6],"]")
        
        p_rep_psych_initial <- 
          paste0(format.round(100*table_2_n$`Psychology and Health`[1]/table_2_n$Total[1],1),"% [n = ",table_2_n$`Psychology and Health`[1],"/",table_2_n$Total[1],"]")
        p_rep_psych_replis <- 
          paste0(format.round(100*table_2_n$`Psychology and Health`[6]/table_2_n$Total[6],1),"% [n = ",table_2_n$`Psychology and Health`[6],"/",table_2_n$Total[6],"]")
        p_rep_psych_final <- 
          paste0(format.round(100*table_2_n$`Psychology and Health`[8]/table_2_n$Total[8],1),"% [n = ",table_2_n$`Psychology and Health`[8],"/",table_2_n$Total[8],"]")
        
        p_rep_business_initial <- 
          paste0(format.round(100*table_2_n$Business[1]/table_2_n$Total[1],1),"% [n = ",table_2_n$Business[1],"/",table_2_n$Total[1],"]")
        p_rep_business_final <- 
          paste0(format.round(100*table_2_n$Business[8]/table_2_n$Total[8],1),"% [n = ",table_2_n$Business[8],"/",table_2_n$Total[8],"]")
        
        p_rep_min_diff_econ_edu_polisci_soc <- paste0(format.round(100*max(abs(table_2_p[c("Economics and Finance","Education","Political Science","Sociology and Criminology")][1,]-
          table_2_p[c("Economics and Finance","Education","Political Science","Sociology and Criminology")][8,])),0),"%")
        
        p_rep_2010_initial <- 
          paste0(format.round(100*table_3_n$'2010'[1]/table_2_n$Total[1],1),"% [n = ",table_3_n$'2010'[1],"/",table_3_n$Total[1],"]")
        p_rep_2010_final <- 
          paste0(format.round(100*table_3_n$'2010'[8]/table_2_n$Total[8],1),"% [n = ",table_3_n$'2010'[8],"/",table_3_n$Total[8],"]")
        
        p_rep_2011_initial <- 
          paste0(format.round(100*table_3_n$'2011'[1]/table_2_n$Total[1],1),"% [n = ",table_3_n$'2011'[1],"/",table_3_n$Total[1],"]")
        p_rep_2011_final <- 
          paste0(format.round(100*table_3_n$'2011'[8]/table_2_n$Total[8],1),"% [n = ",table_3_n$'2011'[8],"/",table_3_n$Total[8],"]")
        
        p_rep_2015_initial <- 
          paste0(format.round(100*table_3_n$'2015'[1]/table_2_n$Total[1],1),"% [n = ",table_3_n$'2015'[1],"/",table_3_n$Total[1],"]")
        p_rep_2015_final <- 
          paste0(format.round(100*table_3_n$'2015'[8]/table_2_n$Total[8],1),"% [n = ",table_3_n$'2015'[8],"/",table_3_n$Total[8],"]")
        
        p_max_expectation_repli_completed_except_2015 <- paste0(format.round(100*max(
          abs(table_3_p$"2009"[6]-.1),
          abs(table_3_p$"2010"[6]-.1),
          abs(table_3_p$"2011"[6]-.1),
          abs(table_3_p$"2012"[6]-.1),
          abs(table_3_p$"2013"[6]-.1),
          abs(table_3_p$"2014"[6]-.1),
          abs(table_3_p$"2016"[6]-.1),
          abs(table_3_p$"2017"[6]-.1),
          abs(table_3_p$"2018"[6]-.1)
          ),1),"%")
        p_repli_completed_2015 <- paste0(format.round(100*
          abs(table_3_p$"2015"[6]),1),"%")
        
        n_claims_init_sample_p1 <- nrow(status %>% filter(p1_delivery))
        
        n_papers_p1_repli_completed <- length(unique(repli_outcomes[repli_outcomes$type_internal!="p2",]$paper_id))
        n_papers_p2_repli_completed <- length(unique(repli_outcomes[repli_outcomes$type_internal=="p2",]$paper_id))
        
        n_claims_eligible_repli <- nrow(status %>% filter(RR))
        
        n_claims_eligible_repli_multi_claim <- nrow(status %>% filter(bushel))
        
        n_claims_eligible_repli_single_claim <- n_claims_eligible_repli-n_claims_eligible_repli_multi_claim
        
        n_claims_init_sample_p2 <- nrow(status %>% filter(p2_delivery))
        
        n_claims_repli_p2 <- nrow(repli_outcomes %>% filter(type_internal=="p2"))
        
        repli_binary_score_p1 <-
          na.omit(merge(repli_binary[c("paper_id","claim_id","repli_score_criteria_met")],
                repli_outcomes[c("claim_id","type_internal")],
                by="claim_id",all.x=TRUE,all.y=FALSE)) %>%
          filter(type_internal!="p2") %>% 
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>%
          ungroup()
        
        p_repli_success_score_criteria_met_p1 <- paste0(format.round(
          100*sum(repli_binary_score_p1$repli_score_criteria_met*repli_binary_score_p1$weight)/
          sum(repli_binary_score_p1$weight),1),"%")
        
        repli_binary_score_p2 <-
          na.omit(merge(repli_binary[c("paper_id","claim_id","repli_score_criteria_met")],
                        repli_outcomes[c("claim_id","type_internal")],
                        by="claim_id",all.x=TRUE,all.y=FALSE)) %>%
          filter(type_internal=="p2") %>% 
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>%
          ungroup()
        
        p_repli_success_score_criteria_met_p2 <- paste0(format.round(
          100*sum(repli_binary_score_p2$repli_score_criteria_met*repli_binary_score_p2$weight)/
            sum(repli_binary_score_p2$weight),1),"%")
      }
      
      # Results: General
      {
        n_claims <- length(unique(repli_outcomes_merged$claim_id))
        
        n_papers <- length(unique(repli_outcomes_merged$paper_id))
        
        n_journals <- length(unique(repli_outcomes_merged$publication_standard))
      }
      
      # Results: Evaluating the replication outcome against the null hypothesis of no effect
      {
        n_papers_repli_stat_sig_claims_same_dir <- 
          format.round(sum(as.numeric((!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value <= 0.05 & 
                            !is.na(repli_outcomes$repli_pattern_criteria_met) & repli_outcomes$repli_pattern_criteria_met==TRUE))*
              repli_outcomes$weight
          ),1)
        
        p_papers_repli_stat_sig_claims_same_dir <- 
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean((!is.na(x$repli_p_value) & x$repli_p_value <= 0.05 &
                                             !is.na(x$repli_pattern_criteria_met) & x$repli_pattern_criteria_met==TRUE),
                                          x$weight)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1,na.rm = FALSE
          )$formatted.text
        
        
        n_papers_repli_stat_sig_claims_opp_dir <- 
          format.round(sum(as.numeric((!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value <= 0.05 & 
                            !is.na(repli_outcomes$repli_pattern_criteria_met) & repli_outcomes$repli_pattern_criteria_met==FALSE))*
                repli_outcomes$weight
          ),1)
        
        p_papers_repli_stat_sig_claims_opp_dir <- 
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean((!is.na(x$repli_p_value) & x$repli_p_value <= 0.05 &
                                             !is.na(x$repli_pattern_criteria_met) & x$repli_pattern_criteria_met==FALSE),
                                          x$weight)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1,na.rm = FALSE
          )$formatted.text
        
        n_papers_repli_non_stat_sig <- 
          format.round(sum(as.numeric(!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value > 0.05)*
                repli_outcomes$weight),1)
        
        p_papers_repli_non_stat_sig <- 
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(!is.na(x$repli_p_value) & as.numeric(x$repli_p_value > 0.05),
                                          x$weight)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1,na.rm = FALSE
          )$formatted.text
        
        n_claims_repli_stat_sig_claims_same_dir <- 
          sum(as.numeric((!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value <= 0.05 & 
                            !is.na(repli_outcomes$repli_pattern_criteria_met) & repli_outcomes$repli_pattern_criteria_met==TRUE))
          )
        
        p_claims_repli_stat_sig_claims_same_dir <- 
          format.text.percent(sum(as.numeric((!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value <= 0.05 & 
                                              !is.na(repli_outcomes$repli_pattern_criteria_met) & repli_outcomes$repli_pattern_criteria_met==TRUE))),
                                n_claims)
        
        n_claims_repli_stat_sig_claims_opp_dir <- 
          sum(as.numeric((!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value <= 0.05 & 
                            !is.na(repli_outcomes$repli_pattern_criteria_met) & repli_outcomes$repli_pattern_criteria_met==FALSE))
          )
        
        p_claims_repli_stat_sig_claims_opp_dir <- 
          format.text.percent(sum(as.numeric((!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value <= 0.05 & 
                                                !is.na(repli_outcomes$repli_pattern_criteria_met) & repli_outcomes$repli_pattern_criteria_met==FALSE))),
                              n_claims)
        
        n_claims_repli_non_stat_sig <- 
          sum(as.numeric(!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value > 0.05))
        
        p_claims_repli_non_stat_sig <- 
          format.text.percent(sum(as.numeric(!is.na(repli_outcomes$repli_p_value) & repli_outcomes$repli_p_value > 0.05)),
                              n_claims)
        
        df.power <- repli_outcomes %>%
          filter(!is.na(rr_power_100_original_effect)) %>% group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>%
          ungroup()
        
        median_power_100_effect <- paste0(format.round(
          100*weighted.median(df.power$rr_power_100_original_effect,df.power$weight),
          digits=1),"%")
        
        p_repli_50_power_for_100_effect <- 
          paste0(format.round(
            100*
              sum(as.integer(df.power$rr_power_100_original_effect >=.5) * df.power$weight)/
              sum( df.power$weight),
            digits=1),"%")
        p_repli_75_power_for_100_effect <- 
          paste0(format.round(
            100*
              sum(as.integer(df.power$rr_power_100_original_effect >=.75) * df.power$weight)/
              sum( df.power$weight),
            digits=1),"%")
        
        df.power <- repli_outcomes[!is.na(repli_outcomes$repli_power_for_50_effect),] %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n())
        
        p_repli_90_power_for_50_effect <- 
          paste0(format.round(
            100*
              sum(as.integer(df.power$repli_power_for_50_effect >=.9) * df.power$weight)/
              sum( df.power$weight),
            digits=1),"%")
        
        df.power <- repli_outcomes[!is.na(repli_outcomes$repli_power_for_75_effect),] %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n())
        
        p_repli_90_power_for_75_effect <- 
          paste0(format.round(
            100*
              sum(as.integer(df.power$repli_power_for_75_effect >=.9) * df.power$weight)/
              sum( df.power$weight),
            digits=1),"%")
        
        df.power <- repli_outcomes %>%
          filter(!is.na(rr_power_100_original_effect_design)) %>% group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>%
          ungroup()
        
        median_power_100_effect_design <- paste0(format.round(
          100*weighted.median(df.power$rr_power_100_original_effect_design,df.power$weight),
          digits=1),"%")
        
        p_repli_50_power_for_100_effect_design <- 
          paste0(format.round(
            100*
              sum(as.integer(df.power$rr_power_100_original_effect_design >=.5) * df.power$weight)/
              sum( df.power$weight),
            digits=1),"%")
        p_repli_75_power_for_100_effect_design <- 
          paste0(format.round(
            100*
              sum(as.integer(df.power$rr_power_100_original_effect_design >=.75) * df.power$weight)/
              sum( df.power$weight),
            digits=1),"%")
        
        
        rm(df.power)
        
        n_bushel_claims_selected <- nrow(non_significant_bushels)
        
        n_bushel_claims_selected_sig <- non_significant_bushels %>%
          filter(nonsig != "T") %>% nrow()
        
        p_bushel_claims_selected_sig <- paste0(format.round(
          100*n_bushel_claims_selected_sig/n_bushel_claims_selected,1),"%")
        
        non_significant_bushels %>%
          count(nonsig != "T") %>%
          mutate(prop = n/sum(n)) %>%
          filter(`nonsig != "T"`) %>%
          pull(prop)

      }
      
      # Results: Evaluating replication success with a variety of binary assessments
      {
        # Prepare binary assessment data
        {
          
          binary.proportions <- 
            do.call(rbind,lapply(df.binvars$binvars.raw,function(binary.var){
              repli_binary_temp <- na.omit(repli_binary[c("paper_id",binary.var)])
              repli_binary_temp <- repli_binary_temp %>%
                group_by(paper_id) %>%
                mutate(weight = 1/n())
              
              n_testable_claims <- nrow(repli_binary_temp)
              p_testable_claims <- n_testable_claims/length(unique(repli_outcomes$claim_id))
              n_passed_claims <- sum(repli_binary_temp[[binary.var]])
              p_passed_temp <- binconf(n_passed_claims,n_testable_claims)
              p_passed_claims <- p_passed_temp[1]
              p_passed_CI_LB_claims <- p_passed_temp[2]
              p_passed_CI_UB_claims <- p_passed_temp[3]
              
              
              n_testable_papers <- length(unique((repli_binary_temp$paper_id)))
              p_testable_papers <- n_testable_papers/length(unique(repli_outcomes$paper_id))
              n_passed_papers <- sum(repli_binary_temp[[binary.var]]*repli_binary_temp$weight)
              p_passed_temp <- cw.proportion(as.integer(repli_binary_temp[[binary.var]]),
                                                repli_binary_temp$weight,repli_binary_temp$paper_id,iters)
              p_passed_papers <- p_passed_temp$point.estimate
              p_passed_CI_LB_papers <- p_passed_temp$CI.lb
              p_passed_CI_UB_papers <- p_passed_temp$CI.ub
              
              data.frame(binary.var,
                         n_testable_claims,p_testable_claims,n_passed_claims,p_passed_claims,p_passed_CI_LB_claims,p_passed_CI_UB_claims,
                         n_testable_papers,p_testable_papers,n_passed_papers,p_passed_papers,p_passed_CI_LB_papers,p_passed_CI_UB_papers
                         )
            }))
          
        }
        
        n_binary_measures <- nrow(df.binvars)
        
        p_claims_binary_measures_measurable_min <- paste0(format.round(min(binary.proportions$p_testable_claims)*100,1),"%")
        p_claims_binary_measures_measurable_max <- paste0(format.round(max(binary.proportions$p_testable_claims)*100,1),"%")
        
        p_papers_binary_measures_measurable_min <- paste0(format.round(min(binary.proportions$p_testable_papers)*100,1),"%")
        p_papers_binary_measures_measurable_max <- paste0(format.round(max(binary.proportions$p_testable_papers)*100,1),"%")
      
        p_binary_passed_papers_min <- paste0(format.round(min(binary.proportions$p_passed_papers)*100,1),"%")
        p_binary_passed_papers_max <- paste0(format.round(max(binary.proportions$p_passed_papers)*100,1),"%")
        p_binary_passed_papers_median <- paste0(format.round(median(binary.proportions$p_passed_papers)*100,1),"%")
        
        p_binary_passed_claims_min <- paste0(format.round(min(binary.proportions$p_passed_claims)*100,1),"%")
        p_binary_passed_claims_max <- paste0(format.round(max(binary.proportions$p_passed_claims)*100,1),"%")
        
        p_binary_passed_papers_MA <- paste0(format.round(binary.proportions[binary.proportions$binary.var=="repli_binary_meta_success",]$p_passed_papers*100,1),"%")
        
        p_binary_passed_papers_orig_wthn <- paste0(format.round(binary.proportions[binary.proportions$binary.var=="repli_binary_orig_wthn",]$p_passed_papers*100,1),"%")
        p_binary_passed_papers_rep_wthn <- paste0(format.round(binary.proportions[binary.proportions$binary.var=="repli_binary_rep_wthn",]$p_passed_papers*100,1),"%")
      
        n_all_measures_claims <- nrow(na.omit(repli_binary))
        
        
        # Generate correlations matrix
        {
          # repli_binary <- repli_outcomes[c("paper_id", "claim_id",
          #                                  df.binvars$binvars.raw)]
          
          # Summarize by proportions w/ CIs
          repli_binary_binaries <- repli_binary[,3:ncol(repli_binary)]
          repli_binary_binaries <- repli_binary_binaries[c(binvars.order)]
          colnames(repli_binary_binaries) <- df.binvars$binvars.full.text
          
          # Generate a results matrix
          repli_binary_cors <- matrix(NA,nrow=ncol(repli_binary_binaries),ncol=ncol(repli_binary_binaries))
          rownames(repli_binary_cors) <- colnames (repli_binary_cors) <- colnames(repli_binary_binaries)
          
          get_wt_corr <- function(i.x,i.y){
            df.cors <- repli_binary_binaries[,c(i.x,i.y)]
            df.cors$paper_id <- repli_binary$paper_id
            df.cors <- na.omit(df.cors)
            df.cors <- df.cors %>%
              group_by(paper_id) %>%
              mutate(weight = 1/n())
            
            weightedCorr(as.integer(unlist(df.cors[,1])),as.integer(unlist(df.cors[,2])),weights=df.cors$weight,method="Spearman")
          }
          
          # Get pairwise complete weighted correlations
          invisible(lapply(1:nrow(repli_binary_cors),function(i){
            lapply(1:(i),function(j){
              if(i!=j){
                repli_binary_cors[i,j] <<- get_wt_corr(i,j)
              }
            })
          }))
        }
        cors.list <- c(na.omit(c(repli_binary_cors)))
        
        repli_binary_cors_intervals <- repli_binary_cors[c(df.binvars[df.binvars$binvars.group=="Contained within interval measures",]$binvars.full.text),c(df.binvars[df.binvars$binvars.group=="Contained within interval measures",]$binvars.full.text)]
        cors_list_intervals <-  paste(format.round(sort(c(na.omit(c(repli_binary_cors_intervals)))),2),collapse=", ")
        
        repli_binary_cors_intervals_vs_non_interval <- c(c(na.omit(c(repli_binary_cors[c(df.binvars[df.binvars$binvars.group=="Contained within interval measures",]$binvars.full.text),c(df.binvars[df.binvars$binvars.group!="Contained within interval measures",]$binvars.full.text)]))),
                                                         c(na.omit(c(repli_binary_cors[c(df.binvars[df.binvars$binvars.group!="Contained within interval measures",]$binvars.full.text),c(df.binvars[df.binvars$binvars.group=="Contained within interval measures",]$binvars.full.text)]))))
        repli_binary_cors_intervals_vs_non_interval_median <- format.round(median(repli_binary_cors_intervals_vs_non_interval),2)
        repli_binary_cors_intervals_vs_non_interval_min <- format.round(min(repli_binary_cors_intervals_vs_non_interval),2)
        repli_binary_cors_intervals_vs_non_interval_max <- format.round(max(repli_binary_cors_intervals_vs_non_interval),2)
        
        binary_cors_median <- format.round(median(cors.list),2)
        binary_cors_min <- format.round(min(cors.list),2)
        binary_cors_max <- format.round(max(cors.list),2)
        
        df.repli_binary_calculatable <- repli_binary %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n())
        
        weights <- df.repli_binary_calculatable$weight
        lapply(3:(ncol(df.repli_binary_calculatable)-1),function(i) {
          df.repli_binary_calculatable[,i] <<- as.integer(!is.na(df.repli_binary_calculatable[,i])) * df.repli_binary_calculatable$weight
        })

        repli_binary_calculatable <- colSums(df.repli_binary_calculatable[,3:(ncol(df.repli_binary_calculatable)-1)])/sum(df.repli_binary_calculatable$weight)
        
        repli_binary_calculatable_min <- paste0(format.round(100*min(repli_binary_calculatable),1),"%")
        repli_binary_calculatable_max <- paste0(format.round(100*max(repli_binary_calculatable),1),"%")
        repli_binary_calculatable_median <- paste0(format.round(100*median(repli_binary_calculatable),1),"%")
      }
      
      # Results: Evaluating replication effect size against original effect size
      {
        
        all_effects <- repli_outcomes %>% 
          filter(!is_covid) %>% 
          filter(repli_version_of_record) %>% 
          select(paper_id,report_id, claim_id, repli_pattern_criteria_met, success = repli_score_criteria_met,
                 repli_conv_r, repli_conv_r_lb, repli_conv_r_ub) %>% 
          left_join(
            orig_outcomes %>% 
              select(claim_id, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub) %>% 
              distinct(),
            by = "claim_id"
          ) %>% 
          drop_na(repli_conv_r) %>% 
          drop_na(orig_conv_r) %>% 
          mutate(across(contains("orig"), abs)) %>% 
          mutate(
            across(
              contains("repli"),
              function(x) ifelse(repli_pattern_criteria_met, abs(x), -1*abs(x))
            )
          ) %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        
        abs_effect_smaller_n_claims <- sum(abs(all_effects$repli_conv_r) < abs(all_effects$orig_conv_r))
        abs_effect_smaller_denom_claims <- sum(!is.na(all_effects$repli_conv_r) & !is.na(all_effects$orig_conv_r))
        abs_effect_smaller_p_claims <- paste0(format.round(100*abs_effect_smaller_n_claims/abs_effect_smaller_denom_claims,1),"%")
        
        abs_effect_smaller_n_papers <- sum((abs(all_effects$repli_conv_r) < abs(all_effects$orig_conv_r))*all_effects$weight)
        abs_effect_smaller_n_papers_formatted <- format.round(abs_effect_smaller_n_papers,1)
        abs_effect_smaller_denom_papers <- sum(all_effects$weight)
        abs_effect_smaller_p_papers <- paste0(format.round(100*abs_effect_smaller_n_papers/abs_effect_smaller_denom_papers,1),"%")
        
        table_4_median_R_papers_neg_ratio <- 
          paste0(format.round(100*(table_4_median_4_1-table_4_median_4_2)/table_4_median_4_1,1),"%")
        
        table_4_median_R_claims_neg_ratio <- 
          paste0(format.round(100*(table_4_median_4_3-table_4_median_4_4)/table_4_median_4_3,1),"%")
        
        corr_pearsons_orig_v_repli <- 
          format.round(weightedCorr(abs(all_effects$orig_conv_r),abs(all_effects$repli_conv_r),method="Spearman",
                     weights=all_effects$weight),2)
      }
      
      # Results: Original and replication effects by discipline
      {
        
        fields.order <- c("business","economics and finance","education","political science",
                          "psychology and health","sociology and criminology")
        
        repli_effects <- repli_outcomes %>%
          filter(!is_covid) %>%
          filter(repli_version_of_record) %>%
          select(report_id, claim_id,repli_sample_size_value, success = repli_score_criteria_met, repli_type,
                 repli_conv_r, repli_conv_r_lb, repli_conv_r_ub, repli_effect_size_type, repli_effect_size_value) %>%
          mutate(ser_effect = ifelse(str_detect(repli_effect_size_type, "ser_"), abs(repli_effect_size_value), NA)) %>%
          mutate(across(contains("conv"), abs))
        
        orig_effects <- orig_outcomes %>%
          semi_join(repli_effects, by = "claim_id") %>%
          select(claim_id, paper_id,orig_sample_size_value, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub,
                 orig_effect_size_type_repli, orig_effect_size_value_repli) %>%
          mutate(ser_effect = ifelse(str_detect(orig_effect_size_type_repli, "ser_"), abs(orig_effect_size_value_repli), NA)) %>%
          mutate(across(contains("conv"), abs)) %>%
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>%
          left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id")
        
        effects_combined <- merge(orig_effects,repli_effects,by="claim_id",all.x = TRUE,all.y = FALSE)
        
        effects_combined <- effects_combined %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        
        effects_by_field <- do.call(rbind,lapply(fields.order, function(field) {
          data <- effects_combined[effects_combined$field==field,]
          c1.numerator <- sum(as.numeric(data$success)*data$weight)
          c1.denom <- length(unique(data$paper_id))
          c1.p <- sum(as.numeric(data$success)*data$weight)/length(unique(data$paper_id))
          c2.numerator <- sum(as.numeric(data$success))
          c2.denom <- length(unique(data$claim_id))
          c2.p <- sum(as.numeric(data$success))/length(unique(data$claim_id))
          data.frame(field,
                     c1.numerator,
                     c1.denom,
                     c1.p,
                     c2.numerator,
                     c2.denom,
                     c2.p)
        }))
        
        # table_5_min_p_c1 <- paste0(format.round(min(100*table_5_p[,1]),1),"%")
        # table_5_max_p_c1 <- paste0(format.round(max(100*table_5_p[,1]),1),"%")
        # table_5_median_p_c1 <- paste0(format.round(median(100*table_5_p[,1]),1),"%")
        # table_5_min_p_c2 <- paste0(format.round(min(100*table_5_p[,2]),1),"%")
        # table_5_max_p_c2 <- paste0(format.round(max(100*table_5_p[,2]),1),"%")
        # table_5_median_p_c2 <- paste0(format.round(median(100*table_5_p[,2]),1),"%")
        
        repli_rate_field_min_p_papers <- paste0(format.round(min(100*effects_by_field$c1.p),1),"%")
        repli_rate_field_max_p_papers <- paste0(format.round(max(100*effects_by_field$c1.p),1),"%")
        repli_rate_field_median_p_papers <- paste0(format.round(median(100*effects_by_field$c1.p),1),"%")
        repli_rate_field_min_p_claims <- paste0(format.round(min(100*effects_by_field$c2.p),1),"%")
        repli_rate_field_max_p_claims <- paste0(format.round(max(100*effects_by_field$c2.p),1),"%")
        repli_rate_field_median_p_claims <- paste0(format.round(median(100*effects_by_field$c2.p),1),"%")
        
        chisq.fields <- wtd.chi.sq(effects_combined$success,effects_combined$field,weight=effects_combined$weight)
        chisq_success_fields_pvalue_papers <- format.round(chisq.fields[3],2)
        
        chisq.fields <- wtd.chi.sq(effects_combined$success,effects_combined$field,weight=rep(1,nrow(effects_combined)))
        chisq_success_fields_pvalue_claims <- format.round(chisq.fields[3],2)
      }
      
      # Results: Original and replication effects by year of original publication
      {
        repli_effects <- repli_outcomes %>% 
          filter(!is_covid) %>% 
          filter(repli_version_of_record) %>% 
          select(report_id, claim_id, repli_sample_size_value, success = repli_score_criteria_met, repli_type,
                 repli_conv_r, repli_conv_r_lb, repli_conv_r_ub, repli_effect_size_type, repli_effect_size_value) %>%
          mutate(ser_effect = ifelse(str_detect(repli_effect_size_type, "ser_"), abs(repli_effect_size_value), NA)) %>% 
          mutate(across(contains("conv"), abs))
        
        orig_effects <- orig_outcomes %>% 
          select(claim_id, paper_id,orig_sample_size_value, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub,
                 orig_effect_size_type_repli, orig_effect_size_value_repli,pub_year,field) %>%
          semi_join(repli_effects, by = "claim_id") %>% 
          mutate(ser_effect = ifelse(str_detect(orig_effect_size_type_repli, "ser_"), abs(orig_effect_size_value_repli), NA)) %>% 
          mutate(across(contains("conv"), abs))
        
        effects_combined <- merge(repli_effects,orig_effects,by="claim_id",all.x = TRUE,all.y = FALSE)
        
        effects_combined <- effects_combined %>%
          filter(!is.na(repli_conv_r)) %>% 
          filter(!is.na(orig_conv_r)) %>% 
          group_by(paper_id) %>%
          mutate(weight = 1/n())
        
        orig_papers_pearsons_r_papers_median <- 
          bootstrap.clust(data=effects_combined,
                          FUN=function(x) {
                            weighted.median(x$orig_conv_r,x$weight)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_conv_r","orig_conv_r","weight"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          digits=2,na.rm = FALSE
          )$formatted.text
        
        repli_papers_pearsons_r_papers_median <- 
          bootstrap.clust(data=effects_combined,
                          FUN=function(x) {
                            weighted.median(x$repli_conv_r,x$weight)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_conv_r","orig_conv_r","weight"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          digits=2,na.rm = FALSE
          )$formatted.text
        
        ratio_diff_papers_repli_v_orig <- 
          bootstrap.clust(data=effects_combined,
                          FUN=function(x) {
                            (weighted.median(x$orig_conv_r,x$weight)-weighted.median(x$repli_conv_r,x$weight))/
                              weighted.median(x$orig_conv_r,x$weight)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_conv_r","orig_conv_r","weight","paper_id"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          digits=1,na.rm = FALSE,format.percent = TRUE
          )$formatted.text
        
        ratio_sv_median_papers_repli_v_orig <- 
          bootstrap.clust(data=effects_combined,
                          FUN=function(x) {
                            (weighted.median(x$orig_conv_r,x$weight)^2-weighted.median(x$repli_conv_r,x$weight)^2)/
                              weighted.median(x$orig_conv_r,x$weight)^2
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_conv_r","orig_conv_r","weight","paper_id"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          digits=1,na.rm = FALSE,format.percent = TRUE
          )$formatted.text
        
        
        pearsons_r_papers_orig_v_repli_same_dir <- 
          bootstrap.clust(data=effects_combined[(effects_combined$repli_conv_r>=0)==(effects_combined$repli_conv_r>=0),],
                        FUN=function(x) {
                            weightedCorr(x=x$repli_conv_r,
                                         y=x$orig_conv_r,
                                         method="Pearson",weights=x$weight)
                          },
                        clustervar = "paper_id",
                        keepvars=c("repli_conv_r","orig_conv_r","weight"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        digits=3,na.rm = FALSE
                        )$formatted.text
        
        pearsons_r_claims_orig_v_repli_same_dir <- 
          bootstrap.clust(data=effects_combined[(effects_combined$repli_conv_r>=0)==(effects_combined$repli_conv_r>=0),],
                          FUN=function(x) {
                            weightedCorr(x=x$repli_conv_r,
                                         y=x$orig_conv_r,
                                         method="Pearson")
                          },
                          clustervar = "claim_id",
                          keepvars=c("repli_conv_r","orig_conv_r","weight"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          digits=3,na.rm = FALSE
          )$formatted.text
        
        median_r_by_year <- 
          do.call(rbind,lapply(2009:2018,function(year){
            data <- effects_combined[effects_combined$pub_year==year,]
            median.orig <- median(data$orig_conv_r)
            median.repli <- median(data$repli_conv_r)
            median.diff <- abs(median.orig-median.repli)
            data.frame(year,median.orig,median.repli,median.diff)
          }))
        
        year_pearsons_r_median_diff_min <- 
          median_r_by_year[median_r_by_year$median.diff==min(median_r_by_year$median.diff),]$year
        year_pearsons_r_median_diff_max <- 
          median_r_by_year[median_r_by_year$median.diff==max(median_r_by_year$median.diff),]$year
        pearsons_r_orig_median_diff_yearmin <- 
          format.round(
            median_r_by_year[median_r_by_year$median.diff==min(median_r_by_year$median.diff),]$median.orig,2)
        pearsons_r_repli_median_diff_yearmin <- 
          format.round(
            median_r_by_year[median_r_by_year$median.diff==min(median_r_by_year$median.diff),]$median.repli,2)
        pearsons_r_orig_median_diff_yearmax <- 
          format.round(
            median_r_by_year[median_r_by_year$median.diff==max(median_r_by_year$median.diff),]$median.orig,2)
        pearsons_r_repli_median_diff_yearmax <- 
          format.round(
            median_r_by_year[median_r_by_year$median.diff==max(median_r_by_year$median.diff),]$median.repli,2)
        
        
        chisq.years <- wtd.chi.sq(effects_combined$success,effects_combined$pub_year,weight=effects_combined$weight)
        chisq_success_years_pvalue <- format.round(chisq.years[3],2)
      }
      
      # Results: Original and replication effects by new data or secondary data replication
      {
        repli_effects <- repli_outcomes %>%
          filter(!is_covid) %>%
          filter(repli_version_of_record) %>%
          select(report_id, claim_id,repli_sample_size_value, success = repli_score_criteria_met, repli_type,
                 repli_conv_r, repli_conv_r_lb, repli_conv_r_ub, repli_effect_size_type, repli_effect_size_value) %>%
          mutate(ser_effect = ifelse(str_detect(repli_effect_size_type, "ser_"), abs(repli_effect_size_value), NA)) %>%
          mutate(across(contains("conv"), abs))
        
        orig_effects <- orig_outcomes %>%
          semi_join(repli_effects, by = "claim_id") %>%
          select(claim_id, paper_id,orig_sample_size_value, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub,
                 orig_effect_size_type_repli, orig_effect_size_value_repli) %>%
          mutate(ser_effect = ifelse(str_detect(orig_effect_size_type_repli, "ser_"), abs(orig_effect_size_value_repli), NA)) %>%
          mutate(across(contains("conv"), abs)) %>%
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>%
          left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id")
        
        effects_combined <- merge(orig_effects,repli_effects,by="claim_id",all.x = TRUE,all.y = FALSE)
        
        effects_combined <- effects_combined %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        
        ratio_papers_repli_stat_sig_same_pattern_new_data_vs_secondary <- 
          bootstrap.clust(data=effects_combined,
                        FUN=function(x) {
                          (sum(x[x$repli_type=="new data",]$success*x[x$repli_type=="new data",]$weight)/length(unique(x[x$repli_type=="new data",]$paper_id)))/
                            (sum(x[x$repli_type=="secondary data",]$success*x[x$repli_type=="secondary data",]$weight)/length(unique(x[x$repli_type=="secondary data",]$paper_id)))
                        },
                        clustervar = "paper_id",
                        keepvars=c("repli_type","success","weight","paper_id"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        digits=3,na.rm = FALSE
        )$formatted.text
        
        ratio_claims_repli_stat_sig_same_pattern_new_data_vs_secondary <- 
          bootstrap.clust(data=effects_combined,
                          FUN=function(x) {
                            (sum(x[x$repli_type=="new data",]$success)/length(unique(x[x$repli_type=="new data",]$claim_id)))/
                              (sum(x[x$repli_type=="secondary data",]$success)/length(unique(x[x$repli_type=="secondary data",]$claim_id)))
                          },
                          clustervar = "claim_id",
                          keepvars=c("repli_type","success","weight","paper_id"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          digits=3,na.rm = FALSE
          )$formatted.text
        
        effects_combined <- effects_combined %>%
          filter(!is.na(repli_conv_r) & !is.na(orig_conv_r)) %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        
        secondary_repli_by_field <- na.omit(do.call(rbind,lapply(unique(repli_outcomes_merged$field),function(field) {
          data <- repli_outcomes_merged[!is.na(repli_outcomes_merged$field) & repli_outcomes_merged$field==field,]
          data <- data %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
          n_secondary_data <- sum(as.integer(data$repli_type=="secondary data")*data$weight)
          n_total <- length(unique(data$paper_id))
          p <- n_secondary_data/n_total
          p.formatted <- paste0(format.round(100*p,1),"%")
          p.formatted.combined <- paste0(p.formatted,", n=",n_secondary_data,"/",n_total,"")
          data.frame(field,n_secondary_data,n_total,p,p.formatted,p.formatted.combined)
        })))
        
        secondary_repli_by_field <- secondary_repli_by_field[order(-secondary_repli_by_field$p),]

        for (i in 1:nrow(secondary_repli_by_field)){
          assign(paste0("field_",i,"_highest_p_secondary_data"),secondary_repli_by_field$field[i])
          assign(paste0("p_",i,"_highest_p_secondary_data"),secondary_repli_by_field$p.formatted.combined[i])
        }
        
        repli_outcomes_nd <- repli_outcomes[repli_outcomes$repli_type=="new data",] %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        
        repli_outcomes_sd <- repli_outcomes[repli_outcomes$repli_type=="secondary data",] %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        
        median_power_100_effect_new_data_papers <- 
          paste0(format.round(100*weighted.median(repli_outcomes_nd$rr_power_100_original_effect,repli_outcomes_nd$weight,na.rm=TRUE),1),"%")
        median_power_100_effect_new_data_claims <- 
          paste0(format.round(100*median(repli_outcomes_nd$rr_power_100_original_effect,na.rm=TRUE),1),"%")
        
        median_power_100_effect_secondary_data_papers <- 
          paste0(format.round(100*weighted.median(repli_outcomes_sd$rr_power_100_original_effect,repli_outcomes_sd$weight,na.rm=TRUE),1),"%")
        median_power_100_effect_secondary_data_claims <- 
          paste0(format.round(100*median(repli_outcomes_sd$rr_power_100_original_effect,na.rm=TRUE),1),"%")
        
        mean_power_100_effect_new_data_papers <- 
          paste0(format.round(100*weighted.mean(repli_outcomes_nd$rr_power_100_original_effect,repli_outcomes_nd$weight,na.rm=TRUE),1),"%")
        mean_power_100_effect_new_data_claims <- 
          paste0(format.round(100*mean(repli_outcomes_nd$rr_power_100_original_effect,na.rm=TRUE),1),"%")
        
        mean_power_100_effect_secondary_data_papers <- 
          paste0(format.round(100*weighted.mean(repli_outcomes_sd$rr_power_100_original_effect,repli_outcomes_sd$weight,na.rm=TRUE),1),"%")
        mean_power_100_effect_secondary_data_claims <- 
          paste0(format.round(100*mean(repli_outcomes_sd$rr_power_100_original_effect,na.rm=TRUE),1),"%")
        
        effects_combined_nd <- effects_combined[effects_combined$repli_type=="new data",] %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        n_new_data_papers <- length(unique(effects_combined_nd$paper_id))
        n_new_data_papers_repli_r_smaller <-
          sum(as.integer(effects_combined_nd$repli_conv_r<effects_combined_nd$orig_conv_r)*
                effects_combined_nd$weight)
        n_new_data_papers_repli_r_smaller_formatted <- format.round(n_new_data_papers_repli_r_smaller,1)
        p_new_data_papers_repli_r_smaller <- paste0(format.round(
          100*n_new_data_papers_repli_r_smaller/n_new_data_papers,1
        ),"%")
        
        effects_combined_sd <- effects_combined[effects_combined$repli_type=="secondary data",] %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        n_secondary_data_papers <- length(unique(effects_combined_sd$paper_id))
        n_secondary_data_papers_repli_r_smaller <-
          sum(as.integer(effects_combined_sd$repli_conv_r<effects_combined_sd$orig_conv_r)*
                effects_combined_sd$weight)
        n_secondary_data_papers_repli_r_smaller_formatted <- format.round(n_secondary_data_papers_repli_r_smaller,1)
        p_secondary_data_papers_repli_r_smaller <- paste0(format.round(
          100*n_secondary_data_papers_repli_r_smaller/n_secondary_data_papers,1
        ),"%")
      }
      
      # Discussion
      {
        
      }
      
      # Sampling frame and selection of claims for replication
      {
        
        n_journals_SCORE_all <- paper_metadata_orig %>%
          filter(!is_covid) %>%
          count(publication_standard) %>%
          nrow()
        
        n_papers_selected_repli <- status %>% filter(RR) %>% nrow()
        
        n_papers_initial_sample <- status %>% filter(p1_delivery | p2_delivery) %>% nrow()
        
        n_repli_papers_eligible <- status %>% filter(RR | p2_delivery) %>% nrow()
        
        n_papers_p2_constrained <-  status %>% filter(p2_delivery) %>% nrow()
        
        n_repli_papers_eligible_single_claim <- status %>%
          filter(RR | p2_delivery) %>%
          anti_join(status %>% filter(bushel), by = "paper_id") %>%
          nrow()
        
        n_repli_papers_eligible_multi_claim <- status %>% filter(bushel) %>% nrow()
        
        n_repli_hybrid <- repli_outcomes_orig %>%
          filter(!is_covid) %>%
          filter(repli_type == "original and secondary data") %>%
          nrow()
        
        n_total_replis_conducted <- repli_outcomes_orig %>%
          filter(!is_covid & repli_type != "original and secondary data") %>%
          filter(is.na(manylabs_type) | manylabs_type != "aggregation") %>%
          mutate(rep_attempt = select(., c(rr_id, claim_id)) %>% apply(1, function(x) str_c(x, collapse = " -- "))) %>%
          count(rep_attempt) %>%
          nrow()
        
        n_repli_attempt_started <- all_rr_attempts %>%
          filter(field != "covid") %>%
          filter(str_detect(type, "Replication")) %>%
          pull(rr_id) %>% unique() %>% length()
        
        n_repli_claims_same_protocol <- repli_outcomes_orig %>%
          filter(!is_covid) %>%
          filter(is_manylabs == "traditional") %>%
          count(claim_id) %>%
          nrow()
        
        n_repli_claims_distinct_protocol <- repli_outcomes_orig %>%
          filter(!is_covid) %>%
          filter(is_manylabs == "posthoc") %>%
          count(claim_id) %>%
          nrow()
          
      }
      
      # The unbearable inscrutability of replication
      {
        pearson_r_measurable_psych_claims_n <- sum(!is.na(repli_outcomes_merged[repli_outcomes_merged$field=="Psychology and Health",]$repli_conv_r))
        pearson_r_measurable_psych_claims_p <- paste0(format.round(
          100*pearson_r_measurable_psych_claims_n/nrow(repli_outcomes_merged[repli_outcomes_merged$field=="Psychology and Health",]),1),"%")
        
        pearson_r_measurable_econ_claims_n <- sum(!is.na(repli_outcomes_merged[repli_outcomes_merged$field=="Economics and Finance",]$repli_conv_r))
        pearson_r_measurable_econ_claims_p <- paste0(format.round(
          100*pearson_r_measurable_econ_claims_n/nrow(repli_outcomes_merged[repli_outcomes_merged$field=="Economics and Finance",]),1),"%")
      }

    }
    
    # Supplement stats
    {
      # Attrition of replication attempts selected in Phase 1
      {
        n_papers_repl_team_ident_but_not_complete <- rr_sourced %>%
          filter(type == "replication") %>%
          semi_join(status %>% filter(p1_delivery), by = "paper_id") %>%
          anti_join(repli_export, by = "paper_id") %>%
          select(paper_id) %>%
          distinct() %>%
          nrow()
        
        n_papers_repli_never_started <- rr_sourced %>%
          filter(type == "replication") %>%
          semi_join(status %>% filter(p1_delivery), by = "paper_id") %>%
          anti_join(repli_export, by = "paper_id") %>%
          select(paper_id) %>%
          distinct() %>%
          anti_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>% nrow()
        
        p_papers_repli_never_started <- paste0(format.round(
          100*n_papers_repli_never_started/n_papers_repl_team_ident_but_not_complete,1
        ),"%")
        
        n_papers_repli_OSF_no_prereg <- rr_sourced %>%
          filter(type == "replication") %>%
          semi_join(status %>% filter(p1_delivery), by = "paper_id") %>%
          anti_join(repli_export, by = "paper_id") %>%
          select(paper_id) %>%
          distinct() %>%
          semi_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>%
          left_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>%
          mutate(
            registered = !outcome & results_available == "no" & (!is.na(registrations) | prereg_completion == "approve"),
            prereg = !outcome & results_available == "no" & is.na(registrations) & prereg_completion == "complete",
            part = !outcome & results_available == "no" & is.na(registrations) & prereg_completion %in% c("near-complete", "partial", "minimal"),
          ) %>%
          filter(is.na(registered) & is.na(prereg) & !part) %>%
          nrow()
          
        p_papers_repli_OSF_no_prereg <- paste0(format.round(
          100*n_papers_repli_OSF_no_prereg/n_papers_repl_team_ident_but_not_complete,1
        ),"%")
        
        n_papers_repli_prereg_started_not_finished <- rr_sourced %>%
          filter(type == "replication") %>%
          semi_join(status %>% filter(p1_delivery), by = "paper_id") %>%
          anti_join(repli_export, by = "paper_id") %>%
          select(paper_id) %>%
          distinct() %>%
          semi_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>%
          left_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>%
          mutate(
            registered = !outcome & results_available == "no" & (!is.na(registrations) | prereg_completion == "approve"),
            prereg = !outcome & results_available == "no" & is.na(registrations) & prereg_completion == "complete",
            part = !outcome & results_available == "no" & is.na(registrations) & prereg_completion %in% c("near-complete", "partial", "minimal"),
          ) %>%
          filter(part) %>%
          nrow()
        
        p_papers_repli_prereg_started_not_finished <- paste0(format.round(
          100*n_papers_repli_prereg_started_not_finished/n_papers_repl_team_ident_but_not_complete,1
        ),"%")
        
        n_papers_repli_completed_prereg <- rr_sourced %>%
          filter(type == "replication") %>%
          semi_join(status %>% filter(p1_delivery), by = "paper_id") %>%
          anti_join(repli_export, by = "paper_id") %>%
          select(paper_id) %>%
          distinct() %>%
          semi_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>%
          left_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>%
          mutate(
            registered = !outcome & results_available == "no" & (!is.na(registrations) | prereg_completion == "approve"),
            prereg = !outcome & results_available == "no" & is.na(registrations) & prereg_completion == "complete",
            part = !outcome & results_available == "no" & is.na(registrations) & prereg_completion %in% c("near-complete", "partial", "minimal"),
          ) %>%
          filter(prereg & !registered) %>%
          nrow()
        
        p_papers_repli_completed_prereg <- paste0(format.round(
          100*n_papers_repli_completed_prereg/n_papers_repl_team_ident_but_not_complete,1
        ),"%")
        
        n_papers_repli_registered_study <- rr_sourced %>%
          filter(type == "replication") %>%
          semi_join(status %>% filter(p1_delivery), by = "paper_id") %>%
          anti_join(repli_export, by = "paper_id") %>%
          select(paper_id) %>%
          distinct() %>%
          semi_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>%
          left_join(
            all_rr_attempts %>%
              filter(field != "covid") %>%
              filter(str_detect(type, "Replication")),
            by = "paper_id"
          ) %>%
          mutate(
            registered = !outcome & results_available == "no" & (!is.na(registrations) | prereg_completion == "approve"),
            prereg = !outcome & results_available == "no" & is.na(registrations) & prereg_completion == "complete",
            part = !outcome & results_available == "no" & is.na(registrations) & prereg_completion %in% c("near-complete", "partial", "minimal"),
          ) %>%
          filter(prereg & !registered) %>%
          nrow()
        
        p_papers_repli_registered_study <- paste0(format.round(
          100*n_papers_repli_registered_study/n_papers_repl_team_ident_but_not_complete,1
        ),"%")
      }
      
      # Non-random selection and no attrition of replications selected in Phase 2
      {
        n_papers_p2_repli_completed_nd <- 
          repli_outcomes %>%
          filter(type_internal == "p2") %>%
          filter(repli_type == "new data") %>%
          nrow()
        
        n_papers_p2_repli_completed_sd <- 
          repli_outcomes %>%
          filter(type_internal == "p2") %>%
          filter(repli_type == "secondary data") %>%
          nrow()
      }
      
      # Excluded cases
      {
        id_excluded_case_orig_negative <-  repli_case_exclusions %>%
          left_join(repli_export %>% select(rr_id, paper_id) %>% distinct(), by = "rr_id") %>%
          filter(exclude_reason == "Non-significant original findings") %>%
          pull(paper_id) %>%
          unique()
        
        id_excluded_sample_size_too_small <- repli_case_exclusions %>%
          left_join(repli_export %>% select(rr_id, paper_id) %>% distinct(), by = "rr_id") %>%
          filter(exclude_reason == "Incomplete or insufficient data collection") %>%
          pull(paper_id) %>%
          unique() %>%
          str_c(., collapse = ", ")
        
        n_excluded_power_all_approach_nd <- repli_case_exclusions %>%
          left_join(repli_export %>% select(rr_id, paper_id) %>% distinct(), by = "rr_id") %>%
          filter(exclude_reason == "Power/sample size issue") %>%
          left_join(repli_export %>% select(rr_id, rr_type_internal) %>% distinct(), by = "rr_id") %>%
          filter(rr_type_internal == "Direct Replication") %>%
          nrow()
        
        id_excluded_power_all_approach_nd <- repli_case_exclusions %>%
          left_join(repli_export %>% select(rr_id, paper_id) %>% distinct(), by = "rr_id") %>%
          filter(exclude_reason == "Power/sample size issue") %>%
          left_join(repli_export %>% select(rr_id, rr_type_internal) %>% distinct(), by = "rr_id") %>%
          filter(rr_type_internal == "Direct Replication") %>%
          pull(paper_id) %>%
          unique() %>%
          str_c(., collapse = ", ")
        
        n_excluded_power_all_approach_sd <-  repli_case_exclusions %>%
          left_join(repli_export %>% select(rr_id, paper_id) %>% distinct(), by = "rr_id") %>%
          filter(exclude_reason == "Power/sample size issue") %>%
          left_join(repli_export %>% select(rr_id, rr_type_internal) %>% distinct(), by = "rr_id") %>%
          filter(rr_type_internal == "Data Analytic Replication") %>%
          nrow()
        
        id_excluded_power_all_approach_sd <- repli_case_exclusions %>%
          left_join(repli_export %>% select(rr_id, paper_id) %>% distinct(), by = "rr_id") %>%
          filter(exclude_reason == "Power/sample size issue") %>%
          left_join(repli_export %>% select(rr_id, rr_type_internal) %>% distinct(), by = "rr_id") %>%
          filter(rr_type_internal == "Data Analytic Replication") %>%
          pull(paper_id) %>%
          unique() %>%
          str_c(., collapse = ", ")
        
        n_claims_excluded <- repli_case_exclusions %>%
          left_join(repli_export %>% select(rr_id, paper_id) %>% distinct(), by = "rr_id") %>%
          nrow()
        
        n_papers_excluded <- repli_case_exclusions %>%
          left_join(repli_export %>% select(rr_id, paper_id) %>% distinct(), by = "rr_id") %>%
          pull(paper_id) %>%
          unique() %>%
          length()
        
        n_excluded_claims_nonsig <- repli_case_exclusions %>%
          filter(exclude_reason != "Non-significant original findings") %>%
          nrow()
        
        n_excluded_claims_nonsig_success <- repli_case_exclusions %>%
          filter(exclude_reason != "Non-significant original findings") %>%
          left_join(repli_export %>% select(report_id = unique_report_id, score = rr_repl_exact_replicated_reported), by = "report_id") %>%
          filter(score) %>%
          nrow()
      }
      
      # Sample and Data
      {
        n_claims_potential_repli <- status %>%
          filter(pool) %>%
          nrow()
        
        n_claims_eligible_nonrand <- status %>%
          filter(bushel) %>%
          nrow()
      }
      
      # Sample Size Planning 
      {
        # New data replications conducted during Phase 1
        p_claim_nd_attempt_75_90_power_p1 <- paste0(format.round(100*(
          repli_outcomes_orig %>%
          filter(!is_covid & repli_version_of_record) %>%
          filter(repli_type == "new data") %>%
          filter(nchar(rr_id) < 5) %>%
          drop_na(repli_power_for_75_effect) %>%
          count(repli_power_for_75_effect >= .9) %>%
          mutate(prop = n/sum(n)) %>%
          filter(`repli_power_for_75_effect >= 0.9`) %>%
          pull(prop)
          ),0),"%")
          
        p_claim_nd_attempt_75_90_power_achieved_p1 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(repli_type == "new data") %>%
            filter(nchar(rr_id) < 5) %>%
            pull(repli_power_for_75_effect) %>%
            mean(na.rm = T)
        ),0),"%")
        
        median_claim_nd_attempt_75_90_power_achieved_p1 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(nchar(rr_id) < 5) %>%
            filter(repli_type == "new data") %>%
            pull(repli_power_for_75_effect) %>%
            median(na.rm = T)
        ),0),"%")
        
        p_claim_sd_attempt_75_90_power_achieved_p1 <- paste0(format.round(100*(
          repli_outcomes %>%
          filter(!is_covid & repli_version_of_record) %>%
          filter(repli_type == "secondary data") %>%
          filter(nchar(rr_id) < 5) %>%
          pull(rr_power_100_original_effect) %>%
          mean(na.rm = T)
        ),0),"%")
        
        median_claim_sd_attempt_75_90_power_achieved_p1 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(repli_type == "secondary data") %>%
            filter(nchar(rr_id) < 5) %>%
            pull(rr_power_100_original_effect) %>%
            median(na.rm = T)
        ),0),"%")
        
        p_claim_sd_attempt_50_100_power_p1 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(repli_type == "secondary data") %>%
            filter(nchar(rr_id) < 5) %>%
            drop_na(rr_power_100_original_effect) %>%
            count(rr_power_100_original_effect >= .5) %>%
            mutate(prop = n/sum(n)) %>%
            filter(`rr_power_100_original_effect >= 0.5`) %>%
            pull(prop)
        ),0),"%")
        
        p_claim_sd_attempt_50_100_power_achieved_p1 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(repli_type == "secondary data") %>%
            filter(nchar(rr_id) < 5) %>%
            pull(rr_power_100_original_effect) %>%
            mean(na.rm = T)
        ),0),"%")
        
        median_claim_sd_attempt_50_100_power_achieved_p1 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(repli_type == "secondary data") %>%
            filter(nchar(rr_id) < 5) %>%
            pull(rr_power_100_original_effect) %>%
            median(na.rm = T)
        ),0),"%")
        
        # New data replications conducted during Phase 2
        p_claim_nd_attempt_75_90_power_p2 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(nchar(rr_id) > 4) %>%
            filter(repli_type == "new data") %>%
            count(repli_power_for_75_effect >= .9) %>%
            drop_na() %>%
            mutate(prop = n/sum(n)) %>%
            filter(`repli_power_for_75_effect >= 0.9`) %>%
            pull(prop)
        ),0),"%")
        
        p_claim_sd_attempt_75_90_power_achieved_p2 <- paste0(format.round(100*(
          repli_outcomes %>%
          filter(!is_covid & repli_version_of_record) %>%
          filter(nchar(rr_id) > 4) %>%
          filter(repli_type == "new data") %>%
          pull(repli_power_for_75_effect) %>%
          mean(na.rm = T)
        ),0),"%")
        
        median_claim_sd_attempt_75_90_power_achieved_p2 <- paste0(format.round(100*(
          repli_outcomes %>%
          filter(!is_covid & repli_version_of_record) %>%
          filter(nchar(rr_id) > 4) %>%
          filter(repli_type == "new data") %>%
          pull(repli_power_for_75_effect) %>%
          median(na.rm = T)
        ),0),"%")
        
        median_claim_sd_attempt_50_100_power_p2 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(repli_type == "secondary data") %>%
            filter(nchar(rr_id) > 4) %>%
            drop_na(rr_power_100_original_effect) %>%
            count(rr_power_100_original_effect >= .5) %>%
            mutate(prop = n/sum(n)) %>%
            filter(`rr_power_100_original_effect >= 0.5`) %>%
            pull(prop)
        ),0),"%")
        
        median_claim_sd_attempt_50_100_power_achieved_p2 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(repli_type == "secondary data") %>%
            filter(nchar(rr_id) > 4) %>%
            pull(rr_power_100_original_effect) %>%
            mean(na.rm = T)
        ),0),"%")
        
        median_claim_sd_attempt_50_100_power_achieved_p2 <- paste0(format.round(100*(
          repli_outcomes %>%
            filter(!is_covid & repli_version_of_record) %>%
            filter(repli_type == "secondary data") %>%
            filter(nchar(rr_id) > 4) %>%
            pull(rr_power_100_original_effect) %>%
            median(na.rm = T)
        ),0),"%")
        
        # SER Method for estimating power versus traditional power analysis
        n_claims_SER_power <- 
          orig_dataset %>%
          semi_join(repli_outcomes %>% filter(!is_covid & repli_version_of_record), by = c("unique_claim_id" = "claim_id")) %>%
          filter(str_detect(original_statistic_analysis_type_statsteam, "ser")) %>%
          nrow()
        
        
      }
      
      # Data collection & analysis
      {
        
      }
      
    }
    
    
    # Clean up input values and export
    {
      rm(iters,orig_outcomes,repli_outcomes,repli_outcomes_merged)
      return(rev(as.list(environment())))
    }
  }

  figures <- function(repli_outcomes,orig_outcomes,paper_metadata,repli_binary,iters = 100){
    # Initialization and data preparation
    {
      
      # Set defaults for convenience
      {
        if (!exists("iters")){ iters <- 100}
        if (!exists("generate_binary_data")){ generate_binary_data <- FALSE}
      }
      
      # Trim out non-version of record lines and non-COVID entries
      {
        repli_outcomes_orig <- repli_outcomes
        
        repli_outcomes <- repli_outcomes[repli_outcomes$repli_version_of_record,]
        repli_outcomes <- repli_outcomes[!repli_outcomes$is_covid,]
      }
      
      # Generate binary data. Note: generate_binary_data flag by default is set 
      # to FALSE, which pulls the binary data precalculated from the data pipeline.
      # If set to TRUE, the code will run to generate the binary success measures
      # from scratch. This is time consuming, but will produce identical results)
      {
        if(generate_binary_data==TRUE ){
          # FILL IN WITH code from Andrew
          
        } else {
          # Switch to paper_id / claim_id coding
          repli_binary <- merge(repli_outcomes[c("paper_id","claim_id","report_id","repli_score_criteria_met")],
                                repli_binary,all.x = FALSE,all.y=TRUE,by="report_id")
          repli_binary$report_id <- NULL
        }
      }
      
      # Set binary measures variable names
      {
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
        
        binvars.raw <- c("repli_score_criteria_met",
                         "repli_binary_analyst",
                         "repli_binary_orig_wthn",
                         "repli_binary_rep_wthn",
                         "repli_binary_wthn_pi",
                         "repli_binary_meta_success",
                         "repli_binary_sum_p",
                         "repli_binary_telescopes",
                         "repli_binary_bf_result",
                         "repli_binary_bayes_rep",
                         "repli_binary_bma_result",
                         "repli_binary_skep_p",
                         "repli_binary_correspondence")
        binvars.order <- binvars.raw
        
        binvars.full.text <- c("Statistical signifance + pattern",
                           "Subjective interpretation",
                           "Original effect in replication's CI",
                           "Replication effect in original's CI",
                           "Replication effect in prediction interval",
                           "Meta-analysis",
                           "Sum of p-values",
                           "Small telescopes",
                           "Bayes factor",
                           "Replication Bayes factor",
                           "Bayesian meta-analysis",
                           "Skeptical p-value",
                           "Correspondence test")
        binvars.group <- c("Statistic significance / p-value based measures",
                           "SCORE specific",
                           "Contained within interval measures",
                           "Contained within interval measures",
                           "Contained within interval measures",
                           "Meta analysis based measures",
                           "Statistic significance / p-value based measures",
                           "Combined effect magnitude and uncertainty measures",
                           "Combined effect magnitude and uncertainty measures",
                           "Combined effect magnitude and uncertainty measures",
                           "Meta analysis based measures",
                           "Statistic significance / p-value based measures",
                           "Combined effect magnitude and uncertainty measures")
        
        binvars.order <- c("repli_binary_analyst",
                           "repli_score_criteria_met",
                           "repli_binary_sum_p",
                           "repli_binary_skep_p",
                           "repli_binary_orig_wthn",
                           "repli_binary_rep_wthn",
                           "repli_binary_wthn_pi",
                           "repli_binary_meta_success",
                           "repli_binary_bma_result",
                           "repli_binary_telescopes",
                           "repli_binary_bf_result",
                           "repli_binary_bayes_rep",
                           "repli_binary_correspondence")
        
        df.binvars <- data.frame(binvars.raw,binvars.full.text,binvars.group)
        df.binvars <- do.call(rbind,lapply(binvars.order,function(x) {
          df.binvars[df.binvars$binvars.raw==x,]
        }))
      }
    }
    
    # Figure generation
    {
      # Figure 1: Replication success rates across 13 binary assessments for papers
      {
        # Data wrangling
        {
          # repli_binary <- repli_outcomes[c("paper_id", "claim_id",
          #                                  df.binvars$binvars.raw)]
          
          # Summarize by proportions w/ CIs
          binary.proportions <- 
            do.call(rbind,lapply(df.binvars$binvars.raw,function(binary.var){
              repli_binary_temp <- na.omit(repli_binary[c("paper_id",binary.var)])
              repli_binary_temp <- repli_binary_temp %>%
                group_by(paper_id) %>%
                mutate(weight = 1/n())
              
              n_testable_claims <- nrow(repli_binary_temp)
              p_testable_claims <- n_testable_claims/length(unique(repli_outcomes$claim_id))
              n_passed_claims <- sum(repli_binary_temp[[binary.var]])
              p_passed_temp <- binconf(n_passed_claims,n_testable_claims)
              p_passed_claims <- p_passed_temp[1]
              p_passed_CI_LB_claims <- p_passed_temp[2]
              p_passed_CI_UB_claims <- p_passed_temp[3]
              
              n_testable_papers <- length(unique((repli_binary_temp$paper_id)))
              p_testable_papers <- n_testable_papers/length(unique(repli_outcomes$paper_id))
              n_passed_papers <- sum(repli_binary_temp[[binary.var]]*repli_binary_temp$weight)
              p_passed_temp <- cw.proportion(as.integer(repli_binary_temp[[binary.var]]),
                                             repli_binary_temp$weight,repli_binary_temp$paper_id,iters)
              p_passed_papers <- p_passed_temp$point.estimate
              p_passed_CI_LB_papers <- p_passed_temp$CI.lb
              p_passed_CI_UB_papers <- p_passed_temp$CI.ub
              
              data.frame(binary.var,
                         n_testable_claims,p_testable_claims,n_passed_claims,p_passed_claims,p_passed_CI_LB_claims,p_passed_CI_UB_claims,
                         n_testable_papers,p_testable_papers,n_passed_papers,p_passed_papers,p_passed_CI_LB_papers,p_passed_CI_UB_papers
              )
            }))
          
          binary.proportions$binary.var <- ordered(binary.proportions$binary.var,
                                                   levels = binvars.raw,
                                                   labels= binvars.full.text)
          
            
          # Reorder by size
          binary.proportions <- binary.proportions[order(-binary.proportions$p_passed_papers),]
          binary.proportions$binary.var <- ordered(as.character(binary.proportions$binary.var),
                                                   levels = binary.proportions$binary.var,
                                                   labels= binary.proportions$binary.var)
          
          binary.proportions$num <- rev(1:nrow(binary.proportions))
          
          binary.proportions$n.text <- paste0("n=",format.round(binary.proportions$n_passed_papers,1),"/",binary.proportions$n_testable_papers)
          
          binary.proportions$num.claims <- rev(1:nrow(binary.proportions))
          
          binary.proportions$n.text.claims <- paste0("n=",format.round(binary.proportions$n_passed_claims,0),"/",binary.proportions$n_testable_claims)
        }
        
        # Chart generation
        {
          #ggplot(binary.proportions, aes(x=binary.var,y=PointEst)) + 
          figure_1 <- ggplot(binary.proportions, aes(x=reorder(binary.var, -p_passed_papers),y=p_passed_papers)) + 
            geom_bar(stat="identity",fill="grey90") +
            geom_text(aes(x=num,y=0.02,label=n.text),hjust=0,size=3)+
            funkyheatmap::geom_rounded_rect(
              aes(xmin=num-.45,xmax=num+.45,ymin = p_passed_CI_LB_papers, ymax = p_passed_CI_UB_papers),
              radius=unit(3, "points"),show.legend=FALSE,color="black",size=0,fill=palette_score_charts[7])+
            geom_segment(aes(x=num-.45,xend=num+.45,y = p_passed_papers, yend = p_passed_papers),color="white")+
            # geom_errorbar(aes(y=PointEst,ymin=Lower, ymax=Upper), width=.2)+
            theme_minimal()+
            scale_x_discrete(limits=rev(binary.proportions$binary.var))+
            coord_flip()+
            scale_y_continuous(expand=expansion(add = c(0, .05)),
                               labels = scales::percent_format(),
                               breaks=c(0,.25,.5,.75,1),
                               limits=c(0,1))+
            theme(legend.position = "none",
                  legend.title=element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.line = element_blank(),
            )+
            xlab("Binary success measure")+
            ylab("Proportion successfully replicated according to measure")
          
          figure_1_claims <- ggplot(binary.proportions, aes(x=reorder(binary.var, -p_passed_claims),y=p_passed_claims)) + 
            geom_bar(stat="identity",fill="grey90") +
            geom_text(aes(x=num.claims,y=0.02,label=n.text.claims),hjust=0,size=3)+
            funkyheatmap::geom_rounded_rect(
              aes(xmin=num-.45,xmax=num+.45,ymin = p_passed_CI_LB_claims, ymax = p_passed_CI_UB_claims),
              radius=unit(3, "points"),show.legend=FALSE,color="black",size=0,fill=palette_score_charts[7])+
            geom_segment(aes(x=num.claims-.45,xend=num.claims+.45,y = p_passed_claims, yend = p_passed_claims),color="white")+
            # geom_errorbar(aes(y=PointEst,ymin=Lower, ymax=Upper), width=.2)+
            theme_minimal()+
            scale_x_discrete(limits=rev(binary.proportions$binary.var))+
            coord_flip()+
            scale_y_continuous(expand=expansion(add = c(0, .05)),
                               labels = scales::percent_format(),
                               breaks=c(0,.25,.5,.75,1),
                               limits=c(0,1))+
            theme(legend.position = "none",
                  legend.title=element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.line = element_blank(),
            )+
            xlab("Binary success measure")+
            ylab("Proportion successfully replicated according to measure")
        }
      }
      
      # (Depracated) Binary counts
      if(FALSE){
        # Data wrangling
        {
          # repli_binary <- repli_outcomes[c("paper_id", "claim_id",
          #                                  df.binvars$binvars.raw)]
          # # Drop everything that doesn't have all measures
          # repli_binary <- na.omit(repli_binary)

          
          
          # Get repli binary results
          repli_binary_rowsums <- repli_binary[,3:ncol(repli_binary)]
          repli_binary_rowsums <- repli_binary_rowsums[c(binvars.raw)]
          colnames(repli_binary_rowsums) <- binvars.full.text
          
          invisible(lapply(1:ncol(repli_binary_rowsums),function(x) {
            repli_binary_rowsums[[colnames(repli_binary_rowsums[x])]] <<- as.integer(repli_binary_rowsums[[colnames(repli_binary_rowsums[x])]]) 
          }))
          
          repli_binary_rowsums$binary_sums <- rowSums(repli_binary_rowsums,na.rm = TRUE)
          
          # vars.order <- colSums(repli_binary_rowsums,na.rm=TRUE)
          # vars.order <- vars.order[1:(length(vars.order)-1)]
          # vars.order <- names(vars.order[order(-vars.order)])
          
          vars.order <- as.character(binary.proportions$binary.var) # Pulled from Fig 1
          
          repli_binary_rowsums <- repli_binary_rowsums[c(vars.order,"binary_sums")]
          
          df.counts <- do.call(rbind,lapply(0:(length(binvars.raw)),function (i) {
            n <- sum(repli_binary_rowsums$binary_sums==i,na.rm=TRUE)
            #print(paste0(i,", ",n))
            
            repli_binary_rowsums_subset <- repli_binary_rowsums[repli_binary_rowsums$binary_sums==i,]
            colsums <- t(colSums(repli_binary_rowsums_subset, na.rm=TRUE))
            df.out <- data.frame("n"=i,"total"=n, t(colSums(repli_binary_rowsums_subset, na.rm=TRUE)))
            colnames(df.out) <- c("n","total",colnames(colsums))
            df.out
            #data.frame(n)
          }))
          
          rownames(df.counts) <- df.counts$n
          df.counts$n <- df.counts$binary_sums <-  NULL
          df.counts <- as.data.frame(t(df.counts))
        }
        
        # Plot generation
        {
          bar_width <- 0.7
          chart.palette <- c(palette_score_charts[7],"grey90")
          
          
          bar.ind <- function(i.binvar,i.count){
            ggplot() + theme_nothing()+
              funkyheatmap::geom_rounded_rect(aes(xmin = (1-bar_width)/2, xmax = 1-(1-bar_width)/2,
                                                  ymin = 0, ymax = 1-(df.counts[[as.character(i.count)]][i.binvar+1]/df.counts[[as.character(i.count)]][1])),
                                              fill=chart.palette[1],radius=.1)+
              funkyheatmap::geom_rounded_rect(aes(xmin = (1-bar_width)/2, xmax = 1-(1-bar_width)/2,
                                                  ymax = 1, ymin = 1-(df.counts[[as.character(i.count)]][i.binvar+1]/df.counts[[as.character(i.count)]][1])),
                                              fill=chart.palette[2],radius=.1)
          }
          rel.label.width <- 0.3
          
          bar.row <- function(i.binvar){
            row.label <- ggplot()+theme_nothing()+
              annotate("text",x=1,y=0.5,label=rownames(df.counts)[i.binvar+1],hjust=1,vjust=0.5,fontface="bold")+
              xlim(0,1)+ylim(0,1)
            
            
            plotlist <- lapply(0:(ncol(df.counts)-1),function(i.count) {
              bar.ind(i.binvar,i.count)
              
            })
            bars <- plot_grid(plotlist = plotlist,nrow=1,align="v")
            plot_grid(row.label,bars,nrow=1,rel_widths=c(rel.label.width,1))
          }
          
          main.bar.plots <- plot_grid(plotlist= {
            lapply(1:length(binvars.raw),function(i.binvar) {
              bar.row(i.binvar)
            })
          },nrow=length(binvars.raw))
          
          countlabels.row <- function(){
            row.label <- ggplot()+theme_nothing()+
              annotate("text",x=1,y=0.5,label="",hjust=1,vjust=0.5)+
              xlim(0,1)+ylim(0,1)
            
            plotlist <- lapply(0:(ncol(df.counts)-1),function(i.count) {
              ggplot()+theme_nothing()+
                annotate("text",x=0.5,y=0.5,label=as.character(i.count),hjust=0.5,vjust=0.5,fontface="bold")+
                xlim(0,1)+ylim(0,1)
              
            })
            bars <- plot_grid(plotlist = plotlist,nrow=1,align="v")
            plot_grid(row.label,bars,nrow=1,rel_widths=c(rel.label.width,1))
          }
          
          nlabels.row <- function(){
            row.label <- ggplot()+theme_nothing()+
              #annotate("text",x=1,y=0.5,label="N studies with X\nreplication success measures",hjust=1,vjust=0.5)+
              annotate("text",x=1,y=0.5,label="",hjust=1,vjust=0.5)+
              xlim(0,1)+ylim(0,1)
            
            plotlist <- lapply(1:(ncol(df.counts)),function(i) {
              ggplot()+theme_nothing()+
                annotate("text",x=0.5,y=0.5,label=paste0("n=",as.character(df.counts[1,i])),hjust=0.5,vjust=0.5)+
                xlim(0,1)+ylim(0,1)
              
            })
            bars <- plot_grid(plotlist = plotlist,nrow=1,align="v")
            plot_grid(row.label,bars,nrow=1,rel_widths=c(rel.label.width,1))
          }
          title.row <- function(){
            row.label <- ggplot()+theme_nothing()+
              annotate("text",x=0.5,y=0.5,label="",hjust=0.5,vjust=0.5)+
              xlim(0,1)+ylim(0,1)
            
            title <- ggplot()+theme_nothing()+
              annotate("text",x=0.5,y=0.5,label="Number of measures indicating replication success",hjust=0.5,vjust=0.5,fontface="bold")+
              xlim(0,1)+ylim(0,1)
            plot_grid(row.label,title,nrow=1,rel_widths=c(rel.label.width,1))
          }
          
          figure_2 <- plot_grid(title.row(),countlabels.row(),nlabels.row(),main.bar.plots,ncol=1,rel_heights = c(1,1,1,12))
          #figure_2
        }

      }

      # Figure 2: Correlation matrix among binary assessments of replication success across papers
      {
        # Data wrangling
        {
          # repli_binary <- repli_outcomes[c("paper_id", "claim_id",
          #                                  df.binvars$binvars.raw)]

          # Summarize by proportions w/ CIs
          repli_binary_binaries <- repli_binary[,3:ncol(repli_binary)]
          repli_binary_binaries <- repli_binary_binaries[c(binvars.order)]
          colnames(repli_binary_binaries) <- df.binvars$binvars.full.text
          
          # Generate a results matrix
          repli_binary_cors <- matrix(NA,nrow=ncol(repli_binary_binaries),ncol=ncol(repli_binary_binaries))
          rownames(repli_binary_cors) <- colnames (repli_binary_cors) <- colnames(repli_binary_binaries)
          
          get_wt_corr <- function(i.x,i.y){
            df.cors <- repli_binary_binaries[,c(i.x,i.y)]
            df.cors$paper_id <- repli_binary$paper_id
            df.cors <- na.omit(df.cors)
            df.cors <- df.cors %>%
              group_by(paper_id) %>%
              mutate(weight = 1/n())
            
            weightedCorr(as.integer(unlist(df.cors[,1])),as.integer(unlist(df.cors[,2])),weights=df.cors$weight,method="Pearson")
          }
          
          # Get pairwise complete weighted correlations
          for(i in 1:nrow(repli_binary_cors)){
            for(j in 1:(i)){
              if(i==j){
                repli_binary_cors[i,j] <- 1
              } else{
                repli_binary_cors[j,i] <- get_wt_corr(i,j)
                repli_binary_cors[i,j] <- NA
              }
            }
          }
          
          binvars.x <- rownames(repli_binary_cors)
          binvars.y <- rownames(repli_binary_cors)
          df.chart <- expand.grid(binvars.x=binvars.x, binvars.y=binvars.y)
          df.chart$corr <-c(repli_binary_cors)
          df.chart$upper_text_label <- str_sub(ifelse(is.na(df.chart$corr) | df.chart$binvars.x==df.chart$binvars.y,"",
                                                      format.round(df.chart$corr,2)), 2, -1)
          df.chart$corrs.all <- ifelse(is.na(df.chart$corr) | df.chart$binvars.x==df.chart$binvars.y,NA,
                                       df.chart$corr)
          
        }
        
        # Chart generation
        {
          chart.palette <- palette_score_charts[7]
          
          top <- ggplot(df.chart, aes(x=binvars.x, y=binvars.y, fill= corr)) + 
            geom_tile()+
            geom_text(aes(x=binvars.y, y=binvars.x, label=upper_text_label),size=3,color="grey30")+
            scale_x_discrete(position = "top") +
            scale_y_discrete(limits=rev)+
            theme_minimal()+
            theme(
              plot.title = element_blank(),
              axis.title = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0,color="black"),
              axis.text.y = element_text(color="black"),
              legend.key.height= unit(1, 'cm'),
              panel.grid = element_blank(),
              legend.position="none"
            ) +
            scale_fill_gradient(low="white", high=chart.palette[1], limits=c(0, 1),na.value="white")
          
          df.chart$group <- "Distribution of\n all 144 correlations"
          bottom <- ggplot(df.chart, aes(x=corrs.all,y=group,fill=..x..)) + 
            #geom_density(fill=palette_score_charts[6])+
            geom_density_ridges_gradient(linetype=0)+
            scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.1),expand=c(0,0))+
            scale_y_discrete(expand=c(0,0))+
            scale_fill_gradient( limits=c(0, 1),low="white", high=chart.palette[1])+
            geom_vline(xintercept=c(0,1))+
            #ylab("Distribution of\n all 144 correlations")+
            # scale_y_discrete(limits=rev)+
            theme_minimal()+
            theme(
              plot.title = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=1 ,size=8),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0,size=6),
              legend.key.height= unit(1, 'cm'),
              panel.grid = element_blank(),
              legend.position="none"
            )
          
          #bottom_row <- plot_grid(ggplot()+theme_nothing(),figure_3_bottom,ggplot()+theme_nothing(),nrow=1,rel_widths = c(1,2,1))
          #figure_3 <- plot_grid(figure_3_top,bottom_row,ncol=1,rel_heights = c(5,1))
          
          figure_2 <- plot_grid(top,bottom,ncol=1,rel_heights = c(5,1),align = "v")
        }
        
      }
      
      # Figure 3: Scatterplot of Pearson’s R effect sizes for original (x-axis) and replication (y-axis) findings by papers.
      {
        # Data wrangling
        {
          all_effects <- repli_outcomes %>% 
            filter(!is_covid) %>% 
            filter(repli_version_of_record) %>% 
            select(paper_id,report_id, claim_id, repli_pattern_criteria_met, success = repli_score_criteria_met,
                   repli_conv_r, repli_conv_r_lb, repli_conv_r_ub) %>% 
            left_join(
              orig_outcomes %>% 
                select(claim_id, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub) %>% 
                distinct(),
              by = "claim_id"
            ) %>% 
            drop_na(repli_conv_r) %>% 
            drop_na(orig_conv_r) %>% 
            mutate(across(contains("orig"), abs)) %>% 
            mutate(
              across(
                contains("repli"),
                function(x) ifelse(repli_pattern_criteria_met, abs(x), -1*abs(x))
              )
            ) %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
        }
        
        # Chart generation
        {
          #chart.palette <- c("tomato3", "deepskyblue4")
          chart.palette <- c(palette_score_charts[2], palette_score_charts[1])
          all_effects$weight.alpha <- all_effects$weight*1
          all_effects$weight.size <- all_effects$weight*1
        
          figure_3 <- 
            ggplot(data = all_effects, aes(x = orig_conv_r, y = repli_conv_r, color = success,weight=weight)) +
            
            # geom_point(aes(alpha = weight),size=3) +
            # scale_alpha_continuous(range=c(.1,.6),guide="none")+
              
            geom_point(aes(size = weight),alpha=.6, stroke=NA) +
            scale_size_continuous(range=c(.5,3),guide="none")+

            # geom_point(aes(alpha = weight,size = weight)) +
            # scale_size_continuous(range=c(.5,3),guide="none")+
            # scale_alpha_continuous(range=c(.1,.6),guide="none")+
              
            geom_rug() +
            scale_color_manual(labels = c("Failed", "Successful"),
                               values = chart.palette) +
            annotate("segment",x=0,y=0,xend=1,yend=1, linetype = 2, color = "slategray") +
            annotate("segment",x=0,y=0,xend=1,yend=0, linetype = "solid", color = "black") +
            annotate("segment",x=0,y=-.55,xend=0,yend=1, linetype = "solid", color = "black") +
            xlim(0, 1) +
            ylim(-0.55, 1) +
            labs(
              x = "Original",
              y = "Replication",
              color = "",
              title = ""
            ) +
            theme_minimal()+
            theme(
              plot.title = element_blank(),
              axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 14),
              legend.text = element_text(size = 16),
              panel.grid = element_blank(),
              legend.position = "bottom"
            )+
            guides(color = guide_legend(override.aes = list(linetype = 0,size=6)))+
            geom_xsidedensity(alpha = .6,aes(fill=success),show_guide=FALSE,color="black")+
            geom_ysidedensity(alpha = .6,aes(fill=success),show_guide=FALSE,color="black")+
            theme(
              ggside.panel.border = element_blank(),
              ggside.panel.grid = element_blank(),
              ggside.panel.background = element_blank(),
              ggside.axis.text = element_blank(),
              ggside.axis.ticks = element_blank(),
              ggside.panel.scale = .3
            )+
            scale_fill_manual(labels = c("Failed", "Successful"),
                               values = chart.palette)
        }
      }
      
      # Figure 4: Scatterplot of Pearson’s r effect sizes for original and replication outcomes for new data (left) and secondary data (right) replication attempts
      {
        # Data wrangling
        {
          all_effects_nd <- repli_outcomes %>% 
            filter(!is_covid) %>% 
            filter(repli_version_of_record) %>% 
            filter(repli_type=="new data") %>% 
            select(paper_id,report_id, claim_id, repli_pattern_criteria_met, success = repli_score_criteria_met,
                   repli_conv_r, repli_conv_r_lb, repli_conv_r_ub) %>% 
            left_join(
              orig_outcomes %>% 
                select(claim_id, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub) %>% 
                distinct(),
              by = "claim_id"
            ) %>% 
            drop_na(repli_conv_r) %>% 
            drop_na(orig_conv_r) %>% 
            mutate(across(contains("orig"), abs)) %>% 
            mutate(
              across(
                contains("repli"),
                function(x) ifelse(repli_pattern_criteria_met, abs(x), -1*abs(x))
              )
            ) %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
          
          all_effects_sd <- repli_outcomes %>% 
            filter(!is_covid) %>% 
            filter(repli_version_of_record) %>% 
            filter(repli_type=="secondary data") %>% 
            select(paper_id,report_id, claim_id, repli_pattern_criteria_met, success = repli_score_criteria_met,
                   repli_conv_r, repli_conv_r_lb, repli_conv_r_ub) %>% 
            left_join(
              orig_outcomes %>% 
                select(claim_id, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub) %>% 
                distinct(),
              by = "claim_id"
            ) %>% 
            drop_na(repli_conv_r) %>% 
            drop_na(orig_conv_r) %>% 
            mutate(across(contains("orig"), abs)) %>% 
            mutate(
              across(
                contains("repli"),
                function(x) ifelse(repli_pattern_criteria_met, abs(x), -1*abs(x))
              )
            ) %>%
            group_by(paper_id) %>%
            mutate(weight = 1/n()) %>%
            ungroup()
        }
        
        # Chart generation
        {
          #chart.palette <- c("tomato3", "deepskyblue4")
          chart.palette <- c(palette_score_charts[2], palette_score_charts[1])
          all_effects_sd$weight.alpha <- all_effects_sd$weight*.6
          all_effects_nd$weight.alpha <- all_effects_nd$weight*.6
          
          panel_left <- ggplot(data = all_effects_nd, aes(x = orig_conv_r, y = repli_conv_r, color = success,weight=weight)) +
            #geom_point(alpha = 0.6, size = 3) +
            # geom_point(aes(alpha = weight), size = 3) +
            # scale_alpha_continuous(range=c(.1,.6),guide="none")+
            
            # geom_point(aes(alpha = weight,size = weight)) +
            # scale_size_continuous(range=c(.5,3),guide="none")+
            # scale_alpha_continuous(range=c(.1,.6),guide="none")+
            
            geom_point(aes(size = weight),alpha=.6, stroke=NA) +
            scale_size_continuous(range=c(.5,3),guide="none")+
            
            geom_rug() +
            scale_color_manual(labels = c("Failed", "Successful"),
                               values = chart.palette) +
            annotate("segment",x=0,y=0,xend=1,yend=1, linetype = 2, color = "slategray") +
            annotate("segment",x=0,y=0,xend=1,yend=0, linetype = "solid", color = "black") +
            annotate("segment",x=0,y=-.55,xend=0,yend=1, linetype = "solid", color = "black") +
            xlim(0, 1) +
            ylim(-0.55, 1) +
            scale_x_continuous(breaks=seq(0,1,.25),labels=c("0",".25",".5",".75",1))+
            labs(
              x = "Original",
              y = "Replication",
              color = "",
              title = "New data"
            ) +
            theme_minimal()+
            theme(
              plot.title = element_text(size = 16),
              axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 14),
              legend.text = element_text(size = 16),
              panel.grid = element_blank()
            )+
            guides(color = guide_legend(override.aes = list(linetype = 0)))+
            geom_xsidedensity(alpha = .6,aes(fill=success),show_guide=FALSE,color="black")+
            geom_ysidedensity(alpha = .6,aes(fill=success),show_guide=FALSE,color="black")+
            theme(
              ggside.panel.border = element_blank(),
              ggside.panel.grid = element_blank(),
              ggside.panel.background = element_blank(),
              ggside.axis.text = element_blank(),
              ggside.axis.ticks = element_blank(),
              ggside.panel.scale = .3,
              legend.position = "none"
            )+
            scale_fill_manual(labels = c("Failed", "Successful"),
                              values = chart.palette)
          
          panel_right <- ggplot(data = all_effects_sd, aes(x = orig_conv_r, y = repli_conv_r, color = success,weight=weight)) +
            #geom_point(alpha = 0.6, size = 3) +
            # geom_point(aes(alpha = weight), size = 3) +
            # scale_alpha_continuous(range=c(.1,.6),guide="none")+
            
            # geom_point(aes(alpha = weight,size = weight)) +
            # scale_size_continuous(range=c(.5,3),guide="none")+
            # scale_alpha_continuous(range=c(.1,.6),guide="none")+
            
            geom_point(aes(size = weight),alpha=.6, stroke=NA) +
            scale_size_continuous(range=c(.5,3),guide="none")+
            
            geom_rug() +
            scale_color_manual(labels = c("Failed", "Successful"),
                               values = chart.palette) +
            annotate("segment",x=0,y=0,xend=1,yend=1, linetype = 2, color = "slategray") +
            annotate("segment",x=0,y=0,xend=1,yend=0, linetype = "solid", color = "black") +
            annotate("segment",x=0,y=-.55,xend=0,yend=1, linetype = "solid", color = "black") +
            xlim(0, 1) +
            ylim(-0.55, 1) +
            scale_x_continuous(breaks=seq(0,1,.25),labels=c("0",".25",".5",".75",1))+
            labs(
              x = "Original",
              y = "Replication",
              color = "",
              title = "Secondary data"
            ) +
            theme_minimal()+
            theme(
              plot.title = element_text(size = 16),
              axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 14),
              legend.text = element_text(size = 16),
              panel.grid = element_blank()
            )+
            guides(color = guide_legend(override.aes = list(linetype = 0)))+
            geom_xsidedensity(alpha = .6,aes(fill=success),show_guide=FALSE,color="black")+
            geom_ysidedensity(alpha = .6,aes(fill=success),show_guide=FALSE,color="black")+
            theme(
              ggside.panel.border = element_blank(),
              ggside.panel.grid = element_blank(),
              ggside.panel.background = element_blank(),
              ggside.axis.text = element_blank(),
              ggside.axis.ticks = element_blank(),
              ggside.panel.scale = .3,
              legend.position = "none"
            )+
            scale_fill_manual(labels = c("Failed", "Successful"),
                              values = chart.palette)
          
          figure_4 <- 
            plot_grid(plot_grid(panel_left,panel_right),
                    get_legend(ggplot(data = all_effects_sd, aes(x = orig_conv_r, y = repli_conv_r, color = success,weight=weight)) +
                                 geom_point(aes(alpha = weight), size = 3) +
                                 scale_alpha_continuous(range=c(.1,.6),guide="none")+
                                 scale_color_manual(labels = c("Failed", "Successful"),
                                                    values = chart.palette)+
                                 theme_minimal()+
                                 
                                 theme(
                                   legend.title=element_blank(),
                                   panel.grid = element_blank(),
                                   legend.direction = "horizontal",
                                   legend.text = element_text(size = 16)
                                 )
                               ),nrow=2,rel_heights = c(10,1))

        }
      }
      
      # # (depracated) Figure 4: Distribution of Pearson’s R effect sizes across papers for original and replication findings by discipline
      # {
      #   # Data wrangling
      #   {
      #     all_effects <- repli_outcomes %>% 
      #       filter(!is_covid) %>% 
      #       filter(repli_version_of_record) %>% 
      #       mutate(repli_conv_r = ifelse(repli_pattern_criteria_met, repli_conv_r, -1*repli_conv_r)) %>%
      #       select(claim_id,repli_conv_r) %>% 
      #       left_join(
      #         orig_outcomes %>% 
      #           select(paper_id,claim_id, orig_conv_r) %>% 
      #           distinct(),
      #         by = "claim_id"
      #       ) %>% 
      #       drop_na(repli_conv_r) %>% 
      #       drop_na(orig_conv_r)%>%
      #       group_by(paper_id) %>%
      #       mutate(weight=1/n()) %>%
      #       mutate(across(c(orig_conv_r, repli_conv_r), abs))
      #     
      #        
      #       
      #     
      #     all_effects <- merge(all_effects,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
      #     # all_effects$field <- str_to_title(all_effects$COS_pub_category)
      #     # all_effects$field <- str_to_title(all_effects$COS_pub_category)
      #     # all_effects$field <- str_replace_all(all_effects$field," ","\n")
      #     # all_effects$field <- str_replace_all(all_effects$field,"And","and")
      #     # group_order <- unique(all_effects$field)
      #     all_effects$field <- ordered(all_effects$COS_pub_category,labels=fields.abbreviated,
      #                                  levels=fields.raw)
      #     
      #     all_effects$pub_year <- ordered(all_effects$pub_year,
      #                                     levels=c(min(all_effects$pub_year):max(all_effects$pub_year)),
      #                                     labels=c(min(all_effects$pub_year):max(all_effects$pub_year)))
      #     
      #     # Create weighting scheme
      #     all_effects <- all_effects %>%
      #       group_by(paper_id) %>%
      #       mutate(paper_count = n(),
      #              weight = 1/n())
      #     
      #     # Convert to long
      #     all_effects <- all_effects %>%
      #       pivot_longer(cols=c(repli_conv_r,orig_conv_r))
      #     
      #     all_effects$name <- ordered(all_effects$name,
      #                                 levels=c("orig_conv_r","repli_conv_r"),
      #                                 labels=c("Original effect size","Replication effect size"))
      #     
      #   }
      #   # Plot
      #   {
      #     
      #     chart.palette <- c(palette_score_charts[1], palette_score_charts[5])
      #     alpha.level <- .6
      #     figure_4 <-
      #      ggplot(data=all_effects,aes(x=value,y=field,fill=name,weight=weight))+
      #       geom_density_ridges(alpha=alpha.level,scale = 0.9) +
      #       theme_minimal()+
      #       theme(legend.position = "bottom",
      #             legend.title=element_blank(),
      #             panel.border = element_blank(),
      #             panel.grid = element_blank(),
      #             axis.title=element_blank(),
      #             axis.text.y=element_text(vjust=0,face = "bold",size=12,color="black")
      #             #axis.text = element_blank(),
      #             #axis.line = element_blank(),
      #             #axis.ticks = element_blank()
      #       ) +
      #       scale_x_continuous(expand=c(0,0))+
      #       scale_fill_manual(values=chart.palette)+
      #       xlim(0,1)+
      #       geom_vline(aes(xintercept=0),linetype=1,color="grey90") +
      #       geom_vline(aes(xintercept=1),linetype=1,color="grey90")
      #   }
      # }
      # 
      # # (depracated) Figure 5: Distribution of Pearson’s R effect sizes across papers for original and replication findings by publication year
      # {
      #   # Data wrangling
      #   {
      #     all_effects <- repli_outcomes %>% 
      #       filter(!is_covid) %>% 
      #       filter(repli_version_of_record) %>% 
      #       mutate(repli_conv_r = ifelse(repli_pattern_criteria_met, repli_conv_r, -1*repli_conv_r)) %>%
      #       select(claim_id,repli_conv_r) %>% 
      #       left_join(
      #         orig_outcomes %>% 
      #           select(paper_id,claim_id, orig_conv_r) %>% 
      #           distinct(),
      #         by = "claim_id"
      #       ) %>% 
      #       drop_na(repli_conv_r) %>% 
      #       drop_na(orig_conv_r)%>%
      #       group_by(paper_id) %>%
      #       mutate(weight=1/n())%>%
      #       mutate(across(c(orig_conv_r, repli_conv_r), abs))
      #     
      #     all_effects <- merge(all_effects,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
      #     all_effects$field <- str_to_title(all_effects$COS_pub_category)
      #     all_effects$field <- str_to_title(all_effects$COS_pub_category)
      #     all_effects$field <- str_replace_all(all_effects$field," ","\n")
      #     all_effects$field <- str_replace_all(all_effects$field,"And","and")
      #     group_order <- unique(all_effects$field)
      #     all_effects$field <- ordered(all_effects$field,labels=group_order,levels=group_order)
      #     
      #     all_effects$pub_year <- ordered(all_effects$pub_year,
      #                                     levels=c(min(all_effects$pub_year):max(all_effects$pub_year)),
      #                                     labels=c(min(all_effects$pub_year):max(all_effects$pub_year)))
      #     
      #     # Create weighting scheme
      #     all_effects <- all_effects %>%
      #       group_by(paper_id) %>%
      #       mutate(paper_count = n(),
      #              weight = 1/n())
      #     
      #     # Convert to long
      #     all_effects <- all_effects %>%
      #       pivot_longer(cols=c(repli_conv_r,orig_conv_r))
      #     
      #     all_effects$name <- ordered(all_effects$name,
      #                                 levels=c("orig_conv_r","repli_conv_r"),
      #                                 labels=c("Original effect size","Replication effect size"))
      #     
      #     
      #   }
      #   # Plot
      #   {
      #     
      #     chart.palette <- c(palette_score_charts[1], palette_score_charts[5])
      #     alpha.level <- .6
      #     
      #     figure_5 <- ggplot(data=all_effects,aes(x=value,y=pub_year,fill=name,weight=weight))+
      #         geom_density_ridges(alpha=alpha.level,scale = 0.9)+
      #         theme_minimal()+
      #         theme(legend.position = "bottom",
      #               legend.title=element_blank(),
      #               panel.border = element_blank(),
      #               panel.grid = element_blank(),
      #               axis.title=element_blank(),
      #               axis.text.y=element_text(vjust=0,face = "bold",size=12,color="black")
      #               #axis.text = element_blank(),
      #               #axis.line = element_blank(),
      #               #axis.ticks = element_blank()
      #         ) +
      #       scale_x_continuous(expand=c(0,0))+
      #       scale_fill_manual(values=chart.palette)+
      #       xlim(0,1)+
      #       geom_vline(aes(xintercept=0),linetype=1,color="grey90") +
      #       geom_vline(aes(xintercept=1),linetype=1,color="grey90")
      #     }
      # }
      
      # Supplement Fig 1
      {
        supp_figure_1 <- repli_outcomes %>% 
          filter(!is_covid & repli_type != "original and secondary data") %>% # exclude covid and hybrids 
          filter(is_manylabs == "not manylabs" | manylabs_type != "aggregation") %>% 
          group_by(claim_id, rr_id) %>% 
          arrange(desc(repli_sample_size_value)) %>% # exclude stage 1 when stage 2 is available
          slice(1) %>% 
          ungroup() %>% 
          mutate(p2 = nchar(rr_id) > 4) %>%
          mutate(type = select(., c(p2, repli_type)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>% 
          select(report_id, type, power = repli_power_for_75_effect) %>% 
          mutate(power = round(power*100, 0)) %>% 
          drop_na() %>% 
          ggplot(aes(x = power, fill = type)) +
          geom_density(alpha = 0.5) +
          scale_fill_manual(values = c("tomato", "deepskyblue", "tomato4", "deepskyblue4"), 
                            labels = c("P1 new data", "P1 secondary data", "P2 new data", "P2 secondary data")) +
          labs(
            x = "Power (%)",
            y = "",
            fill = ""
          ) +
          theme_light() +
          theme(
            axis.text.y = element_blank(),
            legend.position = "bottom",
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks.y = element_blank()
          )
      }
      

    }
    
    # Export
    {
      return(list(
        "figure_1"=figure_1,
        "figure_2"=figure_2,
        "figure_3"=figure_3,
        "figure_4"=figure_4,
        "supp_figure_1"=supp_figure_1))
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
    library(wCorr)
    library(corrplot)
    library(cowplot)
    library(ggridges) #note: using the github version, as the CRAN hasn't been pushed to get the weights functionality
    library(ggside)
    library(weights)
    
    drive_auth(Sys.getenv("google_oauth_email"))
    #drive_deauth()
    # Common functions
    source(file="Analysis/common functions.R")
  }
  
  # Load data
  objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata",
                       "status","all_rr_attempts","repli_binary","publications",
                       "non_significant_bushels","rr_sourced")
  for(i in 1:length(objects_to_load)){
    assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
    #save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
  }
  
  # Pull paper to find what tags are in paper
  paper_text <- drive_read_string(file=googledrive::as_id("1mauNAwu0eZfvh5-5p44wnKz8NQL-5Tm_bAyZNseTONo"),
                                  type = "text/plain",encoding="UTF-8")  %>%
    strsplit(split = "(\r\n|\r|\n)") %>%
    .[[1]]
  paper_text <- paste0(paper_text,collapse="  ")

  # Pull paper to find what tags are calculated
  tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
  tags <- tags[tags!=""]
  tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)

  # Generate stats
  results_tagged_stats <- tagged_stats(iters = 1000,
                                       repli_outcomes=repli_outcomes,
                                       orig_outcomes=orig_outcomes,
                                       paper_metadata=paper_metadata,
                                       all_rr_attempts=all_rr_attempts,
                                       repli_binary=repli_binary,
                                       status=status,
                                       non_significant_bushels=non_significant_bushels,
                                       rr_sourced=rr_sourced,
                                       publications=publications)

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
  ss <- "https://docs.google.com/spreadsheets/d/1uaR6vYAMVywk1liQeKRyRCDQcs--teFpRsWzHrvfg3s"
  range_delete(ss,range="A:H")
  range_write(ss,data = data.frame(tags=paste0("{",tags,"}"),values_text), range = "A1",col_names=FALSE)

  # Generate figures
  if (0==1){
    generated_figures <- figures(repli_outcomes,orig_outcomes,paper_metadata,repli_binary,iters=1000)
    
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 1.png",
      plot = generated_figures$figure_1,
      width = 3000,height = 1000,units = "px",bg="white"
    )
    # ggsave(
    #   "Analysis/Paper 5/Code and data/Figures/figure 1 claims.png",
    #   plot = generated_figures$figure_1_claims,
    #   width = 3000,height = 1000,units = "px",bg="white"
    # )
    # ggsave(
    #   "Analysis/Paper 5/Code and data/Figures/figure 2.png",
    #   plot = generated_figures$figure_2,
    #   width = 2800,height = 1300,units = "px",bg="white"
    # )
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 2.png",
      plot = generated_figures$figure_2,
      width = 2000,height = 2000,units = "px",bg="white"
    )
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 3.png",
      plot = generated_figures$figure_3,
      width = 2000,height = 2000,units = "px",bg="white"
    )
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 4.png",
      plot = generated_figures$figure_4,
      width = 2000,height = 1200,units = "px",bg="white"
    )
    # ggsave(
    #   "Analysis/Paper 5/Code and data/Figures/figure 5.png",
    #   plot = generated_figures$figure_5,
    #   width = 2000,height = 2000,units = "px",bg="white"
    # )
  }
  
}
