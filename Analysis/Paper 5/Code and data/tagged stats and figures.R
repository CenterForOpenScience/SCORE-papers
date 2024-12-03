
# Functions (also readable by external sources)
{
  # Create an object that contains the tagged stats
  tagged_stats <- function(iters = 100,repli_outcomes,orig_outcomes,paper_metadata,all_rr_attempts){
  
    # Data preparation
    {
      # Temporary for convenience
      objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata","status","all_rr_attempts")
      for(i in 1:length(objects_to_load)){
        assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
      }
      
      # Trim out non-version of record lines
      repli_outcomes_orig <- repli_outcomes
      
      repli_outcomes <- repli_outcomes[repli_outcomes$repli_version_of_record,]
      
      paper_metadata <- paper_metadata[c("paper_id","publication_standard","COS_pub_category","pub_year")]
      paper_metadata$field <- str_to_title(paper_metadata$COS_pub_category)
      paper_metadata$field <- str_replace(paper_metadata$field,"And","and")
      
      orig_outcomes <- merge(orig_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
      
      repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                     by="claim_id",all.x=TRUE,all.y=FALSE)
      orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
      repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
      repli_outcomes_merged <- repli_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
      
      #paper_metadata$field <- str_to_title(paper_metadata$COS_pub_category)
      
    }
    
    # Stats
    {
      # Tables
      {
        # Table 2
        {
          fields.order <- c("Psychology and Health","Business","Sociology and Criminology",
                            "Economics and Finance","Political Science","Education")
          
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
          
          r1.data <- status %>% filter(p1_delivery)
          r2.data <- status %>% filter(RR)
          r3.data <- status %>% filter(bushel)
          r4.data <- status %>% filter(RR & !bushel)
          r5.data <- all_rr_attempts  %>%
                             filter(str_detect(type, "Replication")) %>%
                             select(paper_id) %>% 
                             distinct() %>% 
                             semi_join(status %>% filter(RR), by = "paper_id")
          r6.data <- repli_outcomes %>% 
                             filter(repli_type != "original and secondary data") %>% 
                             filter(!is_covid) %>% 
                             select(paper_id) %>% 
                             distinct() %>% 
                             semi_join(status %>% filter(RR), by = "paper_id")
          r7.data <- repli_outcomes %>%
                             filter(repli_type != "original and secondary data") %>% 
                             filter(!is_covid) %>% 
                             semi_join(status %>% filter(RR), by = "paper_id") %>% 
                             filter(is.na(manylabs_type) | manylabs_type != "aggregation") %>% 
                             select(paper_id, claim_id, rr_id) %>% 
                             distinct() %>% 
                             select(paper_id)
          r8.data <- repli_outcomes %>%
                             filter(repli_type != "original and secondary data") %>% 
                             filter(!is_covid) %>% 
                             semi_join(status %>% filter(RR), by = "paper_id") %>% 
                             select(paper_id, claim_id) %>% 
                             distinct() %>% 
                             select(paper_id)
          
          for (i in 1:8){
            assign(paste0("r",i),format.row(get(paste0("r",i,".data"))))
            assign(paste0("r",i,".p"),p.row(get(paste0("r",i,".data"))))
            assign(paste0("r",i,".p.formatted"),
                   paste0(format.round(100*p.row(get(paste0("r",i,".data"))),0),"%")
                   )
          }
          
          table_2 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8)
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
          fields.order <- c("Psychology And Health","Business","Sociology And Criminology",
                            "Economics And Finance","Political Science","Education")
          
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
          
          r1 <- format.row(status %>% filter(p1_delivery))
          r2 <- format.row(status %>% filter(RR))
          r3 <- format.row(status %>% filter(bushel))
          r4 <- format.row(status %>% filter(RR & !bushel))
          r5 <- format.row(all_rr_attempts  %>%
                             filter(str_detect(type, "Replication")) %>%
                             select(paper_id) %>% 
                             distinct() %>% 
                             semi_join(status %>% filter(RR), by = "paper_id"))
          r6 <- format.row(repli_outcomes %>% 
                             filter(repli_type != "original and secondary data") %>% 
                             filter(!is_covid) %>% 
                             select(paper_id) %>% 
                             distinct() %>% 
                             semi_join(status %>% filter(RR), by = "paper_id") )
          r7 <- format.row(repli_outcomes %>%
                             filter(repli_type != "original and secondary data") %>% 
                             filter(!is_covid) %>% 
                             semi_join(status %>% filter(RR), by = "paper_id") %>% 
                             filter(is.na(manylabs_type) | manylabs_type != "aggregation") %>% 
                             select(paper_id, claim_id, rr_id) %>% 
                             distinct() %>% 
                             select(paper_id) )
          
          
          r8 <- format.row(repli_outcomes %>%
                             filter(repli_type != "original and secondary data") %>% 
                             filter(!is_covid) %>% 
                             semi_join(status %>% filter(RR), by = "paper_id") %>% 
                             select(paper_id, claim_id) %>% 
                             distinct() %>% 
                             select(paper_id))
          
          table_3 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8)
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
            c2.numerator <- sum(as.numeric(data$success))
            c2.numerator.formatted <- format.round(c2.numerator,0)
            c2.denom <- length(unique(data$claim_id))
            c2.denom.formatted <- format.round(c2.denom,0)
            c2.p <- sum(as.numeric(data$success))/length(unique(data$claim_id))
            c2.p.formatted <- paste0(format.round(100*c2.p,1),"%")
            c2 <- paste0(c2.numerator.formatted," / ",c2.denom.formatted," (",
                         c2.p.formatted,")")
            data.frame(field,c1,c2,
                       c1.numerator,c1.numerator.formatted,
                       c1.denom,c1.denom.formatted,
                       c1.p,c1.p.formatted,
                       c2.numerator,c2.numerator.formatted,
                       c2.denom,c2.denom.formatted,
                       c2.p,c2.p.formatted)
          }))
          
          #table_5 <- table_5[order(table_5$field),]

          table_5_numerator <- table_5[c("c1.numerator","c2.numerator")]
          table_5_numerator_formatted <- table_5[c("c1.numerator.formatted","c2.numerator.formatted")]
          table_5_denom <- table_5[c("c1.denom","c2.denom")]
          table_5_denom_formatted <- table_5[c("c1.denom.formatted","c2.denom.formatted")]
          table_5_p <- table_5[c("c1.p","c2.p")]
          table_5_p_formatted <- table_5[c("c1.p.formatted","c2.p.formatted")]

          table_5 <- table_5[c("c1","c2")]
          
          for (row in 1:nrow(table_5)){
            for (col in 1:ncol(table_5)){
              assign(paste0("table_5_",row,"_",col),
                     table_5[row,col])
              assign(paste0("table_5_numerator_",row,"_",col),
                     table_5_numerator[row,col])
              assign(paste0("table_5_numerator_formatted_",row,"_",col),
                     table_5_numerator_formatted[row,col])
              assign(paste0("table_5_denom_",row,"_",col),
                     table_5_denom[row,col])
              assign(paste0("table_5_numerator_formatted_",row,"_",col),
                     table_5_denom_formatted[row,col])
              assign(paste0("table_5_p_",row,"_",col),
                     table_5_p[row,col])
              assign(paste0("table_5_p_formatted_",row,"_",col),
                     table_5_p_formatted[row,col])
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
        
        

      }
      
      # Results: Evaluating replication effect size against original effect size
      {
        table_4_median_R_papers_neg_ratio <- 
          paste0(format.round(100*(table_4_median_4_3-table_4_median_4_4)/table_4_median_4_3,1),"%")
        
        table_4_median_R_claims_neg_ratio <- 
          paste0(format.round(100*(table_4_median_4_1-table_4_median_4_2)/table_4_median_4_1,1),"%")
      }
      
      # Results: Original and replication effects by discipline
      {
        table_5_min_p_c1 <- paste0(format.round(min(100*table_5_p[,1]),1),"%")
        table_5_max_p_c1 <- paste0(format.round(max(100*table_5_p[,1]),1),"%")
        table_5_median_p_c1 <- paste0(format.round(median(100*table_5_p[,1]),1),"%")
        table_5_min_p_c2 <- paste0(format.round(min(100*table_5_p[,2]),1),"%")
        table_5_max_p_c2 <- paste0(format.round(max(100*table_5_p[,2]),1),"%")
        table_5_median_p_c2 <- paste0(format.round(median(100*table_5_p[,2]),1),"%")
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

      }
      
      # Discussion
      {
        p_repli_50_power_for_100_effect <- paste0(format.round(100*
                                                                 sum(repli_outcomes$power_for_effect_size=="50% for 100%",na.rm = TRUE)/
                                                                 sum(!is.na(repli_outcomes$power_for_effect_size),na.rm = TRUE),
                                                               digits=1),"%")
        
        p_repli_90_power_for_50_effect <- paste0(format.round(100*
                                                                sum(repli_outcomes$power_for_effect_size=="90% for 50%",na.rm = TRUE)/
                                                                sum(!is.na(repli_outcomes$power_for_effect_size),na.rm = TRUE),
                                                              digits=1),"%")
        
        p_repli_90_power_for_75_effect <- paste0(format.round(100*
                                                                sum(repli_outcomes$power_for_effect_size=="90% for 75%",na.rm = TRUE)/
                                                                sum(!is.na(repli_outcomes$power_for_effect_size),na.rm = TRUE),
                                                              digits=1),"%")
      }
      
      # Archive
      if(FALSE){
        p_repli_stat_sig_claims_same_dir_wtd <- 
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_p_value <= 0.05 & x$repli_pattern_criteria_met==TRUE,
                                          w=x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_effect_size_smaller_v_orig_business <- "Pending effect size decisions"
        
        n_effect_size_smaller_v_orig_business <- "Pending effect size decisions"
        
        p_effect_size_smaller_v_orig_econ <- "Pending effect size decisions"
        
        n_effect_size_smaller_v_orig_econ <- "Pending effect size decisions"
        
        p_effect_size_smaller_v_orig_edu <- "Pending effect size decisions"
        
        n_effect_size_smaller_v_orig_edu <- "Pending effect size decisions"
        
        p_effect_size_smaller_v_orig_polisci <- "Pending effect size decisions"
        
        n_effect_size_smaller_v_orig_polisci <- "Pending effect size decisions"
        
        p_effect_size_smaller_v_orig_psych <- "Pending effect size decisions"
        
        n_effect_size_smaller_v_orig_psych <- "Pending effect size decisions"
        
        p_effect_size_smaller_v_orig_soc <- "Pending effect size decisions"
        
        n_effect_size_smaller_v_orig_soc <- "Pending effect size decisions"
        
        n_journals_business <- length(unique(repli_outcomes_merged[repli_outcomes_merged$COS_pub_category=="business",]$publication_standard))
        
        n_journals_econ <- length(unique(repli_outcomes_merged[repli_outcomes_merged$COS_pub_category=="economics and finance",]$publication_standard))
        
        n_journals_edu <- length(unique(repli_outcomes_merged[repli_outcomes_merged$COS_pub_category=="education",]$publication_standard))
        
        n_journals_polisci <- length(unique(repli_outcomes_merged[repli_outcomes_merged$COS_pub_category=="political science",]$publication_standard))
        
        n_journals_psych <- length(unique(repli_outcomes_merged[repli_outcomes_merged$COS_pub_category=="psychology and health",]$publication_standard))
        
        n_journals_soc <- length(unique(repli_outcomes_merged[repli_outcomes_merged$COS_pub_category=="sociology and criminology",]$publication_standard))
        
        n_papers_initial_sample <- "Pending paper process data pipeline"
        
        n_papers_randomly_selected <- "Pending paper process data pipeline"
        
        n_papers_eligible <- "Pending paper process data pipeline"
        
        n_papers_matched_with_researchers <- "Pending paper process data pipeline"
        
        n_replis_started_from_draft	 <- "Pending paper process data pipeline"
        
        n_papers_repli_started_from_draft	 <- "Pending paper process data pipeline"
        
        p_replis_of_eligible_from_draft <- "Pending paper process data pipeline"
        
        n_repli_designs_reviewed_and_prereged <- "Pending paper process data pipeline"
        
        p_repli_designs_reviewed_and_prereged <- "Pending paper process data pipeline"
        
        n_replis_completed <- "Pending paper process data pipeline"
        
        n_replis_started <- "Pending paper process data pipeline"
        
        p_replis_completed <- "Pending paper process data pipeline"
        
        n_papers_single_claim <- "Pending paper process data pipeline"
        
        n_papers_multi_claim <- "Pending paper process data pipeline"
        
        n_papers_multi_repli_single_claim_same_protocol <- "Pending paper process data pipeline"
        
        n_papers_multi_repli_single_claim_diff_protocol <- "Pending paper process data pipeline"
        
        n_repli_hybrid <- "Pending paper process data pipeline"
        
        n_replis_completed_from_draft <- "Pending paper process data pipeline"
        
        n_repli_total <- "Pending paper process data pipeline"
        
        n_replis_papers_started_from_draft <- "Pending paper process data pipeline"
        
        p_repli_stat_sig_claims_any_dir <- 
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            mean(x$repli_p_value <= 0.05)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_repli_stat_sig_claims_any_dir_wtd <- 
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_p_value <= 0.05,
                                          w=x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        n_repli_stat_sig_claims_same_dir <- sum(repli_outcomes$repli_p_value <= 0.05 & repli_outcomes$repli_pattern_criteria_met==TRUE)
        
        p_repli_stat_sig_claims_opposite_dir_wtd <-
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_p_value <= 0.05 & x$repli_pattern_criteria_met==FALSE,w=x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        n_repli_stat_sig_claims_opposite_dir <-
          sum(repli_outcomes$repli_p_value <= 0.05 & repli_outcomes$repli_pattern_criteria_met==FALSE)
        
        p_repli_stat_sig_claims_opposite_dir <-
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            mean(x$repli_p_value <= 0.05 & x$repli_pattern_criteria_met==FALSE,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_repli_null_sig_claims_or_opposite_dir_wtd <-
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_p_value > 0.05 | x$repli_pattern_criteria_met==FALSE,w=x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_repli_null_sig_claims_or_opposite_dir_wtd <-
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_p_value > 0.05 | x$repli_pattern_criteria_met==FALSE,w=x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        n_repli_null_sig_claims_or_opposite_dir <- sum(repli_outcomes$repli_p_value > 0.05 | repli_outcomes$repli_pattern_criteria_met==FALSE)
        
        p_repli_null_sig_claims_or_opposite_dir <-
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            mean(x$repli_p_value > 0.05 | x$repli_pattern_criteria_met==FALSE,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        repli_outcomes_merged_pvals <- repli_outcomes_merged[!is.na(repli_outcomes_merged$orig_p_value) & !is.na(repli_outcomes_merged$repli_p_value),]
        
        zcurve.summary <- summary(zcurve(p = repli_outcomes_merged_pvals$orig_p_value))
        p_expected_repli_rate_claims	<- format.text.CI(point.estimate = zcurve.summary$coefficients[1,1],
                                                       CI.lb = zcurve.summary$coefficients[1,2],
                                                       CI.ub = zcurve.summary$coefficients[1,3],
                                                       digits=1,alpha=.05,
                                                       format.percent = TRUE)
        
        n_claims_with_pvalues <- nrow(repli_outcomes_merged_pvals)
        
        n_expected_repli_stat_sig_claims <- format.text.CI(
          zcurve.summary$coefficients[1,1]*n_claims_with_pvalues,
          zcurve.summary$coefficients[1,2]*n_claims_with_pvalues,
          zcurve.summary$coefficients[1,3]*n_claims_with_pvalues,
          digits=1)
        
        p_repli_stat_sig_claims_with_pvals_any_dir <- 
          bootstrap.clust(data=repli_outcomes_merged_pvals,
                          FUN=function(x) {
                            mean(x$repli_p_value <= 0.05)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_p_value"),
                          alpha=.05,tails="two-tailed",iters=1000,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        rm(zcurve.summary,repli_outcomes_merged_pvals)
        
        p_orig_ES_within_repli_ES_wtd <- 
          bootstrap.clust(data=repli_outcomes_merged,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$orig_effect_size_value_repli >= x$repli_effect_size_ci_lb & 
                                            x$orig_effect_size_value_repli <= x$repli_effect_size_ci_ub,
                                          x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("orig_effect_size_value_repli","repli_effect_size_ci_lb","repli_effect_size_ci_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_orig_ES_within_repli_ES <- 
          bootstrap.clust(data=repli_outcomes_merged,
                          FUN=function(x) {
                            mean(x$orig_effect_size_value_repli >= x$repli_effect_size_ci_lb & 
                                   x$orig_effect_size_value_repli <= x$repli_effect_size_ci_ub,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("orig_effect_size_value_repli","repli_effect_size_ci_lb","repli_effect_size_ci_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_orig_ES_outside_repli_ES_orig_larger_wtd <-  
          bootstrap.clust(data=repli_outcomes_merged[repli_outcomes_merged$orig_effect_size_value_repli < repli_outcomes_merged$repli_effect_size_ci_lb | 
                                                       repli_outcomes_merged$orig_effect_size_value_repli > repli_outcomes_merged$repli_effect_size_ci_ub,],
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$orig_effect_size_value_repli > x$repli_effect_size_ci_ub,
                                          x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("orig_effect_size_value_repli","repli_effect_size_ci_lb","repli_effect_size_ci_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_orig_ES_outside_repli_ES_orig_smaller_wtd <-  
          bootstrap.clust(data=repli_outcomes_merged[repli_outcomes_merged$orig_effect_size_value_repli < repli_outcomes_merged$repli_effect_size_ci_lb | 
                                                       repli_outcomes_merged$orig_effect_size_value_repli > repli_outcomes_merged$repli_effect_size_ci_ub,],
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$orig_effect_size_value_repli < x$repli_effect_size_ci_lb,
                                          x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("orig_effect_size_value_repli","repli_effect_size_ci_lb","repli_effect_size_ci_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_repli_ES_within_orig_ES_wtd <- 
          bootstrap.clust(data=repli_outcomes_merged,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_effect_size_value >= x$orig_effect_size_ci_lb & 
                                            x$repli_effect_size_value <= x$orig_effect_size_ci_ub,
                                          x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_effect_size_value","orig_effect_size_ci_lb","orig_effect_size_ci_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_repli_ES_within_orig_ES <- 
          bootstrap.clust(data=repli_outcomes_merged,
                          FUN=function(x) {
                            mean(x$repli_effect_size_value >= x$orig_effect_size_ci_lb & 
                                   x$repli_effect_size_value <= x$orig_effect_size_ci_ub,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_effect_size_value","orig_effect_size_ci_lb","orig_effect_size_ci_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_repli_ES_outside_orig_ES_repli_larger_wtd <-  
          bootstrap.clust(data=repli_outcomes_merged[repli_outcomes_merged$repli_effect_size_value < repli_outcomes_merged$orig_effect_size_ci_lb | 
                                                       repli_outcomes_merged$repli_effect_size_value > repli_outcomes_merged$orig_effect_size_ci_ub,],
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_effect_size_value > x$orig_effect_size_ci_ub,
                                          x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_effect_size_value","orig_effect_size_ci_lb","orig_effect_size_ci_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_repli_ES_outside_orig_ES_repli_smaller_wtd <-  
          bootstrap.clust(data=repli_outcomes_merged[repli_outcomes_merged$repli_effect_size_value < repli_outcomes_merged$orig_effect_size_ci_lb | 
                                                       repli_outcomes_merged$repli_effect_size_value > repli_outcomes_merged$orig_effect_size_ci_ub,],
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_effect_size_value < x$orig_effect_size_ci_lb,
                                          x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_effect_size_value","orig_effect_size_ci_lb","orig_effect_size_ci_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        n_repli_ES_within_orig_ES <- sum(
          repli_outcomes_merged$repli_effect_size_value >= repli_outcomes_merged$orig_effect_size_ci_lb & 
            repli_outcomes_merged$repli_effect_size_value <= repli_outcomes_merged$orig_effect_size_ci_ub,na.rm=TRUE)
        
        p_repli_ES_within_orig_PI_ES_wtd <-
          bootstrap.clust(data=repli_outcomes_merged,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(x$repli_effect_size_value >= x$repli_effect_size_pi_lb & 
                                            x$repli_effect_size_value <= x$repli_effect_size_pi_ub,
                                          x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_effect_size_value","repli_effect_size_pi_lb","repli_effect_size_pi_ub"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        n_orig_ES_within_repli_ES <- sum(
          repli_outcomes_merged$orig_effect_size_value_repli >= repli_outcomes_merged$repli_effect_size_ci_lb & 
            repli_outcomes_merged$orig_effect_size_value_repli <= repli_outcomes_merged$repli_effect_size_ci_ub,na.rm=TRUE)
        
        n_orig_ES_and_repli_ES <- sum(!is.na(repli_outcomes_merged$orig_effect_size_value_repli) & 
                                        !is.na(repli_outcomes_merged$repli_effect_size_value))
        
        p_repli_stat_sig_claims_same_dir <-
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            mean(x$repli_p_value <= 0.05 & x$repli_pattern_criteria_met==TRUE,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        median_orig_sample_size_wtd <- weighted.median(orig_outcomes$orig_sample_size_value,w=orig_outcomes$weight,na.rm = TRUE)
        
        SD_orig_sample_size_wtd <- round(sqrt(Hmisc::wtd.var(orig_outcomes$orig_sample_size_value,weights=orig_outcomes$weight)),0)
        
        median_repli_sample_size_wtd <- weighted.median(repli_outcomes$repli_sample_size_value,w=repli_outcomes$weight,na.rm = TRUE)
        
        SD_repli_sample_size_wtd <- round(sqrt(Hmisc::wtd.var(repli_outcomes$repli_sample_size_value,weights=repli_outcomes$weight)),0)
        
        figure_effect_size_comparison <- "Pending figure"
        
        n_figure_x_left	 <- "Pending figure"
        
        n_figure_x_right	 <- "Pending figure"
        
        papers_pearsons_estimatable_repli <- unique(repli_outcomes[!is.na(repli_outcomes$repli_pearsons_r_value),]$paper_id)
        papers_pearsons_estimatable_orig <- unique(orig_outcomes[!is.na(orig_outcomes$orig_pearsons_r_value),]$paper_id)
        papers_pearsons_estimatable_orig_and_repli <- papers_pearsons_estimatable_repli[papers_pearsons_estimatable_repli %in% papers_pearsons_estimatable_orig]
        
        n_papers_orig_and_repli_pearsons_estimatable <- length(papers_pearsons_estimatable_orig_and_repli)
        
        orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable <- ifelse(orig_outcomes$paper_id %in% papers_pearsons_estimatable_orig_and_repli & !is.na(orig_outcomes$orig_pearsons_r_value),
                                                                             orig_outcomes$paper_id,NA)
        
        orig_outcomes <- orig_outcomes %>%
          group_by(paper_id_orig_and_repli_pearsons_estimatable) %>%
          mutate(weight.estimatable = 1/n()) %>%
          ungroup()
        
        median_orig_pearsons_both_estimatable_wtd <- round(weighted.median(orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$orig_pearsons_r_value,
                                                                           orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$weight.estimatable,na.rm=TRUE),3)
        
        SD_orig_pearsons_both_estimatable_wtd <- round(sqrt(Hmisc::wtd.var(orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$orig_pearsons_r_value,
                                                                           orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$weight.estimatable,na.rm=TRUE)),3)
        
        repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable <- ifelse(repli_outcomes$paper_id %in% papers_pearsons_estimatable_orig_and_repli & !is.na(repli_outcomes$repli_pearsons_r_value),
                                                                              repli_outcomes$paper_id,NA)
        
        repli_outcomes <- repli_outcomes %>%
          group_by(paper_id_orig_and_repli_pearsons_estimatable) %>%
          mutate(weight.estimatable = 1/n()) %>% 
          ungroup()
        
        median_repli_pearsons_both_estimatable_wtd <- round(weighted.median(repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$repli_pearsons_r_value,
                                                                            repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$weight.estimatable,na.rm=TRUE),3)
        
        SD_repli_pearsons_both_estimatable_wtd <- round(sqrt(Hmisc::wtd.var(repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$repli_pearsons_r_value,
                                                                            repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$weight.estimatable,na.rm=TRUE)),3)
        
        repli_outcomes_merged$repli_pearsons_ratio_v_orig <- repli_outcomes_merged$repli_pearsons_r_value/repli_outcomes_merged$orig_pearsons_r_value
        
        p_median_pearsons_neg_ratio_v_orig_wtd <- 
          bootstrap.clust(data=repli_outcomes_merged[],
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.median(1-x$repli_pearsons_ratio_v_orig,x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_pearsons_ratio_v_orig"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        repli_outcomes_SER <- repli_outcomes[repli_outcomes$repli_effect_size_type=="ser_method",]
        orig_outcomes_SER <- orig_outcomes[orig_outcomes$orig_effect_size_type_repli=="ser_method" & !is.na(orig_outcomes$orig_effect_size_type_repli),]
        repli_outcomes_SER_merged <- merge(repli_outcomes_SER,orig_outcomes_SER[,!(names(orig_outcomes_SER) %in% c("paper_id"))],
                                           by="claim_id",all.x=TRUE,all.y=FALSE)
        
        papers_SER_estimatable_repli <- unique(repli_outcomes_SER[!is.na(repli_outcomes_SER$repli_effect_size_value),]$paper_id)
        papers_SER_estimatable_orig <- unique(orig_outcomes_SER[!is.na(orig_outcomes_SER$orig_effect_size_value_repli),]$paper_id)
        papers_SER_estimatable_orig_and_repli <- papers_SER_estimatable_repli[papers_SER_estimatable_repli %in% papers_SER_estimatable_orig]
        
        n_papers_orig_and_repli_SER_estimatable <- length(papers_SER_estimatable_orig_and_repli)
        
        orig_outcomes_SER$paper_id_orig_and_repli_SER_estimatable <- ifelse(orig_outcomes_SER$paper_id %in% papers_SER_estimatable_orig_and_repli & !is.na(orig_outcomes_SER$orig_effect_size_value_repli),
                                                                            orig_outcomes_SER$paper_id,NA)
        
        orig_outcomes_SER <- orig_outcomes_SER %>% 
          group_by(paper_id_orig_and_repli_SER_estimatable) %>% 
          mutate(weight.estimatable = 1/n())
        
        median_orig_SER_both_estimatable_wtd <- round(
          weighted.median(abs(orig_outcomes_SER[!is.na(orig_outcomes_SER$paper_id_orig_and_repli_SER_estimatable),]$orig_effect_size_value_repli),
                          orig_outcomes_SER[!is.na(orig_outcomes_SER$paper_id_orig_and_repli_SER_estimatable),]$weight.estimatable,na.rm=TRUE),
          3)
        
        SD_orig_SER_both_estimatable_wtd <- round(sqrt(Hmisc::wtd.var(
          abs(orig_outcomes_SER[!is.na(orig_outcomes_SER$paper_id_orig_and_repli_SER_estimatable),]$orig_effect_size_value_repli),
          orig_outcomes_SER[!is.na(orig_outcomes_SER$paper_id_orig_and_repli_SER_estimatable),]$weight.estimatable,na.rm=TRUE)),
          2)
        
        repli_outcomes_SER$paper_id_orig_and_repli_SER_estimatable <- ifelse(repli_outcomes_SER$paper_id %in% papers_SER_estimatable_orig_and_repli & !is.na(repli_outcomes_SER$repli_effect_size_value),
                                                                             repli_outcomes_SER$paper_id,NA)
        
        repli_outcomes_SER <- repli_outcomes_SER %>% 
          group_by(paper_id_orig_and_repli_SER_estimatable) %>% 
          mutate(weight.estimatable = 1/n())
        
        median_repli_SER_both_estimatable_wtd <- round(
          weighted.median(abs(repli_outcomes_SER[!is.na(repli_outcomes_SER$paper_id_orig_and_repli_SER_estimatable),]$repli_effect_size_value),
                          repli_outcomes_SER[!is.na(repli_outcomes_SER$paper_id_orig_and_repli_SER_estimatable),]$weight.estimatable,na.rm=TRUE),
          2)
        
        SD_repli_SER_both_estimatable_wtd <- round(sqrt(Hmisc::wtd.var(
          abs(repli_outcomes_SER[!is.na(repli_outcomes_SER$paper_id_orig_and_repli_SER_estimatable),]$repli_effect_size_value),
          repli_outcomes_SER[!is.na(repli_outcomes_SER$paper_id_orig_and_repli_SER_estimatable),]$weight.estimatable,na.rm=TRUE)),
          2)
        
        repli_outcomes_SER_merged$repli_SER_ratio_v_orig <- abs(repli_outcomes_SER_merged$repli_effect_size_value)/abs(repli_outcomes_SER_merged$orig_effect_size_value_repli)
        
        p_median_SER_neg_ratio_v_orig_wtd <- 
          bootstrap.clust(data=repli_outcomes_SER_merged,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.median(1-x$repli_SER_ratio_v_orig,x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_SER_ratio_v_orig"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_neg_ratio_all_ES_repli_vs_orig <- "Pending discussion, probably drop"
        
        
        ratio_repli_stat_sig_same_dir_new_data_vs_secondary <- 
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            mean(x[x$repli_type=="new data",]$repli_p_value<=.05 & x[x$repli_type=="new data",]$repli_pattern_criteria_met==TRUE,na.rm=TRUE)/
                              mean(x[x$repli_type=="secondary data",]$repli_p_value<=.05 & x[x$repli_type=="secondary data",]$repli_pattern_criteria_met==TRUE,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("repli_type","repli_p_value","repli_pattern_criteria_met"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=FALSE,digits=1
          )$formatted.text
        
        #Replication attempts using new data were X times as likely to have replications that were statistically significant and in the same direction with those replications using secondary data.
        
        n_repli_stat_sig_claims <- sum(repli_outcomes$repli_p_value <= 0.05)
        
        p_repli_stat_sig_claims <- 
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            mean(as.numeric(x$repli_p_value <= 0.05),na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("claim_id","repli_p_value"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_repli_stat_sig_claims_wtd <-
          bootstrap.clust(data=repli_outcomes,
                          FUN=function(x) {
                            x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                            weighted.mean(as.numeric(x$repli_p_value <= 0.05),w=x$weight,na.rm=TRUE)
                          },
                          clustervar = "paper_id",
                          keepvars=c("paper_id","claim_id","repli_p_value"),
                          alpha=.05,tails="two-tailed",iters=iters,
                          format.percent=TRUE,digits=1
          )$formatted.text
        
        p_replication_rate_success_field_max <- "Pending paper process data pipeline"
        
        p_findings_within_1_SD <- "Pending discussion, probably drop"
        
        p_findings_within_2_SD <- "Pending discussion, probably drop"
        
        p_repli_orig_author_reviewed <- "Pending paper process data pipeline"
        
        
      }

    }
    
  
    # Clean up input values and export
    {
      rm(iters,orig_outcomes,repli_outcomes,repli_outcomes_merged)
      return(rev(as.list(environment())))
    }
  }

  figures <- function(repli_outcomes,orig_outcomes,paper_metadata){
    # Initialization and data preparation
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
      
    }
    
    # Figure generation
    {
    
      # Figure 1
      {
        figure_1 <- ggplot()
      }
      
      # Figure 2
      {
        figure_2 <- ggplot()
      }
      
      # Figure 3
      {
        all_effects <- repli_outcomes %>% 
          filter(!is_covid) %>% 
          filter(repli_version_of_record) %>% 
          select(report_id, claim_id, repli_pattern_criteria_met, success = repli_score_criteria_met,
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
          )
        
        
        p <- all_effects %>% 
          ggplot(aes(x = orig_conv_r, y = repli_conv_r, color = success)) +
          geom_point(alpha = 0.6, size = 3) +
          geom_rug() +
          scale_color_manual(labels = c("Failed", "Successful"),
                             values = c("tomato3", "deepskyblue4")) +
          #geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "slategray") +
          #geom_segment(aes(x=0,y=0,xend=1,yend=1), linetype = 2, color = "slategray") +
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
          )
        
        figure_3 <- ggMarginal(p, type = "density", groupFill = T)
      }
      
      # Figure 4
      {
        # Data wrangling
        {
          all_effects <- repli_outcomes %>% 
            filter(!is_covid) %>% 
            filter(repli_version_of_record) %>% 
            select(claim_id,repli_conv_r) %>% 
            left_join(
              orig_outcomes %>% 
                select(paper_id,claim_id, orig_conv_r) %>% 
                distinct(),
              by = "claim_id"
            ) %>% 
            drop_na(repli_conv_r) %>% 
            drop_na(orig_conv_r)%>%
            group_by(paper_id) %>%
            mutate(weight=1/n())
          
          all_effects <- merge(all_effects,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
          all_effects$field <- str_to_title(all_effects$COS_pub_category)
          all_effects$field <- str_to_title(all_effects$COS_pub_category)
          all_effects$field <- str_replace_all(all_effects$field," ","\n")
          all_effects$field <- str_replace_all(all_effects$field,"And","and")
          group_order <- unique(all_effects$field)
          all_effects$field <- ordered(all_effects$field,labels=group_order,levels=group_order)
          
          all_effects$pub_year <- ordered(all_effects$pub_year,
                                          levels=c(min(all_effects$pub_year):max(all_effects$pub_year)),
                                          labels=c(min(all_effects$pub_year):max(all_effects$pub_year)))
          
          # Convert to long
          all_effects <- all_effects %>%
            pivot_longer(cols=c(repli_conv_r,orig_conv_r))
          
          all_effects$name <- ordered(all_effects$name,
                                      levels=c("orig_conv_r","repli_conv_r"),
                                      labels=c("Original effect size","Replication effect size"))
          
        }
        # Plot
        {
          figure_4 <- ggplot(data=all_effects,aes(x=value,y=pub_year,fill=name))+
              geom_density_ridges(alpha=.6,scale = 0.9)+
              theme_minimal()+
              theme(legend.position = "bottom",
                    legend.title=element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank(),
                    axis.title=element_blank(),
                    axis.text.y=element_text(vjust=0,face = "bold")
                    #axis.text = element_blank(),
                    #axis.line = element_blank(),
                    #axis.ticks = element_blank()
              ) +
              scale_fill_manual(values=c(palette_score_charts[1],palette_score_charts[5]))+
              xlim(-1,1)+
              geom_vline(aes(xintercept=0),linetype=2)
          }
      }
      
      # Figure 5
      {
        # Data wrangling
        {
          all_effects <- repli_outcomes %>% 
            filter(!is_covid) %>% 
            filter(repli_version_of_record) %>% 
            select(claim_id,repli_conv_r) %>% 
            left_join(
              orig_outcomes %>% 
                select(paper_id,claim_id, orig_conv_r) %>% 
                distinct(),
              by = "claim_id"
            ) %>% 
            drop_na(repli_conv_r) %>% 
            drop_na(orig_conv_r)%>%
            group_by(paper_id) %>%
            mutate(weight=1/n())
          
          all_effects <- merge(all_effects,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
          # all_effects$field <- str_to_title(all_effects$COS_pub_category)
          # all_effects$field <- str_to_title(all_effects$COS_pub_category)
          # all_effects$field <- str_replace_all(all_effects$field," ","\n")
          # all_effects$field <- str_replace_all(all_effects$field,"And","and")
          # group_order <- unique(all_effects$field)
          all_effects$field <- ordered(all_effects$COS_pub_category,labels=fields.abbreviated,
                                       levels=fields.raw)
          
          all_effects$pub_year <- ordered(all_effects$pub_year,
                                          levels=c(min(all_effects$pub_year):max(all_effects$pub_year)),
                                          labels=c(min(all_effects$pub_year):max(all_effects$pub_year)))
          
          # Convert to long
          all_effects <- all_effects %>%
            pivot_longer(cols=c(repli_conv_r,orig_conv_r))
          
          all_effects$name <- ordered(all_effects$name,
                                      levels=c("orig_conv_r","repli_conv_r"),
                                      labels=c("Original effect size","Replication effect size"))
          
        }
        # Plot
        {
          figure_5 <- ggplot(data=all_effects,aes(x=value,y=field,fill=name))+
            geom_density_ridges(alpha=.6,scale = 0.9)+
            theme_minimal()+
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title=element_blank(),
                  axis.text.y=element_text(vjust=0,face = "bold")
                  #axis.text = element_blank(),
                  #axis.line = element_blank(),
                  #axis.ticks = element_blank()
            ) +
            scale_fill_manual(values=c(palette_score_charts[1],palette_score_charts[5]))+
            xlim(-1,1)+
            geom_vline(aes(xintercept=0),linetype=2)
        }
      }
    }
    
    # Export
    {
      return(list(
        "figure_1"=figure_1,
        "figure_2"=figure_2,
        "figure_3"=figure_3,
        "figure_4"=figure_4,
        "figure_5"=figure_5))
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
    library(Hmisc)
    library(wCorr)
    
    drive_auth(Sys.getenv("google_oauth_email"))
    #drive_deauth()
    # Common functions
    source(file="Analysis/common functions.R")
  }
  
  
  # Load data
  objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata","status","all_rr_attempts")
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
  results_tagged_stats <- tagged_stats(iters = 20,
                                       repli_outcomes=repli_outcomes,
                                       orig_outcomes=orig_outcomes,
                                       paper_metadata=paper_metadata,
                                       all_rr_attempts=all_rr_attempts)
  
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
  if (1==1){
    generated_figures <- figures(repli_outcomes,orig_outcomes,paper_metadata)
    
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 1.png",
      plot = generated_figures$figure_1,
      width = 6000,height = 2500,units = "px",bg="white"
    )
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 2.png",
      plot = generated_figures$figure_2,
      width = 6000,height = 2000,units = "px",bg="white"
    )
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 3.png",
      plot = generated_figures$figure_3,
      width = 2000,height = 2000,units = "px",bg="white"
    )
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 4.png",
      plot = generated_figures$figure_4,
      width = 2000,height = 2000,units = "px",bg="white"
    )
    ggsave(
      "Analysis/Paper 5/Code and data/Figures/figure 5.png",
      plot = generated_figures$figure_5,
      width = 2000,height = 2000,units = "px",bg="white"
    )
    
  }
  
}
