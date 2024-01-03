# Create an object that contains the tagged stats for figure 5
paper_5_stats <- function(iters = 100,repli_outcomes,orig_outcomes,df.papers=NA){
  
  #library(Hmisc)

  # Data preparation
  {
    repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                   by="claim_id",all.x=TRUE,all.y=FALSE)
  }
  
  # Stats
  {
    n_claims <- length(unique(repli_outcomes$claim_id))
    
    n_papers <- length(unique(repli_outcomes$paper_id))
    
    repli_outcomes_merged$repli_effect_size_ratio_v_orig <- abs(repli_outcomes_merged$repli_effect_size_value)/abs(repli_outcomes_merged$orig_effect_size_value_repli)
    df.SER <- repli_outcomes_merged[repli_outcomes_merged$orig_effect_size_type_repli=="ser_method",]
    p_med_SER_neg_ratio_v_orig_weighted_obj <- bootstrap.clust(data=df.SER[c("paper_id","claim_id","repli_effect_size_ratio_v_orig")],FUN=
                                                      function(data) {
                                                        data <- data %>% add_count(paper_id)
                                                        data$weight <- 1/data$n
                                                        weighted.median(1-data$repli_effect_size_ratio_v_orig,data$weight,na.rm=TRUE)
                                                      }, 
                                                    clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_med_SER_neg_ratio_v_orig_weighted <- paste0(format.pct(p_med_SER_neg_ratio_v_orig_weighted_obj$point.estimate,1),
                                         "% (95% CI ",
                                         format.pct(p_med_SER_neg_ratio_v_orig_weighted_obj$CI.lb,1),
                                         "-",
                                         format.pct(p_med_SER_neg_ratio_v_orig_weighted_obj$CI.ub,1),
                                         "%)")
    rm(df.SER,p_med_SER_neg_ratio_v_orig_weighted_obj)
    
    p_repli_stat_sig_claims_same_dir_weighted_obj <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_p_value","repli_pattern_direction")],FUN=
                                                                       function(data) {
                                                                         data <- data %>% add_count(paper_id)
                                                                         data$weight <- 1/data$n
                                                                         weighted.mean(data$repli_p_value <= 0.05 & data$repli_pattern_direction==TRUE,w=data$weight,na.rm=TRUE)
                                                                         #weighted.mean(data$repli_pattern_direction & data$repli_p_value<=.05,data$weight,na.rm=TRUE)
                                                                       },
                                                                     clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_stat_sig_claims_same_dir_weighted <- paste0(format.pct(p_repli_stat_sig_claims_same_dir_weighted_obj$point.estimate,1),
                                                        "% (95% CI ",
                                                        format.pct(p_repli_stat_sig_claims_same_dir_weighted_obj$CI.lb,1),
                                                        "-",
                                                        format.pct(p_repli_stat_sig_claims_same_dir_weighted_obj$CI.ub,1),
                                                        "%)")
    rm(p_repli_stat_sig_claims_same_dir_weighted_obj)
    
    p_effect_size_smaller_v_orig_business <- "Pending paper process data pipeline"
    
    n_effect_size_smaller_v_orig_business <- "Pending paper process data pipeline"
    
    p_effect_size_smaller_v_orig_econ <- "Pending paper process data pipeline"
    
    n_effect_size_smaller_v_orig_econ <- "Pending paper process data pipeline"
    
    p_effect_size_smaller_v_orig_edu <- "Pending paper process data pipeline"
    
    n_effect_size_smaller_v_orig_edu <- "Pending paper process data pipeline"
    
    p_effect_size_smaller_v_orig_polisci <- "Pending paper process data pipeline"
    
    n_effect_size_smaller_v_orig_polisci <- "Pending paper process data pipeline"
    
    p_effect_size_smaller_v_orig_psych <- "Pending paper process data pipeline"
    
    n_effect_size_smaller_v_orig_psych <- "Pending paper process data pipeline"
    
    p_effect_size_smaller_v_orig_soc <- "Pending paper process data pipeline"
    
    n_effect_size_smaller_v_orig_soc <- "Pending paper process data pipeline"
    
    n_journals <- "Pending paper process data pipeline"
    
    n_journals_business <- "Pending paper process data pipeline"
    
    n_journals_econ <- "Pending paper process data pipeline"
    
    n_journals_edu <- "Pending paper process data pipeline"
    
    n_journals_polisci <- "Pending paper process data pipeline"
    
    n_journals_psych <- "Pending paper process data pipeline"
    
    n_journals_soc <- "Pending paper process data pipeline"
    
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
    
    n_repli_stat_sig_claims_same_dir <- sum(repli_outcomes$repli_p_value <= 0.05 & repli_outcomes$repli_pattern_direction==TRUE)
    
    p_repli_stat_sig_claims_opposite_dir_weighted_obj <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_p_value","repli_pattern_direction")],FUN=
                                                                       function(data) {
                                                                         data <- data %>% add_count(paper_id)
                                                                         data$weight <- 1/data$n
                                                                         weighted.mean(data$repli_p_value <= 0.05 & data$repli_pattern_direction==FALSE,w=data$weight,na.rm=TRUE)
                                                                         #weighted.mean(data$repli_pattern_direction & data$repli_p_value<=.05,data$weight,na.rm=TRUE)
                                                                       },
                                                                     clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_stat_sig_claims_opposite_dir_weighted <- paste0(format.pct(p_repli_stat_sig_claims_opposite_dir_weighted_obj$point.estimate,1),
                                                        "% (95% CI ",
                                                        format.pct(p_repli_stat_sig_claims_opposite_dir_weighted_obj$CI.lb,1),
                                                        "-",
                                                        format.pct(p_repli_stat_sig_claims_opposite_dir_weighted_obj$CI.ub,1),
                                                        "%)")
    rm(p_repli_stat_sig_claims_opposite_dir_weighted_obj)
    
    n_repli_stat_sig_claims_opposite_dir <- sum(repli_outcomes$repli_p_value <= 0.05 & repli_outcomes$repli_pattern_direction==FALSE)
    
    p_repli_null_sig_claims_or_opposite_dir_weighted_obj <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_p_value","repli_pattern_direction")],FUN=
                                                                           function(data) {
                                                                             data <- data %>% add_count(paper_id)
                                                                             data$weight <- 1/data$n
                                                                             weighted.mean(data$repli_p_value > 0.05 | data$repli_pattern_direction==FALSE,w=data$weight,na.rm=TRUE)
                                                                           },
                                                                         clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_null_sig_claims_or_opposite_dir_weighted <- paste0(format.pct(p_repli_null_sig_claims_or_opposite_dir_weighted_obj$point.estimate,1),
                                                            "% (95% CI ",
                                                            format.pct(p_repli_null_sig_claims_or_opposite_dir_weighted_obj$CI.lb,1),
                                                            "-",
                                                            format.pct(p_repli_null_sig_claims_or_opposite_dir_weighted_obj$CI.ub,1),
                                                            "%)")
    rm(p_repli_null_sig_claims_or_opposite_dir_weighted_obj)
    
    n_repli_null_sig_claims_or_opposite_dir <- sum(repli_outcomes$repli_p_value > 0.05 | repli_outcomes$repli_pattern_direction==FALSE)
    
    p_repli_null_sig_claims_or_opposite_dir_obj <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_p_value","repli_pattern_direction")],FUN=
                                                                              function(data) {
                                                                                mean(data$repli_p_value > 0.05 | data$repli_pattern_direction==FALSE,na.rm=TRUE)
                                                                              },
                                                                            clustervar = "claim_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_null_sig_claims_or_opposite_dir <- paste0(format.pct(p_repli_null_sig_claims_or_opposite_dir_obj$point.estimate,1),
                                                               "% (95% CI ",
                                                               format.pct(p_repli_null_sig_claims_or_opposite_dir_obj$CI.lb,1),
                                                               "-",
                                                               format.pct(p_repli_null_sig_claims_or_opposite_dir_obj$CI.ub,1),
                                                               "%)")
    rm(p_repli_null_sig_claims_or_opposite_dir_obj)
    
    n_expected_repli_stat_sig_claims_same_dir	<- "Pending discussion"
    
    p_expected_repli_stat_sig_claims_same_dir <- "Pending discussion"
    
    p_repli_stat_sig_claims_same_dir_obj <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_p_value","repli_pattern_direction")],FUN=
                                                                       function(data) {
                                                                         mean(data$repli_p_value <= 0.05 & data$repli_pattern_direction==TRUE,na.rm=TRUE)
                                                                       },
                                                                     clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_stat_sig_claims_same_dir <- paste0(format.pct(p_repli_stat_sig_claims_same_dir_obj$point.estimate,1),
                                                        "% (95% CI ",
                                                        format.pct(p_repli_stat_sig_claims_same_dir_obj$CI.lb,1),
                                                        "-",
                                                        format.pct(p_repli_stat_sig_claims_same_dir_obj$CI.ub,1),
                                                        "%)")
    rm(p_repli_stat_sig_claims_same_dir_obj)
    
    orig_outcomes <- orig_outcomes %>% add_count(paper_id)
    orig_outcomes$weight <- 1/orig_outcomes$n
    median_orig_sample_size_weighted <- weighted.median(orig_outcomes$orig_sample_size_value,w=orig_outcomes$weight,na.rm = TRUE)
    
    SD_orig_sample_size_weighted <- round(sqrt(Hmisc::wtd.var(orig_outcomes$orig_sample_size_value,weights=orig_outcomes$weight)),0)

    repli_outcomes <- repli_outcomes %>% add_count(paper_id)
    repli_outcomes$weight <- 1/repli_outcomes$n
    median_repli_sample_size_weighted <- weighted.median(repli_outcomes$repli_sample_size_value,w=repli_outcomes$weight,na.rm = TRUE)
    
    SD_repli_sample_size_weighted <- round(sqrt(Hmisc::wtd.var(repli_outcomes$repli_sample_size_value,weights=repli_outcomes$weight)),0)
    
    figure_effect_size_comparison <- "Pending figure"
    
    n_figure_x_left	 <- "Pending figure"
    
    n_figure_x_right	 <- "Pending figure"
    
    papers_pearsons_estimatable_repli <- unique(repli_outcomes[!is.na(repli_outcomes$repli_pearsons_r_value),]$paper_id)
    papers_pearsons_estimatable_orig <- unique(orig_outcomes[!is.na(orig_outcomes$orig_pearsons_r_value),]$paper_id)
    papers_pearsons_estimatable_orig_and_repli <- papers_pearsons_estimatable_repli[papers_pearsons_estimatable_repli %in% papers_pearsons_estimatable_orig]
    
    n_papers_orig_and_repli_Pearsons_estimatable <- length(papers_pearsons_estimatable_orig_and_repli)
    
    orig_outcomes$paper_id_orig_and_repli_Pearsons_estimatable <- ifelse(orig_outcomes$paper_id %in% papers_pearsons_estimatable_orig_and_repli & !is.na(orig_outcomes$orig_pearsons_r_value),
                                                                         orig_outcomes$paper_id,NA)
    orig_outcomes$n <- NULL
    orig_outcomes <- orig_outcomes %>% add_count(paper_id_orig_and_repli_Pearsons_estimatable)
    orig_outcomes$weight <- 1/orig_outcomes$n
    median_orig_Pearsons_both_estimatable_weighted <- round(weighted.median(orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_Pearsons_estimatable),]$orig_pearsons_r_value,
                                                                      orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_Pearsons_estimatable),]$weight,na.rm=TRUE),3)
    
    SD_orig_Pearsons_both_estimatable_weighted <- round(sqrt(Hmisc::wtd.var(orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_Pearsons_estimatable),]$orig_pearsons_r_value,
    orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_Pearsons_estimatable),]$weight,na.rm=TRUE)),3)
    
    
    repli_outcomes$paper_id_orig_and_repli_Pearsons_estimatable <- ifelse(repli_outcomes$paper_id %in% papers_pearsons_estimatable_orig_and_repli & !is.na(repli_outcomes$repli_pearsons_r_value),
                                                                          repli_outcomes$paper_id,NA)
    repli_outcomes$n <- NULL
    repli_outcomes <- repli_outcomes %>% add_count(paper_id_orig_and_repli_Pearsons_estimatable)
    repli_outcomes$weight <- 1/repli_outcomes$n
    median_repli_Pearsons_both_estimatable_weighted <- round(weighted.median(repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_Pearsons_estimatable),]$repli_pearsons_r_value,
                                                                            repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_Pearsons_estimatable),]$weight,na.rm=TRUE),3)
    
    SD_repli_Pearsons_both_estimatable_weighted <- round(sqrt(Hmisc::wtd.var(repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_Pearsons_estimatable),]$repli_pearsons_r_value,
                                                                            repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_Pearsons_estimatable),]$weight,na.rm=TRUE)),3)
    
    
    

    # p_Pearsons_neg_ratio_v_orig	MISSING
    # n_papers_orig_and_repli_SER_estimatable	MISSING
    # median_orig_SER_both_estimatable	MISSING
    # SD_orig_SER_both_estimatable	MISSING
    # median_repli_SER_both_estimatable	MISSING
    # SD_repli_SER_both_estimatable	MISSING
    
    n_repli_stat_sig_claims <- sum(repli_outcomes$repli_p_value <= 0.05)
    
    p_repli_stat_sig_claims_obj <- bootstrap.clust(data=repli_outcomes[c("claim_id","repli_p_value")],FUN=
                                                  function(data) {
                                                    mean(as.numeric(data$repli_p_value <= 0.05),na.rm=TRUE)
                                                  },
                                                clustervar = "claim_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_stat_sig_claims <- paste0(format.pct(p_repli_stat_sig_claims_obj$point.estimate,1),
                                                "% (95% CI ",
                                                format.pct(p_repli_stat_sig_claims_obj$CI.lb,1),
                                                "-",
                                                format.pct(p_repli_stat_sig_claims_obj$CI.ub,1),
                                                "%)")
    rm(p_repli_stat_sig_claims_obj)
    
    p_repli_stat_sig_claims_weighted_obj <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_p_value")],FUN=
                                                         function(data) {
                                                           data <- data %>% add_count(paper_id)
                                                           data$weight <- 1/data$n
                                                           weighted.mean(as.numeric(data$repli_p_value <= 0.05),w=data$weight,na.rm=TRUE)
                                                         },
                                                       clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_stat_sig_claims_weighted <- paste0(format.pct(p_repli_stat_sig_claims_weighted_obj$point.estimate,1),
                                      "% (95% CI ",
                                      format.pct(p_repli_stat_sig_claims_weighted_obj$CI.lb,1),
                                      "-",
                                      format.pct(p_repli_stat_sig_claims_weighted_obj$CI.ub,1),
                                      "%)")
    rm(p_repli_stat_sig_claims_weighted_obj)
    
    #p_effect_size_neg_ratio_v_orig	MISSING
    #p_replication_rate_succss_field_max	MISSING
  }
  

  # Clean up input values and export
  {
    rm(iters,orig_outcomes,repli_outcomes,df.papers,repli_outcomes_merged)
    return(rev(as.list(environment())))
  }
}