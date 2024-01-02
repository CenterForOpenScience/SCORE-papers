# Create an object that contains the tagged stats for figure 5
paper_5_stats <- function(iters = 100,repli_outcomes,orig_outcomes,df.papers=NA){

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
    median_SER_effect_size_ratio_v_orig <- bootstrap.clust(data=df.SER[c("paper_id","claim_id","repli_effect_size_ratio_v_orig")],FUN=
                                                      function(data) {
                                                        data <- data %>% add_count(paper_id)
                                                        data$weight <- 1/data$n
                                                        weighted.median(1-data$repli_effect_size_ratio_v_orig,data$weight,na.rm=TRUE)
                                                      }, 
                                                    clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_effect_size_neg_ratio_v_orig <- paste0(format.pct(median_SER_effect_size_ratio_v_orig$point.estimate,1),
                                         "% (95% CI ",
                                         format.pct(median_SER_effect_size_ratio_v_orig$CI.lb,1),
                                         "-",
                                         format.pct(median_SER_effect_size_ratio_v_orig$CI.ub,1),
                                         "%)")
    rm(df.SER,median_SER_effect_size_ratio_v_orig)
    
    p_findings_stat_sig_and_in_direct_obj <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_pattern_direction","repli_p_value")],FUN=
                                                  function(data) {
                                                    data <- data %>% add_count(paper_id)
                                                    data$weight <- 1/data$n
                                                    weighted.mean(data$repli_pattern_direction & data$repli_p_value<=.05,data$weight,na.rm=TRUE)
                                                  },
                                                clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_findings_stat_sig_and_in_direct <- paste0(format.pct(p_findings_stat_sig_and_in_direct_obj$point.estimate,1),
                                                "% (95% CI ",
                                                format.pct(p_findings_stat_sig_and_in_direct_obj$CI.lb,1),
                                                "-",
                                                format.pct(p_findings_stat_sig_and_in_direct_obj$CI.ub,1),
                                                "%)")
    rm(p_findings_stat_sig_and_in_direct_obj)
    
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
    
    n_papers_eligible <- "Pending paper process data pipeline"
    
    n_papers_matched_with_researchers <- "Pending paper process data pipeline"
    
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
    
    n_repli_total <- "Pending paper process data pipeline"
    
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
                                                           weighted.mean(as.numeric(data$repli_p_value <= 0.05),weight=data$weight,na.rm=TRUE)
                                                         },
                                                       clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_stat_sig_claims_weighted <- paste0(format.pct(p_repli_stat_sig_claims_weighted_obj$point.estimate,1),
                                      "% (95% CI ",
                                      format.pct(p_repli_stat_sig_claims_weighted_obj$CI.lb,1),
                                      "-",
                                      format.pct(p_repli_stat_sig_claims_weighted_obj$CI.ub,1),
                                      "%)")
    rm(p_repli_stat_sig_claims_weighted_obj)
    
    p_repli_stat_sig_claims_same_dir_weighted_obj <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_pattern_direction")],FUN=
                                                              function(data) {
                                                                data <- data %>% add_count(paper_id)
                                                                data$weight <- 1/data$n
                                                                weighted.mean(as.numeric(data$repli_pattern_direction <= 0.05),weight=data$weight,na.rm=TRUE)
                                                              },
                                                            clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=iters)
    p_repli_stat_sig_claims_same_dir_weighted <- paste0(format.pct(p_repli_stat_sig_claims_same_dir_weighted_obj$point.estimate,1),
                                               "% (95% CI ",
                                               format.pct(p_repli_stat_sig_claims_same_dir_weighted_obj$CI.lb,1),
                                               "-",
                                               format.pct(p_repli_stat_sig_claims_same_dir_weighted_obj$CI.ub,1),
                                               "%)")
    rm(p_repli_stat_sig_claims_same_dir_weighted_obj)
  }
  

  # Clean up input values and export
  {
    rm(iters,orig_outcomes,repli_outcomes,df.papers,repli_outcomes_merged)
    return(rev(as.list(environment())))
  }
}