
# Functions (also readable by external sources)
{
  # Create an object that contains the tagged stats
  tagged_stats <- function(iters = 100,repli_outcomes,orig_outcomes,paper_metadata,all_rr_attempts){
  
    # Data preparation
    {
      paper_metadata <- paper_metadata[c("paper_id","publication_standard","COS_pub_category","pub_year")]
      orig_outcomes <- merge(orig_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
      
      repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                     by="claim_id",all.x=TRUE,all.y=FALSE)
      orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
      repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
      repli_outcomes_merged <- repli_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
      
      paper_metadata$field <- str_to_title(paper_metadata$COS_pub_category)
      
    }
    
    # Stats
    {
      n_claims <- length(unique(repli_outcomes_merged$claim_id))
      
      n_papers <- length(unique(repli_outcomes_merged$paper_id))
      
      n_journals <- length(unique(repli_outcomes_merged$publication_standard))
      
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
      
      # Table 2
      {
        fields.order <- c("Psychology And Health","Business","Sociology And Criminology",
                          "Economics And Finance","Political Science","Education")
        
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
        table_2 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8)
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
          select(report_id, claim_id, paper_id,repli_sample_size_value, success = repli_score_criteria_met, repli_type,
                 repli_conv_r, repli_conv_r_lb, repli_conv_r_ub, repli_effect_size_type, repli_effect_size_value) %>%
          mutate(ser_effect = ifelse(str_detect(repli_effect_size_type, "ser_"), abs(repli_effect_size_value), NA)) %>% 
          mutate(across(contains("conv"), abs))
        
        orig_effects <- orig_outcomes %>% 
          select(claim_id, paper_id,orig_sample_size_value, orig_conv_r, orig_conv_r_lb, orig_conv_r_ub,
                 orig_effect_size_type_repli, orig_effect_size_value_repli) %>%
          semi_join(repli_effects, by = "claim_id") %>% 
          
          mutate(ser_effect = ifelse(str_detect(orig_effect_size_type_repli, "ser_"), abs(orig_effect_size_value_repli), NA)) %>% 
          mutate(across(contains("conv"), abs))
        
        # Number of papers and claims
        r1 <- c(length(unique(orig_effects$paper_id)),length(unique(repli_effects$paper_id)),
                length(unique(orig_effects$claim_id)),length(unique(repli_effects$claim_id)))
        
        repli_effects <- repli_effects %>% 
          filter(!is.na(repli_conv_r)) %>%
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup()
        orig_effects <- orig_effects %>% 
          filter(!is.na(orig_conv_r)) %>%
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup()
        
        # Number of effect sizes
        r2 <- c(sum(as.numeric(!is.na(orig_effects$orig_conv_r))*orig_effects$weight),
                sum(as.numeric(!is.na(repli_effects$repli_conv_r))*repli_effects$weight),
                sum(!is.na(orig_effects$orig_conv_r)),
                sum(!is.na(repli_effects$repli_conv_r))
                )
        
        # IQR of effect sizes
        r3 <- c(paste0(weighted.quantile(orig_effects$orig_sample_size_value,orig_effects$weight,.5)," (",format.round(weighted.quantile(orig_effects$orig_sample_size_value,orig_effects$weight,.75) - weighted.quantile(orig_effects$orig_sample_size_value,orig_effects$weight,.25),1),")"),
                paste0(weighted.quantile(repli_effects$repli_sample_size_value,repli_effects$weight,.5)," (",format.round(weighted.quantile(repli_effects$repli_sample_size_value,repli_effects$weight,.75) - weighted.quantile(repli_effects$repli_sample_size_value,repli_effects$weight,.25),1),")"),
                paste0(quantile(orig_effects$orig_sample_size_value,.5,na.rm=TRUE)," (",IQR(orig_effects$orig_sample_size_value,na.rm=TRUE),")"),
                paste0(quantile(repli_effects$repli_sample_size_value,.5,na.rm=TRUE)," (",IQR(repli_effects$repli_sample_size_value,na.rm=TRUE),")")
        )
        
        # Pearsons R effect size
        r4 <- c(paste0(format.round(weighted.quantile(orig_effects$orig_conv_r,orig_effects$weight,.5),2)," (",format.round(sqrt(wtd.var(orig_effects$orig_conv_r,orig_effects$weight)),2),")"),
                paste0(format.round(weighted.quantile(repli_effects$repli_conv_r,repli_effects$weight,.5),2)," (",format.round(sqrt(wtd.var(repli_effects$repli_conv_r,repli_effects$weight)),2),")"),
                paste0(format.round(quantile(orig_effects$orig_conv_r,.5),2)," (",format.round(sd(orig_effects$orig_conv_r,1),2),")"),
                paste0(format.round(quantile(repli_effects$repli_conv_r,.5),2)," (",format.round(sd(repli_effects$repli_conv_r,1),2),")")
                
        )
        
        table_4 <- rbind(r1,r2,r3,r4)
        for (row in 1:nrow(table_4)){
          for (col in 1:ncol(table_4)){
            assign(paste0("table_4_",row,"_",col),
                   table_4[row,col])
          }
        }
        rm(r1,r2,r3,r4)
      }

      # Table 5
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

        table_5 <- do.call(rbind,lapply(unique(effects_combined$field), function(field) {
          data <- effects_combined[effects_combined$field==field,]
          c1 <- paste0(format.round(sum(as.numeric(data$success)*data$weight),1)," / ",length(unique(data$paper_id))," (",
                       format.round(100 *sum(as.numeric(data$success)*data$weight)/length(unique(data$paper_id)),1),"%)")

          c2 <- paste0(sum(data$success)," / ",nrow(data)," (",
                       format.round(100 *sum(data$success)/nrow(data),1),"%)")
          data.frame(field,c1,c2)
        }))

        table_5 <- table_5[order(table_5$field),]
        table_5$field <- NULL

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
        
        r1 <- c(paste0(format.round(length(unique(r1.data$paper_id)),1)," of ",length(unique(r1.data$paper_id)),
                       " (",format.round(100*length(unique(r1.data$paper_id))/length(unique(r1.data$paper_id)),1),"%)"),
                paste0(format.round(sum(r1.data$success*r1.data$weight),1)," of ",length(unique(r1.data$paper_id)),
                       " (",format.round(100*sum(r1.data$success*r1.data$weight)/length(unique(r1.data$paper_id)),1),"%)"),
                paste0(nrow(r1.data)," of ",nrow(r1.data)," (",format.round(100*nrow(r1.data)/nrow(r1.data),1),"%)"),
                paste0(sum(r1.data$success)," of ",nrow(r1.data)," (",format.round(100*sum(r1.data$success)/nrow(r1.data),1),"%)"))
        
        r2.data <- effects_combined[effects_combined$repli_type=="secondary data",]
        
        r2 <- c(paste0(format.round(length(unique(r2.data$paper_id)),1)," of ",length(unique(r2.data$paper_id)),
                       " (",format.round(100*length(unique(r2.data$paper_id))/length(unique(r2.data$paper_id)),1),"%)"),
                paste0(format.round(sum(r2.data$success*r2.data$weight),1)," of ",length(unique(r2.data$paper_id)),
                       " (",format.round(100*sum(r2.data$success*r2.data$weight)/length(unique(r2.data$paper_id)),1),"%)"),
                paste0(nrow(r2.data)," of ",nrow(r2.data)," (",format.round(100*nrow(r2.data)/nrow(r2.data),1),"%)"),
                paste0(sum(r2.data$success)," of ",nrow(r2.data)," (",format.round(100*sum(r2.data$success)/nrow(r2.data),1),"%)"))
        
        r3.data <- r1.data %>%
          filter(!is.na(orig_conv_r)) %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        
        r3 <- c(paste0(format.round(weighted.quantile(r3.data$orig_conv_r,r3.data$weight,.5),2),
                       " (",format.round(sqrt(wtd.var(r3.data$orig_conv_r,r3.data$weight)),2),")"),
                paste0(format.round(weighted.quantile(r3.data$repli_conv_r,r3.data$weight,.5),2),
                       " (",format.round(sqrt(wtd.var(r3.data$repli_conv_r,r3.data$weight)),2),")"),
                paste0(format.round(quantile(r3.data$orig_conv_r,.5,na.rm=TRUE),2)," (",format.round(sd(r3.data$orig_conv_r,1),2),")"),
                paste0(format.round(quantile(r3.data$repli_conv_r,.5,na.rm=TRUE),2)," (",format.round(sd(r3.data$repli_conv_r,1),2),")")
        )
        
        r4.data <- r2.data %>%
          filter(!is.na(orig_conv_r)) %>%
          group_by(paper_id) %>%
          mutate(weight = 1/n()) %>%
          ungroup()
        
        r4 <- c(paste0(format.round(weighted.quantile(r4.data$orig_conv_r,r4.data$weight,.5),2),
                       " (",format.round(sqrt(wtd.var(r4.data$orig_conv_r,r4.data$weight)),2),")"),
                paste0(format.round(weighted.quantile(r4.data$repli_conv_r,r4.data$weight,.5),2),
                       " (",format.round(sqrt(wtd.var(r4.data$repli_conv_r,r4.data$weight)),2),")"),
                paste0(format.round(quantile(r4.data$orig_conv_r,.5,na.rm=TRUE),2)," (",format.round(sd(r4.data$orig_conv_r,1),2),")"),
                paste0(format.round(quantile(r4.data$repli_conv_r,.5,na.rm=TRUE),2)," (",format.round(sd(r4.data$repli_conv_r,1),2),")")
        )
        
        table_6 <- rbind(r1,r2,r3,r4)
        for (row in 1:nrow(table_6)){
          for (col in 1:ncol(table_6)){
            assign(paste0("table_6_",row,"_",col),
                   table_6[row,col])
          }
        }
        rm(r1,r2,r3,r4)
        
        
      }

    }
    
  
    # Clean up input values and export
    {
      rm(iters,orig_outcomes,repli_outcomes,repli_outcomes_merged)
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
    library(Hmisc)
    
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
  tags <- tags[is.na(as.numeric(tags))]
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
  ss <- "https://docs.google.com/spreadsheets/d/18l82lxIwItqeC2m9RW_dIj03cHY_Im8W91Z5RQ7VN1o"
  range_delete(ss,range="A:H")
  range_write(ss,data = data.frame(tags=paste0("{",tags,"}"),values_text), range = "A1",col_names=FALSE,sheet="Data")
  
  # sheet_write(data.frame(tags=paste0("{",tags,"}"),values_text),
  #             ss=ss,sheet = "Sheet1")
  
}
