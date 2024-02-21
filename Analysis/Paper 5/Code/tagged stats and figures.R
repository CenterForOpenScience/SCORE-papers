
# Functions (also readable by external sources)
{
  # Create an object that contains the tagged stats for figure 5
  tagged_stats <- function(iters = 100,repli_outcomes,orig_outcomes,df.papers=NA){
  
    # Data preparation
    {
      paper_metadata <- paper_metadata[c("paper_id","publication_standard","COS_pub_category","pub_year")]
      orig_outcomes <- merge(orig_outcomes,paper_metadata,by="paper_id",all.x = TRUE,all.y=FALSE)
      
      repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                     by="claim_id",all.x=TRUE,all.y=FALSE)
      orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
      repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
      repli_outcomes_merged <- repli_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n())
      
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
                          weighted.mean(x$repli_p_value <= 0.05 & x$repli_pattern_direction==TRUE,
                                        w=x$weight,na.rm=TRUE)
                        },
                        clustervar = "paper_id",
                        keepvars=c("repli_p_value","repli_pattern_direction"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        format.percent=TRUE,digits=1
        )$formatted.text
  
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
      
      p_repli_stat_sig_claims_any_dir <- 
        bootstrap.clust(data=repli_outcomes,
                        FUN=function(x) {
                          mean(x$repli_p_value <= 0.05)
                        },
                        clustervar = "paper_id",
                        keepvars=c("repli_p_value","repli_pattern_direction"),
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
                        keepvars=c("repli_p_value","repli_pattern_direction"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        format.percent=TRUE,digits=1
        )$formatted.text
  
      n_repli_stat_sig_claims_same_dir <- sum(repli_outcomes$repli_p_value <= 0.05 & repli_outcomes$repli_pattern_direction==TRUE)
      
      p_repli_stat_sig_claims_opposite_dir_wtd <-
        bootstrap.clust(data=repli_outcomes,
                        FUN=function(x) {
                          x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                          weighted.mean(x$repli_p_value <= 0.05 & x$repli_pattern_direction==FALSE,w=x$weight,na.rm=TRUE)
                        },
                        clustervar = "paper_id",
                        keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_direction"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        format.percent=TRUE,digits=1
        )$formatted.text
  
      n_repli_stat_sig_claims_opposite_dir <-
        sum(repli_outcomes$repli_p_value <= 0.05 & repli_outcomes$repli_pattern_direction==FALSE)
  
      p_repli_stat_sig_claims_opposite_dir <-
        bootstrap.clust(data=repli_outcomes,
                        FUN=function(x) {
                          mean(x$repli_p_value <= 0.05 & x$repli_pattern_direction==FALSE,na.rm=TRUE)
                        },
                        clustervar = "paper_id",
                        keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_direction"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        format.percent=TRUE,digits=1
        )$formatted.text
  
      p_repli_null_sig_claims_or_opposite_dir_wtd <-
        bootstrap.clust(data=repli_outcomes,
                        FUN=function(x) {
                          x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                          weighted.mean(x$repli_p_value > 0.05 | x$repli_pattern_direction==FALSE,w=x$weight,na.rm=TRUE)
                        },
                        clustervar = "paper_id",
                        keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_direction"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        format.percent=TRUE,digits=1
        )$formatted.text
  
      p_repli_null_sig_claims_or_opposite_dir_wtd <-
        bootstrap.clust(data=repli_outcomes,
                        FUN=function(x) {
                          x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                          weighted.mean(x$repli_p_value > 0.05 | x$repli_pattern_direction==FALSE,w=x$weight,na.rm=TRUE)
                        },
                        clustervar = "paper_id",
                        keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_direction"),
                        alpha=.05,tails="two-tailed",iters=iters,
                        format.percent=TRUE,digits=1
        )$formatted.text
  
      n_repli_null_sig_claims_or_opposite_dir <- sum(repli_outcomes$repli_p_value > 0.05 | repli_outcomes$repli_pattern_direction==FALSE)
  
      p_repli_null_sig_claims_or_opposite_dir <-
        bootstrap.clust(data=repli_outcomes,
                        FUN=function(x) {
                          mean(x$repli_p_value > 0.05 | x$repli_pattern_direction==FALSE,na.rm=TRUE)
                        },
                        clustervar = "paper_id",
                        keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_direction"),
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
                          mean(x$repli_p_value <= 0.05 & x$repli_pattern_direction==TRUE,na.rm=TRUE)
                        },
                        clustervar = "paper_id",
                        keepvars=c("paper_id","claim_id","repli_p_value","repli_pattern_direction"),
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
        mutate(weight.estimatable = 1/n())
  
      median_orig_pearsons_both_estimatable_wtd <- round(weighted.median(orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$orig_pearsons_r_value,
                                                                        orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$weight.estimatable,na.rm=TRUE),3)
  
      SD_orig_pearsons_both_estimatable_wtd <- round(sqrt(Hmisc::wtd.var(orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$orig_pearsons_r_value,
      orig_outcomes[!is.na(orig_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$weight.estimatable,na.rm=TRUE)),3)
  
      repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable <- ifelse(repli_outcomes$paper_id %in% papers_pearsons_estimatable_orig_and_repli & !is.na(repli_outcomes$repli_pearsons_r_value),
                                                                            repli_outcomes$paper_id,NA)
      
      repli_outcomes <- repli_outcomes %>%
        group_by(paper_id_orig_and_repli_pearsons_estimatable) %>%
        mutate(weight.estimatable = 1/n())
      
      median_repli_pearsons_both_estimatable_wtd <- round(weighted.median(repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$repli_pearsons_r_value,
                                                                              repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$weight.estimatable,na.rm=TRUE),3)
  
      SD_repli_pearsons_both_estimatable_wtd <- round(sqrt(Hmisc::wtd.var(repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$repli_pearsons_r_value,
                                                                              repli_outcomes[!is.na(repli_outcomes$paper_id_orig_and_repli_pearsons_estimatable),]$weight.estimatable,na.rm=TRUE)),3)
  
      repli_outcomes_merged$repli_pearsons_ratio_v_orig <- repli_outcomes_merged$repli_pearsons_r_value/repli_outcomes_merged$orig_pearsons_r_value
      
      p_median_pearsons_neg_ratio_v_orig_wtd <- 
        bootstrap.clust(data=repli_outcomes_merged,
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
                          mean(x[x$repli_type=="new data",]$repli_p_value<=.05 & x[x$repli_type=="new data",]$repli_pattern_direction==TRUE,na.rm=TRUE)/
                            mean(x[x$repli_type=="secondary data",]$repli_p_value<=.05 & x[x$repli_type=="secondary data",]$repli_pattern_direction==TRUE,na.rm=TRUE)
                        },
                        clustervar = "paper_id",
                        keepvars=c("repli_type","repli_p_value","repli_pattern_direction"),
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
      
      n_papers_available_to_score <- "Pending paper process data pipeline"
      
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
      
    }
    
  
    # Clean up input values and export
    {
      rm(iters,orig_outcomes,repli_outcomes,df.papers,repli_outcomes_merged)
      return(rev(as.list(environment())))
    }
  }
}

# Run tag generation for testing
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
    
    drive_auth(Sys.getenv("google_oauth_email"))
    # Common functions
    source(file="Analysis/common functions.R")
  }
  
  
  # Load data
    objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata")
    for(i in 1:length(objects_to_load)){
      assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
      save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
    }


    # Pull paper to find what tags are in paper
    paper_5_text <- drive_read_string(file=googledrive::as_id("1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg"),
                                      type = "text/plain",encoding="UTF-8")  %>%
      strsplit(split = "(\r\n|\r|\n)") %>%
      .[[1]]
    paper_5_text <- paste0(paper_5_text,collapse="  ")

    # Pull paper to find what tags are calculated
      tags <- unique(str_match_all(paper_5_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
      tags <- tags[tags!=""]
      tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)

    # Generate stats
      results_tagged_stats <- tagged_stats(iters = 20,repli_outcomes_default_subset(),orig_outcomes,df.papers=NA)

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
    sheet_write(data.frame(tags,values_text),
                ss="https://docs.google.com/spreadsheets/d/1iIBhBsbvz89sZCDRFn9wghh17RExMa5XxQPLhlB_Bt8",sheet = "Sheet1")



}
