# Create an object that contains the tagged stats for figure 5
paper_5_stats <- function(iters = 100,repli_outcomes,orig_outcomes,df.papers=NA){

  # Data preparation
  {
    repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                   by="claim_id",all.x=TRUE,all.y=FALSE)
    orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
    repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
    repli_outcomes_merged <- repli_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n())
  }
  
  # Stats
  {
    n_claims <- length(unique(repli_outcomes$claim_id))
    
    n_papers <- length(unique(repli_outcomes$paper_id))
    
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

    n_expected_repli_stat_sig_claims_same_dir	<- "Pending discussion"

    p_expected_repli_stat_sig_claims_same_dir <- "Pending discussion"

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
  }
  

  # Clean up input values and export
  {
    rm(iters,orig_outcomes,repli_outcomes,df.papers,repli_outcomes_merged)
    return(rev(as.list(environment())))
  }
}