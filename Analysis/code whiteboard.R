# Startup and initialization
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  
  library(targets)
  library(ggplot2)
  library(ggalluvial)
  library(tidyr)
  library(dplyr)
  library(devtools)
  library(ggsankey)
  library(plotly)
  library(viridis)
  
}

tar_make() # run this to pull new targets
#rmarkdown::render("targets.Rmd")

#tar_load("repli_export")
#tar_load("repli_primary")
tar_load("repli_outcomes")
#tar_load("orig_statistics_dataset_p1")

# #tar_load("repli_export")
# #tar_load("repli_primary")
# tar_load("repli_outcomes")
# #tar_load("orig_statistics_dataset_p1")
# tar_load("orig_dataset")


# Update app data
if (FALSE){
  tar_load("repli_outcomes")
  save(repli_outcomes,file="Analysis/Data exploration app/repli_outcomes.RData")

  tar_load("orig_dataset")
  save(orig_dataset,file="Analysis/Data exploration app/orig_dataset.RData")
  
  tar_load("repro_export")


}


# UI and selection options data
{
  select_rr_type_set <- c("Direct Replication","Data Analytic Replication","Hybrid")
  select_rr_type_labels <- c("Direct Replication","Data Analytic Replication","Hybrid")
  
  select_generalizability_set  <- c(FALSE,TRUE)
  select_generalizability_labels  <- c("Standard","Generalizability study")
  
  select_rr_is_manylabs_set <- c("non_ml","ml_count","ml_instance_primary","ml_aggregation")
  select_rr_is_manylabs_labels <- c("Not ManyLabs","Count","Primary","Aggregation")
  
  select_rr_analytic_sample_stage_set <- c("stage 1","stage 2","threshold","no target","lab target")
  select_rr_analytic_sample_stage_labels <- c("Stage 1","Stage 2","Threshold","No target","Lab target")
}

# Generate selection data functions
{
  df_repli_subsetted <- function(select_rr_type_selection_ui = c(TRUE,FALSE,FALSE),
                                           select_generalizability_selection_ui = c(TRUE,FALSE),
                                           select_rr_is_manylabs_selection_ui = c(TRUE,FALSE,FALSE,FALSE),
                                           select_rr_analytic_sample_stage_selection_ui = c(TRUE,TRUE,FALSE,FALSE,FALSE)){
    
    # Pull data
    #tar_load("repli_export")
    tar_load("repli_outcomes")
    #tar_load("repli_primary")
    
    # Temporary for data "cleaning"
    repli_outcomes$generalizability <- ifelse(repli_outcomes$paper_id %in% c("Br0x", "EQxa", "J4W9", "plLK", "rjb", "zlm2"),
                                            TRUE,FALSE)
    repli_outcomes$claim_id_unique <- paste0(repli_outcomes$paper_id,"_",repli_outcomes$claim_id)
    repli_outcomes$rr_type_internal <- NULL
    
    select_rr_type_selection <- select_rr_type_set[select_rr_type_selection_ui]
    select_generalizability_selection <- select_generalizability_set[select_generalizability_selection_ui]
    select_rr_is_manylabs_selection <- select_rr_is_manylabs_set[select_rr_is_manylabs_selection_ui]
    select_rr_analytic_sample_stage_selection <- select_rr_analytic_sample_stage_set[select_rr_analytic_sample_stage_selection_ui]
    
    # Create selected dataset
    repli_outcomes_selected <- repli_outcomes[repli_outcomes$rr_type %in% select_rr_type_selection &
                                            repli_outcomes$generalizability %in% select_generalizability_selection &
                                            repli_outcomes$rr_is_manylabs %in% select_rr_is_manylabs_selection &
                                            repli_outcomes$rr_analytic_sample_stage %in% select_rr_analytic_sample_stage_selection,]
    # Generate selection weights and set indexes (for combining)
    repli_outcomes_selected <- repli_outcomes_selected %>% 
      group_by(paper_id) %>% 
      mutate(weight_by_paper_id = 1/n())
    repli_outcomes_selected <- repli_outcomes_selected %>% 
      group_by(claim_id_unique) %>% 
      mutate(weight_by_claim_id = 1/n())
  }
}

# Aesthetic functions
{
  GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                             draw_group = function(self, data, ..., draw_quantiles = NULL) {
                               data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                               grp <- data[1, "group"]
                               newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                               newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                               newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                               
                               if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                 stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                           1))
                                 quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                                 aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                 aesthetics$alpha <- rep(1, nrow(quantiles))
                                 both <- cbind(quantiles, aesthetics)
                                 quantile_grob <- GeomPath$draw_panel(both, ...)
                                 ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                               }
                               else {
                                 ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                               }
                             })
  
  geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                                draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                                show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
          position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
          params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
  }
  # Color palette
  {
    palette_weezer_blue <- c("#00a2e7","#dee5cd","#010c09","#083259","#b2915f","#d7b1b7","#00374b","#124e80", "#001212")
    palette_weezer_pinkerton <- c("#d5bf98","#14140b","#70624b","#8a8d82","#304251","#465656","#945a2d","#708090")
    palette_weezer_green <- c("#bece30","#020100","#4f6238","#cac986","#981f2c","#c13f33","#461005")
    palette_weezer_maladroit <- c("#e0dcce","#575b61","#b69e44","#953d31","#e5b066","#343729","#3e3131")
    palette_weezer_make_believe <- c("#000000","#EAECEB","#C2C2C2","#A0A0A0","#313131")
    palette_weezer_red <- c("#ED1B34","#8A817C","#141311","#8B8D9C","#332E28")
    palette_weezer_raditude <- c("#EC2221","#FBFFFB","#FDF600","#CEB181","#4E1110")
    palette_weezer_everything <- c("#E8A662","#F4F5F1","#463D47","#7F3009","#35180E","F6F3CF")
    palette_weezer_white <- c("#FDFDFD","#242424","#E3E3E3","#B6B6B6","#EEEDED")
    palette_weezer_pacific_daydream <- c("#1E3555","#5C6455","#FBE4BC","#1D1F1E","#69797B","#F8E6CF","#F8E6CF")
    palette_weezer_teal <- c("#1DBBBE","#D6A8CD","#F8F8F8","#182633","#90C5DF")
    palette_weezer_black <- c("#2D2B2C","#060606","#E9E7E8","#0E0E0E")
    palette_weezer_ok_human <- c("#B2A17A","#B3B470","#B1A78F","#D1BE8F","#726D5C","#B8B6A6","#5B4F3F")
    palette_weezer_van_weezer <- c("#B2023E","#E933D3","#770265","#170032","#FDF8FF","#170032","#5329F9","#F3FED5")
    
    plaette_score_charts <- c(palette_weezer_blue[1],
                              palette_weezer_red[1],
                              palette_weezer_green[1],
                              palette_weezer_teal[1],
                              
                              palette_weezer_pinkerton[1],
                              palette_weezer_van_weezer[3]
    )
  }
}

# Main 3 parter
{
  tar_load("repli_outcomes")
  df.chart <- repli_outcomes
  #df.chart <- df_repli_subsetted()
  
  # Merge in orig data
  tar_load("orig_dataset")
  df.chart.orig <- orig_dataset[c("unique_claim_id",
                                  "original_effect_size_value_reported",
                                  "original_effect_size_type_reference",
                                  "original_pearsons_r_numeric")]
  df.chart <- merge(df.chart,df.chart.orig,by.x="claim_id",by.y="unique_claim_id",all.x=TRUE,all.y=FALSE)

  df.chart$orig_pearsons_r <- as.numeric(df.chart$original_pearsons_r_numeric)
  df.chart$orig_effect_size_value <- as.numeric(df.chart$original_effect_size_value_reported)
  
  # Gather up new vs originals
  # Pearsons
  df.chart.pearsons <- df.chart[c("repli_pearsons_r_value","orig_pearsons_r")]
  df.chart.pearsons$stat_type <- "Pearson's R"
  colnames(df.chart.pearsons) <- c("Replication","Original","stat_type")
  
  # All others
  df.chart.others <- df.chart[c("repli_effect_size_value","orig_effect_size_value","repli_effect_size_type")]
  colnames(df.chart.others) <- c("Replication","Original","stat_type")
  types_to_keep <- c("ser_method")
  df.chart.others <- df.chart.others[df.chart.others$stat_type %in% types_to_keep,]
  
  # Combine
  df.chart <- rbind(df.chart.pearsons,df.chart.others)
  df.chart <- df.chart %>% pivot_longer(!"stat_type", names_to = "comparison", values_to = "ES_value")
  df.chart <- na.omit(df.chart)
  df.chart$stat_type <- factor(df.chart$stat_type,
                               labels=c("Pearson's R","SER"),
                               levels=c("Pearson's R","ser_method"))
  df.chart$comparison <- factor(df.chart$comparison,
                                labels=c("Replication","Original"),
                                levels=c("Replication","Original"))
  df.chart$jitter_bias <- ifelse(df.chart$comparison=="original",-.2,.2)
  
  ggplot(data=df.chart,aes(y=ES_value,x=stat_type,fill=reorder(comparison, desc(comparison)))) +
    geom_split_violin()+
    geom_point(position=position_jitterdodge(),size=.2)+
    theme_bw()+
    scale_fill_manual(values=plaette_score_charts)+
    geom_hline(aes(yintercept =0),linetype=3,color="#454545")+
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.line = element_line(color="#393939"),
      legend.title=element_blank()
    )+
    scale_y_continuous(expand=c(0,0),limits=c(-0,1))+
    xlab("Statistic type")+
    ylab("Effect size value")
  
}

  
   
# Generate alluvial
{
  # Data generation
  
  tar_load("repli_outcomes")
  tar_load("orig_dataset")
  tar_load("repro_export")
  
  df.alluvial <- orig_dataset[c("paper_id","claim_id")]
  df.alluvial$claim_id <- paste0(df.alluvial$paper_id,"_",df.alluvial$claim_id)
  
  repro.alluvial <- repro_export[c("paper_id","claim_id")]
  repro.alluvial$claim_id <- paste0(repro.alluvial$paper_id,"_",repro.alluvial$claim_id)
  repro.alluvial$paper_id <- NULL
  repro.alluvial$reproduction <- TRUE
  repro.alluvial$reproduction_outcome <- ifelse(repro_export$rr_repro_success_reported=="yes","Reproduced","Not reproduced")
  
  repli.alluvial <- repli_outcomes[c("claim_id")]
  repli.alluvial$replication <- TRUE
  repli.alluvial$replication_outcome <- ifelse(repli_outcomes$repli_pattern_criteria_met==TRUE,"Replicated","Not replicated")
  
  df.alluvial <- merge(df.alluvial,repro.alluvial,by = "claim_id",all.x=TRUE,all.y = FALSE)
  df.alluvial <- merge(df.alluvial,repli.alluvial,by = "claim_id",all.x=TRUE,all.y = FALSE)
  
  
  df.alluvial <- rbind(repro.alluvial,repli.alluvial)
  
  # df.repli.no.hier <- repli_outcomes[c("paper_id","claim_id","is_manylabs","power_for_effect_size")]
  # df.repli.no.hier$claim_id <- paste0(df.repli.no.hier$paper_id,"_", df.repli.no.hier$claim_id)
  
  #df.repli.no.hier <- df.repli.no.hier[df.repli.no.hier$paper_id %in% unique(df.repli.no.hier$paper_id)[60:120],]
  
  df.alluvial <- df.alluvial %>%
    ggsankey::make_long(paper_id,claim_id,type,outcome)
  
  ggplot(df.alluvial, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) + 
    ggsankey::geom_alluvial(flow.alpha = 0.75, show.legend = FALSE,space=1) +
    #ggsankey::geom_sankey()+
    #geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5)+
    theme_bw() + 
    theme(legend.position = "none") +
    theme(axis.title = element_blank()
          , axis.text.x = element_blank()
          , axis.ticks = element_blank()  
          , panel.grid = element_blank()) +
    labs(fill = 'Nodes')+
    coord_flip()
}

library(ggplot2)

ID <- c(1,1,2,2,3,3,4,4,5,5)
group <- c(20,20, 50, 50,20, 20, 80, 80, 80, 80)
condition <- c("med", "placebo","med", "placebo","med", "placebo","med", "placebo","med", "placebo")
PropYes <- c(0.13, 0.15, 0.25, 0.13, 0.54, 0.34, 0.23, 0.45, 0.142, 0.344)
exampleData <- data.frame(ID, group, condition, PropYes)
exampleData <- within(exampleData, {
  group <- as.factor(group)
  condition <- as.factor(condition)
})

pd = position_dodge(width=0)
ggplot(exampleData, aes(x=factor(condition), y=PropYes, 
                        color=factor(group), group=factor(ID))) + 
  geom_point(position=pd) + geom_line(position=pd)


ggplot(df.chart, aes(x=comparison, y=ES_value, 
                        group=factor(group))) + 
  geom_point(position=pd) + geom_line(position=pd)

ggplot(df.chart, aes(x=comparison, y=ES_value, 
                     group=factor(group))) + 
  geom_point() + geom_line()




# Startup and initialization
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  
  library(targets)
  library(ggplot2)
  library(ggalluvial)
  library(tidyr)
  library(dplyr)
  library(devtools)
  library(ggsankey)
  library(plotly)
  library(viridis)
}

tar_load("repli_outcomes")

bootstrap.clust <- function(data=NA,FUN=NA,clustervar=NA,
                            alpha=.05,tails="two-tailed",iters=100){
  # Set up cluster designations
    if(anyNA(clustervar)){ data$cluster.id <- 1:nrow(data) 
    } else { data$cluster.id <- data[[clustervar]] }
    cluster.set <- unique(data$cluster.id)
  # Generate original target variable
    point.estimate <- FUN(data)
  # Create distribution of bootstrapped samples
    estimates.bootstrapped <- replicate(iters,{
      # Generate sample of clusters to include
        clust.list <- sample(cluster.set,length(cluster.set),replace = TRUE)
      # Build dataset from cluster list
        data.clust <- do.call(rbind,lapply(1:length(clust.list), function(x) {
          data[data$cluster.id == clust.list[x],]
        }))
      # Run function on new data
        tryCatch(FUN(data.clust),finally=NA)
    },
    simplify=TRUE)
  # Outcomes measures
    if(is.matrix(estimates.bootstrapped)){
      n_estimates <- nrow(estimates.bootstrapped)
      SE <- unlist(lapply(1:n_estimates,function(x) {sd(estimates.bootstrapped[x,],na.rm = TRUE)}))
      if (tails == "two-tailed"){
        CI.lb <- unlist(lapply(1:n_estimates,function(x) {quantile(estimates.bootstrapped[x,], alpha/2,na.rm = TRUE)}))
        CI.ub <- unlist(lapply(1:n_estimates,function(x) {quantile(estimates.bootstrapped[x,], 1-alpha/2,na.rm = TRUE)}))
      } else if (tails == "one-tailed, upper"){
        CI.lb <- unlist(lapply(1:n_estimates,function(x) {NA}))
        CI.ub <- unlist(lapply(1:n_estimates,function(x) {quantile(estimates.bootstrapped[x,], 1-alpha,na.rm = TRUE)}))
      } else if (tails == "one-tailed, lower"){
        CI.lb <- unlist(lapply(1:n_estimates,function(x) {quantile(estimates.bootstrapped[x,], alpha,na.rm = TRUE)}))
        CI.ub <- unlist(lapply(1:n_estimates,function(x) {NA}))
      }
    } else {
      SE <- sd(estimates.bootstrapped,na.rm = TRUE)
      if (tails == "two-tailed"){
        CI.lb <- quantile(estimates.bootstrapped, alpha/2,na.rm = TRUE)
        CI.ub <- quantile(estimates.bootstrapped, 1-alpha/2,na.rm = TRUE)
      } else if (tails == "one-tailed, upper"){
        CI.lb <- NA
        CI.ub <- quantile(estimates.bootstrapped, 1-alpha,na.rm = TRUE)
      } else if (tails == "one-tailed, lower"){
        CI.lb <- quantile(estimates.bootstrapped, alpha,na.rm = TRUE)
        CI.ub <- NA
      }
    }
    
  # Outputs
    return(list("point.estimate"=point.estimate,"SE"=SE,
                "CI.lb"=CI.lb,"CI.ub"=CI.ub,"estimates.bootstrapped"=estimates.bootstrapped))
}

test <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_pattern_criteria_met")],FUN=function(data) {
  mean(data$repli_pattern_criteria_met,na.rm=TRUE)
},clustervar = "paper_id")
test$point.estimate

test <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_pattern_criteria_met")],FUN=function(data) {
  mean(data$repli_pattern_criteria_met,na.rm=TRUE)
},clustervar = "paper_id")
test$point.estimate

test <- bootstrap.clust(data=repli_outcomes[c("paper_id","claim_id","repli_pattern_criteria_met","repli_type")],FUN=function(data) {
  reg <- lm("repli_pattern_criteria_met ~ repli_type",data=data)
  reg$coefficients
},clustervar = "paper_id")
test$point.estimate

test <- lm("repli_pattern_criteria_met ~ repli_type",data=repli_outcomes)



repli_outcomes$new_data <- repli_outcomes$repli_type=="new data"
table(repli_outcomes$repli_pattern_criteria_met,repli_outcomes$new_data)

rr <- (sum(repli_outcomes[repli_outcomes$repli_type=="new data",]$repli_pattern_criteria_met==TRUE)/sum(repli_outcomes$repli_type=="new data"))/
(sum(repli_outcomes[repli_outcomes$repli_type=="secondary data",]$repli_pattern_criteria_met==TRUE)/sum(repli_outcomes$repli_type=="secondary data"))

test$coefficients
