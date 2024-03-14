
# Run tag generation for testing
{

  # Initial setup and libraries
  {
    rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
    
    write_to_google_sheets <- FALSE

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
    library(DescTools)
    library(cowplot)

    drive_auth(Sys.getenv("google_oauth_email"))

    
  }


  # Load data
    objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata")
    for(i in 1:length(objects_to_load)){
      assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
      save(list=objects_to_load[i],file=paste0("Analysis/Paper 5/Code and data/Commons/",objects_to_load[i],".RData"))
    }
    
    source(file="Analysis/common functions.R")
    file.copy("Analysis/common functions.R", "Analysis/Paper 5/Code and data/Commons/common functions.R",overwrite = TRUE)
    
    source(file="Analysis/Paper 5/Code and data/tagged stats and figures.R")
    #file.copy("Analysis/Paper 5/Code and data/Commons/tagged stats and figures.R", "Analysis/Data exploration app/tagged stats and figures.R",overwrite = TRUE)
    


    # Pull paper to find what tags are in paper
    paper_text <- drive_read_string(file=googledrive::as_id("1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg"),
                                      type = "text/plain",encoding="UTF-8")  %>%
      strsplit(split = "(\r\n|\r|\n)") %>%
      .[[1]]
    paper_text <- paste0(paper_text,collapse="  ")

    # Pull paper to find what tags are calculated
      tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
      tags <- tags[tags!=""]
      tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)

    # Generate stats
      results_tagged_stats <- tagged_stats(iters = 20,repli_outcomes_default_subset(),orig_outcomes,paper_metadata)

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
      if (write_to_google_sheets){
        sheet_write(data.frame(tags,values_text),
                    ss="https://docs.google.com/spreadsheets/d/1iIBhBsbvz89sZCDRFn9wghh17RExMa5XxQPLhlB_Bt8",sheet = "Paper 5")
      }
    
}

# Figures
{
  # Figure X: Effect size comparisons
  {
    # Data creation
      rr_stat_outcomes_selected <- "Pearson's R"
      repli_outcomes_vs_orig_null <- 0
  
      df.chart <- repli_outcomes_default_subset()
      df.chart.orig <- orig_outcomes
      df.chart.orig$paper_id <- NULL
      
      # Merge in orig data
      df.chart <- merge(df.chart,df.chart.orig,by="claim_id",all.x=TRUE,all.y=FALSE)
      
      df.chart$orig_pearsons_r <- as.numeric(df.chart$orig_pearsons_r_value)
      df.chart$orig_effect_size_value <- as.numeric(df.chart$orig_effect_size_value_repli)
      
      # Gather up new vs originals
      # Pearsons
      df.chart.pearsons <- df.chart[c("paper_id","repli_pearsons_r_value","orig_pearsons_r")]
      df.chart.pearsons$stat_type <- "Pearson's R"
      colnames(df.chart.pearsons) <- c("paper_id","Replication","Original","stat_type")
      
      # All others
      df.chart.others <- df.chart[c("paper_id","repli_effect_size_value","orig_effect_size_value","repli_effect_size_type")]
      colnames(df.chart.others) <- c("paper_id","Replication","Original","stat_type")
      
      # Combine and select
      df.chart <- rbind(df.chart.pearsons,df.chart.others)
      df.chart$pair <- as.character(1:nrow(df.chart))
      df.chart <- na.omit(df.chart)
      df.chart.wide <- df.chart
      df.chart <- df.chart %>% pivot_longer(!"stat_type" & !"pair"& !"paper_id", names_to = "comparison", values_to = "ES_value")
      
      df.chart$comparison <- factor(df.chart$comparison,
                                    labels=c("Replication","Original"),
                                    levels=c("Replication","Original"))
      df.chart <- df.chart[df.chart$stat_type %in% rr_stat_outcomes_selected,]
      
      wm.orig.obj <- bootstrap.clust(data=df.chart[df.chart$comparison=="Original",],
                                     FUN=function(x) {
                                       x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                                       weighted.mean(x$ES_value,x$weight,na.rm=TRUE)
                                     },
                                     clustervar = "paper_id",
                                     keepvars=c("paper_id","comparison","ES_value"),
                                     alpha=.05,tails="two-tailed",iters=200,
                                     format.percent=TRUE,digits=0
      )
      wm.orig <- wm.orig.obj$point.estimate
      
      wm.repli.obj <- bootstrap.clust(data=df.chart[df.chart$comparison=="Replication",],
                                      FUN=function(x) {
                                        x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                                        weighted.mean(x$ES_value,x$weight,na.rm=TRUE)
                                      },
                                      clustervar = "paper_id",
                                      keepvars=c("paper_id","comparison","ES_value"),
                                      alpha=.05,tails="two-tailed",iters=200,
                                      format.percent=TRUE,digits=1
      )
      wm.repli <- wm.repli.obj$point.estimate
      
      wm.diff.obj <- bootstrap.clust(data=df.chart.wide[df.chart.wide$stat_type=="Pearson's R",],
                                     FUN=function(x) {
                                       x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                                       weighted.mean(x$Original-x$Replication,x$weight,na.rm=TRUE)
                                     },
                                     clustervar = "paper_id",
                                     keepvars=c("paper_id","Original","Replication"),
                                     alpha=.05,tails="two-tailed",iters=200,
                                     format.percent=TRUE,digits=0
      )
      
      df.chart <- df.chart %>% group_by(paper_id) %>% mutate(weight = 2/n())
      df.chart.wide <- df.chart.wide %>% group_by(paper_id) %>% mutate(weight = 1/n())
    
    # Chart generation
      p1 <- ggplot(data=df.chart,aes(y=ES_value,x=reorder(comparison, desc(comparison)),fill=reorder(comparison, desc(comparison)))) +
        theme_bw()+
        scale_fill_manual(values=palette_score_charts)+
        geom_hline(aes(yintercept =0),color="grey50")+
        geom_hline(aes(yintercept =1),color="grey50")+
        theme(
          legend.position = "bottom",
          panel.grid = element_blank(),
          axis.line = element_blank(),
          legend.title=element_blank(),
          axis.title.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          aspect.ratio = 1
        )+
        scale_y_continuous(breaks=seq(0,1,1/4))+
        coord_cartesian(ylim=c(0,1))+
        ylab("Pearson's R effect size value")
  
      p1 <- p1 + geom_line(aes(group=pair),color="grey88",show.legend=FALSE)
      
      p1 <- p1 +
        geom_split_violin(show.legend=FALSE,linetype=0,aes(weight=weight))
      
      # p1 <- p1 + 
      #   annotate("rect", xmin = 0.5, xmax = 1, ymin = wm.orig.obj$CI.ub, ymax = 1,
      #            alpha = .4,fill = "white") +
      #   annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = wm.orig.obj$CI.lb,
      #            alpha = .4,fill = "white") +
      #   annotate("rect", xmin = 2, xmax = 2.5, ymin = wm.repli.obj$CI.ub, ymax = 1,
      #            alpha = .4,fill = "white") +
      #   annotate("rect", xmin = 2, xmax = 2.5, ymin = 0, ymax = wm.repli.obj$CI.lb,
      #            alpha = .4,fill = "white")
      
      left_x_center <- .55
      right_x_center <- 2.45
      bar_width <- .04
      bracket_color="grey10"
      # p1 <- p1 +
      #   #geom_segment(y=wm.orig,yend=wm.orig,x=left_x_center-bar_width*2,xend=left_x_center+bar_width*2,color=bracket_color)+
      #   geom_segment(y=wm.orig.obj$CI.lb,yend=wm.orig.obj$CI.ub,x=left_x_center,xend=left_x_center,color=bracket_color) +
      #   geom_segment(y=wm.orig.obj$CI.lb,yend=wm.orig.obj$CI.lb,x=left_x_center-bar_width,xend=left_x_center+bar_width,color=bracket_color)+
      #   geom_segment(y=wm.orig.obj$CI.ub,yend=wm.orig.obj$CI.ub,x=left_x_center-bar_width,xend=left_x_center+bar_width,color=bracket_color)+
      #   #geom_segment(y=wm.repli,yend=wm.repli,x=right_x_center-bar_width*2,xend=right_x_center+bar_width*2,color=bracket_color)+
      #   geom_segment(y=wm.repli.obj$CI.lb,yend=wm.repli.obj$CI.ub,x=right_x_center,xend=right_x_center,color=bracket_color) +
      #   geom_segment(y=wm.repli.obj$CI.lb,yend=wm.repli.obj$CI.lb,x=right_x_center-bar_width,xend=right_x_center+bar_width,color=bracket_color)+
      #   geom_segment(y=wm.repli.obj$CI.ub,yend=wm.repli.obj$CI.ub,x=right_x_center-bar_width,xend=right_x_center+bar_width,color=bracket_color)
      
      p1 <- p1 +
        annotate("text", x=left_x_center, y=wm.orig.obj$point.estimate+.03,
                 label= paste0("Mean:\n",format.round(wm.orig.obj$point.estimate,2)),
                 vjust=0,size=4) +
        annotate("text", x=right_x_center, y=wm.repli.obj$point.estimate+.03,
                 label= paste0("Mean:\n",format.round(wm.repli.obj$point.estimate,2)),
                 vjust=0,size=4)
      
      
      p1 <- p1 + 
        geom_segment(y=wm.orig,yend=wm.orig,x=-Inf,xend=1.47,linetype=2) +
        geom_segment(y=wm.repli,yend=wm.repli,x=1.53,xend=Inf,linetype=2) +
        
        geom_segment(y=wm.orig,yend=wm.orig,x=1.47,xend=1.53,linetype=1,
                     color=MixColor(palette_score_charts[1],palette_score_charts[2],0.5)) +
        geom_segment(y=wm.repli,yend=wm.repli,x=1.47,xend=1.53,linetype=1,
                     color=MixColor(palette_score_charts[1],palette_score_charts[2],0.5)) +
        
        geom_segment(y=wm.orig,yend=wm.repli,x=1.5,xend=1.5,
                     color=MixColor(palette_score_charts[1],palette_score_charts[2],0.5)) +
        

        annotate("text", x=1.45, y=(wm.orig+wm.repli)/2,
               label= "Mean\ndifference:",
                        hjust=1,size=4)+
        annotate("text", x=1.55, y=(wm.orig+wm.repli)/2,
                 label= -round(wm.diff.obj$point.estimate,2),
                 #label= -round(wm.orig-wm.repli,2),
                 hjust=0,size=4)
    
    p2 <- ggplot(data=df.chart.wide[df.chart.wide$stat_type=="Pearson's R",],aes(x=Original,y=Replication)) +
      theme_bw()+
      scale_fill_manual(values=palette_score_charts)+
      geom_segment(aes(x=0,xend=1,y=0,yend=0),color="grey50")+
      geom_segment(aes(x=0,xend=0,y=0,yend=1),color="grey50")+
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line = element_blank(),
        legend.title=element_blank(),
        #axis.title.x = element_blank(),
        panel.border = element_blank(),
        #axis.ticks.x = element_blank(),
        aspect.ratio = 1
      )+
      # scale_y_continuous(breaks=seq(0,1,1/4))+
      # coord_cartesian(ylim=c(0,1))+
      xlab("Original Pearson's R Value")+
      ylab("Replication Pearson's R Value")+
      geom_point(alpha=.6,shape=16,
                 color=MixColor(palette_score_charts[1],palette_score_charts[2],0.5))+
      geom_smooth(method = "glm", fullrange=TRUE,
                  #method.args = list(family = "binomial"),
                  method.args = list(family = "quasibinomial"),
                  color=MixColor(palette_score_charts[1],palette_score_charts[2],0.5),
                  fill=MixColor(palette_score_charts[1],palette_score_charts[2],0.5))
    
    
    
    p3 <- plot_grid(p1, p2,align = "h")
    p3
    
    
  }
  
}
