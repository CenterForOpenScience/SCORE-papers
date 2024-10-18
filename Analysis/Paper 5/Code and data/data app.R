
# Setup
{
  # Libraries
  {
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
    library(zcurve)
    library(ggridges)
    library(cowplot)
    library(tidyverse)
    library(metafor)
    library(glue)
  }
  
  # Data loading
  {
    # Check if this is being run from shinyapps.io or from the github folder (in
    # which case the data gets pulled from the targets output)
    objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata")
    if (file.exists("Analysis/common functions.R")) {
      # Being run from github/locally, get raw data and copy data files into
      # same level folder for uploading
      run_location <- "local"
      for(i in 1:length(objects_to_load)){
        assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
        save(list=objects_to_load[i],file=paste0("Analysis/Paper 5/Code and data/Commons/",objects_to_load[i],".RData"))
      }
      
      source(file="Analysis/common functions.R")
      file.copy("Analysis/common functions.R", "Analysis/Paper 5/Code and data/Commons/common functions.R",overwrite = TRUE)
      
      source(file="Analysis/Paper 5/Code and data/tagged stats and figures.R")
      #file.copy("Analysis/Paper 5/Code and data/Commons/tagged stats and figures.R", "Analysis/Data exploration app/tagged stats and figures.R",overwrite = TRUE)
      
    } else {
      # Being run on shinyapps.io; data files already in folder
      run_location <- "server"
      for(i in 1:length(objects_to_load)){
        load(file=paste0("Commons/",objects_to_load[i],".RData"))
      }
      source("Commons/common functions.R")
      source("tagged stats and figures.R")
      drive_deauth()
    }
  }
  
  # Data manipulation and other setup
  {
    # RR UI and selection options data
    {
      select_repli_type_set <- c("new data","secondary data")
      select_repli_type_labels <- c("New data","Secondary data")
      select_repli_type_selected_default <- c("new data","secondary data")
      
      select_repli_version_of_record_set  <- c(TRUE,FALSE)
      select_repli_version_of_record_labels  <- c("VoR","Not VoR")
      select_repli_version_of_record_selected_default <- c(TRUE)
      
      select_repli_is_generalizability_set  <- c(FALSE,TRUE)
      select_repli_is_generalizability_labels  <- c("Standard","Generalizability study")
      select_repli_is_generalizability_selected_default <- c(FALSE)
      
      select_manylabs_set <- c("not_manylabs","ml_aggregation","ml_count","ml_instance_primary")
      select_manylabs_labels <- c("Not ManyLabs","ManyLabs Aggregation","ManyLabs Count","ManyLabs Instance Primary")
      select_manylabs_selected_default <- c("not_manylabs","ml_aggregation")
      
      select_power_for_effect_size_set <- c("50% for 100%","90% for 50%","90% for 75%","lab power analysis","not performed")
      select_power_for_effect_size_labels <- c("50% for 100%","90% for 50%","90% for 75%", "lab power analysis","Not performed")
      select_power_for_effect_size_selected_default <- select_power_for_effect_size_set
    }
  }
  
}

ui <- {
  
  fluidPage(title = "SCORE data visualization playground",
          page_sidebar(
            theme = bs_theme(bootswatch = "minty"),
            # Dataset sidebar ----
            sidebar = sidebar(
              h3("Calculation options"),
              numericInput("repli_bootstrap_iterations",
                           "Bootstrap iterations",
                           value=10,min=10,max=2000,step=5
              ),
              h3("Dataset selection"),
              checkboxGroupInput("select_repli_type_selected",
                                 "Replication type (repli_type)",
                                 choiceNames = unique(select_repli_type_labels),
                                 choiceValues = unique(select_repli_type_set),
                                 selected = c(select_repli_type_selected_default)
              ),
              checkboxGroupInput("select_repli_version_of_record_selected",
                                 "Version of Record (repli_version_of_record)",
                                 choiceNames = unique(select_repli_version_of_record_labels),
                                 choiceValues = unique(select_repli_version_of_record_set),
                                 selected = c(select_repli_version_of_record_selected_default)
              ),
              checkboxGroupInput("select_repli_is_generalizability_selected",
                                 "Generalizability (repli_is_generalizability)",
                                 choiceNames = unique(select_repli_is_generalizability_labels),
                                 choiceValues = unique(select_repli_is_generalizability_set),
                                 selected = c(select_repli_is_generalizability_selected_default)
              ),
              checkboxGroupInput("select_manylabs_selected",
                                 "ManyLabs (is_manylabs, manylabs_type)",
                                 choiceNames = unique(select_manylabs_labels),
                                 choiceValues = unique(select_manylabs_set),
                                 selected = c(select_manylabs_selected_default)
              ),
              checkboxGroupInput("select_power_for_effect_size_selected",
                                 "Power for effect size (power_for_effect_size)",
                                 choiceNames = unique(select_power_for_effect_size_labels), 
                                 choiceValues = unique(select_power_for_effect_size_set),
                                 selected = c(select_power_for_effect_size_selected_default)
              ),
              
            ),
            # Main page ----
            navbarPage("",
                       tabPanel("Tagged statistics",
                                p("Paper 5 here: https://docs.google.com/document/d/1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg/edit"),
                                p("May take a moment to load due to bootstrap iterations..."),
                                DTOutput("paper_5_stats_table")
                       ),
                       tabPanel("Data properties",
                                htmlOutput("repli_data_text")
                       ),
                       tabPanel("Figure 1: Binary replication measures",
                                plotOutput("repli_binary_measures"),
                       ),
                       tabPanel("Figure 2: Replication effect sizes vs original",
                                plotOutput("repli_effect_sizes_vs_orig"),
                       ),
                       tabPanel("Figure 3: Replication effect size distributions vs original by field",
                                plotOutput("repli_effect_sizes_vs_orig_by_field"),
                       ),
                       # tabPanel("Dataset",
                       #          DTOutput("repli_data_table")
                       # ),
                       # 
                       # tabPanel("Chart: repli vs original ES",
                       #          plotOutput("repli_outcomes_vs_orig"),
                       #          h4("Options:"),
                       #          fluidRow(
                       #            column(4,
                       #                   #checkboxGroupInput(
                       #                   radioButtons(
                       #                     "rr_stat_outcomes_selected", "Effect size stats types",
                       #                     choiceNames = c("Pearson's R",unique(as.character(repli_outcomes$repli_effect_size_type))),
                       #                     choiceValues = c("Pearson's R",unique(as.character(repli_outcomes$repli_effect_size_type))),
                       #                     selected = c("Pearson's R"),
                       #                     inline=FALSE
                       #                   )
                       #            ),
                       #            column(1),
                       #            column(7,
                       #                   checkboxInput("repli_outcomes_vs_orig_abs",
                       #                                 "Take absolute value of effect size",TRUE),
                       #                   p("Chart elements:"),
                       #                   checkboxInput("repli_outcomes_vs_orig_weighted_medians",
                       #                                 "Median effect size lines (weighted)",TRUE),
                       #                   checkboxInput("repli_outcomes_vs_orig_smoothed_dist",
                       #                                 "Smoothed distributions",TRUE),
                       #                   checkboxInput("repli_outcomes_vs_orig_points",
                       #                                 "Raw data points",TRUE),
                       #                   checkboxInput("repli_outcomes_vs_orig_lines",
                       #                                 "Lines",TRUE),
                       #                   checkboxInput("repli_outcomes_vs_orig_points_jitter",
                       #                                 "Jittered raw data points",FALSE),
                       #                   checkboxInput("repli_outcomes_vs_orig_dotplot",
                       #                                 "Dot plot",FALSE),
                       #                   
                       #                   p("Chart extent limits"),
                       #                   fluidRow(
                       #                     column(6,numericInput("repli_outcomes_vs_orig_lb",
                       #                                           "Lower bound",0)),
                       #                     column(6,numericInput("repli_outcomes_vs_orig_ub",
                       #                                           "Upper bound",1))
                       #                   ),
                       #                   numericInput("repli_outcomes_vs_orig_null",
                       #                                "Null value",0)
                       #            )
                       #          )
                       # ),
                       # tabPanel("Chart: Repli success vars",
                       #          plotOutput("repli_success_vars"),
                       #          h4("Options:"),
                       # ),
                       # tabPanel("Chart: Repli success vars vs sample size",
                       #          
                       #          selectInput("repli_success_sample_size_var", "Select success outcome variable",
                       #                      choices = c("SCORE criteria","Pattern criteria","Interpretation supported","Repli point estimate in orig CI","Orig point estimate in repli CI")),
                       #          plotOutput("repli_success_sample_size")
                       #          
                       # )
                  )
                )

  )
}

server <- function(input, output, session) {
  # Data generation
  df_repli_subsetted <- reactive({
    df <- repli_outcomes
    
    df <- df[df$repli_type %in% input$select_repli_type_selected,]
    df <- df[df$repli_version_of_record %in% input$select_repli_version_of_record_selected,]
    df <- df[df$repli_is_generalizability %in% input$select_repli_is_generalizability_selected,]
    
    if ("not_manylabs" %in% input$select_manylabs_selected){
      df[df$manylabs_type %in% input$select_manylabs_selected | df$is_manylabs==FALSE,]
    } else {
      df <- df[df$manylabs_type %in% input$select_manylabs_selected,]
    }
    
    df <- df[df$power_for_effect_size %in% input$select_power_for_effect_size_selected,]
    
    df
  })
  # Objects / charts / figures
  output$repli_outcomes_vs_orig <- renderPlot({
    df.chart <- df_repli_subsetted()
    df.chart.orig <- orig_outcomes
    
    # Merge in orig data
    df.chart <- merge(df.chart,df.chart.orig,by="claim_id",all.x=TRUE,all.y=FALSE)
    
    df.chart$orig_pearsons_r <- as.numeric(df.chart$orig_pearsons_r_value)
    df.chart$orig_effect_size_value <- as.numeric(df.chart$orig_effect_size_value_repli)
    
    # Gather up new vs originals
    # Pearsons
    df.chart.pearsons <- df.chart[c("repli_pearsons_r_value","orig_pearsons_r")]
    df.chart.pearsons$stat_type <- "Pearson's R"
    colnames(df.chart.pearsons) <- c("Replication","Original","stat_type")
    
    # All others
    df.chart.others <- df.chart[c("repli_effect_size_value","orig_effect_size_value","repli_effect_size_type")]
    colnames(df.chart.others) <- c("Replication","Original","stat_type")
    
    # Combine and select
    df.chart <- rbind(df.chart.pearsons,df.chart.others)
    df.chart$pair <- as.character(1:nrow(df.chart))
    df.chart <- na.omit(df.chart)
    df.chart <- df.chart %>% pivot_longer(!"stat_type" & !"pair", names_to = "comparison", values_to = "ES_value")
    
    df.chart$comparison <- factor(df.chart$comparison,
                                  labels=c("Replication","Original"),
                                  levels=c("Replication","Original"))
    df.chart <- df.chart[df.chart$stat_type %in% input$rr_stat_outcomes_selected,]
    
    # Add options
    if (input$repli_outcomes_vs_orig_abs==TRUE){
      df.chart$ES_value <- abs(df.chart$ES_value)
    }
    
    # Chart generation
    p <- ggplot(data=df.chart,aes(y=ES_value,x=reorder(comparison, desc(comparison)),fill=reorder(comparison, desc(comparison)))) +
      theme_bw()+
      scale_fill_manual(values=palette_score_charts)+
      geom_hline(aes(yintercept =input$repli_outcomes_vs_orig_null),linetype=3,color="#454545")+
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line = element_line(color="#393939"),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        aspect.ratio = 1
      )+
      scale_y_continuous(breaks=seq(input$repli_outcomes_vs_orig_lb,input$repli_outcomes_vs_orig_ub,(input$repli_outcomes_vs_orig_ub-input$repli_outcomes_vs_orig_lb)/5))+
      coord_cartesian(ylim=c(input$repli_outcomes_vs_orig_lb,input$repli_outcomes_vs_orig_ub))+
      ylab("Effect size value")
    
    pd = position_dodge(width=0.4)
    if (input$repli_outcomes_vs_orig_lines == TRUE){
      p <- p + geom_line(aes(group=pair),color="grey",show.legend=FALSE)
    }
    if (input$repli_outcomes_vs_orig_smoothed_dist == TRUE){
      p <- p + geom_split_violin(show.legend=FALSE)
    }
    
    if (input$repli_outcomes_vs_orig_points_jitter == TRUE){
      p <- p + geom_point(position=position_jitterdodge(),size=1,show.legend=FALSE)
    }
    if (input$repli_outcomes_vs_orig_points == TRUE){
      p <- p + geom_point(position=pd,size=1,show.legend=FALSE)
    }
    if (input$repli_outcomes_vs_orig_dotplot == TRUE){
      p <- p+geom_dotplot(binaxis = "y",
                          stackdir = "center",
                          dotsize = 0.5,show.legend=FALSE)
    }
    if (input$repli_outcomes_vs_orig_weighted_medians == TRUE){
      wm.orig <- weighted.median(df.chart[df.chart$comparison=="Original",]$ES_value,na.rm=TRUE)
      wm.repli <- weighted.median(df.chart[df.chart$comparison=="Replication",]$ES_value,na.rm=TRUE)
      
      p <- p + geom_segment(y=wm.orig,yend=wm.orig,x=-Inf,xend=1.47,linetype=3)
      p <- p + geom_segment(y=wm.repli,yend=wm.repli,x=1.53,xend=Inf,linetype=3)
      
      p <- p + geom_segment(y=wm.orig,yend=wm.orig,x=1.47,xend=1.53,linetype=1)
      p <- p + geom_segment(y=wm.repli,yend=wm.repli,x=1.47,xend=1.53,linetype=1)
      
      p <- p + geom_segment(y=wm.orig,yend=wm.repli,x=1.5,xend=1.5)
      
    }
    
    p
  })
  
  output$repli_success_vars <- renderPlot({
    df <- df_repli_subsetted()
    
    df$repli_interpret_supported_yes <- df$repli_interpret_supported=="yes"
    
    criteria_vars <- c("repli_score_criteria_met","repli_pattern_criteria_met","repli_interpret_supported_yes")
    criteria_vars_labels <- c("SCORE criteria","Pattern criteria","Interpretation supported")
    
    df.chart <- do.call(rbind,lapply(1:length(criteria_vars),function(x) {
      mean.repli.success.weighted <- bootstrap.clust(data=df[c("paper_id","claim_id",criteria_vars[x])],FUN=
                                                       function(data) {
                                                         data <- data %>% add_count(paper_id)
                                                         data$weight <- 1/data$n
                                                         weighted.mean(data[[criteria_vars[x]]],data$weight,na.rm=TRUE)
                                                         
                                                       }, 
                                                     clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=input$repli_bootstrap_iterations)
      df.chart <- data.frame(mean.repli.success.weighted$estimates.bootstrapped)
      colnames(df.chart) <- c("estimates_bootstrapped")
      df.chart$type <- criteria_vars[x]
      df.chart
    }))
    
    df.chart$type <- factor(df.chart$type,labels=criteria_vars_labels,levels=criteria_vars)
    
    ggplot(df.chart,aes(x=estimates_bootstrapped,fill=type))+
      theme_bw()+
      scale_fill_manual(values=palette_score_charts)+
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line = element_line(color="#393939"),
        legend.title=element_blank(),
        #axis.title.y = element_blank(),
        panel.border = element_blank()
      )+
      xlab("Percent meeting replication success criteria")+
      ylab("Density")+
      coord_cartesian(xlim=c(0,1))+
      scale_x_continuous(labels = scales::percent,expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      geom_density(alpha=.5)
  })
  
  output$repli_success_sample_size <- renderPlot({
    df.chart <- df_repli_subsetted()
    
    # Merge in orig data
    df.chart <- merge(df.chart,orig_outcomes,by="claim_id",all.x=TRUE,all.y=FALSE)
    
    # Sample size calc
    df.chart$log_sample_size_ratio <- log(df.chart$repli_sample_size_value / df.chart$orig_sample_size_value)
    
    # Outcome vars
    
    if (input$repli_success_sample_size_var=="SCORE criteria"){
      df.chart$outcome <- as.numeric(df.chart$repli_score_criteria_met)
    } else if (input$repli_success_sample_size_var=="Pattern criteria"){
      df.chart$outcome <- as.numeric(df.chart$repli_pattern_criteria_met)
    } else if (input$repli_success_sample_size_var=="Interpretation supported"){
      df.chart$outcome <- as.numeric(df.chart$repli_interpret_supported=="yes")
    } else if (input$repli_success_sample_size_var=="Repli point estimate in orig CI"){
      df.chart$outcome <- as.numeric(df.chart$repli_effect_size_value >= df.chart$orig_effect_size_ci_lb & df.chart$repli_effect_size_value <= df.chart$orig_effect_size_ci_ub)
    } else if (input$repli_success_sample_size_var=="Orig point estimate in repli CI"){
      df.chart$outcome <- as.numeric(df.chart$orig_effect_size_value_repli >= df.chart$repli_effect_size_ci_lb & df.chart$orig_effect_size_value_repli <= df.chart$repli_effect_size_ci_ub)
    }
    
    ggplot(df.chart,aes(x=log_sample_size_ratio,y=outcome))+
      theme_bw()+
      scale_fill_manual(values=palette_score_charts)+
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line = element_line(color="#393939"),
        legend.title=element_blank(),
        panel.border = element_blank()
      )+
      geom_vline(xintercept=0,linetype=2)+
      ylab("Percent meeting replication success criteria")+
      xlab("Log ratio sample size of replication : original\nHigher indicates greater relative sample size in replication vs original")+
      scale_x_continuous(expand=c(0,0))+
      scale_y_continuous(expand=c(0,0),labels = scales::percent)+
      coord_cartesian(ylim=c(0, 1),xlim=c(-2,2))+
      geom_smooth(method = "loess")
  })
  
  output$repli_data_table <- renderDT(df_repli_subsetted(), options = list(lengthChange = FALSE),rownames= FALSE)
  
  output$repli_data_text <- renderText({
    df <- df_repli_subsetted()
    
    text <- paste0("Replications (n): ",nrow(df))
    text <- paste0(text,"<br/>","Papers (n): ",length(unique(df$paper_id)))
    text <- paste0(text,"<br/>","Claims (n): ",length(unique(df$claim_id)))
    HTML(text)
  })
  
  output$paper_5_stats_table <- renderDT({
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
    
    values_text <- tagged_stats(input$repli_bootstrap_iterations,
                                repli_outcomes = df_repli_subsetted(),
                                orig_outcomes = orig_outcomes,
                                paper_metadata = paper_metadata)
    # Generate list of tags
    values_text <- do.call(c,lapply(1:length(tags),function(x) {
      tag_to_find <- tags[x]
      if(tag_to_find %in% names(values_text)){
        as.character(values_text[[tag_to_find]])
      } else {
        "MISSING"
      }
    }))
    
    output_table <- data.frame(tags,values_text)
    output_table
    
  },options = list(pageLength=-1,
                   lengthChange = FALSE),
  rownames= FALSE
  )
  
  output$repli_binary_measures <- renderPlot({
    # Original within repli
    {
      orig_within <- repli_outcomes %>% 
        select(claim_id, paper_id,repli_conv_r_lb, repli_conv_r_ub) %>% 
        left_join(orig_outcomes %>% select(claim_id, orig_conv_r), by = "claim_id") %>% 
        mutate(orig_wthn = between(orig_conv_r, repli_conv_r_lb, repli_conv_r_ub)) %>% 
        drop_na(orig_wthn)
      
      
      orig_within$orig_conv_r_rescaled <- do.call(c,lapply(1:nrow(orig_within), function(i) {
        scales::rescale(orig_within$orig_conv_r[i],from=c(orig_within$repli_conv_r_lb[i],orig_within$repli_conv_r_ub[i]),to=c(-1,1))
      }))
      
      
      build <- ggplot2::ggplot_build(ggplot(data=orig_within,aes(x=orig_conv_r_rescaled))+
                                       geom_density()+
                                       geom_vline(xintercept=c(-1,1),linetype=3)+
                                       xlim(c(-4,4)))
      
      df_breaks <- build$data[[1]] %>% 
        mutate(status = case_when(x < -1 ~ 'class1', # Arbitrary value, just as an example
                                  x > 1 ~ 'class2',
                                  TRUE ~ 'class3'))
      p.orig_within <- df_breaks %>% 
        ggplot() +
        geom_area(
          aes(x = x,y=y,fill = status),
          linetype=0
        )+
        scale_fill_manual(values=c(palette_score_charts[7],palette_score_charts[7],palette_score_charts[1]))+
        #scale_alpha_manual(values=c(.2,.2,1))+
        geom_vline(xintercept=c(-1,1),linetype=3)+
        theme_minimal()+
        scale_x_continuous(limits=c(-4,4),breaks=c(-1,1),labels=c("Replication\n95% CI Lower Bound","Replication\n95% CI Upper Bound"))+
        theme(legend.position = "none",
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.title=element_blank(),
              axis.text.y=element_blank()
              #axis.text = element_blank(),
              #axis.line = element_blank(),
              #axis.ticks = element_blank()
        )+
        annotate(geom="text",x=0,y=0.02,label="X% of Original effect sizes\nwithin 95% CI of Replication effect sizes",color="black",vjust=0)+
      ylim(c(0,.4))
    }
    
    # Repli within orig
    {
      repli_within <- orig_outcomes %>% 
        select(claim_id, paper_id,orig_conv_r_lb, orig_conv_r_ub) %>% 
        left_join(repli_outcomes %>% select(claim_id, repli_conv_r), by = "claim_id") %>% 
        mutate(repli_wthn = between(repli_conv_r, orig_conv_r_lb, orig_conv_r_ub)) %>% 
        drop_na(repli_wthn)
    }
    
    # Score criteria
    {
      score_criteria <- repli_outcomes %>% 
        select(claim_id, paper_id, repli_score_criteria_met,repli_pattern_criteria_met, repli_p_value) %>%
        mutate(sig = repli_p_value <=.05,
               dir = repli_pattern_criteria_met,
               score_criteria_recalc = sig & dir,
               agreement = repli_score_criteria_met==score_criteria_recalc)
    }
    
    # Analyst interpretation
    {
      analyst_interpretation_supported <- repli_outcomes %>% 
        select(claim_id, paper_id,analyst_interpretation_supported= repli_interpret_supported) %>% 
        filter(analyst_interpretation_supported != "complicated") %>% 
        mutate(analyst_interpretation_supported=analyst_interpretation_supported=="yes")
    }
    
    # Meta analytic setup
    {
      meta <-  function(x) {
        es <- escalc(
          measure = "GEN",
          yi = c(x[1], x[2]),
          sei = c(x[3], x[4])
        )
        mod <- rma(yi = yi, vi = vi, data = es, method = "FE")
        c(mod$b, mod$ci.lb, mod$ci.ub) %>% as.character() %>% str_c(., collapse = ", ")
      }
      
      repli_meta <- repli_outcomes %>% 
        mutate(repli_df1 = ifelse(is.na(repli_effective_df1), repli_stat_dof_1, repli_effective_df1)) %>% 
        mutate(repli_size = ifelse(is.na(repli_effective_sample_size), repli_sample_size_value, repli_effective_sample_size)) %>% 
        mutate(
          repli_se = ifelse(
            repli_stat_type == "t",
            sqrt((1 - repli_conv_r^2)/repli_df1),
            sqrt((1 - repli_conv_r^2)/repli_size)
          )
        )
      orig_meta <- orig_outcomes %>% 
        mutate(orig_df1 = ifelse(is.na(original_effective_df1_reference), orig_stat_dof_1, original_effective_df1_reference)) %>% 
        mutate(orig_size = ifelse(is.na(original_effective_sample_size), orig_sample_size_value, original_effective_sample_size)) %>% 
        mutate(
          orig_se = ifelse(
            orig_stat_type == "t",
            sqrt((1 - orig_conv_r^2)/orig_df1),
            sqrt((1 - orig_conv_r^2)/orig_size)
          )
        )
      combined_meta <- repli_meta %>% 
        select(report_id, paper_id, claim_id, repli_conv_r, repli_se) %>% 
        left_join(orig_meta %>% select(claim_id, orig_conv_r, orig_se), by = "claim_id") %>% 
        drop_na(repli_se) %>% 
        drop_na(orig_se) %>% 
        mutate(es = select(., c(repli_conv_r, orig_conv_r, repli_se, orig_se)) %>% apply(1, meta)) %>% 
        separate(es, into = c("b", "lb_b", "ub_b"), sep = ", ") %>% 
        mutate(across(.cols = c(b, lb_b, ub_b), as.numeric))
    }
    
    # Meta analytic success (same sign and direction of effect)
    {
      metaanalytic <- combined_meta %>% 
        mutate(sig = sign(lb_b) == sign(ub_b)) %>% 
        mutate(dir = sign(orig_conv_r) == sign(lb_b)) %>% 
        mutate(meta_success = sig & dir)
    }
    
    # Replication within original prediction interval
    {
      # repli within original prediction interval
      # defining prediction intervals as done in https://osf.io/z4gjn:
      # comparison_es_lb_pi_nativeunits <- es_o - se_comb*qnorm(.975)
      # comparison_es_ub_pi_nativeunits <- es_o + se_comb*qnorm(.975)
      # NOTE: these can go above 1 and below -1, so just manually capping them for now
      repli_within_orig_pred <- combined_meta %>% 
        mutate(se_comb = select(., c(orig_se, repli_se)) %>% apply(1, function(x) sqrt((x[1])^2 + (x[2])^2))) %>% 
        mutate(
          o_lb_pi = select(., c(orig_conv_r, se_comb)) %>% apply(1, function(x) x[1] - (x[2])*qnorm(.975)),
          o_ub_pi = select(., c(orig_conv_r, se_comb)) %>% apply(1, function(x) x[1] + (x[2])*qnorm(.975))
        ) %>% 
        mutate(
          o_lb_pi = ifelse(o_lb_pi < -1, -1, o_lb_pi),
          o_ub_pi = ifelse(o_ub_pi > 1, 1, o_ub_pi),
        ) %>% 
        mutate(wthn_pi = between(repli_conv_r, o_lb_pi, o_ub_pi))
    }
    
    # Graphics functions
    {
      rescaled_density <- function(plotdata = repli_within,
                                   xvar = "repli_conv_r",
                                   lb = "orig_conv_r_lb",
                                   ub = "orig_conv_r_ub",
                                   bounds = 4,
                                   axis.labels = c("Original\n95% CI Lower Bound","Original\n95% CI Upper Bound"),
                                   fill.color = palette_score_charts[2],
                                   alpha = .6,
                                   ymax = .4) {
        
        plotdata$xvar <- plotdata[[xvar]]
        plotdata$lb <- plotdata[[lb]]
        plotdata$ub <- plotdata[[ub]]
        
        # Rescale
        plotdata$x_rescaled <- do.call(c,lapply(1:nrow(plotdata), function(i) {
          scales::rescale(plotdata$xvar[i],from=c(plotdata$lb[i],plotdata$ub[i]),to=c(-1,1))
        }))
        
        
        ggplot(data=plotdata,aes(x=x_rescaled))+
          geom_density(fill=fill.color,linetype=0,size=0)+
          annotate("rect",xmin=-bounds,xmax=-1,ymin=0,ymax=ymax,
                   fill="white",alpha=alpha)+
          annotate("rect",xmin=1,xmax=bounds,ymin=0,ymax=ymax,
                   fill="white",alpha=alpha)+
          geom_vline(xintercept=c(-1,1),linetype=3)+
          xlim(c(-bounds,bounds))+
          ylim(c(0,ymax))+
          theme_minimal()+
          scale_x_continuous(limits=c(-bounds,bounds),
                             breaks=c(-1,1),
                             labels=axis.labels)+
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title=element_blank(),
                axis.text.y=element_blank()
          )
      }
      
      simple_bar <- function(data,xvar,
                             fill.color=palette_score_charts[7],
                             background.color="grey90",ypadding=0.5,
                             display_percent=TRUE){
        
        proportion <- sum(data[[xvar]])/sum(!is.na(data[[xvar]]))
        
        plot <- ggplot()+
          annotate("rect",xmin=0,xmax=1,ymin=0,ymax=1,fill=background.color)+
          funkyheatmap::geom_rounded_rect(aes(xmin=0,xmax=proportion,ymin=0,ymax=1),
                                          #fill=fill.color,
                                          radius=unit(3, "points"),show.legend=FALSE,
                                          color="black",
                                          size=0)+
          theme_light() +
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title=element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
          )+
          ylim(c(-ypadding,1+ypadding))
        
        if(display_percent==TRUE){
          plot <- plot+
            annotate("text",x=proportion+.01,y=0.5,vjust=0.5,hjust=0,
                     label=paste0(format.round(proportion*100,1),"%")
                     )
        plot
        }
        
    }
    }
    
    # Plots
    {
      colwidths <- c(1,2,2)
      
      row0 <- plot_grid(ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="Replication rate measure",size=6),
                        ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="Summary %",size=6),
                        ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="Detailed distribution data",size=6),
                        nrow=1,rel_widths = colwidths
      )
      
      row1 <- plot_grid(ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="% Original Effect Sizes\nwithin Replication 95% CI"),
                        simple_bar(orig_within,"orig_wthn"),
                        rescaled_density(plotdata=orig_within,"orig_conv_r","repli_conv_r_lb","repli_conv_r_ub",
                                         axis.labels = c("Replication 95% CI\nLower Bound","Replication 95% CI\nUpper Bound"),
                                         fill.color = palette_score_charts[3]),
                        nrow=1,rel_widths = colwidths
                        )
      
      row2 <- plot_grid(ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="% Replication Effect Sizes\nwithin Original 95% CI"),
                        simple_bar(repli_within,"repli_wthn"),
                        rescaled_density(plotdata=repli_within,"repli_conv_r","orig_conv_r_lb","orig_conv_r_ub",
                                         axis.labels = c("Original 95% CI\nLower Bound","Original 95% CI\nUpper Bound"),
                                         fill.color = palette_score_charts[4],),
                        nrow=1,rel_widths = colwidths
      )
      
      row3 <- plot_grid(ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="% Replication Effect Sizes\nwithin Predicted 95% CI"),
                        simple_bar(repli_within_orig_pred,"wthn_pi"),
                        rescaled_density(plotdata=repli_within_orig_pred,"repli_conv_r","o_lb_pi","o_ub_pi",
                                         axis.labels = c("Prediction\n95% CI\nLower Bound","Prediction\n95% CI\nUpper Bound"),
                                         fill.color = palette_score_charts[6],),
                        nrow=1,rel_widths = colwidths
      )
      
      # Data wrangling for  bars
      {
        cat <- c(
          "Both same direction\nand p-value ≤ .05",
          "Replication p-value ≤ .05",
          "Replication and original\neffect size\nin same direction",
          "Neither same\ndirection nor\np-value ≤ .05")
        cat2 <- c("Both",
                  "Either",
                  "Either",
                  "Neither")
        cat3 <- c(
          NA,"Sig only",
          "Dir only",NA)
  
        nesting.structure <- data.frame(cat,cat2,cat3)
        nesting.structure$cat <- ordered(nesting.structure$cat,levels=cat,labels=cat)
        nesting.structure$cat2 <- ordered(nesting.structure$cat2,levels=cat2,labels=cat2)
        nesting.structure$cat3 <- ordered(nesting.structure$cat3,levels=na.omit(cat3),labels=na.omit(cat3))
        
        chart.palette <- c(palette_score_charts[5],
                           palette_score_charts[2],
                           palette_score_charts[1],
                           "grey80")
        
        score_criteria$cat <- ifelse(score_criteria$sig&score_criteria$dir,
                                     cat[1],NA)
        score_criteria$cat <- ifelse(score_criteria$sig&!score_criteria$dir,
                                     cat[2],score_criteria$cat)
        score_criteria$cat <- ifelse(!score_criteria$sig&score_criteria$dir,
                                     cat[3],score_criteria$cat)
        score_criteria$cat <- ifelse(!score_criteria$sig&!score_criteria$dir,
                                     cat[4],score_criteria$cat)
        
        cats_rects <- rounded.bars(data=score_criteria[!is.na(score_criteria$cat),],nesting.structure = nesting.structure,
                                   chart.palette=chart.palette)$cats_rects
        bars <- rounded.bars(data=score_criteria[!is.na(score_criteria$cat),],nesting.structure = nesting.structure,
                     chart.palette=chart.palette)$plot+
          geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=as.character(cats_rects$cat)),
                    color=c("white","white","black","black"),size=3)
      }
      
      row4 <- plot_grid(ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="% Replications Meeting\nSCORE Criteria"),
                        simple_bar(score_criteria,"repli_score_criteria_met"),
                        bars,
                        nrow=1,rel_widths = colwidths
      )
      
      row5 <- plot_grid(ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="% Replications Meeting\nAnalyst Interpretation Match"),
                        simple_bar(repli_within_orig_pred,"wthn_pi"),
                        ggplot()+theme_nothing()+xlim(c(0,1))+ylim(c(0,1))+
                          annotate("text",x=0.5,y=0.5,vjust=0.5,hjust=0.5,
                                   label="No additional details"),
                        nrow=1,rel_widths = colwidths
      )
      
      plot_grid(row0,row1,row2,row3,row4,row5,ncol=1)
      
    }
    
  })
  

  output$repli_effect_sizes_vs_orig <- renderPlot({
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
    
    
    fig1 <- all_effects %>% 
      ggplot(aes(x = orig_conv_r, y = repli_conv_r, color = success)) +
      geom_point(alpha = 0.6, size = 3) +
      geom_rug() +
      scale_color_manual(labels = c("Failed", "Successful"),
                         values = c("tomato3", "deepskyblue4")) +
      geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "slategray") +
      xlim(0, 1) +
      ylim(-0.55, 1) +
      labs(
        x = "Original",
        y = "Replication",
        color = "",
        title = ""
      ) +
      theme(
        plot.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.position = "bottom"
      )
    
    ggMarginal(fig1, type = "density", groupFill = T)
  })

  output$repli_effect_sizes_vs_orig_by_field <- renderPlot({
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
      
      ggplot(data=all_effects,aes(x=value,y=pub_year,fill=name))+
        geom_density_ridges(alpha=.6,scale = 0.9)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.title=element_blank(),
              axis.text.y=element_text(vjust=0)
              #axis.text = element_blank(),
              #axis.line = element_blank(),
              #axis.ticks = element_blank()
        ) +
        scale_fill_manual(values=c(palette_score_charts[1],palette_score_charts[5]))+
        xlim(-1,1)+
        geom_vline(aes(xintercept=0),linetype=2)

    }
  })

}

shinyApp(ui, server)