
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
    
  }
  
  # Data loading
  {
    if (file.exists("repli_outcomes.RData")) {
      load(file="repli_outcomes.RData")
    } else {
      load(file="Analysis/Data exploration app/repli_outcomes.RData")
    }
    
    if (file.exists("orig_analytic.RData")) {
      load(file="orig_analytic.RData")
    } else {
      load(file="Analysis/Data exploration app/orig_analytic.RData")
    }
    
    if (file.exists("common functions.R")) {
      source("common functions.R")
    } else {
      source(file="Analysis/Data exploration app/common functions.R")
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
      
      select_is_manylabs_set <- c(FALSE,TRUE)
      select_is_manylabs_labels <- c("Not ManyLabs","ManyLabs")
      select_is_manylabs_selected_default <- c(FALSE)
      
      select_power_for_effect_size_set <- c("50% for 100%","90% for 50%","90% for 75% lab power analysis","not performed")
      select_power_for_effect_size_labels <- c("50% for 100%","90% for 50%","90% for 75% lab power analysis","Not performed")
      select_power_for_effect_size_selected_default <- select_power_for_effect_size_set
    }
  }
  
}


ui <- {

fluidPage(title = "SCORE data visualization playground",
  tabsetPanel(
    tabPanel("Replications",
      page_sidebar(
        theme = bs_theme(bootswatch = "minty"),
        
        sidebar = sidebar(
          h3("Dataset selection"),
          checkboxGroupInput(
            "select_repli_type_selected", "Replication type (repli_type)",
            choiceNames = unique(select_repli_type_labels), 
            choiceValues = unique(select_repli_type_set),
            selected = c(select_repli_type_selected_default)
          ),
          checkboxGroupInput(
            "select_repli_version_of_record_selected", "Version of Record (repli_version_of_record)",
            choiceNames = unique(select_repli_version_of_record_labels), 
            choiceValues = unique(select_repli_version_of_record_set),
            selected = c(select_repli_version_of_record_selected_default)
          ),
          checkboxGroupInput(
            "select_repli_is_generalizability_selected", "Generalizability (repli_is_generalizability)",
            choiceNames = unique(select_repli_is_generalizability_labels), 
            choiceValues = unique(select_repli_is_generalizability_set),
            selected = c(select_repli_is_generalizability_selected_default)
          ),
          checkboxGroupInput(
            "select_is_manylabs_selected", "Many Labs (is_manylabs)",
            choiceNames = unique(select_is_manylabs_labels), 
            choiceValues = unique(select_is_manylabs_set),
            selected = c(select_is_manylabs_selected_default)
          ),
          checkboxGroupInput(
            "select_power_for_effect_size_selected", "Replication type (power_for_effect_size)",
            choiceNames = unique(select_power_for_effect_size_labels), 
            choiceValues = unique(select_power_for_effect_size_set),
            selected = c(select_power_for_effect_size_selected_default)
          ),
        ),
        navbarPage("",
           tabPanel("Data properties",
                    htmlOutput("repli_data_text")
           ),
          tabPanel("Dataset",
                  DTOutput("repli_data_table")
          ),
          tabPanel("Key stats whiteboard",
                   p("takes a bit to load..."),
                   htmlOutput("repli_success_text")
          ),
          tabPanel("Paper 5 stats",
                   p("Paper 5 here: https://docs.google.com/document/d/1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg/edit"),
                   htmlOutput("paper_5_stats_text")
          ),
          tabPanel("Chart: repli vs original ES",
                   
                   plotOutput("repli_outcomes_vs_orig"),
                   h4("Options:"),
                   fluidRow(
                     column(4,
                       #checkboxGroupInput(
                       radioButtons(
                         "rr_stat_outcomes_selected", "Effect size stats types",
                         choiceNames = c("Pearson's R",unique(as.character(repli_outcomes$repli_effect_size_type))),
                         choiceValues = c("Pearson's R",unique(as.character(repli_outcomes$repli_effect_size_type))),
                         selected = c("Pearson's R"),
                         inline=FALSE
                       )
                     ),
                     column(1),
                     column(7,
                            p("Chart elements; add:"),
                            checkboxInput("repli_outcomes_vs_orig_smoothed_dist",
                                          "Smoothed distributions",TRUE),
                            checkboxInput("repli_outcomes_vs_orig_points",
                                          "Raw data points",TRUE),
                            checkboxInput("repli_outcomes_vs_orig_lines",
                                          "Lines",TRUE),
                            checkboxInput("repli_outcomes_vs_orig_points_jitter",
                                           "Jittered raw data points",FALSE),
                            checkboxInput("repli_outcomes_vs_orig_dotplot",
                                          "Dot plot",FALSE),
                            
                            p("Chart extent limits"),
                            fluidRow(
                              column(6,numericInput("repli_outcomes_vs_orig_lb",
                                                    "Lower bound",0)),
                              column(6,numericInput("repli_outcomes_vs_orig_ub",
                                                    "Upper bound",1))
                            ),
                            numericInput("repli_outcomes_vs_orig_null",
                                         "Null value",0),
                            checkboxInput("repli_outcomes_vs_orig_abs",
                                         "Take absolute value of effect size",FALSE)
                            
                     )
                   )
          ),
          tabPanel("Chart: Repli success vars",
                   plotOutput("repli_success_vars"),
                   h4("Options:"),
          ),
          tabPanel("Chart: Repli success vars vs sample size",
                   
                   selectInput("repli_success_sample_size_var", "Select success outcome variable",
                               choices = c("SCORE criteria","Pattern criteria","Interpretation supported","Repli point estimate in orig CI")),
                   plotOutput("repli_success_sample_size")
                   
          )
        )
      )
    ),
    tabPanel("Reproductions")
  )
)
}

server <- function(input, output, session) {
  # Replication
    # Data generation
      df_repli_subsetted <- reactive({
        df <- repli_outcomes
    
        df <- df[df$repli_type %in% input$select_repli_type_selected,]
        df <- df[df$repli_version_of_record %in% input$select_repli_version_of_record_selected,]
        df <- df[df$repli_is_generalizability %in% input$select_repli_is_generalizability_selected,]
        # note: change bottom ones to:
        # df <- df[df$repli_is_manylabs %in% input$select_is_manylabs_selected,]
        # df <- df[df$repli_power_for_effect_size %in% input$select_power_for_effect_size_selected,]
        df <- df[df$is_manylabs %in% input$select_is_manylabs_selected,]
        df <- df[df$power_for_effect_size %in% input$select_power_for_effect_size_selected,]
    
        df
      })
    # Objects / charts / figures
      output$repli_outcomes_vs_orig <- renderPlot({
        df.chart <- df_repli_subsetted()
        df.chart.orig <- orig_analytic
        
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
            panel.border = element_blank()
          )+
          scale_y_continuous(limits=c(input$repli_outcomes_vs_orig_lb,input$repli_outcomes_vs_orig_ub))+
          #scale_x_discrete(expand = c(4, 0))+
          theme(aspect.ratio = 1)+
          #xlab("Statistic type")+
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
        
        p
      })
  
      output$repli_success_vars <- renderPlot({
        df <- df_repli_subsetted()
        
        #TEMPORARY
        #df <- repli_outcomes
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
                                                         clustervar = "paper_id", alpha=.05,tails="two-tailed",iters=200)
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
          scale_x_continuous(limits=c(0,1),labels = scales::percent,expand=c(0,0))+
          scale_y_continuous(expand=c(0,0))+
          geom_density(alpha=.5)
      })
      
      output$repli_success_sample_size <- renderPlot({
        df.chart <- df_repli_subsetted()

        #TEMPORARY
        #df.chart <- repli_outcomes
        
      # Merge in orig data
        df.chart <- merge(df.chart,orig_analytic,by="claim_id",all.x=TRUE,all.y=FALSE)
        
      # Sample size calc
        df.chart$log_sample_size_ratio <- log(df.chart$repli_sample_size_value / df.chart$orig_sample_size_value)
        
      # Outcome vars
        df.chart$repli_interpret_supported_yes <- df.chart$repli_interpret_supported=="yes"

        if (input$repli_success_sample_size_var=="SCORE criteria"){
          df.chart$outcome <- as.numeric(df.chart$repli_score_criteria_met)
        } else if (input$repli_success_sample_size_var=="Pattern criteria"){
          df.chart$outcome <- as.numeric(df.chart$repli_pattern_criteria_met)
        } else if (input$repli_success_sample_size_var=="Interpretation supported"){
          df.chart$outcome <- as.numeric(df.chart$repli_interpret_supported_yes)
        } else if (input$repli_success_sample_size_var=="Repli point estimate in orig CI"){
          df.chart$outcome <- as.numeric(df.chart$repli_effect_size_value >= df.chart$orig_effect_size_ci_lb & df.chart$repli_effect_size_value <= df.chart$orig_effect_size_ci_ub)
        }
        
        ggplot(df.chart,aes(x=log_sample_size_ratio,y=outcome))+
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
          geom_vline(xintercept=0,linetype=2)+
          ylab("Percent meeting replication success criteria")+
          xlab("Log ratio sample size of replication : original\nHigher indicates greater relative sample size in replication vs original")+
          scale_x_continuous(expand=c(0,0))+
          scale_y_continuous(expand=c(0,0),labels = scales::percent)+
          coord_cartesian(ylim=c(0, 1),xlim=c(-2,2))+
          #geom_point()+
          geom_smooth(method = "loess")
      })
      
      output$repli_data_table <- renderDT(df_repli_subsetted(), options = list(lengthChange = FALSE))
      
      output$repli_data_text <- renderText({
        df <- df_repli_subsetted()
        
        text <- paste0("Replications (n): ",nrow(df))
        text <- paste0(text,"<br/>","Papers (n): ",length(unique(df$paper_id)))
        text <- paste0(text,"<br/>","Claims (n): ",length(unique(df$claim_id)))
        HTML(text)
      })
      
      output$repli_success_text <- renderText({
        df <- df_repli_subsetted()
        
        text <- ""
        # Replication SCORE criteria met
        { 
          mean.repli.success <- bootstrap.clust(data=df[c("paper_id","claim_id","repli_score_criteria_met")],FUN=
                                                  function(data) {
                                                    mean(data$repli_score_criteria_met,na.rm=TRUE)
                                                  }, 
                                                alpha=.05,tails="two-tailed")
          
          mean.repli.success.weighted <- bootstrap.clust(data=df[c("paper_id","claim_id","repli_score_criteria_met")],FUN=
                                                           function(data) {
                                                             data <- data %>% add_count(paper_id)
                                                             data$weight <- 1/data$n
                                                             weighted.mean(data$repli_score_criteria_met,data$weight,na.rm=TRUE)
                                                           }, 
                                                         clustervar = "paper_id", alpha=.05,tails="two-tailed")
          
          text <- paste0(text,"<b>Percent meeting replication SCORE criteria:</b> ")
          text <- paste0(text,"(n=",length(na.omit(df$repli_score_criteria_met)),")")
          text <- paste0(text,"<br/>")
          text <- paste0(text,"Unweighted/unclustered: ",round(mean.repli.success$point.estimate,3)*100,"%")
          text <- paste0(text," (95% CI: ",round(mean.repli.success$CI.lb,3)*100," - ", round(mean.repli.success$CI.ub,3)*100,"%)")
          text <- paste0(text,"<br/>")
          
          text <- paste0(text,"Clustered/weighted at the paper level: ",round(mean.repli.success.weighted$point.estimate,3)*100,"%")
          text <- paste0(text," (95% CI: ",round(mean.repli.success.weighted$CI.lb,3)*100," - ", round(mean.repli.success.weighted$CI.ub,3)*100,"%)")
          text <- paste0(text,"<br/>")
          text <- paste0(text,"<br/>")
        }
        # Replication pattern criteria met
        { 
          mean.repli.success <- bootstrap.clust(data=df[c("paper_id","claim_id","repli_pattern_criteria_met")],FUN=
            function(data) {
              mean(data$repli_pattern_criteria_met,na.rm=TRUE)
            }, 
          alpha=.05,tails="two-tailed")
          
          mean.repli.success.weighted <- bootstrap.clust(data=df[c("paper_id","claim_id","repli_pattern_criteria_met")],FUN=
              function(data) {
                data <- data %>% add_count(paper_id)
                data$weight <- 1/data$n
                weighted.mean(data$repli_pattern_criteria_met,data$weight,na.rm=TRUE)
              }, 
            clustervar = "paper_id", alpha=.05,tails="two-tailed")
          
          text <- paste0(text,"<b>Percent meeting replication pattern criteria:</b> ")
          text <- paste0(text,"(n=",length(na.omit(df$repli_pattern_criteria_met)),")")
          text <- paste0(text,"<br/>")
          text <- paste0(text,"Unweighted/unclustered: ",round(mean.repli.success$point.estimate,3)*100,"%")
          text <- paste0(text," (95% CI: ",round(mean.repli.success$CI.lb,3)*100," - ", round(mean.repli.success$CI.ub,3)*100,"%)")
          text <- paste0(text,"<br/>")
          
          text <- paste0(text,"Clustered/weighted at the paper level: ",round(mean.repli.success.weighted$point.estimate,3)*100,"%")
          text <- paste0(text," (95% CI: ",round(mean.repli.success.weighted$CI.lb,3)*100," - ", round(mean.repli.success.weighted$CI.ub,3)*100,"%)")
          text <- paste0(text,"<br/>")
          text <- paste0(text,"<br/>")
        }
        # Interpretation supported 
        {
          mean.repli.success <- bootstrap.clust(data=df[c("paper_id","claim_id","repli_interpret_supported")],FUN=
                function(data) {
                  mean(data$repli_interpret_supported=="yes",na.rm=TRUE)
                }, 
                alpha=.05,tails="two-tailed",iters=100)
          
          mean.repli.success.weighted <- bootstrap.clust(data=df[c("paper_id","claim_id","repli_interpret_supported")],FUN=
               function(data) {
                 data <- data %>% add_count(paper_id)
                 data$weight <- 1/data$n
                 weighted.mean(data$repli_interpret_supported=="yes",data$weight,na.rm=TRUE)
               }, 
              clustervar = "paper_id", alpha=.05,tails="two-tailed")
          
          text <- paste0(text,"<b>Percent interpretation supported (subjective assessment by lab):</b> ")
          text <- paste0(text,"(n=",length(na.omit(df$repli_interpret_supported)),")")
          text <- paste0(text,"<br/>")
          text <- paste0(text,"Unweighted/unclustered: ",round(mean.repli.success$point.estimate,3)*100,"%")
          text <- paste0(text," (95% CI: ",round(mean.repli.success$CI.lb,3)*100," - ", round(mean.repli.success$CI.ub,3)*100,"%)")
          text <- paste0(text,"<br/>")
          
          text <- paste0(text,"Clustered/weighted at the paper level: ",round(mean.repli.success.weighted$point.estimate,3)*100,"%")
          text <- paste0(text," (95% CI: ",round(mean.repli.success.weighted$CI.lb,3)*100," - ", round(mean.repli.success.weighted$CI.ub,3)*100,"%)")
          text <- paste0(text,"<br/>")
          text <- paste0(text,"<br/>")
        }
        # Replication success by type
        { 
          rr.success.repli.type.weighted <- bootstrap.clust(data=df[c("paper_id","claim_id","repli_pattern_criteria_met","repli_type")],FUN=
                                                           function(data) {
                                                             data <- data %>% add_count(paper_id)
                                                             data$weight <- 1/data$n
                                                             probability.ratio(exposure = data$repli_type=="new data",
                                                                               outcome = data$repli_pattern_criteria_met,
                                                                               weight = data$weight)
                                                           }, 
                                                         clustervar = "paper_id", alpha=.05,tails="two-tailed")
          
          text <- paste0(text,"<b>Relative proportion replication success by data type: </b>",round(rr.success.repli.type.weighted$point.estimate,3))
          text <- paste0(text," (95% CI: ",round(rr.success.repli.type.weighted$CI.lb,3)," - ", round(rr.success.repli.type.weighted$CI.ub,3),")")
          text <- paste0(text,"<br/>")
          text <- paste0(text,"Interpretation: Replication attempts using new data were ",round(rr.success.repli.type.weighted$point.estimate,3),
                         " times as likely to have replication criteria met compared with those replications using pre-existing/secondary data.")
          text <- paste0(text,"<br/>")
        }

        HTML(text)
      })
      
      output$paper_5_stats_text <- renderText({
        df <- df_repli_subsetted()
        
        text <- ""
        
        # Abstract
        text <- paste0(text,"<b>","Abstract","</b>")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_n_claims: ")
        text <- paste0(text,length(unique(df$claim_id)))
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_n_papers: ")
        text <- paste0(text,length(unique(df$paper_id)))
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_p_effect_size_ratio_v_orig: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_p_findings_stat_sig_and_in_direct: ")
        repli_score_criteria_met <- bootstrap.clust(data=df[c("paper_id","claim_id","repli_score_criteria_met")],FUN=
                                                         function(data) {
                                                           data <- data %>% add_count(paper_id)
                                                           data$weight <- 1/data$n
                                                           weighted.mean(data$repli_score_criteria_met,data$weight,na.rm=TRUE)
                                                         }, 
                                                       clustervar = "paper_id", alpha=.05,tails="two-tailed")
        text <- paste0(text,round(repli_score_criteria_met$point.estimate,3)*100,"% (95% CI ")
        text <- paste0(text,round(repli_score_criteria_met$CI.lb,3)*100,"-",round(repli_score_criteria_met$CI.ub,3)*100,"%)")
        
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_p_effect_size_smaller_v_orig_business: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_n_effect_size_smaller_v_orig_business: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_p_effect_size_smaller_v_orig_econ: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_n_effect_size_smaller_v_orig_econ: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_p_effect_size_smaller_v_orig_edu: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_n_effect_size_smaller_v_orig_edu: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_p_effect_size_smaller_v_orig_polisci: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_n_effect_size_smaller_v_orig_polisci: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_p_effect_size_smaller_v_orig_psych: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_n_effect_size_smaller_v_orig_psych: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_p_effect_size_smaller_v_orig_soc: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        text <- paste0(text,"repli_n_effect_size_smaller_v_orig_soc: ")
        text <- paste0(text,"PENDING")
        text <- paste0(text,"<br/>")
        
        HTML(text)
      })
}

shinyApp(ui, server)