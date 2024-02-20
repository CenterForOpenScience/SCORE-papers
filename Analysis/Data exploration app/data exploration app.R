
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
  }
  
  # Data loading
  {
    # Check if this is being run from shinyapps.io or from the github folder (in
    # which case the data gets pulled from the targets output)
    if (file.exists("Analysis/common functions.R")) {
      # Being run from github/locally, get raw data and copy data files into
      # same level folder for uploading
      run_location <- "local"
      objects_to_load <- c("repli_outcomes","orig_outcomes","repro_outcomes")
      for(i in 1:length(objects_to_load)){
        assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
        save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
      }
      
      source(file="Analysis/common functions.R")
      file.copy("Analysis/common functions.R", "Analysis/Data exploration app/common functions.R",overwrite = TRUE)
      
      source(file="Analysis/Paper 5/Code/tagged stats and figures.R")
      file.copy("Analysis/Paper 5/Code/tagged stats and figures.R", "Analysis/Data exploration app/tagged stats and figures.R",overwrite = TRUE)

    } else {
      # Being run on shinyapps.io; data files already in folder
      run_location <- "server"
      load(file="repli_outcomes.RData")
      load(file="repro_outcomes.RData")
      load(file="orig_outcomes.RData")
      source("common functions.R")
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
  tabsetPanel(
    tabPanel("Replications",{
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
           tabPanel("Paper 5 stats",
                    p("Paper 5 here: https://docs.google.com/document/d/1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg/edit"),
                    p("May take a moment to load due to bootstrap iterations..."),
                    DTOutput("paper_5_stats_table")
           ),
           tabPanel("Data properties",
            htmlOutput("repli_data_text")
           ),
          tabPanel("Dataset",
                  DTOutput("repli_data_table")
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
                            checkboxInput("repli_outcomes_vs_orig_abs",
                                          "Take absolute value of effect size",TRUE),
                            p("Chart elements:"),
                            checkboxInput("repli_outcomes_vs_orig_weighted_medians",
                                          "Median effect size lines (weighted)",TRUE),
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
                                         "Null value",0)
                     )
                   )
          ),
          tabPanel("Chart: Repli success vars",
                   plotOutput("repli_success_vars"),
                   h4("Options:"),
          ),
          tabPanel("Chart: Repli success vars vs sample size",
                   
                   selectInput("repli_success_sample_size_var", "Select success outcome variable",
                               choices = c("SCORE criteria","Pattern criteria","Interpretation supported","Repli point estimate in orig CI","Orig point estimate in repli CI")),
                   plotOutput("repli_success_sample_size")
                   
          )
        )
      )
    }),
    tabPanel("Reproductions",{
      page_sidebar(
        theme = bs_theme(bootswatch = "minty"),
        # Dataset sidebar ----
        sidebar = sidebar(
          h3("Calculation options"),
          numericInput("repro_bootstrap_iterations",
                       "Bootstrap iterations",
                       value=10,min=10,max=2000,step=5
          ),
          h3("Dataset selection"),
          
        ),
        # Main page ----
        navbarPage("",
                   tabPanel("Data properties",
                            htmlOutput("repro_data_text")
                   ),
                   tabPanel("Dataset",
                            DTOutput("repro_data_table")
                   ),
                   tabPanel("Stats whiteboard",
                            p("takes a bit to load..."),
                            #htmlOutput("repro_whiteboard_text")
                   ),
        )
      )
    })
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
                                              orig_outcomes = orig_outcomes)
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
    # Reproductions
      # Data generation
        df_repro_subsetted <- reactive({
          df <- repro_outcomes
  
          
          df
        })
      # Charts and figures
        output$repro_data_table <- renderDT(df_repli_subsetted(), options = list(lengthChange = FALSE),rownames= FALSE)
        
        output$repro_data_text <- renderText({
          df <- df_repro_subsetted()
          
          text <- paste0("Reproductions (n): ",nrow(df))
          text <- paste0(text,"<br/>","Papers (n): ",length(unique(df$paper_id)))
          text <- paste0(text,"<br/>","Claims (n): ",length(unique(df$claim_id)))
          HTML(text)
        })
}

shinyApp(ui, server)