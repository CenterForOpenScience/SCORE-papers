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
  }
  
  # Data loading
  {
    load(file="repli_outcomes.RData")
    # Temporary for data cleaning
    repli_outcomes$generalizability <- ifelse(repli_outcomes$paper_id %in% c("Br0x", "EQxa", "J4W9", "plLK", "rjb", "zlm2"),
                                              TRUE,FALSE)
    repli_outcomes$claim_id_unique <- paste0(repli_outcomes$paper_id,"_",repli_outcomes$claim_id)
    repli_outcomes$rr_type_internal <- NULL
  }
  
  # Data manipulation and other setup
  {
    # RR UI and selection options data
    {
      select_rr_type_set <- c("Direct Replication","Data Analytic Replication","Hybrid")
      select_rr_type_labels <- c("Direct Replication","Data Analytic Replication","Hybrid")
      select_rr_type_selected_default <- c("Direct Replication")
      
      select_generalizability_set  <- c(FALSE,TRUE)
      select_generalizability_labels  <- c("Standard","Generalizability study")
      select_generalizability_selected_default <- c(FALSE)
      
      select_rr_is_manylabs_set <- c("non_ml","ml_count","ml_instance_primary","ml_aggregation")
      select_rr_is_manylabs_labels <- c("Not ManyLabs","Count","Primary","Aggregation")
      select_rr_is_manylabs_selected_default <- c("non_ml")
      
      select_rr_analytic_sample_stage_set <- c("stage 1","stage 2","threshold","no target","lab target")
      select_rr_analytic_sample_stage_labels <- c("Stage 1","Stage 2","Threshold","No target","Lab target")
      select_rr_analytic_sample_stage_selected_default <- c("stage 1")
    }
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
}

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Replications",
      page_sidebar(
        theme = bs_theme(bootswatch = "minty"),
        
        sidebar = sidebar(
          h3("Dataset selection"),
          checkboxGroupInput(
            "select_rr_type_selected", "Replication type (rr_type)",
            choiceNames = unique(select_rr_type_labels), 
            choiceValues = unique(select_rr_type_set),
            selected = c(select_rr_type_selected_default)
          ),
          checkboxGroupInput(
            "select_generalizability_selected", "Generalizability (generalizability)",
            choiceNames = unique(select_generalizability_labels), 
            choiceValues = unique(select_generalizability_set),
            selected = c(select_generalizability_selected_default)
          ),
          checkboxGroupInput(
            "select_rr_is_manylabs_selected", "Many Labs (rr_is_manylabs)",
            choiceNames = unique(select_rr_is_manylabs_labels), 
            choiceValues = unique(select_rr_is_manylabs_set),
            selected = c(select_rr_is_manylabs_selected_default)
          ),
          checkboxGroupInput(
            "select_rr_analytic_sample_stage_selected", "Replication type (rr_analytic_sample_stage)",
            choiceNames = unique(select_rr_analytic_sample_stage_labels), 
            choiceValues = unique(select_rr_analytic_sample_stage_set),
            selected = c(select_rr_analytic_sample_stage_selected_default)
          ),
        ),
        navbarPage("",
          tabPanel("Replication stats vs original",
                   # checkboxGroupInput(
                   #   "rr_stat_outcomes_selected", "Outcome stats types",
                   #   choiceNames = c("Pearson's R",unique(repli_outcomes$rr_effect_size_type_reported)), 
                   #   choiceValues = c("Pearson's R",unique(repli_outcomes$rr_effect_size_type_reported)),
                   #   selected = c("Pearson's R")
                   # ),
                   plotOutput("repli_outcomes_vs_orig"),
                   ),
          tabPanel("Tab 2"),
          tabPanel("Tab 3")
          
        )
      )
    ),
    tabPanel("Reproductions")
  )
)

server <- function(input, output, session) {
  df.repli.subsetted <- reactive({
    df <- repli_outcomes

    df <- df[df$rr_type %in% input$select_rr_type_selected,]
    df <- df[df$generalizability %in% input$select_generalizability_selected,]
    df <- df[df$rr_is_manylabs %in% input$select_rr_is_manylabs_selected,]
    df <- df[df$rr_analytic_sample_stage %in% input$select_rr_analytic_sample_stage_selected,]

    df
    
  })
  
  output$repli_outcomes_vs_orig <- renderPlot({
    df.chart <- df.repli.subsetted()
    
    # TEMPORARY FOR MADE UP DATA
    df.chart$rr_pearsons_r_value_reference <- df.chart$rr_pearsons_r_value*1.1
    
    # Gather up new vs originals
    # Pearsons
    df.chart.pearsons <- df.chart[c("rr_pearsons_r_value","rr_effect_size_value_reference")]
    df.chart.pearsons$stat_type <- "Pearson's R"
    colnames(df.chart.pearsons) <- c("Replication","Original","stat_type")
    
    # All others
    df.chart.others <- df.chart[c("rr_statistic_value_reported","rr_effect_size_value_reference","rr_effect_size_type_reported")]
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
    
    ggplot(data=df.chart,aes(y=ES_value,x=stat_type,fill=comparison)) +
      geom_split_violin()+
      #geom_histogram(fill="#2B2484",alpha=.4)+
      #geom_density(fill="#2B2484",alpha=.8)+
      #scale_x_continuous(expand=c(0,0))+
      #scale_y_continuous(expand=c(0,0))+
      theme_minimal()+
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line = element_line(color="#393939")
      )+
      xlab("Statistic type")+
      ylab("Effect size value")

  })
  
  output$data.table.temp = renderDT(
    df.repli.subsetted(), options = list(lengthChange = FALSE)
  )
  
}

shinyApp(ui, server)