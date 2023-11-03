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
    if (file.exists("repli_outcomes.RData")) { load(file="repli_outcomes.RData") }
    else {load(file="Analysis/Data exploration app/repli_outcomes.RData") }

  }
  
  # Data manipulation and other setup
  {
    # RR UI and selection options data
    {
      select_repli_type_set <- c("new data","secondary data")
      select_repli_type_labels <- c("New data","Secondary data")
      select_repli_type_selected_default <- c("new data")
      
      select_is_generalizability_set  <- c(FALSE,TRUE)
      select_is_generalizability_labels  <- c("Standard","Generalizability study")
      select_is_generalizability_selected_default <- c(FALSE)
      
      select_is_manylabs_set <- c(FALSE,TRUE)
      select_is_manylabs_labels <- c("Not ManyLabs","ManyLabs")
      select_is_manylabs_selected_default <- c(FALSE)
      
      select_power_for_effect_size_set <- c("50% for 100%","90% for 50%","90% for 75% lab power analysis","not performed")
      select_power_for_effect_size_labels <- c("50% for 100%","90% for 50%","90% for 75% lab power analysis","Not performed")
      select_power_for_effect_size_selected_default <- select_power_for_effect_size_set
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

ui <- {
fluidPage(
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
            "select_is_generalizability_selected", "Generalizability (is_generalizability)",
            choiceNames = unique(select_is_generalizability_labels), 
            choiceValues = unique(select_is_generalizability_set),
            selected = c(select_is_generalizability_selected_default)
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
          tabPanel("Alluvial chart",
                   plotOutput("repli_alluvial"),
          ),
          tabPanel("Replication stats vs original",
                   # checkboxGroupInput(
                   #   "rr_stat_outcomes_selected", "Outcome stats types",
                   #   choiceNames = c("Pearson's R",unique(repli_outcomes$rr_effect_size_type_reported)), 
                   #   choiceValues = c("Pearson's R",unique(repli_outcomes$rr_effect_size_type_reported)),
                   #   selected = c("Pearson's R")
                   # ),
                   plotOutput("repli_outcomes_vs_orig"),
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
        df <- df[df$is_generalizability %in% input$select_is_generalizability_selected,]
        df <- df[df$is_manylabs %in% input$select_is_manylabs_selected,]
        df <- df[df$power_for_effect_size %in% input$select_power_for_effect_size_selected,]
    
        df
      })
    # Objects / charts / figures
      output$repli_outcomes_vs_orig <- renderPlot({
        df.chart <- df_repli_subsetted()
        
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
  
      output$repli_data_table <- renderDT(df_repli_subsetted(), options = list(lengthChange = FALSE))
      
      output$repli_alluvial <- renderPlot({
        df.repli.no.hier <- repli_outcomes[c("paper_id","claim_id","is_manylabs","power_for_effect_size","rr_id")]
        df.repli.no.hier$claim_id <- paste0(df.repli.no.hier$paper_id,"_", df.repli.no.hier$claim_id)
        
        df.repli.no.hier <- df.repli.no.hier[df.repli.no.hier$paper_id %in% unique(df.repli.no.hier$paper_id)[60:120],]
        
        df <- df.repli.no.hier %>%
          ggsankey::make_long(paper_id, claim_id,is_manylabs,power_for_effect_size)
        
        pl <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) + 
          ggsankey::geom_alluvial(flow.alpha = 0.75, show.legend = FALSE,space=1) +
          #geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5)+
          theme_bw() + 
          theme(legend.position = "none") +
          theme(axis.title = element_blank()
                , axis.text.y = element_blank()
                , axis.ticks = element_blank()  
                , panel.grid = element_blank()) +
          labs(fill = 'Nodes')
        #pl <- pl + scale_fill_viridis_d(option = "inferno")
        pl
      })
      
      output$repli_data_text <- renderText({
        df <- df_repli_subsetted()
        
        text <- paste0("Replications (n): ",nrow(df))
        text <- paste0(text,"<br/>","Papers (n): ",length(unique(df$paper_id)))
        text <- paste0(text,"<br/>","Claims (n): ",length(unique(df$claim_id)))
        HTML(text)
      })
  
}

shinyApp(ui, server)