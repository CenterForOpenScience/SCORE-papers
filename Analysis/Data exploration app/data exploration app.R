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
    if (file.exists("repli_outcomes.RData")) {
      load(file="repli_outcomes.RData")
    } else {
      load(file="Analysis/Data exploration app/repli_outcomes.RData")
    }
    if (file.exists("orig_dataset.RData")) {
      load(file="orig_dataset.RData")
    } else {
      load(file="Analysis/Data exploration app/orig_dataset.RData")
    }

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

# Aesthetic functions and presets
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
                   
                   plotOutput("repli_outcomes_vs_orig"),
                   h4("Options:"),
                   fluidRow(
                     column(4,
                       checkboxGroupInput(
                         "rr_stat_outcomes_selected", "Effect size stats types",
                         choiceNames = c("Pearson's R",unique(as.character(repli_outcomes$repli_effect_size_type))),
                         choiceValues = c("Pearson's R",unique(as.character(repli_outcomes$repli_effect_size_type))),
                         selected = c("Pearson's R","ser_method"),
                         inline=FALSE
                       )
                     ),
                     column(1),
                     column(7,
                            p("Chart extent limits"),
                            fluidRow(
                              column(6,numericInput("repli_outcomes_vs_orig_lb",
                                                    "Lower bound",0)),
                              column(6,numericInput("repli_outcomes_vs_orig_ub",
                                                    "Upper bound",1))
                            )
                     )
                   )
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
        df <- df[df$repli_is_generalizability %in% input$select_is_generalizability_selected,]
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
        # if (file.exists("orig_dataset.RData")) {
        #   load(file="orig_dataset.RData")
        # } else {
        #   load(file="Analysis/Data exploration app/orig_dataset.RData")
        # }
        df.chart.orig <- orig_dataset
        
      # Merge in orig data
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
          # types_to_keep <- c("ser_method")
          # df.chart.others <- df.chart.others[df.chart.others$stat_type %in% types_to_keep,]
        
        # Combine and select
          df.chart <- rbind(df.chart.pearsons,df.chart.others)
          df.chart <- df.chart %>% pivot_longer(!"stat_type", names_to = "comparison", values_to = "ES_value")
          df.chart <- na.omit(df.chart)
          df.chart$comparison <- factor(df.chart$comparison,
                                        labels=c("Replication","Original"),
                                        levels=c("Replication","Original"))
          df.chart <- df.chart[df.chart$stat_type %in% input$rr_stat_outcomes_selected,]
          
      # Chart generation
        ggplot(data=df.chart,aes(y=ES_value,x=stat_type,fill=reorder(comparison, desc(comparison)))) +
          geom_split_violin()+
          geom_point(position=position_jitterdodge(),size=.5)+
          theme_bw()+
          scale_fill_manual(values=plaette_score_charts)+
          theme(
            legend.position = "bottom",
            panel.grid = element_blank(),
            axis.line = element_line(color="#393939"),
            legend.title=element_blank()
          )+
          scale_y_continuous(expand=c(0,0),
                             limits=c(input$repli_outcomes_vs_orig_lb,input$repli_outcomes_vs_orig_ub))+
          xlab("Statistic type")+
          ylab("Effect size value")
      })
  
      output$repli_data_table <- renderDT(df_repli_subsetted(), options = list(lengthChange = FALSE))
      
      output$repli_alluvial <- renderPlot({
        df.repli.no.hier <- repli_outcomes[c("paper_id","claim_id","is_manylabs","power_for_effect_size")]
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