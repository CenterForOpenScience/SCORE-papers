
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
    library(funkyheatmap)
    library(tidyverse)
    library(cowplot)
    library(colorspace)
  }
  
  # Data loading
  {
    # Check if this is being run from shinyapps.io or from the github folder (in
    # which case the data gets pulled from the targets output)
    objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes","paper_metadata","status","repro_export","stitched_claims")
    if (file.exists("Analysis/common functions.R")) {
      # Being run from github/locally, get raw data and copy data files into
      # same level folder for uploading
      run_location <- "local"
      for(i in 1:length(objects_to_load)){
        assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
        save(list=objects_to_load[i],file=paste0("Analysis/Paper 3/Code and data/Commons/",objects_to_load[i],".RData"))
      }
      
      source(file="Analysis/common functions.R")
      file.copy("Analysis/common functions.R", "Analysis/Paper 3/Code and data/Commons/common functions.R",overwrite = TRUE)
      
      source(file="Analysis/Paper 3/Code and data/tagged stats and figures.R")
      #file.copy("Analysis/Paper 3/Code and data/Commons/tagged stats and figures.R", "Analysis/Data exploration app/tagged stats and figures.R",overwrite = TRUE)
      
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
    print(run_location)
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
              numericInput("bootstrap_iterations",
                           "Bootstrap iterations",
                           value=10,min=10,max=2000,step=5
              ),
              h3("Dataset selection"),
              
              
            ),
            # Main page ----
            navbarPage("",
                       tabPanel("Tagged statistics",
                                p("Paper 3 here: https://docs.google.com/document/d/1yqMVMzZMmGMyPG4IiD_urFHmBfztFXv-om-Y2M0T7jQ/edit"),
                                p("May take a moment to load due to bootstrap iterations..."),
                                DTOutput("tagged_stats_table")
                       ),
                       # tabPanel("Data properties",
                       #          #htmlOutput("repli_data_text")
                       # ),
                       # tabPanel("Dataset",
                       #          #DTOutput("repli_data_table")
                       # ),
                       # tabPanel("Figure 1: PR code/data overall",
                       #          plotOutput("pr_outcomes_overall")
                       # ),
                       tabPanel("Figure 1: PR code/data year",
                                plotOutput("pr_outcomes_by_year")
                       ),
                       tabPanel("Figure 2: PR code/data field",
                                plotOutput("pr_outcomes_by_field")
                       ),
                       tabPanel("Figure 3: PR code/data econ/polisci vs others",
                                plotOutput("pr_outcomes_by_year_econ_vs")
                       ),
                       
                       tabPanel("Figure 4: OR by year",
                                plotOutput("or_outcomes_by_year")
                       ),
                       tabPanel("Figure 5: OR by field",
                                plotOutput("or_outcomes_by_field")
                       ),
                       tabPanel("Figure 6: OR by year and field",
                                plotOutput("or_outcomes_by_year_and_field")
                       ),
                       tabPanel("Figure X: OR by repro type",
                                plotOutput("or_outcomes_by_repro_type")
                       ),
                       
                  )
                )

  )
}

server <- function(input, output, session) {
  drive_deauth()
  
  output$tagged_stats_table <- renderDT({
    # Pull paper to find what tags are in paper
    drive_deauth()
    paper_text <- drive_read_string(file=googledrive::as_id("1yqMVMzZMmGMyPG4IiD_urFHmBfztFXv-om-Y2M0T7jQ"),
                                      type = "text/plain",encoding="UTF-8")  %>%
      strsplit(split = "(\r\n|\r|\n)") %>%
      .[[1]]
    paper_text <- paste0(paper_text,collapse="  ")
    
    # Pull paper to find what tags are calculated
    tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
    tags <- tags[tags!=""]
    tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)
    
    values_text <- tagged_stats(repro_outcomes=repro_outcomes,
                                pr_outcomes=pr_outcomes,
                                orig_outcomes=orig_outcomes,
                                paper_metadata=paper_metadata)
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
  
  output$pr_outcomes_by_field <- renderPlot({
    # Data wrangling
    {
      data <- pr_outcomes %>% 
        filter(!covid) %>% 
        mutate(d_c_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared != "no") %>% 
        mutate(d_c_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared != "no") %>% 
        mutate(d_c_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared != "no") %>% 
        mutate(d_only_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared == "no") %>% 
        mutate(d_only_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared == "no") %>% 
        mutate(d_only_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared == "no") %>% 
        mutate(no_data_code_available = OA_data_shared == "no" & OA_code_shared != "no") %>% 
        mutate(no_data_no_code = OA_data_shared == "no" & OA_code_shared == "no") %>% 
        select(paper_id, d_c_open, d_c_protect, d_c_sharing, d_only_open, d_only_sharing, no_data_code_available, no_data_no_code) %>% 
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>% 
        filter(response)
      
      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
      
      data$field <- str_to_title(data$COS_pub_category)
      
      # Assign group
      data$field <- str_to_title(data$COS_pub_category)
      data$field <- str_replace_all(data$field," ","\n")
      data$field <- str_replace_all(data$field,"And","and")
      group_order <- unique(data$field)
      
      data$group <- ordered(data$field,levels=group_order,labels=group_order)
      
      data$cat <- ordered(data$cat,
                          levels = c("d_c_open","d_c_protect","d_c_sharing","d_only_open",
                                     "d_only_sharing","no_data_code_available","no_data_no_code"),
                          labels = c("Open data,\ncode available", "Data restricted,\ncode available", 
                                     "Data shared directly,\ncode available", "Open data,\ncode unavailable",
                                     "Data shared directly,\ncode unavailable",
                                     "Data unavailable,\ncode available",
                                     "Neither data nor\ncode available")
      )
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      
      nesting.structure$cat2 <- c("Both","Both","Both",
                                  "Either",
                                  "Either",
                                  "Either",
                                  "Neither")
      nesting.structure$cat3 <- c(NA, NA, NA,"Data only","Data only","Code only",NA)
      
      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
      
    }
    
    # Aesthetic setup
    # bars_range <- c(0,.25)
    col_widths <- c(0.8,8,1.5)
    n_bins_max <- 80
    
    chart.palette <- c(palette_score_charts[5],
                       lighten(palette_score_charts[5],amount=.25),
                       lighten(palette_score_charts[5],amount=.5),
                       palette_score_charts[2],
                       lighten(palette_score_charts[2],amount=.25),
                       palette_score_charts[1],
                       "grey80"
    )
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
      rounded.bars_plot <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot
      
      snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                  chart.palette = chart.palette,
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    })
    # Blank / right axis
    group_label <- ggplot()+theme_nothing()
    
    rounded.bars_plot <- ggplot()+theme_nothing()
    
    snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                chart.palette = "white",
                                n_bins_max=n_bins_max,
                                axis_only = TRUE)$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All fields")
    
    rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis = FALSE)$plot
    
    snakebins_plot <- ggplot()+theme_nothing()
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Axis row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                      chart.palette = rep("white",length(chart.palette)),
                                      axis_only = TRUE,)$plot+
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$cats_rects
    rounded.bars_plot <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","white","black","white","black","black","black"))+
      geom_segment(x=1/3,xend=1/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3+.05,y=.5,yend=0.5,linetype=3)+
      ylim(0,1.2)+
      annotate("text",x=1/6,y=1.05,label="Both Code and Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=1/2,y=1.05,label="Either Code or Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=5/6,y=1.05,label="Neither Code nor Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=2/3+.02,
               y=.25,label="Data only",
               color="black",size=3.7,hjust=0)+
      annotate("text",x=2/3+.02,
               y=.75,label="Code only",
               color="black",size=3.7,hjust=0)
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Display plots
    plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.7,2))
  })
  
  output$pr_outcomes_by_year <- renderPlot({
    # Data wrangling
    {
      data <- pr_outcomes %>% 
        filter(!covid) %>% 
        mutate(d_c_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared != "no") %>% 
        mutate(d_c_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared != "no") %>% 
        mutate(d_c_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared != "no") %>% 
        mutate(d_only_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared == "no") %>% 
        mutate(d_only_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared == "no") %>% 
        mutate(d_only_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared == "no") %>% 
        mutate(no_data_code_available = OA_data_shared == "no" & OA_code_shared != "no") %>% 
        mutate(no_data_no_code = OA_data_shared == "no" & OA_code_shared == "no") %>% 
        select(paper_id, d_c_open, d_c_protect, d_c_sharing, d_only_open, d_only_sharing, no_data_code_available, no_data_no_code) %>% 
        pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>% 
        filter(response)
      
      data <- merge(data,paper_metadata[c("paper_id","pub_year")],by="paper_id",all.x =TRUE,all.y=FALSE)

      
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
      
      data$cat <- ordered(data$cat,
                          levels = c("d_c_open","d_c_protect","d_c_sharing","d_only_open",
                                     "d_only_sharing","no_data_code_available","no_data_no_code"),
                          labels = c("Open data,\ncode available", "Data restricted,\ncode available", 
                                     "Data shared directly,\ncode available", "Open data,\ncode unavailable",
                                     "Data shared directly,\ncode unavailable",
                                     "Data unavailable,\ncode available",
                                     "Neither data nor\ncode available")
      )
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      
      nesting.structure$cat2 <- c("Both","Both","Both",
                                  "Either",
                                  "Either",
                                  "Either",
                                  "Neither")
      nesting.structure$cat3 <- c(NA, NA, NA,"Data only","Data only","Code only",NA)
      
      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
      
    }
    
    # Aesthetic setup
    # bars_range <- c(0,.25)
    col_widths <- c(0.8,8,1.5)
    n_bins_max <- 80
    
    chart.palette <- c(palette_score_charts[5],
                     lighten(palette_score_charts[5],amount=.25),
                     lighten(palette_score_charts[5],amount=.5),
                     palette_score_charts[2],
                     lighten(palette_score_charts[2],amount=.25),
                     palette_score_charts[1],
                     "grey80"
   )
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
      rounded.bars_plot <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot
      
      snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                  chart.palette = chart.palette,
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    })
    # Blank / right axis
    group_label <- ggplot()+theme_nothing()
    
    rounded.bars_plot <- ggplot()+theme_nothing()
    
    snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                chart.palette = "white",
                                n_bins_max=n_bins_max,
                                axis_only = TRUE)$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All years")
    
    rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis = FALSE)$plot
    
    snakebins_plot <- ggplot()+theme_nothing()
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Axis row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    rounded.bars_plot <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$cats_rects
    rounded.bars_plot <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE,legend=TRUE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","white","black","white","black","black","black"))+
      geom_segment(x=1/3,xend=1/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3+.05,y=.5,yend=0.5,linetype=3)+
      ylim(0,1.2)+
      annotate("text",x=1/6,y=1.05,label="Both Code and Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=1/2,y=1.05,label="Either Code or Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=5/6,y=1.05,label="Neither Code nor Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=2/3+.02,
               y=.25,label="Data only",
               color="black",size=3.7,hjust=0)+
      annotate("text",x=2/3+.02,
               y=.75,label="Code only",
               color="black",size=3.7,hjust=0)

    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars_plot,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Display plots
    plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.7,3))
  })
  
  output$pr_outcomes_overall <- renderPlot({
    data <- pr_outcomes %>% 
      filter(!covid) %>% 
      mutate(d_c_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared != "no") %>% 
      mutate(d_c_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared != "no") %>% 
      mutate(d_c_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared != "no") %>% 
      mutate(d_only_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared == "no") %>% 
      mutate(d_only_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared == "no") %>% 
      mutate(d_only_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared == "no") %>% 
      mutate(no_data_code_available = OA_data_shared == "no" & OA_code_shared != "no") %>% 
      mutate(no_data_no_code = OA_data_shared == "no" & OA_code_shared == "no") %>% 
      select(paper_id, d_c_open, d_c_protect, d_c_sharing, d_only_open, d_only_sharing, no_data_code_available, no_data_no_code) %>% 
      pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>% 
      filter(response)
    
    data$cat <- ordered(data$cat,
                        levels = c("d_c_open","d_c_protect","d_c_sharing","d_only_open",
                                   "d_only_sharing","no_data_code_available","no_data_no_code"),
                        labels = c("Open data,\ncode available", "Data restricted,\ncode available", 
                                   "Data shared directly,\ncode available", "Open data,\ncode unavailable",
                                   "Data shared directly,\ncode unavailable",
                                   "Data unavailable,\ncode available",
                                   "Neither data nor\ncode available")
                        
                        
    )
    # Define nesting structure
    cat <- levels(data$cat)
    nesting.structure <- data.frame(cat)
    
    nesting.structure$cat2 <- c("Open data,\ncode available", "Data restricted,\ncode available", 
                                "Data shared directly,\ncode available", "Either data\nor code",
                                "Either data\nor code",
                                "Either data\nor code",
                                "Neither data\nnor code")
    nesting.structure$cat3 <- c(NA, NA, NA,"Data only","Data only","Code only",NA)
    
    
    nesting.structure$cat4 <- c("Both", "Both", "Both",
                                "Either",
                                "Either",
                                "Either",
                                "Neither")
    
    plot <- bars.and.snakebins(data,nesting.structure,
                               # chart.palette = rev(c("grey75",palette_score_charts[1],
                               #                        palette_score_charts[2],palette_score_charts[2],
                               #                        palette_score_charts[5],palette_score_charts[5],palette_score_charts[5])),
                               chart.palette = c(palette_score_charts[5],
                                                 lighten(palette_score_charts[5],amount=.25),
                                                 lighten(palette_score_charts[5],amount=.5),
                                                 palette_score_charts[2],
                                                 lighten(palette_score_charts[2],amount=.25),
                                                 palette_score_charts[1],
                                                 "grey80"
                               ),
                               display_axis = TRUE,
                               #n_bins = 5,
                               n_bins_max = 650,
                               bin_width_x = .01,
                               bins_offset = .2)$plot
    cats_rects <- bars.and.snakebins(data,nesting.structure)$cats_rects
    cats_rects$cat4 <- c("Both","Both","Both","Either","Either","Either","Neither")
    
    cats_rects_collapsed <- cats_rects %>%
      group_by(cat4) %>%
      summarise(xmin = min(xmin),
                xmax = max(xmax),
                ymin = min(ymin),
                ymax = max(ymax),
                n=sum(count))
    plot +
      funkyheatmap::geom_rounded_rect(data=cats_rects_collapsed,inherit.aes = FALSE,
                                      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                      radius=unit(3, "points"),show.legend=FALSE,
                                      alpha=0,
                                      #size=1,
                                      size=0,
                                      color="black")+
      
      annotate("text",vjust=1,size=5.5,
               x=c((cats_rects[cats_rects$cat=="Data shared directly,\ncode available",]$xmax+cats_rects[cats_rects$cat=="Open data,\ncode available",]$xmin)/2,
                   (max(cats_rects[cats_rects$cat2=="Either data\nor code",]$xmax)+min(cats_rects[cats_rects$cat2=="Either data\nor code",]$xmin))/2,
                   (cats_rects[cats_rects$cat=="Neither data nor\ncode available",]$xmax+cats_rects[cats_rects$cat=="Neither data nor\ncode available",]$xmin)/2),
               y=c(1.3,1.3,1.3),
               label=c("Both Data and\nCode Available",
                       "Either Data or\nCode Available",
                       "Neither Data nor\nCode Available")
      )+
      annotate("text",vjust=1,size=4,
               x=c((cats_rects[cats_rects$cat=="Data shared directly,\ncode available",]$xmax+cats_rects[cats_rects$cat=="Open data,\ncode available",]$xmin)/2,
                   (max(cats_rects[cats_rects$cat2=="Either data\nor code",]$xmax)+min(cats_rects[cats_rects$cat2=="Either data\nor code",]$xmin))/2,
                   (cats_rects[cats_rects$cat=="Neither data nor\ncode available",]$xmax+cats_rects[cats_rects$cat=="Neither data nor\ncode available",]$xmin)/2),
               y=c(1.13,1.13,1.13),
               label=c(paste0(format.round((cats_rects_collapsed$xmax[3]-cats_rects_collapsed$xmin[3])*100,1),
                              "%\nn = ",cats_rects_collapsed$n[3],
                              " / ",sum(cats_rects_collapsed$n)),
                       paste0(format.round((cats_rects_collapsed$xmax[2]-cats_rects_collapsed$xmin[2])*100,1),
                              "%\nn = ",cats_rects_collapsed$n[2],
                              " / ",sum(cats_rects_collapsed$n)),
                       paste0(format.round((cats_rects_collapsed$xmax[1]-cats_rects_collapsed$xmin[1])*100,1),
                              "%\nn = ",cats_rects_collapsed$n[1],
                              " / ",sum(cats_rects_collapsed$n))))+
      geom_segment(x=cats_rects_collapsed[cats_rects_collapsed$cat4=="Either",]$xmin,
                   xend=cats_rects_collapsed[cats_rects_collapsed$cat4=="Either",]$xmin-.01,
                   y=1.01,yend=1.1,
                   linetype=3,inherit.aes=FALSE)+
      geom_segment(x=cats_rects_collapsed[cats_rects_collapsed$cat4=="Either",]$xmax,
                   xend=cats_rects_collapsed[cats_rects_collapsed$cat4=="Either",]$xmax+.01,
                   y=1.01,yend=1.1,
                   linetype=3,inherit.aes=FALSE)+
      geom_segment(x=cats_rects[!is.na(cats_rects$cat3)&cats_rects$cat3=="Code only",]$xmax,
                   xend=cats_rects[!is.na(cats_rects$cat3)&cats_rects$cat3=="Code only",]$xmax+.02,
                   y=cats_rects[!is.na(cats_rects$cat3)&cats_rects$cat3=="Code only",]$ymin,
                   yend=0.5,
                   linetype=3,inherit.aes=FALSE)+
      geom_segment(x=cats_rects[!is.na(cats_rects$cat3)&cats_rects$cat3=="Code only",]$xmax+.02,
                   xend=cats_rects[!is.na(cats_rects$cat3)&cats_rects$cat3=="Code only",]$xmax+.02+.05,
                   y=0.5,
                   yend=0.5,
                   linetype=3,inherit.aes=FALSE)+
      annotate("text",x=cats_rects$xcenter[1],y=cats_rects$ycenter[1],label=cats_rects$cat[1],color="white",size=3.7)+
      annotate("text",x=cats_rects$xcenter[2],y=-.1,label=cats_rects$cat[2],color="black",size=3.7)+
      annotate("text",x=cats_rects$xcenter[3],y=cats_rects$ycenter[3],label="Data shared\ndirectly,\nCode available",color="black",size=3.7)+
      annotate("text",x=cats_rects$xcenter[4],y=cats_rects$ycenter[4],label="Open data,\nCode\nunavailable",color="white",size=3.7)+
      annotate("text",x=cats_rects$xcenter[5],y=-.1,label=cats_rects$cat[5],color="black",size=3.7)+
      annotate("text",x=cats_rects$xcenter[6],y=cats_rects$ycenter[6],label=cats_rects$cat[6],color="black",size=3.7)+
      annotate("text",x=cats_rects[!is.na(cats_rects$cat3)&cats_rects$cat3=="Code only",]$xmax+.03,
               y=.75,label=paste0("Code only\n",
                                  format.round((sum(cats_rects[cats_rects$cat3=="Code only",]$count,na.rm=TRUE)/ sum(cats_rects[!is.na(cats_rects$cat3),]$count,na.rm=TRUE))*100,1),
                                  "%\nn = ",sum(cats_rects[cats_rects$cat3=="Code only",]$count,na.rm=TRUE),
                                  " / ",sum(cats_rects[!is.na(cats_rects$cat3),]$count,na.rm=TRUE)),
               color="black",size=3.7,hjust=0)+
      annotate("text",x=cats_rects[!is.na(cats_rects$cat3)&cats_rects$cat3=="Code only",]$xmax+.03,
               y=.25,label=paste0("Data only\n",
                                  format.round((sum(cats_rects[cats_rects$cat3=="Data only",]$count,na.rm=TRUE)/ sum(cats_rects[!is.na(cats_rects$cat3),]$count,na.rm=TRUE))*100,1),
                                  "%\nn = ",sum(cats_rects[cats_rects$cat3=="Data only",]$count,na.rm=TRUE),
                                  " / ",sum(cats_rects[!is.na(cats_rects$cat3),]$count,na.rm=TRUE)),
               color="black",size=3.7,hjust=0)+
      annotate("text",vjust=0,
               x=c(.1,.1,.1),
               y=c(-.1,-.1,-.1),
               label=c(" ",
                       " ",
                       " "))

  })
  
  output$pr_outcomes_by_year_econ_vs <- renderPlot({
    data <- pr_outcomes %>% 
      filter(!covid) %>% 
      mutate(d_c_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared != "no") %>% 
      mutate(d_c_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared != "no") %>% 
      mutate(d_c_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared != "no") %>% 
      mutate(d_only_open = data_available == "Yes" & restricted_data == "No" & OA_code_shared == "no") %>% 
      mutate(d_only_protect = data_available == "Yes" & restricted_data != "No" & OA_code_shared == "no") %>% 
      mutate(d_only_sharing = str_detect(OA_data_shared, "yes") & OA_code_shared == "no") %>% 
      mutate(no_data_code_available = OA_data_shared == "no" & OA_code_shared != "no") %>% 
      mutate(no_data_no_code = OA_data_shared == "no" & OA_code_shared == "no") %>% 
      select(paper_id, d_c_open, d_c_protect, d_c_sharing, d_only_open, d_only_sharing, no_data_code_available, no_data_no_code) %>% 
      pivot_longer(cols = -paper_id, names_to = "cat", values_to = "response") %>% 
      filter(response)
    
    data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],by="paper_id",all.x =TRUE,all.y=FALSE)
    group_order <- seq(min(data$pub_year),max(data$pub_year,1))
    #data$pub_year <- ordered(data$pub_year,levels=group_order,labels=group_order)
    data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
    
    data$cat <- ordered(data$cat,
                        levels = c("d_c_open","d_c_protect","d_c_sharing","d_only_open",
                                   "d_only_sharing","no_data_code_available","no_data_no_code"),
                        labels = c("Open data,\ncode available", "Data restricted,\ncode available", 
                                   "Data shared directly,\ncode available", "Open data,\ncode unavailable",
                                   "Data shared directly,\ncode unavailable",
                                   "Data unavailable,\ncode available",
                                   "Neither data nor\ncode available")
    )
    data.econ.poli <- data[data$COS_pub_category=="economics and finance" | data$COS_pub_category=="political science",]
    data.other <- data[!data$COS_pub_category=="economics and finance" & !data$COS_pub_category=="political science",]
    # Define nesting structure
    cat <- levels(data$cat)
    nesting.structure <- data.frame(cat)
    
    nesting.structure$cat2 <- c("Both","Both","Both",
                                "Either",
                                "Either",
                                "Either",
                                "Neither")
    nesting.structure$cat3 <- c(NA, NA, NA,"Data only","Data only","Code only",NA)
    
    nesting.structure$cat <- ordered(nesting.structure$cat)
    nesting.structure$cat2 <- ordered(nesting.structure$cat2)
    nesting.structure$cat3 <- ordered(nesting.structure$cat3)
    
    chart.palette <- c(palette_score_charts[5],
                      lighten(palette_score_charts[5],amount=.25),
                      lighten(palette_score_charts[5],amount=.5),
                      palette_score_charts[2],
                      lighten(palette_score_charts[2],amount=.25),
                      palette_score_charts[1],
                      "grey80"
    )
    
    plotlist.left <- lapply(1:length(group_order),function(x) 
      rounded.bars(data.econ.poli[data.econ.poli$pub_year==group_order[x],],nesting.structure,
                         chart.palette = chart.palette,
                         display_axis = FALSE,
                         flip_x=TRUE)$plot+
        theme(axis.title.y=element_blank())+
        ylab(group_order[x])
    )
    plotlist.left[[length(plotlist.left)+1]] <-
      rounded.bars(data[data$pub_year==group_order[1],],nesting.structure,
                   chart.palette = rep("white",length(group_order)),
                   axis_only = TRUE,flip_x=TRUE)$plot
    plot.left <- plot_grid(plotlist=plotlist.left,ncol=1,align = "v")
    
    plotlist.right <- lapply(1:length(group_order),function(x) 
      rounded.bars(data.other[data.other$pub_year==group_order[x],],nesting.structure,
                         chart.palette = chart.palette,
                         display_axis = FALSE,)$plot+
        theme(axis.title.y=element_blank())+
        ylab(group_order[x])
    )
    plotlist.right[[length(plotlist.right)+1]] <-
      rounded.bars(data[data$pub_year==group_order[1],],nesting.structure,
                         chart.palette = rep("white",length(group_order)),
                         axis_only = TRUE)$plot
    plot.right <- plot_grid(plotlist=plotlist.right,ncol=1,align = "v")
    
    plotlist.center <- lapply(1:length(group_order),function(x) 
      #ggplot()+theme_blank() +
      ggplot()+theme_minimal() +
        theme(legend.position = "none",
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.title=element_blank(),
              axis.text = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
        lims(x= c(0,1), y = c(0,1))+
        annotate("text",x=0.5,y=0.5,
                 label=group_order[x])
    )
    plotlist.center[[length(plotlist.center)+1]] <-
      ggplot()+theme_nothing() +
      theme(legend.position = "none",
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.title=element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
      scale_x_continuous(limits=c(0,1),expand=c(0,0))+
      scale_y_continuous(limits=c(0,1),expand=c(0,0))+
      annotate("text",x=0.5,y=0.5,
               label=" ",hjust=0.5)
    
    plot.center <- plot_grid(plotlist=plotlist.center,ncol=1,align = "v",rel_heights = c(rep(1,length(plotlist.center)-1),0.5))
    
    width.ratio <- 8
    
    plot.top.labels <-
      plot_grid(
        ggplot()+theme_nothing() +
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title=element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
          scale_x_continuous(limits=c(-1,0),expand=c(0,0))+
          scale_y_continuous(limits=c(0,1),expand=c(0,0))+
          annotate("text",x=-.5,y=0.5,
                   label="Economics and Political Science",hjust=0.5),
        ggplot()+theme_nothing() +
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title=element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
          scale_x_continuous(limits=c(0,1),expand=c(0,0))+
          scale_y_continuous(limits=c(0,1),expand=c(0,0))+
          annotate("text",x=0.5,y=0.5,
                   label=" "),
        ggplot()+theme_nothing() +
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title=element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
          scale_x_continuous(limits=c(0,1),expand=c(0,0))+
          scale_y_continuous(limits=c(0,1),expand=c(0,0))+
          annotate("text",x=0.5,y=0.5,
                   label="All other fields",hjust=0.5),
        ncol=3,rel_widths = c(width.ratio,1,width.ratio))
    
    plot.main <- plot_grid(plot.left,plot.center,plot.right,ncol=3,rel_widths = c(width.ratio,1,width.ratio))
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$cats_rects
    legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE,legend=TRUE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","white","black","white","black","black","black"))+
      geom_segment(x=1/3,xend=1/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3,y=1,yend=1.2,linetype=3)+
      geom_segment(x=2/3,xend=2/3+.05,y=.5,yend=0.5,linetype=3)+
      ylim(0,1.2)+
      annotate("text",x=1/6,y=1.05,label="Both Code and Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=1/2,y=1.05,label="Either Code or Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=5/6,y=1.05,label="Neither Code nor Data",color="black",size=3.7,vjust=0)+
      annotate("text",x=2/3+.02,
               y=.25,label="Data only",
               color="black",size=3.7,hjust=0)+
      annotate("text",x=2/3+.02,
               y=.75,label="Code only",
               color="black",size=3.7,hjust=0)
    
    
    plot_grid(plot.top.labels,plot.main,legend,
              ncol=1,rel_heights = c(1/length(plotlist.left),1,3/length(plotlist.left)))
  })
  
  output$or_outcomes_by_year <- renderPlot({
    data <- status %>%
      filter(RR) %>%
      filter(p1_delivery | p2_delivery) %>%
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
      select(claim_id) %>%
      bind_rows(
        stitched_claims %>%
          mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
          select(claim_id)
      )
    data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
    data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
                  by="claim_id",all.x=TRUE,all.y=FALSE)
    data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                  by="paper_id",all.x =TRUE,all.y=FALSE)
    
    data<-data[data$is_covid==FALSE,]
    data$is_covid <- NULL
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    data <- data[data$repro_outcome_overall!="none",]
    data <- data %>%
      group_by(paper_id) %>%
      mutate(weight=1/n())
    data$field <- str_to_title(data$COS_pub_category)
    
    # Assign group
    group_order <- seq(min(data$pub_year),max(data$pub_year,1))
    data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
    
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    
    data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
    
    data$cat <- ordered(data$repro_outcome_overall,
                        labels=c( "Push Button\nReproduced",
                                  "Precisely\nReproduced",
                                  "Approximately\nReproduced",
                                  "Not\nReproduced",
                                  "Not\nAttempted"),
                        levels=c( "Push Button",
                                  "Precise",
                                  "Approximate",
                                  "Not",
                                  "Not Attempted"))
    
    data <- data %>%
      group_by(paper_id) %>%
      mutate(weight=1/n())
    
    # Define nesting structure
    cat <- levels(data$cat)
    nesting.structure <- data.frame(cat)
    
    nesting.structure$cat2 <- nesting.structure$cat
    nesting.structure$cat3 <- nesting.structure$cat
    
    nesting.structure$cat <- ordered(nesting.structure$cat)
    nesting.structure$cat2 <- ordered(nesting.structure$cat2)
    nesting.structure$cat3 <- ordered(nesting.structure$cat3)
    
    chart.palette <- c(palette_score_charts[1],
                       palette_score_charts[4],
                       palette_score_charts[3],
                       palette_score_charts[2],
                       "grey90")
    
    data.largecat <- data
    levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
    cat <- levels(data.largecat$cat)
    nesting.structure.largecat <- data.frame(cat)
    
    nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
    nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat
    
    nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
    nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
    nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)
    
    chart.palette.largecat <- c(palette_score_charts[5],chart.palette[5])
    
    # Aesthetic setup
    bars_range <- c(0,.25)
    col_widths <- c(.4,1.2,4,1)
    n_bins_max <- 80
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
      cats_rects <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                 weightvar="weight",
                                 chart.palette = chart.palette,
                                 display_axis = FALSE)$cats_rects
      cats_rects_final <- cats_rects[nrow(cats_rects),]
      rounded.bars.cutoff <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          weightvar="weight",
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot+
        funkyheatmap::geom_rounded_rect(data=cats_rects_final,aes(xmin=cats_rects_final$xmin,
                                                                  ymin=cats_rects_final$ymin,
                                                                  xmax=bars_range[2],
                                                                  ymax=cats_rects_final$ymax),
                                        radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
        xlim(bars_range)+
        ggpattern::geom_rect_pattern(data=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
                                                               ymin=cats_rects_final$ymin,
                                                               xmax=bars_range[2],
                                                               ymax=cats_rects_final$ymax),
                                     pattern="gradient",pattern_orientation = "horizontal",
                                     pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
      
      rounded.bars.largecat <- rounded.bars(data.largecat[data.largecat$group==group_order[x],],nesting.structure.largecat,
                                            weightvar="weight",
                                            chart.palette = chart.palette.largecat,
                                            display_axis = FALSE)$plot
      
      snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                  chart.palette = chart.palette,
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    })
    # Blank / right axis
    group_label <- ggplot()+theme_nothing()
    
    rounded.bars.cutoff <- ggplot()+theme_nothing()
    
    rounded.bars.largecat <- ggplot()+theme_nothing()
    
    snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                chart.palette = "white",
                                n_bins_max=n_bins_max,
                                axis_only = TRUE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All years")
    
    cats_rects <- rounded.bars(data,nesting.structure,
                               weightvar="weight",
                               chart.palette = chart.palette,
                               display_axis = FALSE)$cats_rects
    cats_rects_final <- cats_rects[nrow(cats_rects),]
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        weightvar="weight",
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot+
      funkyheatmap::geom_rounded_rect(aes(xmin=cats_rects_final$xmin,
                                          ymin=cats_rects_final$ymin,
                                          xmax=bars_range[2],
                                          ymax=cats_rects_final$ymax),
                                      radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
      xlim(bars_range)+
      ggpattern::geom_rect_pattern(data=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
                                                             ymin=cats_rects_final$ymin,
                                                             xmax=bars_range[2],
                                                             ymax=cats_rects_final$ymax),
                                   pattern="gradient",pattern_orientation = "horizontal",
                                   pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
    
    rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                          weightvar="weight",
                                          chart.palette = chart.palette.largecat,
                                          display_axis = FALSE)$plot
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Axis row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    
    rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                          chart.palette = rep("white",length(chart.palette.largecat)),
                                          axis_only = TRUE)$plot+
      scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.cutoff <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("black","black","black","white","black"))
    
    data.legend <- data.largecat %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure.largecat,
                                      chart.palette = chart.palette.largecat,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.largecat <- rounded.bars(data.legend,nesting.structure.largecat,
                                          chart.palette = chart.palette.largecat,
                                          display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","black"))
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    # Display plots
    plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.5,1.5))
    
  })
  
  output$or_outcomes_by_field <- renderPlot({
    data <- status %>%
      filter(RR) %>%
      filter(p1_delivery | p2_delivery) %>%
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
      select(claim_id) %>%
      bind_rows(
        stitched_claims %>%
          mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
          select(claim_id)
      )
    data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
    data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
                  by="claim_id",all.x=TRUE,all.y=FALSE)
    data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                  by="paper_id",all.x =TRUE,all.y=FALSE)
    
    data<-data[data$is_covid==FALSE,]
    data$is_covid <- NULL
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    data <- data[data$repro_outcome_overall!="none",]
    data <- data %>%
      group_by(paper_id) %>%
      mutate(weight=1/n())
    data$field <- str_to_title(data$COS_pub_category)
    
    # Assign group
    data$field <- str_to_title(data$COS_pub_category)
    data$field <- str_replace_all(data$field," ","\n")
    data$field <- str_replace_all(data$field,"And","and")
    group_order <- unique(data$field)

    data$group <- ordered(data$field,levels=group_order,labels=group_order)
    
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    
    data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
    
    data$cat <- ordered(data$repro_outcome_overall,
                        labels=c( "Push Button\nReproduced",
                                  "Precisely\nReproduced",
                                  "Approximately\nReproduced",
                                  "Not\nReproduced",
                                  "Not\nAttempted"),
                        levels=c( "Push Button",
                                  "Precise",
                                  "Approximate",
                                  "Not",
                                  "Not Attempted"))
    
    data <- data %>%
      group_by(paper_id) %>%
      mutate(weight=1/n())
    
    # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      
      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat
      
      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
      
      chart.palette <- c(palette_score_charts[1],
                         palette_score_charts[4],
                         palette_score_charts[3],
                         palette_score_charts[2],
                         "grey90")
      
      data.largecat <- data
      levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)
      
      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat
      
      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)
      
      chart.palette.largecat <- c(palette_score_charts[5],chart.palette[5])
      
    # Aesthetic setup
      bars_range <- c(0,.4)
      col_widths <- c(.4,1.2,4,1)
      n_bins_max <- 180
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
      cats_rects <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                 weightvar="weight",
                                 chart.palette = chart.palette,
                                 display_axis = FALSE)$cats_rects
      cats_rects_final <- cats_rects[nrow(cats_rects),]
      rounded.bars.cutoff <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          weightvar="weight",
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot+
        funkyheatmap::geom_rounded_rect(data=cats_rects_final,aes(xmin=cats_rects_final$xmin,
                                            ymin=cats_rects_final$ymin,
                                            xmax=bars_range[2],
                                            ymax=cats_rects_final$ymax),
                                        radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
        xlim(bars_range)+
        ggpattern::geom_rect_pattern(data=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
                                                               ymin=cats_rects_final$ymin,
                                                               xmax=bars_range[2],
                                                               ymax=cats_rects_final$ymax),
                                     pattern="gradient",pattern_orientation = "horizontal",
                                     pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
      
      rounded.bars.largecat <- rounded.bars(data.largecat[data.largecat$group==group_order[x],],nesting.structure.largecat,
                                            weightvar="weight",
                                            chart.palette = chart.palette.largecat,
                                            display_axis = FALSE)$plot
      
      snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                  chart.palette = chart.palette,
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    })
    # Blank / right axis
    group_label <- ggplot()+theme_nothing()

    rounded.bars.cutoff <- ggplot()+theme_nothing()
    
    rounded.bars.largecat <- ggplot()+theme_nothing()
    
    snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                chart.palette = "white",
                                n_bins_max=n_bins_max,
                                axis_only = TRUE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All fields")
    
    cats_rects <- rounded.bars(data,nesting.structure,
                               weightvar="weight",
                               chart.palette = chart.palette,
                               display_axis = FALSE)$cats_rects
    cats_rects_final <- cats_rects[nrow(cats_rects),]
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        weightvar="weight",
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot+
      funkyheatmap::geom_rounded_rect(aes(xmin=cats_rects_final$xmin,
                                          ymin=cats_rects_final$ymin,
                                          xmax=bars_range[2],
                                          ymax=cats_rects_final$ymax),
                                      radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
      xlim(bars_range)+
      ggpattern::geom_rect_pattern(data=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
                                                             ymin=cats_rects_final$ymin,
                                                             xmax=bars_range[2],
                                                             ymax=cats_rects_final$ymax),
                                   pattern="gradient",pattern_orientation = "horizontal",
                                   pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
    
    rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                          weightvar="weight",
                                          chart.palette = chart.palette.largecat,
                                          display_axis = FALSE)$plot
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Axis row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    
    rounded.bars.largecat <- rounded.bars(data.largecat,nesting.structure.largecat,
                                          chart.palette = rep("white",length(chart.palette.largecat)),
                                          axis_only = TRUE)$plot+
      scale_x_continuous(breaks=c(0,1),labels=c("0%","100%"))
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                           chart.palette = chart.palette,
                           display_axis=FALSE)$cats_rects
    rounded.bars.cutoff <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("black","black","black","white","black"))
    
    data.legend <- data.largecat %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure.largecat,
                                      chart.palette = chart.palette.largecat,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.largecat <- rounded.bars(data.legend,nesting.structure.largecat,
                                          chart.palette = chart.palette.largecat,
                                          display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("white","black"))
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.largecat,rounded.bars.cutoff,snakebins_plot,ncol=4,rel_widths = col_widths,align="v")
    
    # Display plots
    plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.5,1))
    
  })

  output$or_outcomes_by_year_and_field <- renderPlot({
    # Data manipulation
    {
      data <- status %>%
        filter(RR) %>%
        filter(p1_delivery | p2_delivery) %>%
        mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
        select(claim_id) %>%
        bind_rows(
          stitched_claims %>%
            mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
            select(claim_id)
        )
      data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
      data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall")],
                    by="claim_id",all.x=TRUE,all.y=FALSE)
      data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                    by="paper_id",all.x =TRUE,all.y=FALSE)
      
      data<-data[data$is_covid==FALSE,]
      data$is_covid <- NULL
      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)
      data <- data[data$repro_outcome_overall!="none",]
      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())
      data$field <- str_to_title(data$COS_pub_category)
      
      data$field <- str_to_title(data$COS_pub_category)
      data$field <- str_replace_all(data$field," ","\n")
      data$field <- str_replace_all(data$field,"And","and")
      field_order <- unique(data$field)
      
      data$field <- ordered(data$field,levels=field_order,labels=field_order)
      
      # Assign group
      group_order <- seq(min(data$pub_year),max(data$pub_year,1))
      data$group <- ordered(data$pub_year,levels=group_order,labels=group_order)
      
      data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                           "not attempted",
                                           data$repro_outcome_overall)
      
      data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
      
      data$cat <- ordered(data$repro_outcome_overall,
                          labels=c( "Push Button\nReproduced",
                                    "Precisely\nReproduced",
                                    "Approximately\nReproduced",
                                    "Not\nReproduced",
                                    "Not\nAttempted"),
                          levels=c( "Push Button",
                                    "Precise",
                                    "Approximate",
                                    "Not",
                                    "Not Attempted"))
      
      data <- data %>%
        group_by(paper_id) %>%
        mutate(weight=1/n())
      
      # Define nesting structure
      cat <- levels(data$cat)
      nesting.structure <- data.frame(cat)
      
      nesting.structure$cat2 <- nesting.structure$cat
      nesting.structure$cat3 <- nesting.structure$cat
      
      nesting.structure$cat <- ordered(nesting.structure$cat)
      nesting.structure$cat2 <- ordered(nesting.structure$cat2)
      nesting.structure$cat3 <- ordered(nesting.structure$cat3)
      
      chart.palette <- c(palette_score_charts[1],
                         palette_score_charts[4],
                         palette_score_charts[3],
                         palette_score_charts[2],
                         "grey90")
      
      data.largecat <- data
      levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
      cat <- levels(data.largecat$cat)
      nesting.structure.largecat <- data.frame(cat)
      
      nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
      nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat
      
      nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
      nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
      nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)
      
      chart.palette.largecat <- c(palette_score_charts[5],chart.palette[5])
    }
    
    # Aesthetic setup
    bars_range <- c(0,1)
    col_widths <- c(.4,1.2,4,1)
    #n_bins_max <- 80
    
    # Group by field plots
    plotlist_field <- lapply(levels(data$field),function(field) {
      print(field)
      data_field <- data[data$field==field,]
      
      plotlist <- lapply(1:length(group_order),function(x) {
        cats_rects <- rounded.bars(data_field[data_field$group==group_order[x],],nesting.structure,
                                   weightvar="weight",
                                   chart.palette = chart.palette,
                                   display_axis = FALSE)$cats_rects
        cats_rects_final <- cats_rects[nrow(cats_rects),]
        rounded.bars.cutoff <- rounded.bars(data_field[data_field$group==group_order[x],],nesting.structure,
                                            weightvar="weight",
                                            chart.palette = chart.palette,
                                            display_axis = FALSE)$plot+
          funkyheatmap::geom_rounded_rect(data_field=cats_rects_final,aes(xmin=cats_rects_final$xmin,
                                                                    ymin=cats_rects_final$ymin,
                                                                    xmax=bars_range[2],
                                                                    ymax=cats_rects_final$ymax),
                                          radius=unit(3, "points"),size=0,fill=chart.palette[length(chart.palette)])+
          xlim(bars_range)
          # ggpattern::geom_rect_pattern(data_field=cats_rects_final,aes(xmin=bars_range[2]-(bars_range[2]-bars_range[1])/10,
          #                                                        ymin=cats_rects_final$ymin,
          #                                                        xmax=bars_range[2],
          #                                                        ymax=cats_rects_final$ymax),
          #                              pattern="gradient",pattern_orientation = "horizontal",
          #                              pattern_fill=chart.palette[length(chart.palette)],pattern_fill2="white")
        rounded.bars.cutoff
      })
      
      plotlist <- append(list(ggplot() + theme_nothing()+
                                annotate("text",x=0.5,y=1,label=field)),
                         plotlist)
      
      plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1.5,rep(1,length(group_order))))
    })
    
    # Display plots
    
    year_labels <- lapply(c("",levels(data$group)),function(label) {
      ggplot() + theme_nothing()+
        annotate("text",x=0.5,y=1,label=label)
    })
    year_labels_plot <- plot_grid(plotlist=year_labels,ncol=1,align = "v")
    
    total_plot <- append(list(year_labels_plot),
                             plotlist_field)
    
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE)$cats_rects
    legend <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("black","black","black","white","black"))
    legend_bar <- plot_grid(ggplot()+theme_nothing()+
                              annotate("text",x=0.5,y=1,label=""),
                            legend,nrow=1,rel_widths = c(1,length(levels(data$cat))))
    
    main_plots <- plot_grid(plotlist=total_plot,nrow=1)
    
    plot_grid(main_plots,legend_bar,ncol=1,rel_heights = c(1+length(unique(data$pub_year)),1.2))
  
  })
  
  output$or_outcomes_by_repro_type <- renderPlot({
    data <- status %>%
      filter(RR) %>%
      filter(p1_delivery | p2_delivery) %>%
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>%
      select(claim_id) %>%
      bind_rows(
        stitched_claims %>%
          mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>%
          select(claim_id)
      )
    data$paper_id <- do.call(c,lapply(1:nrow(data),function(x) strsplit(data$claim_id[x],"_")[[1]][1]))
    data <- merge(data,repro_outcomes[c("claim_id","repro_outcome_overall","repro_type")],
                  by="claim_id",all.x=TRUE,all.y=FALSE)
    data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category","is_covid")],
                  by="paper_id",all.x =TRUE,all.y=FALSE)
    
    data<-data[data$is_covid==FALSE,]
    data$is_covid <- NULL
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    data <- data[data$repro_outcome_overall!="none",]
    data <- data %>%
      group_by(paper_id) %>%
      mutate(weight=1/n())
    data$field <- str_to_title(data$COS_pub_category)
    
    # Assign group
    group_order <- c("Source Data Reproduction",
                     "Author Data Reproduction",
                     "Extended Push Button Reproduction",
                     "Push Button Reproduction"
                     )
    data$group <- ordered(data$repro_type,levels=group_order,labels=group_order)
    
    data$repro_outcome_overall <- ifelse(is.na(data$repro_outcome_overall),
                                         "not attempted",
                                         data$repro_outcome_overall)
    
    data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
    
    data$cat <- ordered(data$repro_outcome_overall,
                        labels=c( "Push Button\nReproduced",
                                  "Precisely\nReproduced",
                                  "Approximately\nReproduced",
                                  "Not\nReproduced",
                                  "Not\nAttempted"),
                        levels=c( "Push Button",
                                  "Precise",
                                  "Approximate",
                                  "Not",
                                  "Not Attempted"))
    
    data <- data %>%
      group_by(paper_id) %>%
      mutate(weight=1/n())
    
    # Define nesting structure
    cat <- levels(data$cat)
    nesting.structure <- data.frame(cat)
    
    nesting.structure$cat2 <- nesting.structure$cat
    nesting.structure$cat3 <- nesting.structure$cat
    
    nesting.structure$cat <- ordered(nesting.structure$cat)
    nesting.structure$cat2 <- ordered(nesting.structure$cat2)
    nesting.structure$cat3 <- ordered(nesting.structure$cat3)
    
    chart.palette <- c(palette_score_charts[1],
                       palette_score_charts[4],
                       palette_score_charts[3],
                       palette_score_charts[2],
                       "grey90")
    
    data.largecat <- data
    levels(data.largecat$cat) <- c(rep("Attempted",4),"Not attempted")
    cat <- levels(data.largecat$cat)
    nesting.structure.largecat <- data.frame(cat)
    
    nesting.structure.largecat$cat2 <- nesting.structure.largecat$cat
    nesting.structure.largecat$cat3 <- nesting.structure.largecat$cat
    
    nesting.structure.largecat$cat <- ordered(nesting.structure.largecat$cat)
    nesting.structure.largecat$cat2 <- ordered(nesting.structure.largecat$cat2)
    nesting.structure.largecat$cat3 <- ordered(nesting.structure.largecat$cat3)
    
    chart.palette.largecat <- c(palette_score_charts[5],chart.palette[5])
    
    # Aesthetic setup
    bars_range <- c(0,1)
    col_widths <- c(1,4,1)
    n_bins_max <- 80
    
    # Drop not attempteds / missing repro types
    data <- data[!is.na(data$repro_type),]
    
    # Group by group plots
    plotlist <- lapply(1:length(group_order),function(x) {
      group_label <- ggplot()+theme_nothing()+
        annotate("text",x=0.5,y=1,label=group_order[x])
      
      cats_rects <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                 weightvar="weight",
                                 chart.palette = chart.palette,
                                 display_axis = FALSE)$cats_rects
      cats_rects_final <- cats_rects[nrow(cats_rects),]
      rounded.bars.cutoff <- rounded.bars(data[data$group==group_order[x],],nesting.structure,
                                          weightvar="weight",
                                          chart.palette = chart.palette,
                                          display_axis = FALSE)$plot+
        xlim(bars_range)
      
      snakebins_plot <- snakebins(data[data$group==group_order[x],],nesting.structure,
                                  chart.palette = chart.palette,
                                  n_bins_max=n_bins_max,
                                  display_axis = FALSE,
                                  collapsevar="paper_id")$plot
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    })
    # Blank / right axis
    group_label <- ggplot()+theme_nothing()
    
    rounded.bars.cutoff <- ggplot()+theme_nothing()
    
    snakebins_plot <- snakebins(data[data$group==group_order[1],],nesting.structure,
                                chart.palette = "white",
                                n_bins_max=n_bins_max,
                                axis_only = TRUE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <-
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Totals row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="All types")
    
    cats_rects <- rounded.bars(data,nesting.structure,
                               weightvar="weight",
                               chart.palette = chart.palette,
                               display_axis = FALSE)$cats_rects
    cats_rects_final <- cats_rects[nrow(cats_rects),]
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        weightvar="weight",
                                        chart.palette = chart.palette,
                                        display_axis = FALSE)$plot+
      xlim(bars_range)
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Axis row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    rounded.bars.cutoff <- rounded.bars(data,nesting.structure,
                                        chart.palette = rep("white",length(chart.palette)),
                                        axis_only = TRUE,)$plot+
      scale_x_continuous(limits=bars_range,labels=scales::percent_format())
    
    snakebins_plot <- snakebins(data,nesting.structure,
                                chart.palette = rep("white",length(chart.palette)),
                                n_bins_max=n_bins_max,
                                display_axis = FALSE,
                                collapsevar="paper_id")$plot+
      xlim(0,n_bins_max)
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    
    # Legend row
    group_label <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    data.legend <- data %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure,
                                      chart.palette = chart.palette,
                                      display_axis=FALSE)$cats_rects
    rounded.bars.cutoff <- rounded.bars(data.legend,nesting.structure,
                                        chart.palette = chart.palette,
                                        display_axis=FALSE)$plot+
      geom_text(data=cats_rects_legend,aes(x=xcenter,y=ycenter,label=cat),
                color=c("black","black","black","white"))
    
    data.legend <- data.largecat %>% group_by(cat) %>% summarise(n=n())
    cats_rects_legend <- rounded.bars(data.legend,nesting.structure.largecat,
                                      chart.palette = chart.palette.largecat,
                                      display_axis=FALSE)$cats_rects
    
    snakebins_plot <- ggplot()+theme_nothing()+
      annotate("text",x=0.5,y=1,label="")
    
    plotlist[[length(plotlist)+1]] <- 
      plot_grid(group_label,rounded.bars.cutoff,snakebins_plot,ncol=3,rel_widths = col_widths,align="v")
    # Display plots
    plot_grid(plotlist=plotlist,ncol=1,align = "v",rel_heights = c(rep(1,length(plotlist)-4),0.5,1.2,0.5,.8))
    
  })
}

shinyApp(ui, server)