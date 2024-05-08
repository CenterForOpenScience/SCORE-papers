
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
    library(ggchicklet) #devtools::install_github("hrbrmstr/ggchicklet")
    library(cowplot)
  }
  
  # Data loading
  {
    # Check if this is being run from shinyapps.io or from the github folder (in
    # which case the data gets pulled from the targets output)
    objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes","paper_metadata")
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
                       tabPanel("Figure: Code/Data by field",
                                plotOutput("code_and_data_by_field")
                       ),
                       
                  )
                )

  )
}

server <- function(input, output, session) {
  
  output$tagged_stats_table <- renderDT({
    # Pull paper to find what tags are in paper
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
  
  output$code_and_data_by_field <- renderPlot({
    # Data setup
    {
      pr <- pr_outcomes %>% filter(!covid) %>% 
        select(paper_id, data_available, code_available) %>% 
        rename(d = data_available, code = code_available) %>%
        mutate(
          across(
            .cols = c(d, code),
            .fns = function(x) str_detect(x, "Yes")
          )
        ) %>% 
        left_join(paper_metadata %>% select(paper_id, pub_year,COS_pub_category), by = "paper_id") %>% 
        mutate(field = str_to_sentence(COS_pub_category)) %>% 
        mutate(
          avail = case_when(
            d & !code ~ "data",
            !d & code ~ "code",
            d & code ~ "both",
            !d & !code ~ "neither"
          )
        ) %>% 
        mutate(avail = as_factor(avail) %>% fct_relevel(., "code", "data", "both", "neither")) %>% 
        group_by(field) %>% mutate(pr_ord = mean(avail == "both")) %>% 
        ungroup() %>% group_by(avail != "neither") %>% 
        arrange(avail) %>% ungroup() %>% select(-`avail != "neither"`)
      
      cats <- pr %>% 
        group_by(field, avail != "neither") %>% 
        mutate(rn = row_number()) %>% 
        mutate(cat = percent_rank(rn)) %>% 
        ungroup() %>% select(-`avail != "neither"`) %>% 
        replace_na(list(cat = 1)) %>% 
        mutate(field = as_factor(field) %>% fct_reorder(., pr_ord) %>% fct_rev()) %>% 
        mutate(cat = ifelse(field == "Sociology" & cat == 0, 0.79, cat)) %>% 
        mutate(cat = ifelse(field == "Economics" & avail == "neither" & cat == 0, 0.79, cat))

      cats$avail_collapsed <- str_to_title(ifelse(cats$avail=="code" | cats$avail=="data",
                                                  "Either Code or Data",as.character(cats$avail)))
      cats$avail_collapsed <- ordered(cats$avail_collapsed,
                                      labels=c("Neither","Either Code Or Data","Both"),
                                      levels=c("Neither","Either Code Or Data","Both"))
      cats$avail <- ordered(cats$avail,
                            labels=c("Neither","Code","Data","Both"),
                            levels=c("neither","code","data","both"))
      cats_rects <- cats %>%
        group_by(field,avail,avail_collapsed) %>%
        summarise(count=n())%>%
        group_by(field) %>%
        mutate(proportion=count/sum(count))
      
      cats_rects_chicklet <- cats_rects
      
      cats_rects_both <- cats_rects %>%
        filter(avail=="Both") %>%
        select(field,proportion) %>%
        rename(ymin = proportion)
      cats_rects_neither <- cats_rects %>%
        filter(avail=="Neither") %>%
        select(field,proportion) %>%
        rename(ymax = proportion) %>%
        mutate(ymax = 1-ymax)
      
      cats_rects <- cats_rects %>% 
        left_join(cats_rects_both, by = "field") %>%
        left_join(cats_rects_neither, by = "field") %>%
        filter(avail_collapsed=="Either Code Or Data") %>%
        group_by(field) %>%
        mutate(p_code_or_data = count/sum(count))
      
      cats_rects$xmin <- ifelse(cats_rects$avail=="Code",
                                as.numeric(cats_rects$field)-.45,
                                as.numeric(cats_rects$field)+.45-.9*cats_rects$p_code_or_data)
      cats_rects$xmax <- ifelse(cats_rects$avail=="Code",
                                cats_rects$xmin+.9*cats_rects$p_code_or_data,
                                as.numeric(cats_rects$field)+.45)
    }
    # Chart setup
    {
      palette_bars <- rev(c("grey90",palette_score_charts[1],
                            palette_score_charts[2],
                            palette_score_charts[5]))
      
      plot_grid(snake_bar_n_bin_chart(cats,"field","avail",rounded = TRUE,output="bar chart",
                                      palette_bars = palette_bars) + 
                  funkyheatmap::geom_rounded_rect(data=cats_rects,inherit.aes = FALSE,
                                                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,fill=avail),
                                                  radius=unit(3, "points"),show.legend=FALSE) + 
                  annotate("text",x=1,y=0.03,hjust=0,
                           label="Both Data\nand Code",color="white")+
                  annotate("text",x=1.25,y=mean(cats[cats$field=="Political science",]$avail=="Both") +
                             mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data") + .05,
                           label="Data only",color="black",hjust=0)+
                  annotate("text",x=.75,y=mean(cats[cats$field=="Political science",]$avail=="Both") +
                             mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data") + .05,
                           label="Code only",color="black",hjust=0)+
                  annotate("text",x=1,y=0.97,hjust=1,
                           label="Neither Data\nnor Code",color="black")+
                  geom_segment(aes(x=1-(cats_rects[cats_rects$field=="Political science" & cats_rects$avail=="Code",]$p_code_or_data)*.9,
                                   xend=1,
                                   y=mean(cats[cats$field=="Political science",]$avail=="Both") + 
                                     mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data"),
                                   yend=mean(cats[cats$field=="Political science",]$avail=="Both") + 
                                     mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data") + .05),
                               color="grey50",linetype=3) +
                  geom_segment(aes(x=1,
                                   xend=1,
                                   y=mean(cats[cats$field=="Political science",]$avail=="Both") + 
                                     mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data")+ .05,
                                   yend=mean(cats[cats$field=="Political science",]$avail=="Both") + 
                                     mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data") + .1),
                               color="grey50",linetype=3),
                snake_bar_n_bin_chart(cats,"field","avail",rounded = TRUE,output="snake bin chart",
                                      palette_snakebins = palette_bars),
                align = c("h"),rel_widths = c(4,1))
    }
  })
  
  output$code_and_data_by_field <- renderPlot({
    # Data setup
    {
      pr <- pr_outcomes %>% filter(!covid) %>% 
        select(paper_id, data_available, code_available) %>% 
        rename(d = data_available, code = code_available) %>%
        mutate(
          across(
            .cols = c(d, code),
            .fns = function(x) str_detect(x, "Yes")
          )
        ) %>% 
        left_join(paper_metadata %>% select(paper_id, pub_year,COS_pub_category), by = "paper_id") %>% 
        mutate(field = str_to_sentence(COS_pub_category)) %>% 
        mutate(
          avail = case_when(
            d & !code ~ "data",
            !d & code ~ "code",
            d & code ~ "both",
            !d & !code ~ "neither"
          )
        ) %>% 
        mutate(avail = as_factor(avail) %>% fct_relevel(., "code", "data", "both", "neither")) %>% 
        group_by(field) %>% mutate(pr_ord = mean(avail == "both")) %>% 
        ungroup() %>% group_by(avail != "neither") %>% 
        arrange(avail) %>% ungroup() %>% select(-`avail != "neither"`)
      
      cats <- pr %>% 
        group_by(field, avail != "neither") %>% 
        mutate(rn = row_number()) %>% 
        mutate(cat = percent_rank(rn)) %>% 
        ungroup() %>% select(-`avail != "neither"`) %>% 
        replace_na(list(cat = 1)) %>% 
        mutate(field = as_factor(field) %>% fct_reorder(., pr_ord) %>% fct_rev()) %>% 
        mutate(cat = ifelse(field == "Sociology" & cat == 0, 0.79, cat)) %>% 
        mutate(cat = ifelse(field == "Economics" & avail == "neither" & cat == 0, 0.79, cat))
      
      cats$avail_collapsed <- str_to_title(ifelse(cats$avail=="code" | cats$avail=="data",
                                                  "Either Code or Data",as.character(cats$avail)))
      cats$avail_collapsed <- ordered(cats$avail_collapsed,
                                      labels=c("Neither","Either Code Or Data","Both"),
                                      levels=c("Neither","Either Code Or Data","Both"))
      cats$avail <- ordered(cats$avail,
                            labels=c("Neither","Code","Data","Both"),
                            levels=c("neither","code","data","both"))
      cats_rects <- cats %>%
        group_by(field,avail,avail_collapsed) %>%
        summarise(count=n())%>%
        group_by(field) %>%
        mutate(proportion=count/sum(count))
      
      cats_rects_chicklet <- cats_rects
      
      cats_rects_both <- cats_rects %>%
        filter(avail=="Both") %>%
        select(field,proportion) %>%
        rename(ymin = proportion)
      cats_rects_neither <- cats_rects %>%
        filter(avail=="Neither") %>%
        select(field,proportion) %>%
        rename(ymax = proportion) %>%
        mutate(ymax = 1-ymax)
      
      cats_rects <- cats_rects %>% 
        left_join(cats_rects_both, by = "field") %>%
        left_join(cats_rects_neither, by = "field") %>%
        filter(avail_collapsed=="Either Code Or Data") %>%
        group_by(field) %>%
        mutate(p_code_or_data = count/sum(count))
      
      cats_rects$xmin <- ifelse(cats_rects$avail=="Code",
                                as.numeric(cats_rects$field)-.45,
                                as.numeric(cats_rects$field)+.45-.9*cats_rects$p_code_or_data)
      cats_rects$xmax <- ifelse(cats_rects$avail=="Code",
                                cats_rects$xmin+.9*cats_rects$p_code_or_data,
                                as.numeric(cats_rects$field)+.45)
    }
    # Chart setup
    {
      palette_bars <- rev(c("grey90",palette_score_charts[1],
                            palette_score_charts[2],
                            palette_score_charts[5]))
      
      plot_grid(snake_bar_n_bin_chart(cats,"field","avail",rounded = TRUE,output="bar chart",
                                      palette_bars = palette_bars) + 
                  funkyheatmap::geom_rounded_rect(data=cats_rects,inherit.aes = FALSE,
                                                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,fill=avail),
                                                  radius=unit(3, "points"),show.legend=FALSE) + 
                  annotate("text",x=1,y=0.03,hjust=0,
                           label="Both Data\nand Code",color="white")+
                  annotate("text",x=1.25,y=mean(cats[cats$field=="Political science",]$avail=="Both") +
                             mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data") + .05,
                           label="Data only",color="black",hjust=0)+
                  annotate("text",x=.75,y=mean(cats[cats$field=="Political science",]$avail=="Both") +
                             mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data") + .05,
                           label="Code only",color="black",hjust=0)+
                  annotate("text",x=1,y=0.97,hjust=1,
                           label="Neither Data\nnor Code",color="black")+
                  geom_segment(aes(x=1-(cats_rects[cats_rects$field=="Political science" & cats_rects$avail=="Code",]$p_code_or_data)*.9,
                                   xend=1,
                                   y=mean(cats[cats$field=="Political science",]$avail=="Both") + 
                                     mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data"),
                                   yend=mean(cats[cats$field=="Political science",]$avail=="Both") + 
                                     mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data") + .05),
                               color="grey50",linetype=3) +
                  geom_segment(aes(x=1,
                                   xend=1,
                                   y=mean(cats[cats$field=="Political science",]$avail=="Both") + 
                                     mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data")+ .05,
                                   yend=mean(cats[cats$field=="Political science",]$avail=="Both") + 
                                     mean(cats[cats$field=="Political science",]$avail_collapsed=="Either Code Or Data") + .1),
                               color="grey50",linetype=3),
                snake_bar_n_bin_chart(cats,"field","avail",rounded = TRUE,output="snake bin chart",
                                      palette_snakebins = palette_bars),
                align = c("h"),rel_widths = c(4,1))
    }
  })

}

shinyApp(ui, server)