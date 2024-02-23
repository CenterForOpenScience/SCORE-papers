
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
    objects_to_load <- c("repro_outcomes","orig_outcomes","paper_metadata")
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
                                #p("Paper 5 here: https://docs.google.com/document/d/1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg/edit"),
                                #p("May take a moment to load due to bootstrap iterations..."),
                                #DTOutput("paper_5_stats_table")
                       ),
                       tabPanel("Data properties",
                                #htmlOutput("repli_data_text")
                       ),
                       tabPanel("Dataset",
                                #DTOutput("repli_data_table")
                       ),
                       
                  )
                )

  )
}

server <- function(input, output, session) {
  

}

shinyApp(ui, server)