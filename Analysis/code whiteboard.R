# Startup and initialization
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
   
}

# Data loading
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
    library(colorspace)
  }
  
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

# Find tags in text file
if (FALSE){
  library(stringr)
  library(googledrive)
  paper_5_text <- drive_read_string(file=googledrive::as_id("1dg5aajBhnc4v1i7h1d4oJ0ij4w8joS65CD2Tgv15bjg"),
                      type = "text/plain")  %>%
    strsplit(split = "(\r\n|\r|\n)") %>%
    .[[1]]
  paper_5_text <- paste0(paper_5_text,collapse="  ")
  
  tags <- unique(str_match_all(paper_5_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
  tags <- tags[tags!=""]
  
  
 
}

# zcurve
if (FALSE){
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
  
  repli_outcomes_merged <- merge(repli_outcomes,orig_outcomes[,!(names(orig_outcomes) %in% c("paper_id"))],
                                 by="claim_id",all.x=TRUE,all.y=FALSE)
  orig_outcomes <- orig_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
  repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n())
  repli_outcomes_merged <- repli_outcomes_merged %>% group_by(paper_id) %>% mutate(weight = 1/n())
  
  repli_outcomes_merged_pvals <- repli_outcomes_merged[!is.na(repli_outcomes_merged$orig_p_value) & !is.na(repli_outcomes_merged$repli_p_value),]
  repli_outcomes_merged_pvals_st <- repli_outcomes_merged_pvals[grepl("single-trace", repli_outcomes_merged_pvals$claim_id, fixed = TRUE),]
  
  zcurve.obj <- zcurve(p = repli_outcomes_merged_pvals$orig_p_value)
  # EDR: "Estimated proportion that the reported statistically significant
  # results constitute from all conducted statistical tests."
  EDR(zcurve.obj)
  
  # ODR: Observed proportion of statistically significant results
  ODR(zcurve.obj)
  
  # ERR:the predicted success rate of exact replication studies based on the
  # mean power after selection for significance.
  ERR(zcurve.obj)
  
  zcurve.obj <- zcurve(p = repli_outcomes_merged_pvals_st$orig_p_value)
  # EDR: "Estimated proportion that the reported statistically significant
  # results constitute from all conducted statistical tests."
  EDR(zcurve.obj)
  
  # ODR: Observed proportion of statistically significant results
  ODR(zcurve.obj)
  
  # ERR:the predicted success rate of exact replication studies based on the
  # mean power after selection for significance.
  ERR(zcurve.obj)
  
}

# Test categorical bar and snakebin chart
{
  data <- merge(repro_outcomes[c("paper_id","claim_id","repro_outcome_overall")],pr_outcomes[c("paper_id","covid")],
                by="paper_id",all.x=TRUE,all.y=FALSE)
  data <- merge(data,paper_metadata[c("paper_id","pub_year","COS_pub_category")],
                by="paper_id",all.x =TRUE,all.y=FALSE)
  
  data<-data[data$covid!=TRUE & data$repro_outcome_overall!="none",]
  data$covid <- NULL
  data <- data %>%
    group_by(paper_id) %>%
    mutate(weight=1/n())
  data$field <- str_to_title(data$COS_pub_category)
  group_order <- seq(min(data$pub_year),max(data$pub_year,1))
  data$pub_year <- ordered(data$pub_year,levels=group_order,labels=group_order)
  data$repro_outcome_overall <- str_to_title(data$repro_outcome_overall)
  
  cats.order <- c( "Push Button",
                   "Precise",
                   "Approximate",
                   "Not")
  
  data$cat <- ordered(data$repro_outcome_overall,
                      labels=c( "Push Button\nReproduced",
                                "Precisely\nReproduced",
                                "Approximately\nReproduced",
                                "Not\nReproduced"),
                      levels=c( "Push Button",
                                "Precise",
                                "Approximate",
                                "Not"))
  
  # Define nesting structure
  cat <- levels(data$cat)
  nesting.structure <- data.frame(cat)
  
  nesting.structure$cat2 <- nesting.structure$cat
  nesting.structure$cat3 <- nesting.structure$cat
  
  nesting.structure$cat <- ordered(nesting.structure$cat)
  nesting.structure$cat2 <- ordered(nesting.structure$cat2)
  nesting.structure$cat3 <- ordered(nesting.structure$cat3)
  
  chart.palette <- c(palette_score_charts[1],
                     palette_score_charts[3],
                     palette_score_charts[4],
                     palette_score_charts[2])
  
  
  field <- unique(data$field)[1]
  plotlist.ext <- lapply(unique(data$field),function(field) {
    print(field)
    plotlist <- lapply(1:length(group_order),function(x) {
      plot <- bars.and.snakebins(data[data$pub_year==group_order[x] & data$field==field,],nesting.structure,
                                 chart.palette = chart.palette,
                                 display_axis = FALSE,
                                 #n_bins = 3,
                                 n_bins_max = 100,
                                 bin_width_x = .01,
                                 bins_offset = .05)$plot
      
      plot
    }
    
    )
    plotlist[[length(plotlist)+1]] <- 
      bars.and.snakebins(data[data$pub_year==group_order[1],],nesting.structure,
                         chart.palette = chart.palette,
                         axis_only = TRUE,
                         #n_bins = 3,
                         n_bins_max = 100,
                         bin_width_x = .01,
                         bins_offset = .05)$plot+
      scale_x_continuous(labels=c("0%","100%"),breaks=c(0,1))
    
    title <- ggdraw() + 
      draw_label(
        field,
        #fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      theme(
        plot.margin = margin(0, 0, 0, 0)
      )
    plot_grid(title,plotlist=plotlist,ncol=1,align = "v",rel_heights = c(1,rep(1,length(plotlist)-1),1))
  })
  
  year.labels <- c(" ",group_order," ")
  plotlist.center <- lapply(1:length(year.labels),function(x) 
    ggplot()+theme_blank() +
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
               label=year.labels[x])
  )
  left.labels <- plot_grid(plotlist =plotlist.center,ncol=1)
  
  plot_grid(plot_grid(left.labels,plot_grid(plotlist=plotlist.ext,nrow=1),ncol=2,
            rel_widths = c(1/length(unique(data$field)),1)),
            bars.and.snakebins(data,nesting.structure,
                               chart.palette = chart.palette,
                               legend=TRUE,
                               display_axis = FALSE,
                               legend.color = c("black","black","black","white"),
                               n_bins_max = 100,
                               bin_width_x = .01,
                               bins_offset = .00)$plot+
              scale_x_continuous(limits=c(0,1/4)),
            rel_heights = c(12,1),ncol=1
  )
  
  
}
