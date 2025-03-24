# Run generate_all() to generate all tagged stats and figures

# The resulting objects (results_tagged_stats and results_figures) contain
# listed outputs for all analysis tags contained within the publication,
# accessed with $ (e.g. results_figures$figure_1)

generate_all <- function(){
  
  results_tagged_stats <<- tagged_stats()
  
  results_figures <<- figures()
  
}

# Create an object that contains the tagged stats. Note that ALL objects created
# in this functional environment are exported for potential extraction, so it's
# a good idea to remove unneeded objects (such as intermediary datasets) with 
# rm() as you go.
tagged_stats <- function(){

  # Load libraries
  {
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(Hmisc)
  }
  
  # Load data and common functions
  {
    paper_folder <- "Paper 2" # only used if running from original data source
    
    # Check if loading locally
    if(file.exists("common functions.R") & file.exists("analyst data.RData")){
      load("analyst data.RData")
      source("common functions.R")
    } else {
      load(paste0("Analysis/",paper_folder,"/Code and data/Analyst package/analyst data.RData"))
      source(paste0("Analysis/",paper_folder,"/Code and data/Analyst package/common functions.R"))
    }
  }
  
  # Data preparation
  {
    
  }

  # Tagged stats and text
  {
    # Tables
    {
      # Table X
      {
        table_name <- "table_1"
        
        # Generate the table
        table <- iris
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
            assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
      }
    }
    
    # Text tags
    {
      # Abstract
      {
        n_papers_assessed_process_repro <- pr_outcomes %>% filter(!covid) %>% nrow()
      }
      # Results
      {
        
      }
    }
  }

  # Export everything in environment
  {
    return(rev(as.list(environment())))
  }
}

# Figures
figures <- function(){
  # Setup and initialization
  {
    # Libraries
    {
      library(ggplot2)
    }
    
    # Load data and common functions
    {
      paper_folder <- "Paper 2" # only used if running from original data source
      
      # Check if loading locally
      if(file.exists("common functions.R") & file.exists("analyst data.RData")){
        load("analyst data.RData")
        source("common functions.R")
      } else {
        load(paste0("Analysis/",paper_folder,"/Code and data/Analyst package/analyst data.RData"))
        source(paste0("Analysis/",paper_folder,"/Code and data/Analyst package/common functions.R"))
      }
    }
    
    plotlist <- list()
  }
  
  # Global aesthetic options
  {
    
  }
  
  # General data preparation
  {
    
  }
  
  # Figure 1. _____
  {
    plotname <- "figure_1"
    
    # Data wrangling
    {
      df.plot <- iris
    }
    
    # Aesthetic setup
    {
      point.size <- 3
    }
    
    # Plot generation (ggplot object)
    {
      plot <- ggplot(data=df.plot,mapping=aes(x=Petal.Length,y=Petal.Width,color=Species))+
        geom_point(aes(color=Species),size=point.size)
    }
    
    # Assign plot to list of plots to export
    {
      assign(plotname,plot)
      plotlist <- append(plotlist, list(get(plotname)), after = length(plotlist))
      names(plotlist)[[length(plotlist)]] <- plotname
    }
  }
  
  
  # Export
  {
    return(plotlist)
  }
}
