
# Initial setup and libraries
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  
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
  library(targets)
  library(googlesheets4)
  library(zcurve)
  library(scales)
  library(wCorr)
  library(corrplot)
  library(cowplot)
  library(ggridges) #note: using the github version, as the CRAN hasn't been pushed to get the weights functionality
  library(ggside)
  library(weights)
  library(glue)
  
  drive_auth(Sys.getenv("google_oauth_email"))
  #drive_deauth()
}

# Load and save data and datasets for public running
{
  objects_to_load <- c("repli_outcomes","orig_outcomes","paper_metadata",
                       "status","all_rr_attempts","repli_binary","publications",
                       "non_significant_bushels","rr_sourced","repli_export",
                       "repli_case_exclusions","orig_dataset",
                       "full_dates","never_sourced","ser_power",
                       "traditional_power")
  for(i in 1:length(objects_to_load)){
    assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
  }
  
  save(list=objects_to_load,file="Analysis/Paper 5/Code and data/Analyst package/analyst data.RData")
  file.copy(from="Analysis/common functions.R",to="Analysis/Paper 5/Code and data/Analyst package/common functions.R",overwrite=TRUE)
  file.copy(from="Analysis/Paper 5/Code and data/tagged stats and figures.R",to="Analysis/Paper 5/Code and data/Analyst package/tagged stats and figures.R",overwrite=TRUE)
  file.copy(from="pipeline/data_processing/rr/repli_binary.R",to="Analysis/Paper 5/Code and data/Analyst package/repli_binary.R",overwrite=TRUE)
}

# Pull source code docs
{
  # Common functions
  #source(file="Analysis/common functions.R")
  source(file="Analysis/Paper 5/Code and data/tagged stats and figures.R")
}

# Generate tagged stats in text
{
  # Pull paper to find what tags are in paper
  {
    paper_text <- drive_read_string(file=googledrive::as_id("1mauNAwu0eZfvh5-5p44wnKz8NQL-5Tm_bAyZNseTONo"),
                                    type = "text/plain",encoding="UTF-8")  %>%
      strsplit(split = "(\r\n|\r|\n)") %>%
      .[[1]]
    paper_text <- paste0(paper_text,collapse="  ")
    
    # Pull paper to find what tags are calculated
    tags <- unique(str_match_all(paper_text, "\\{\\s*(.*?)\\s*\\}")[[1]][,2])
    tags <- tags[tags!=""]
    tags <- gsub("\\[\\s*(.*?)\\s*\\]","",tags)
  }
  
  # Generate stats
  results_tagged_stats <<- tagged_stats(iters = 1000)
  
  # Generate list of tags
  values_text <- do.call(c,lapply(1:length(tags),function(x) {
    tag_to_find <- tags[x]
    if(tag_to_find %in% names(results_tagged_stats)){
      as.character(results_tagged_stats[[tag_to_find]])
    } else {
      "MISSING"
    }
  }))
  
  # Export
  ss <- "https://docs.google.com/spreadsheets/d/1uaR6vYAMVywk1liQeKRyRCDQcs--teFpRsWzHrvfg3s"
  range_delete(ss,range="A:H")
  range_write(ss,data = data.frame(tags=paste0("{",tags,"}"),values_text), range = "A1",col_names=FALSE)
}

# Generate figures
{
  generated_figures <- figures(iters = 1000)
  
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure 1.png",
    plot = generated_figures$figure_1,
    width = 3000,height = 1000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure 2.png",
    plot = generated_figures$figure_2,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure 3.png",
    plot = generated_figures$figure_3,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure 4.png",
    plot = generated_figures$figure_4,
    width = 2000,height = 1200,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s1.png",
    plot = generated_figures$figure_s1,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s2.png",
    plot = generated_figures$figure_s2,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s3.png",
    plot = generated_figures$figure_s3,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s4.png",
    plot = generated_figures$figure_s4,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s5.png",
    plot = generated_figures$figure_s5,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s6.png",
    plot = generated_figures$figure_s6,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s7.png",
    plot = generated_figures$figure_s7,
    width = 4000,height = 4000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s8.png",
    plot = generated_figures$figure_s8,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s9.png",
    plot = generated_figures$figure_s9,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s10.png",
    plot = generated_figures$figure_s10,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s11.png",
    plot = generated_figures$figure_s11,
    width = 2000,height = 2000,units = "px",bg="white"
  )
  ggsave(
    "Analysis/Paper 5/Code and data/Figures/figure s12.png",
    plot = generated_figures$figure_s12,
    width = 3000,height = 1000,units = "px",bg="white"
  )
}
