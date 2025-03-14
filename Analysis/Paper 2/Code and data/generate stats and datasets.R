# Initial setup and libraries
{
  library(dplyr)
  library(DT)
  library(tidyr)
  library(pbapply)
  library(googledrive)
  library(stringr)
  library(Hmisc)
  library(targets)
  library(googlesheets4)
  library(DescTools)
}

# Options and setup
{
  generate_tagged_stats <- TRUE
  generate_figures <- TRUE
  
  drive_auth(Sys.getenv("google_oauth_email"))
  paper_folder <- "Paper 3"
  template_doc_id <- "18U3ElDrhF5PltX_UP1XIN1e6CK8nkfOzjSDZr2zF6lo"
  tagged_stats_ss <- "https://docs.google.com/spreadsheets/d/1qs8Ap3wfw-t5SqlDbCa1sQvvtVY4LS38xwxd3rro0po"
}


# Load and save data and datasets for public running
{
  objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes",
                       "paper_metadata","status","stitched_claims",
                       "all_rr_attempts","publications","extracted_claims")
  for(i in 1:length(objects_to_load)){
    assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
  }

  save(list=objects_to_load,file=paste0("Analysis/",paper_folder,"/Code and data/Analyst package/analyst data.RData"))
  file.copy(from=paste0("Analysis/common functions.R",to="Analysis/",paper_folder,"/Code and data/Analyst package/common functions.R",overwrite=TRUE))
  file.copy(from=paste0("Analysis/",paper_folder,"/Code and data/tagged stats and figures.R",to="Analysis/",paper_folder,"/Code and data/Analyst package/tagged stats and figures.R",overwrite=TRUE))
}

# Pull source code docs
{
  source(file="Analysis/",paper_folder,"/Code and data/tagged stats and figures.R")
}

# Generate tagged stats in text
if(generate_tagged_stats==TRUE){
  # Pull paper to find what tags are in paper
  {
    paper_text <- drive_read_string(file=googledrive::as_id(template_doc_id),
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
  
  range_delete(tagged_stats_ss,range="A:H")
  range_write(tagged_stats_ss,data = data.frame(tags=paste0("{",tags,"}"),values_text), range = "A1",col_names=FALSE)
}

# Generate figures
if(generate_figures==TRUE){
  generated_figures <- figures()
  
  ggsave(
    paste0("Analysis/",paper_folder,"/Code and data/Figures/figure 1.png"),
    plot = generated_figures$figure_1,
    width = 6000,height = 2500,units = "px",bg="white"
  )
  
}
