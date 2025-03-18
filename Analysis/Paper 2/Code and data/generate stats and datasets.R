# Run this script to:
# 1) Generate the public code/data package
# 2) Run the code/data package to (if enabled) generate tagged text items and figures
# 3) (If enabled) export tagged text items and figures to a specified Google sheet and folder, respectively

# Initial setup and libraries
{
  library(dplyr)
  library(tidyr)
  library(googledrive)
  library(stringr)
  library(targets)
  library(googlesheets4)
}

# Options
{
  generate_tagged_stats <- TRUE # enables generating tagged statistics and outputting them into the google sheet
  generate_figures <- TRUE # enables generating figures and expoprting them to the project folder
  
  objects_to_load <- c("paper_metadata","repli_outcomes","orig_outcomes")
  
  drive_auth(Sys.getenv("google_oauth_email")) # set a google_oauth_email = <email address> in your .Rprofile so this doesn't need to be changed 
  paper_folder <- "Paper 2"
  template_doc_id <- "18U3ElDrhF5PltX_UP1XIN1e6CK8nkfOzjSDZr2zF6lo" # google doc ID for the template doc to pull tags from (obtained from URL)
  tagged_stats_ss <- "https://docs.google.com/spreadsheets/d/1qs8Ap3wfw-t5SqlDbCa1sQvvtVY4LS38xwxd3rro0po" # URL for the google sheet that tagged stats go to
}

# Load and save data and datasets and generate public package. 
# Note: Run at least this function when updating data, as everything else pulls
# from this package of data and code
{
  for(i in 1:length(objects_to_load)){
    assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
  }

  save(list=objects_to_load,file=paste0("Analysis/",paper_folder,"/Code and data/Analyst package/analyst data.RData"))
  file.copy(from=paste0("Analysis/common functions.R"),to=paste0("Analysis/",paper_folder,"/Code and data/Analyst package/common functions.R"),overwrite=TRUE)
  file.copy(from=paste0("Analysis/",paper_folder,"/Code and data/tagged stats and figures.R"),to=paste0("Analysis/",paper_folder,"/Code and data/Analyst package/tagged stats and figures.R"),overwrite=TRUE)
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
  results_tagged_stats <<- tagged_stats()
  
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
