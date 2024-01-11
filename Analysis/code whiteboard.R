# Startup and initialization
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  

    bootstrap.clust(data=,
                    FUN=function(x) {
                      x <- x %>% group_by(paper_id) %>% mutate(weight = 1/n())
                      
                    },
                    clustervar = "paper_id",
                    keepvars=,
                    alpha=.05,tails="two-tailed",iters=iters,
                    format.percent=TRUE,digits=1
    )$formatted.text
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

