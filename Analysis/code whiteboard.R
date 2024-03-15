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

# dotplot
{
  
  library(tidyverse)
  library(glue)
  library(ggtext)
  library(showtext)
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

  # # metadata
  # pubs <- read_csv("/Users/andrewtyner/Documents/GitHub/SCORE-P2/metadata/CR_metadata.csv")
  # fields <- read_csv("/Users/andrewtyner/Documents/GitHub/SCORE-P2/team_resources/data/publications.csv") %>% 
  #   mutate(field = COS_pub_category) %>% 
  #   mutate(
  #     field = ifelse(COS_pub_category %in% c("marketing/org behavior", "management"), "business", field),
  #     field = ifelse(COS_pub_category == "criminology", "sociology", field),
  #     field = ifelse(COS_pub_category == "public administration", "political science", field),
  #     field = ifelse(COS_pub_category == "health", "psychology", field)
  #   )
  
  objects_to_load <- c("repro_outcomes","pr_outcomes","orig_outcomes","paper_metadata")
      for(i in 1:length(objects_to_load)){
        assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
        #save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
      }
  
  # pr_raw <- read_csv("PR_data_final_with_metadata.csv")
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
    #left_join(fields %>% select(publication_standard, field), by = "publication_standard") %>% 
    #select(-publication_standard) %>% 
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
  
  
  font_add_google("Open sans", "open sans")
  showtext_auto()
  
  cats <- pr %>% 
    group_by(field, avail != "neither") %>% 
    mutate(rn = row_number()) %>% 
    mutate(cat = percent_rank(rn)) %>% 
    ungroup() %>% select(-`avail != "neither"`) %>% 
    replace_na(list(cat = 1)) %>% 
    mutate(field = as_factor(field) %>% fct_reorder(., pr_ord) %>% fct_rev()) %>% 
    mutate(cat = ifelse(field == "Sociology" & cat == 0, 0.79, cat)) %>% 
    mutate(cat = ifelse(field == "Economics" & avail == "neither" & cat == 0, 0.79, cat))
  
  n_bins <- 4
  group_ext <- cats$field
  group_int <- ordered(cats$avail)
  position_nudge_width <- .25/2
  df <- data.frame(group_ext,group_int) %>%
    arrange(group_ext,group_int)
  
  snake_bins <- function(n,n_bins){
    do.call(rbind,lapply(1:ceiling(n/n_bins), function (i){
      if(IsOdd(i)){bin <- 1:n_bins
      } else {bin <- n_bins:1
      }
      y <- rep(i,n_bins)
      data.frame(bin,y)
    }))[1:n,]
  }
  
  df <- cbind(df,
              do.call(rbind,lapply(1:length(unique(df$group_ext)),function(x) {
                snake_bins(n=nrow(df[df$group_ext==unique(df$group_ext)[x],]),n_bins=n_bins)
  })))
  
  df$position_nudge <- position_nudge_width*(df$bin-(n_bins+1)/2)-position_nudge_width/2
  
  ggplot() +
    geom_dotplot(data=df,aes(x=group_ext,(y=y-0.5)*n_bins,fill=group_int),
                 binaxis = "y", binwidth = n_bins,stackratio=0,
                 method = "dotdensity", position_nudge(x = df$position_nudge))+
    scale_fill_manual(
      limits = c("neither", "code", "data", "both"),
      values = c("tomato3", "seagreen3", "khaki3", "deepskyblue4"),
      labels = c("Neither", "Code", "Data", "Code + data")
    ) +
    theme_light() +
    #scale_y_discrete(sec.axis = sec_axis(~.*4))+
    theme(legend.position = "bottom",
          aspect.ratio=.75,)
  
  
  +
    coord_flip() +
    labs(
      x = "",
      y = "",
      title = "",
      fill = "",
    ) +
    theme_light() +
    theme(legend.position = "bottom")
  
  
  
    theme(
      axis.text.x = element_blank(),
      #axis.text.y = element_text(family = "open sans", margin = margin(r = 300), hjust = 0.5),
      #axis.text.y = element_blank(),
      legend.position = "bottom",
      #legend.text = element_text(size = 100),
      text = element_text(family="open sans")
    )
  
  
  
  pr %>% 
    ggplot() +
    # geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat >= 0.8), 
    #              aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
    #              stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = -0.26)) +
    # geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat >= 0.6 & cat < 0.8), 
    #              aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
    #              stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = -0.13)) + 
    # geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat >= 0.4 & cat < 0.6), 
    #              aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
    #              stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = 0)) +
    # geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat >= 0.2 & cat < 0.4), 
    #              aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
    #              stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = 0.13)) +
    # geom_dotplot(data = cats %>% filter(avail == "neither") %>% filter(cat < 0.2), 
    #              aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
    #              stackdir = "down", stackgroups = T, binpositions = "all", position_nudge(x = 0.26)) +
    geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat >= 0.8), 
                 aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
                 stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = -0.26)) +
    geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat >= 0.6 & cat < 0.8), 
                 aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
                 stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = -0.13)) +
    geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat >= 0.4 & cat < 0.6), 
                 aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
                 stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = 0)) +
    geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat >= 0.2 & cat < 0.4), 
                 aes(field, fill = avail), binwidth = 1/3, dotsize = 0.4,
                 stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = 0.13)) +
    geom_dotplot(data = cats %>% filter(avail != "neither") %>% filter(cat < 0.2), 
                 aes(reorder(field, -pr_ord), fill = avail), binwidth = 1/3, dotsize = 0.4,
                 stackdir = "up", stackgroups = T, binpositions = "all", position_nudge(x = 0.26)) +
    geom_text(data = pr, aes(x = field, y = 1, label = glue("{(pr_ord %>% round(2))*100}%")),
              inherit.aes = F, family = "open sans") +
    scale_fill_manual(
      limits = c("neither", "code", "data", "both"),
      values = c("tomato3", "seagreen3", "khaki3", "deepskyblue4"),
      labels = c("Neither", "Code", "Data", "Code + data")
    ) +
    coord_flip() +
    labs(
      x = "",
      y = "",
      title = "",
      fill = "",
    ) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      #axis.text.y = element_text(family = "open sans", margin = margin(r = 300), hjust = 0.5),
      #axis.text.y = element_blank(),
      legend.position = "bottom",
      #legend.text = element_text(size = 100),
      text = element_text(family="open sans")
    )
    #guides(fill = guide_legend(override.aes = list(size = 100)))
}
