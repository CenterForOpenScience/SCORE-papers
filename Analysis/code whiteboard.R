# Startup and initialization
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  
  library(targets)
  library(ggplot2)
  library(ggalluvial)
  library(tidyr)
  library(dplyr)
  library(devtools)
  library(ggsankey)
  library(plotly)
  library(viridis)
}

# tar_make() # run this to pull new targets

<<<<<<< Updated upstream
#tar_load("repli_export")
#tar_load("repli_primary")
tar_load("repli_outcomes")
#tar_load("orig_statistics_dataset_p1")
=======
# #tar_load("repli_export")
# #tar_load("repli_primary")
# tar_load("repli_outcomes")
# #tar_load("orig_statistics_dataset_p1")
# tar_load("orig_dataset")
>>>>>>> Stashed changes

# Update app data
if (FALSE){
  tar_load("repli_outcomes")
  save(repli_outcomes,file="Analysis/Data exploration app/repli_outcomes.RData")
<<<<<<< Updated upstream
=======
  tar_load("orig_dataset")
  save(repli_outcomes,file="Analysis/Data exploration app/orig_dataset.RData")
  
  df_orig_nuthing <- orig_dataset[0,]
  write_sheet(df_orig_nuthing,
    ss="https://docs.google.com/spreadsheets/d/1mdRqLoxHtcYtJg2ozGlnwwVwla9Kb0xcOuViTCLcxgM/edit#gid=2126773854",
              sheet="keep kill modify orig")
>>>>>>> Stashed changes
}


# UI and selection options data
{
  select_rr_type_set <- c("Direct Replication","Data Analytic Replication","Hybrid")
  select_rr_type_labels <- c("Direct Replication","Data Analytic Replication","Hybrid")
  
  select_generalizability_set  <- c(FALSE,TRUE)
  select_generalizability_labels  <- c("Standard","Generalizability study")
  
  select_rr_is_manylabs_set <- c("non_ml","ml_count","ml_instance_primary","ml_aggregation")
  select_rr_is_manylabs_labels <- c("Not ManyLabs","Count","Primary","Aggregation")
  
  select_rr_analytic_sample_stage_set <- c("stage 1","stage 2","threshold","no target","lab target")
  select_rr_analytic_sample_stage_labels <- c("Stage 1","Stage 2","Threshold","No target","Lab target")
}

# Generate selection data functions
{
  df_repli_subsetted <- function(select_rr_type_selection_ui = c(TRUE,FALSE,FALSE),
                                           select_generalizability_selection_ui = c(TRUE,FALSE),
                                           select_rr_is_manylabs_selection_ui = c(TRUE,FALSE,FALSE,FALSE),
                                           select_rr_analytic_sample_stage_selection_ui = c(TRUE,TRUE,FALSE,FALSE,FALSE)){
    
    # Pull data
    #tar_load("repli_export")
    tar_load("repli_outcomes")
    #tar_load("repli_primary")
    
    # Temporary for data "cleaning"
    repli_outcomes$generalizability <- ifelse(repli_outcomes$paper_id %in% c("Br0x", "EQxa", "J4W9", "plLK", "rjb", "zlm2"),
                                            TRUE,FALSE)
    repli_outcomes$claim_id_unique <- paste0(repli_outcomes$paper_id,"_",repli_outcomes$claim_id)
    repli_outcomes$rr_type_internal <- NULL
    
    select_rr_type_selection <- select_rr_type_set[select_rr_type_selection_ui]
    select_generalizability_selection <- select_generalizability_set[select_generalizability_selection_ui]
    select_rr_is_manylabs_selection <- select_rr_is_manylabs_set[select_rr_is_manylabs_selection_ui]
    select_rr_analytic_sample_stage_selection <- select_rr_analytic_sample_stage_set[select_rr_analytic_sample_stage_selection_ui]
    
    # Create selected dataset
    repli_outcomes_selected <- repli_outcomes[repli_outcomes$rr_type %in% select_rr_type_selection &
                                            repli_outcomes$generalizability %in% select_generalizability_selection &
                                            repli_outcomes$rr_is_manylabs %in% select_rr_is_manylabs_selection &
                                            repli_outcomes$rr_analytic_sample_stage %in% select_rr_analytic_sample_stage_selection,]
    # Generate selection weights and set indexes (for combining)
    repli_outcomes_selected <- repli_outcomes_selected %>% 
      group_by(paper_id) %>% 
      mutate(weight_by_paper_id = 1/n())
    repli_outcomes_selected <- repli_outcomes_selected %>% 
      group_by(claim_id_unique) %>% 
      mutate(weight_by_claim_id = 1/n())
  }
}

# Aesthetic functions
{
  GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                             draw_group = function(self, data, ..., draw_quantiles = NULL) {
                               data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                               grp <- data[1, "group"]
                               newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                               newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                               newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                               
                               if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                 stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                           1))
                                 quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                                 aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                 aesthetics$alpha <- rep(1, nrow(quantiles))
                                 both <- cbind(quantiles, aesthetics)
                                 quantile_grob <- GeomPath$draw_panel(both, ...)
                                 ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                               }
                               else {
                                 ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                               }
                             })
  
  geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                                draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                                show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
          position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
          params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
  }
}

# Main 3 parter
{
  tar_load("repli_outcomes")
  df.chart <- repli_outcomes
  #df.chart <- df_repli_subsetted()
  
  # Merge in orig data
  tar_load("orig_dataset")
  df.chart.orig <- orig_dataset[c("unique_claim_id",
                                  "original_effect_size_value_reported",
                                  "original_effect_size_type_reference",
                                  "original_pearsons_r_numeric")]
  df.chart <- merge(df.chart,df.chart.orig,by.x="claim_id",by.y="unique_claim_id",all.x=TRUE,all.y=FALSE)

  df.chart$orig_pearsons_r <- as.numeric(df.chart$original_pearsons_r_numeric)
  df.chart$orig_effect_size_value <- as.numeric(df.chart$original_effect_size_value_reported)
  
  # Gather up new vs originals
  # Pearsons
  df.chart.pearsons <- df.chart[c("repli_pearsons_r_value","orig_pearsons_r")]
  df.chart.pearsons$stat_type <- "Pearson's R"
  colnames(df.chart.pearsons) <- c("Replication","Original","stat_type")
  
  # All others
  df.chart.others <- df.chart[c("repli_effect_size_value","orig_effect_size_value","repli_effect_size_type")]
  colnames(df.chart.others) <- c("Replication","Original","stat_type")
  types_to_keep <- c("ser_method")
  df.chart.others <- df.chart.others[df.chart.others$stat_type %in% types_to_keep,]
  
  # Combine
  df.chart <- rbind(df.chart.pearsons,df.chart.others)
  df.chart <- df.chart %>% pivot_longer(!"stat_type", names_to = "comparison", values_to = "ES_value")
  df.chart <- na.omit(df.chart)
  df.chart$stat_type <- factor(df.chart$stat_type,
                               labels=c("Pearson's R","SER"),
                               levels=c("Pearson's R","ser_method"))
  df.chart$comparison <- factor(df.chart$comparison,
                                labels=c("Replication","Original"),
                                levels=c("Replication","Original"))
  df.chart$jitter_bias <- ifelse(df.chart$comparison=="original",-.2,.2)
  
  ggplot(data=df.chart,aes(y=ES_value,x=stat_type,fill=reorder(comparison, desc(comparison)))) +
    geom_split_violin()+
    geom_point(position=position_jitterdodge(),size=.2)+

    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.line = element_line(color="#393939"),
      legend.title=element_blank()
    )+
    scale_y_continuous(expand=c(0,0),limits=c(-0,1))+
    xlab("Statistic type")+
    ylab("Effect size value")
  
}






# Play area 
if(FALSE){
  # Main 3
  {
    df.hierarchy <- repli_export %>% group_by(paper_id,claim_id,rr_analytic_sample_stage,rr_is_manylabs, .drop = FALSE) %>% count()
    dup_paper_ids <- df.hierarchy$paper_id[df.hierarchy$n==2]
    repli_export_dups <- repli_export[repli_export$paper_id %in% dup_paper_ids,]
    
    test <- repli_export %>% group_by(paper_id,claim_id,rr_is_manylabs, .drop = FALSE) %>% count()
    
  }
  
  
  
  # Generate alluvial
  {
    df.repli.no.hier <- repli_export[c("paper_id","claim_id","rr_is_manylabs","rr_analytic_sample_stage","rr_id")]
    df.repli.no.hier$claim_id <- paste0(df.repli.no.hier$paper_id,"_", df.repli.no.hier$claim_id)
    
    df.repli.no.hier <- df.repli.no.hier[df.repli.no.hier$paper_id %in% unique(df.repli.no.hier$paper_id)[60:120],]
    
    df <- df.repli.no.hier %>%
      #make_long(paper_id, claim_id,rr_is_manylabs,rr_analytic_sample_stage)
      make_long(paper_id, claim_id,rr_is_manylabs,rr_analytic_sample_stage)
    
    
    pl <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) + 
      geom_alluvial(flow.alpha = 0.75, show.legend = FALSE,space=1) +
      #geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5)+
      theme_bw() + 
      theme(legend.position = "none") +
      theme(axis.title = element_blank()
            , axis.text.y = element_blank()
            , axis.ticks = element_blank()  
            , panel.grid = element_blank()) +
      labs(fill = 'Nodes')
     pl <- pl + scale_fill_viridis_d(option = "inferno")
    # pl <- pl + labs(title = "Sankey diagram using ggplot")
    # pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
    pl
  }
}


