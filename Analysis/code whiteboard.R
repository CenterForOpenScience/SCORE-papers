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
}

tar_load("repli_export")
tar_load("repli_outcomes")


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
  generate_selected_repli_data <- function(select_rr_type_selection_ui = c(TRUE,FALSE,FALSE),
                                           select_generalizability_selection_ui = c(TRUE,FALSE),
                                           select_rr_is_manylabs_selection_ui = c(TRUE,FALSE,FALSE,FALSE),
                                           select_rr_analytic_sample_stage_selection_ui = c(TRUE,TRUE,FALSE,FALSE,FALSE)){
    
    # Pull data
    tar_load("repli_export")
    tar_load("repli_outcomes")
    #tar_load("repli_primary")
    
    # Temporary for data "cleaning"
    repli_export$generalizability <- ifelse(repli_export$paper_id %in% c("Br0x", "EQxa", "J4W9", "plLK", "rjb", "zlm2"),
                                            TRUE,FALSE)
    repli_export$claim_id_unique <- paste0(repli_export$paper_id,"_",repli_export$claim_id)
    repli_export$rr_type_internal <- NULL
    
    select_rr_type_selection <- select_rr_type_set[select_rr_type_selection_ui]
    select_generalizability_selection <- select_generalizability_set[select_generalizability_selection_ui]
    select_rr_is_manylabs_selection <- select_rr_is_manylabs_set[select_rr_is_manylabs_selection_ui]
    select_rr_analytic_sample_stage_selection <- select_rr_analytic_sample_stage_set[select_rr_analytic_sample_stage_selection_ui]
    
    # Create selected dataset
    repli_export_selected <- repli_export[repli_export$rr_type %in% select_rr_type_selection &
                                            repli_export$generalizability %in% select_generalizability_selection &
                                            repli_export$rr_is_manylabs %in% select_rr_is_manylabs_selection &
                                            repli_export$rr_analytic_sample_stage %in% select_rr_analytic_sample_stage_selection,]
    # Generate selection weights and set indexes (for combining)
    repli_export_selected <- repli_export_selected %>% 
      group_by(paper_id) %>% 
      mutate(weight_by_paper_id = 1/n())
    repli_export_selected <- repli_export_selected %>% 
      group_by(claim_id_unique) %>% 
      mutate(weight_by_claim_id = 1/n())
  }
}

# Compare power Tillburg vs success rate
{
  df.chart <- generate_selected_repli_data()
  
}

# Basic replication stats
{
  df.chart <- generate_selected_repli_data()
  
  ggplot(data=repli_export,aes(y=)) +
    geom_histogram()+
    coord_flip()+
    theme_minimal()+
    scale_y_continuous(limits=c(-50,50))
  
  target
}






# Play area 
if(FALSE){
  # Testing for full hierarchy
  {
    df.hierarchy <- repli_export %>% group_by(paper_id,claim_id,rr_analytic_sample_stage,rr_is_manylabs, .drop = FALSE) %>% count()
    dup_paper_ids <- df.hierarchy$paper_id[df.hierarchy$n==2]
    repli_export_dups <- repli_export[repli_export$paper_id %in% dup_paper_ids,]
    
    test <- repli_export %>% group_by(paper_id,claim_id,rr_is_manylabs, .drop = FALSE) %>% count()
    
  }
  # Generate Sankey style chart
  {
    # Generate hierarchical structure
    repli_hierarchy_order <- c("paper_id","claim_id","rr_is_manylabs","rr_analytic_sample_stage","rr_id")
    repli_hierarchy_order_label <- c("Unique paper","Unique claim","ManyLabs participation","Sample stage\n(for case of multiple stepped RRs)","Other")
    df.hier <- repli_export[,c()]
    df.hier[[paste0("hier_level_",1)]] <- repli_export[[repli_hierarchy_order[1]]]
    for (i in 2:length(repli_hierarchy_order)){
      df.hier[[paste0("hier_level_",i)]] <- paste(df.hier[[paste0("hier_level_",i-1)]] ,repli_export[[repli_hierarchy_order[i]]],sep="_")
    }
    # Generate alluvial structure
    df.alluvial <- df.hier %>% 
      group_by(across(all_of(colnames(df.hier)))) %>% 
      #group_by(paper_id,claim_id_unique_1,claim_id_unique_2, .drop = FALSE) %>% 
      count()
    
    df.alluvial <- df.alluvial[order(df.alluvial$hier_level_1),]
    
    #
    df.alluvial <- df.alluvial[df.alluvial$hier_level_1 %in% unique(df.alluvial$hier_level_1)[1:10],]
    
    ggplot(df.alluvial,
           aes(y = n, axis1 = hier_level_1, axis2 = hier_level_2,axis3 = hier_level_3,axis4 = hier_level_4,axis5 = hier_level_5,
               fill=hier_level_1)) +
      geom_alluvium() + 
      geom_stratum(size=0.1)+
      theme_minimal()+
      theme(
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      )+
      scale_x_continuous(breaks=1:length(repli_hierarchy_order),labels=repli_hierarchy_order_label)
    #geom_alluvium(aes(fill = Admit), width = 1/12)
  }
  
  
  
  # Generate hierarchy 2
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
    # pl <- pl + scale_fill_viridis_d(option = "inferno")
    # pl <- pl + labs(title = "Sankey diagram using ggplot")
    # pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
    pl
  }
}


