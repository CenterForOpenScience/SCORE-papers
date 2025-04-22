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
    library(tidyverse)
    library(parameters)
    library(kableExtra)
    library(GGally)
    library(glue)
    library(patchwork)
    library(psych)
    library(wCorr)
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
    
    #repli_outcomes <- repli_outcomes %>% group_by(paper_id) %>% mutate(weight = 1/n()) %>% ungroup()
    
    # Set defaults for convenience
    {
      if (!exists("iters")){ iters <- 100}
      if (!exists("generate_binary_outcomes_data")){ generate_binary_outcomes_data <- FALSE}
    }
    
    # correlation CI
    r_ci <- function(x, y) {
      
      out <- psych::corr.test(x, y) %>% pluck("ci") %>% 
        mutate(across(everything(), function(x) formatC(x, format = "f", digits = 2) %>% str_replace_all(., "^0\\.", ".") %>% 
                        str_replace_all(., "^-0\\.", "-.")))
      
      out_r <- out[["r"]]
      out_l <- out[["lower"]]
      out_u <- out[["upper"]]
      
      glue("{out_r} ({out_l}, {out_u})")
      
    }
    
    # fix country variables in stevens
    oa_metadata <- oa_metadata %>% 
      mutate(
        num_countries_western = ifelse(str_detect(countries, "England"), num_countries_western + 1, num_countries_western),
        num_countries_western = ifelse(str_detect(countries, "Scotland"), num_countries_western + 1, num_countries_western)
      )
    
    # rework field vars
    paper_fields <- CR_metadata_prepool %>% 
      left_join(publications %>% select(publication_standard, COS_pub_expanded), by = "publication_standard") %>% 
      mutate(
        field = case_when(
          str_detect(str_to_lower(publication_standard), "financ") ~ "finance",
          str_detect(str_to_lower(publication_standard), "org") ~ "org. behavior",
          COS_pub_expanded == "public administration" ~ "Pub. Admin.",
          TRUE ~ COS_pub_expanded
        )
      ) %>% 
      mutate(field = ifelse(field == "marketing/org behavior", "marketing", field)) %>% 
      mutate(field = str_to_sentence(field)) %>% 
      select(paper_id, field)
    
    ### Principal components analysis
    
    extract_and_scale_pc1 <- function(data, scale_data = TRUE) {
      
      pca <- prcomp(data %>% select_if(is.numeric), scale. = scale_data)
      
      direction <- data %>% select_if(is.numeric) %>% rowMeans(na.rm = T)
      
      d <- data %>%
        mutate(PC1 = pca$x[, 1]) %>%
        mutate(pc1 = (PC1 - min(PC1)) / (max(PC1) - min(PC1))) %>% 
        select(any_of(c("paper_id", "claim_id", "evidence_id")), pc1)
      
      if(cor(d$pc1, direction, use = "complete.obs") < 0) {
        d$pc1 <- 1 - d$pc1
      } else {
        d$pc1 <- d$pc1
      }
      
      d
      
    }
    
    ## Melbourne
    
    melb_p1_pca <- melb_p1 %>% 
      select(paper_id, contains("cs_m")) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    melb_p2_pca <- melb_p2 %>% 
      select(paper_id, contains("cs_m")) %>% 
      extract_and_scale_pc1() %>% 
      rename(ta2_pid = paper_id) %>% 
      left_join(p2_id_key %>% select(paper_id, ta2_pid), by = "ta2_pid") %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)
    
    melb_bushel_pca <- melb_bushel %>% 
      filter(!is.na(evidence_id)) %>% 
      filter(signal == "deviation_replicability") %>% 
      select(paper_id, evidence_id, contains("cs_m")) %>% 
      extract_and_scale_pc1() %>% 
      left_join(stitched_claims %>% select(claim4_id, evidence_id = ta2_claim4_id), by = "evidence_id") %>% 
      mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>% 
      select(paper_id, claim_id, pc1)
    
    melbourne <- melb_p1_pca %>% 
      bind_rows(melb_p2_pca) %>% 
      bind_rows(melb_bushel_pca)
      
    ## Key W
    
    keyw <- kw_scores %>% 
      select(paper_id, contains("cs_m")) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)
    
    ## TwoSix
    ts_s1_pca <- ts_s1 %>% 
      rename(paper_id = ta3_pid) %>% 
      select(paper_id, contains("cs_m")) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    ts_s2_pca <- ts_s2 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    ts_s3_pca <- ts_s3 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    ts_bushel_pca <- ts_bushel %>% 
      left_join(stitched_claims %>% select(paper_id, claim4_id, ta3_claim4_id), by = "ta3_claim4_id") %>% 
      mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      extract_and_scale_pc1()
    
    twosix <- ts_s1_pca %>% 
      bind_rows(ts_s2_pca) %>% 
      bind_rows(ts_s3_pca) %>% 
      bind_rows(ts_bushel_pca)
    
    ## USC
    usc_s1_pca <- usc_s1 %>% 
      rename(paper_id = ta3_pid) %>% 
      select(paper_id, contains("cs_m")) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    usc_s2_pca <- usc_s2 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)
    
    usc_s3_pca <- usc_s3 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    usc_bushel_pca <- usc_bushel %>% 
      select(-paper_id) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      left_join(stitched_claims %>% select(paper_id, claim4_id, ta3_claim4_id), by = "ta3_claim4_id") %>% 
      mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>% 
      extract_and_scale_pc1()
    
    usc <- usc_s1_pca %>% 
      bind_rows(usc_s2_pca) %>% 
      bind_rows(usc_s3_pca) %>% 
      bind_rows(usc_bushel_pca)
    
    ## PSU (m8 is A&M)

    psu_s1_pca <- psu_s1 %>% 
      select(-cs_m8) %>% 
      rename(paper_id = ta3_pid) %>% 
      select(paper_id, contains("cs_m")) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    psu_s2_pca <- psu_s2 %>% 
      select(-cs_m8, -cs_m9, -cs_m10) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    psu_s3_pca <- psu_s3 %>% 
      select(-cs_m6, -cs_m7) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)
    
    psu <- psu_s1_pca %>% 
      bind_rows(psu_s2_pca) %>% 
      bind_rows(psu_s3_pca)
    
    ## A&M
    
    # only one measure in s1
    am_s1_pca <- psu_s1 %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      select(paper_id = ta3_pid, pc1 = cs_m8) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    am_s2_pca <- psu_s2 %>% 
      select(ta3_pid, cs_m8, cs_m9, cs_m10) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)

    am_s3_pca <- psu_s3 %>% 
      select(ta3_pid, cs_m6, cs_m7) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, median(x), x))) %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      extract_and_scale_pc1() %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(paper_id, claim_id, pc1)
    
    texas <- am_s1_pca %>% 
      bind_rows(am_s2_pca) %>% 
      bind_rows(am_s3_pca)
    
    ###
    
    ## Replicability
    replicability <- repli_outcomes %>% 
      filter(!is_covid & repli_version_of_record) %>% 
      select(paper_id, claim_id, score = repli_score_criteria_met)
    
    ## Outcome reproducibility
    outcome <- repro_outcomes %>% 
      semi_join(status %>% filter(p1_delivery|p2_delivery), by = "paper_id") %>% 
      mutate(
        reproduced = case_when(
          repro_outcome_overall %in% c("precise", "push button") ~ 1,
          repro_outcome_overall == "approximate" ~ 0.5,
          repro_outcome_overall %in% c("not", "not attemptable") ~ 0,
          TRUE ~ NA
        )
      ) %>% 
      filter(repro_version_of_record == "T") %>% 
      select(paper_id, claim_id, reproduced)
    
    ## Robustness
    robust <- m100 %>% select(-1) %>% 
      mutate(paper_id = select(., Paper_ID) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% last())) %>% 
      group_by(paper_id) %>% 
      dplyr::summarize(supported = mean(str_detect(Task1_Categorisation, "evidence for the relationship/effect"))) %>% 
      select(paper_id, supported)
    
    ## Process reproducibility (just data)
    process <- pr_outcomes %>% 
      filter(!covid) %>% 
      mutate(pr = OA_data_shared != "no") %>% 
      select(paper_id, pr) %>% 
      mutate(pr = pr*1)
    
    ## StatCheck
    # For now, treat as a paper-level variable averaging across findings
    statcheck <- statcheck <- statcheck %>% 
      as_tibble() %>% 
      filter(!is.na(error)) %>% 
      group_by(paper_id) %>% 
      dplyr::summarize(sc = 1 - mean(error))
    
    ## SciScore
    # remove covid
    sciscore <- sciscore %>% 
      rename(pdf_filename = ID) %>% 
      mutate(paper_id = select(., pdf_filename) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% last())) %>% 
      select(-pdf_filename) %>% 
      relocate(paper_id, .before = Journal) %>% 
      rename(raw = RawSciScore, adjusted = `Adjusted SciScore`) %>% 
      filter(Journal != "covid_pdf")
    
    sciscore$ss <- scales::rescale(sciscore$adjusted, to = c(0, 1))
    
  }

  # Tagged stats and text
  {
    # Tables
    {
      # Table 2
      {
        table_name <- "table_2"
        
        # Generate the table
        tab2_data <- melbourne %>% select(claim_id, melb = pc1) %>% 
          left_join(keyw %>% select(claim_id, kw = pc1), by = "claim_id") %>% 
          left_join(twosix %>% select(claim_id, ts = pc1), by = "claim_id") %>% 
          left_join(usc %>% select(claim_id, u = pc1), by = "claim_id") %>% 
          left_join(psu %>% select(claim_id, p = pc1), by = "claim_id") %>% 
          left_join(texas %>% select(claim_id, a = pc1), by = "claim_id") %>% 
          left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
          pivot_longer(cols = -claim_id, names_to = "measure", values_to = "value") %>% 
          drop_na() %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
          group_by(paper_id, measure) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup()
        
        table <- tab2_data %>%  
          group_by(measure) %>% 
          dplyr::summarize(
            N = round(sum(weight), 1),
            Median = round(weighted.median(value, weight), 2),
            Mean = round(weighted.mean(value, weight), 2),
            SD = round(sqrt(wtd.var(value, weight)), 2)
          ) %>% 
          left_join(
            tab2_data %>% 
              semi_join(repli_outcomes %>% filter(!is_covid & repli_version_of_record), by = "claim_id") %>% 
              group_by(measure) %>% 
              dplyr::summarize(
                N = round(sum(weight), 1),
                Median = round(weighted.median(value, weight), 2),
                Mean = round(weighted.mean(value, weight), 2),
                SD = round(sqrt(wtd.var(value, weight)), 2)
              ),
            by = "measure"
          ) %>% 
          mutate(across(c(where(is.numeric), -N.x, -N.y), function(x) formatC(x, 2, format = "f", digits = 2) %>% as.character() %>% str_replace_all(., "^0\\.", "."))) %>% 
          mutate(measure = as_factor(measure) %>% fct_relevel(., "melb", "kw", "ts", "u", "p", "a", "score")) %>% 
          arrange(measure) %>% 
          mutate(
            measure = case_match(
              measure,
              "melb" ~ "Structured elicitations",
              "kw" ~ "Prediction markets",
              "ts" ~ "A+",
              "u" ~ "MACROSCORE",
              "p" ~ "Synthetic markets A",
              "a" ~ "Synthetic markets B",
              "score" ~ "Replicability"
            )
          ) %>% 
          mutate(across(everything(), as.character)) %>% 
          mutate(across(contains(c("Median", "Sd")), function(x) ifelse(measure == "Replicability", "--", x)))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
            assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
      }
      
      # Table 3 
      {
        table_name <- "table_3"
        
        t3_m <- replicability %>% select(-paper_id) %>% 
          full_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          full_join(robust %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% select(-paper_id), by = "claim_id") %>% 
          full_join(process %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% select(-paper_id), by = "claim_id") %>% 
          left_join(extracted_claims %>% select(claim_id = unique_claim_id, paper_id), by = "claim_id") %>% 
          left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id") %>% 
          mutate(
            field = case_match(
              field,
              "economics and finance" ~ "economics",
              "psychology and health" ~ "psychology",
              "sociology and criminology" ~ "sociology",
              .default = field
            )
          ) %>% 
          select(-claim_id) %>% 
          pivot_longer(-c(paper_id, field), names_to = "metric", values_to = "outcome") %>% 
          drop_na(outcome) %>% 
          group_by(paper_id, metric) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup() %>% 
          group_by(field, metric) %>% 
          group_modify(
            ~ {bootstrap.clust(
              data = .,
              FUN = function(x) {
                weighted.mean(x$outcome, x$weight)
              },
              clustervar = "paper_id",
              keepvars = c("outcome", "weight"),
              alpha = .05, tails = "two-tailed", iters = iters,
              format.percent = F, digits = 2, leading.zero = F, na.rm = T
            )$formatted.text %>% enframe()
            }
          ) %>% 
          ungroup() %>% 
          select(-name)
        
        t3_n <- replicability %>% select(-paper_id) %>% 
          full_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          full_join(robust %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% select(-paper_id), by = "claim_id") %>% 
          full_join(process %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% select(-paper_id), by = "claim_id") %>% 
          left_join(extracted_claims %>% select(claim_id = unique_claim_id, paper_id), by = "claim_id") %>% 
          left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id") %>% 
          mutate(
            field = case_match(
              field,
              "economics and finance" ~ "economics",
              "psychology and health" ~ "psychology",
              "sociology and criminology" ~ "sociology",
              .default = field
            )
          ) %>% 
          select(-claim_id) %>% 
          pivot_longer(-c(paper_id, field), names_to = "metric", values_to = "outcome") %>% 
          drop_na(outcome) %>% 
          group_by(paper_id, metric) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup() %>% 
          group_by(field, metric) %>% 
          dplyr::summarize(N = sum(weight)) %>% 
          ungroup()
          
        table <- t3_n %>% left_join(t3_m, by = c("field", "metric")) %>% 
          pivot_wider(names_from = metric, values_from = c("N", "value")) %>% 
          select(field, N_score, value_score, N_supported, value_supported,
                 N_pr, value_pr, N_reproduced, value_reproduced) %>% 
          mutate(field = str_to_sentence(field)) %>% 
          mutate(across(contains("N_"), function(x) ifelse(is.na(x), 0, x))) %>% 
          mutate(across(contains("value_"), function(x) ifelse(is.na(x), "--", x)))

       
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
      }
      
      # Table 4
      {
        
        table_name <- "table_4"
        
        # for now filter out effect sizes below .02
        es <- es %>% 
          mutate(r_approx = select(., r_approx) %>% apply(1, abs)) %>% 
          filter(r_approx >= .02)
        
        # convert p-values of 0 to .00005 (smallest real p-value) then transforms them for correlation purposes
        sig <- sig %>% 
          filter(sig_format %in% c("interval", "p-value", "significance level")) %>% 
          mutate(p = ifelse(p_tails == "one", p*2, p)) %>% 
          mutate(p = ifelse(sig_format == "interval" & is.na(p), (100 - interval_percent)/100, p)) %>% 
          mutate(p = ifelse(sig_format == "significance level" & is.na(p), level_percent/100, p)) %>% 
          mutate(p = ifelse(p == 0, 0.00005, p)) %>% 
          drop_na(p) %>% 
          mutate(p = select(., p) %>% apply(1, function(x) log10(x)))
        
        hedge <- hedge %>% 
          mutate(
            h = case_when(
              class_id == "strong_boost" ~ 0/3,
              class_id == "weak_boost" ~ 1/3,
              class_id == "weak_hedge" ~ 2/3,
              class_id == "strong_hedge" ~ 3/3
            )
          ) %>% 
          mutate(
            nuance = case_when(
              class_id == "reject_nuance" ~ 0/2,
              class_id == "unspecified_nuance" ~ 1/2,
              class_id == "embrace_nuance" ~ 2/2
            )
          ) %>% 
          group_by(paper_id) %>% 
          dplyr::summarize(hedge = mean(h, na.rm = T), humility = mean(nuance, na.rm = T))
        
        # using natural log of g_score given its heavy skew
        stevens_misc <- oa_metadata %>% 
          mutate(popularity = log(g_score)) %>% 
          select(paper_id, gdp_pc, popularity)
        
        ds <- dataseer %>% 
          mutate(ds_pr = 1*(dataShr == "Yes")) %>% 
          select(paper_id, ds_pr)
        
        tab4_data <- extracted_claims %>% 
          filter(phase != "covid") %>% 
          select(paper_id) %>% distinct() %>% 
          left_join(statcheck, by = "paper_id") %>% 
          left_join(sciscore %>% select(paper_id, ss), by = "paper_id") %>% 
          left_join(es %>% select(-source), by = "paper_id") %>% 
          left_join(sig %>% select(paper_id, p), by = "paper_id") %>% 
          left_join(hedge, by = "paper_id") %>% 
          left_join(stevens_misc, by = "paper_id") %>% 
          left_join(oa_citations, by = "paper_id") %>% 
          left_join(robust, by = "paper_id") %>% 
          left_join(process, by = "paper_id") %>% 
          left_join(ds, by = "paper_id")
        
        table <- tab4_data %>% 
          pivot_longer(cols = -c(paper_id, supported, pr), names_to = "var", values_to = "value") %>% 
          mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
          select(claim_id, var, value, supported, pr) %>% 
          left_join(replicability %>% select(-paper_id), by = "claim_id") %>% 
          left_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          drop_na(value) %>% 
          pivot_longer(
            cols = -c(claim_id, var, value),
            names_to = "outcome", values_to = "result"
          ) %>% 
          drop_na(result) %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup() %>% 
          group_by(var, outcome) %>% 
          group_modify(
            ~ {bootstrap.clust(
              data = .,
              FUN = function(x) {
                weightedCorr(x$value, x$result, method = "Pearson", weights = x$weight)
              },
              clustervar = "paper_id",
              keepvars = c("value", "result", "weight"),
              alpha = .05, tails = "two-tailed", iters = iters,
              format.percent = F, digits = 2, leading.zero = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% enframe()
            }
          ) %>% 
          ungroup() %>% 
          pivot_wider(names_from = outcome, values_from = value) %>% 
          mutate(var = as_factor(var) %>% fct_relevel(., "sc", "ss", "ds_pr", "r_approx", "p", "hedge", "humility", "gdp_pc", "cite_age", "popularity", "cs", "log_count")) %>% 
          arrange(var) %>% 
          mutate(
            var = case_match(
              var,
              "sc" ~ "StatCheck",
              "ss" ~ "SciScore",
              "ds_pr" ~ "DataSeer",
              "r_approx" ~ "Original effect size",
              "p" ~ "Original p-value",
              "hedge" ~ "Hedging",
              "humility" ~ "Intellectual humility",
              "gdp_pc" ~ "GDP per capita",
              "cite_age" ~ "Citation age",
              "popularity" ~ "Topic popularity",
              "log_count" ~ "Author citations",
              "cs" ~ "Paper citations"
            )
          ) %>% 
          select(var, score, supported, pr, reproduced)
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table 5 
      {
        
        table_name <- "table_5"
        
        all_indicators <- melbourne %>% select(claim_id, melb = pc1) %>% 
          left_join(keyw %>% select(claim_id, kw = pc1), by = "claim_id") %>% 
          left_join(twosix %>% select(claim_id, ts = pc1), by = "claim_id") %>% 
          left_join(usc %>% select(claim_id, u = pc1), by = "claim_id") %>% 
          left_join(psu %>% select(claim_id, p = pc1), by = "claim_id") %>% 
          left_join(texas %>% select(claim_id, a = pc1), by = "claim_id") %>% 
          left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
          mutate(across(-claim_id, function(x) (x*1)))
        
        table <- extracted_claims %>% 
          filter(phase != "covid") %>% 
          select(paper_id) %>% distinct() %>% 
          left_join(statcheck, by = "paper_id") %>% 
          left_join(sciscore %>% select(paper_id, ss), by = "paper_id") %>% 
          left_join(ds, by = "paper_id") %>% 
          left_join(es %>% select(-source), by = "paper_id") %>% 
          left_join(sig %>% select(paper_id, p), by = "paper_id") %>% 
          left_join(hedge, by = "paper_id") %>% 
          left_join(stevens_misc, by = "paper_id") %>% 
          left_join(oa_citations, by = "paper_id") %>% 
          pivot_longer(cols = -paper_id, names_to = "var", values_to = "value") %>% 
          mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
          select(claim_id, paper_id, var, value) %>% 
          left_join(all_indicators %>% select(-score), by = "claim_id") %>% 
          drop_na(value) %>% 
          pivot_longer(
            cols = -c(claim_id, paper_id, var, value),
            names_to = "outcome", values_to = "result"
          ) %>% 
          drop_na(result) %>% 
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup() %>% 
          group_by(var, outcome) %>% 
          group_modify(
            ~ {bootstrap.clust(
              data = .,
              FUN = function(x) {
                weightedCorr(x$value, x$result, method = "Pearson", weights = x$weight)
              },
              clustervar = "paper_id",
              keepvars = c("value", "result", "weight"),
              alpha = .05, tails = "two-tailed", iters = iters,
              format.percent = F, digits = 2, leading.zero = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% enframe()
            }
          ) %>% 
          ungroup() %>% 
          select(-name) %>% 
          pivot_wider(names_from = outcome, values_from = value) %>% 
          mutate(var = as_factor(var) %>% fct_relevel(., "sc", "ss", "ds_pr", "r_approx", "p", "hedge", "humility", "gdp_pc", "cite_age", "popularity", "cs", "log_count")) %>% 
          arrange(var) %>% 
          mutate(
            var = case_match(
              var,
              "sc" ~ "StatCheck",
              "ss" ~ "SciScore",
              "ds_pr" ~ "DataSeer",
              "r_approx" ~ "Original effect size",
              "p" ~ "Original p-value",
              "hedge" ~ "Hedging",
              "humility" ~ "Intellectual humility",
              "gdp_pc" ~ "GDP per capita",
              "cite_age" ~ "Citation age",
              "popularity" ~ "Topic popularity",
              "log_count" ~ "Author citations",
              "cs" ~ "Paper citations"
            )
          ) %>% 
          select(var, melb, kw, ts, u, p, a)
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table 6
      {
        table_name <- "table_6"
        
        delivered <- status %>% 
          filter(p1_delivery | p2_delivery) %>% 
          select(paper_id) %>% 
          mutate(
            `Structured elicitations` = paper_id %in% melbourne$paper_id,
            `Prediction markets` = paper_id %in% keyw$paper_id,
            `A+` = paper_id %in% twosix$paper_id,
            MACROSCORE = paper_id %in% usc$paper_id,
            `Synthetic markets A` = paper_id %in% psu$paper_id,
            `Synthetic markets B` = paper_id %in% texas$paper_id,
            Replicability = paper_id %in% replicability$paper_id,
            Robustness = paper_id %in% robust$paper_id,
            `Outcome reproducibility` = paper_id %in% outcome$paper_id,
            `Process reproducibility` = paper_id %in% process$paper_id
          ) %>% 
          pivot_longer(cols = -paper_id, names_to = "metric", values_to = "coverage") %>% 
          select(-paper_id) %>% 
          group_by(metric) %>% 
          dplyr::summarize(coverage = (100*mean(coverage)) %>% round) %>% 
          mutate(coverage = glue("{coverage}%"))
        
        p1 <- status %>% 
          filter(p1_delivery) %>% 
          select(paper_id) %>% 
          mutate(
            `Structured elicitations` = paper_id %in% melbourne$paper_id,
            `Prediction markets` = paper_id %in% keyw$paper_id,
            `A+` = paper_id %in% twosix$paper_id,
            MACROSCORE = paper_id %in% usc$paper_id,
            `Synthetic markets A` = paper_id %in% psu$paper_id,
            `Synthetic markets B` = paper_id %in% texas$paper_id,
            Replicability = paper_id %in% replicability$paper_id,
            Robustness = paper_id %in% robust$paper_id,
            `Outcome reproducibility` = paper_id %in% outcome$paper_id,
            `Process reproducibility` = paper_id %in% process$paper_id
          ) %>% 
          pivot_longer(cols = -paper_id, names_to = "metric", values_to = "coverage") %>% 
          select(-paper_id) %>% 
          group_by(metric) %>% 
          dplyr::summarize(coverage = (100*mean(coverage)) %>% round) %>% 
          mutate(coverage = glue("{coverage}%"))
        
        rr <- status %>% 
          filter(RR) %>% 
          select(paper_id) %>% 
          mutate(
            `Structured elicitations` = paper_id %in% melbourne$paper_id,
            `Prediction markets` = paper_id %in% keyw$paper_id,
            `A+` = paper_id %in% twosix$paper_id,
            MACROSCORE = paper_id %in% usc$paper_id,
            `Synthetic markets A` = paper_id %in% psu$paper_id,
            `Synthetic markets B` = paper_id %in% texas$paper_id,
            Replicability = paper_id %in% replicability$paper_id,
            Robustness = paper_id %in% robust$paper_id,
            `Outcome reproducibility` = paper_id %in% outcome$paper_id,
            `Process reproducibility` = paper_id %in% process$paper_id
          ) %>% 
          pivot_longer(cols = -paper_id, names_to = "metric", values_to = "coverage") %>% 
          select(-paper_id) %>% 
          group_by(metric) %>% 
          dplyr::summarize(coverage = (100*mean(coverage)) %>% round) %>% 
          mutate(coverage = glue("{coverage}%"))
        
        bushel <- status %>% 
          filter(bushel) %>% 
          select(paper_id) %>% 
          mutate(
            `Structured elicitations` = paper_id %in% melbourne$paper_id,
            `Prediction markets` = paper_id %in% keyw$paper_id,
            `A+` = paper_id %in% twosix$paper_id,
            MACROSCORE = paper_id %in% usc$paper_id,
            `Synthetic markets A` = paper_id %in% psu$paper_id,
            `Synthetic markets B` = paper_id %in% texas$paper_id,
            Replicability = paper_id %in% replicability$paper_id,
            Robustness = paper_id %in% robust$paper_id,
            `Outcome reproducibility` = paper_id %in% outcome$paper_id,
            `Process reproducibility` = paper_id %in% process$paper_id
          ) %>% 
          pivot_longer(cols = -paper_id, names_to = "metric", values_to = "coverage") %>% 
          select(-paper_id) %>% 
          group_by(metric) %>% 
          dplyr::summarize(coverage = (100*mean(coverage)) %>% round) %>% 
          mutate(coverage = glue("{coverage}%"))

        bushel_claims <- extracted_claims %>% 
          filter(phase == "P2-bushel") %>% 
          select(claim_id = unique_claim_id) %>% 
          mutate(
            `Structured elicitations` = claim_id %in% melbourne$claim_id,
            `Prediction markets` = claim_id %in% keyw$claim_id,
            `A+` = claim_id %in% twosix$claim_id,
            MACROSCORE = claim_id %in% usc$claim_id,
            `Synthetic markets A` = claim_id %in% psu$claim_id,
            `Synthetic markets B` = claim_id %in% texas$claim_id,
            Replicability = claim_id %in% replicability$claim_id,
            Robustness = claim_id %in% (robust %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% pull(claim_id)),
            `Outcome reproducibility` = claim_id %in% outcome$claim_id,
            `Process reproducibility` = claim_id %in% (process %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% pull(claim_id))
          ) %>% 
          pivot_longer(cols = -claim_id, names_to = "metric", values_to = "coverage") %>% 
          select(-claim_id) %>% 
          group_by(metric) %>% 
          dplyr::summarize(coverage = (100*mean(coverage)) %>% round) %>% 
          mutate(coverage = glue("{coverage}%"))
        
        table <- delivered %>% 
          left_join(p1, by = "metric") %>% 
          left_join(rr, by = "metric") %>% 
          left_join(bushel, by = "metric") %>% 
          left_join(bushel_claims, by = "metric") %>% 
          mutate(
            metric = as_factor(metric) %>% fct_relevel(
              "Structured elicitations",
              "Prediction markets",
              "A+",
              "MACROSCORE",
              "Synthetic markets A",
              "Synthetic markets B",
              "Replicability",
              "Robustness",
              "Outcome reproducibility",
              "Process reproducibility"
            )
          ) %>% 
          arrange(metric) %>% 
          mutate(metric = as.character(metric))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S3
      {
        table_name <- "table_s3"
        
        table <- status %>% 
          filter(prepool) %>% 
          left_join(paper_fields, by = "paper_id") %>% 
          group_by(field) %>% 
          dplyr::summarize(
            prectf = n(),
            ctf = sum(pool),
            precvd = sum(presample | p2_presample),
            cvd = sum(p1_delivery | p2_delivery),
            rr = sum(RR),
            bushel = sum(bushel)
          ) %>% 
          ungroup() %>% 
          mutate(
            across(
              .cols = -field,
              .fns = function(x) ((x/sum(x))*100) %>% round(1),
              .names = "{.col}_perc"
            )
          ) %>% 
          select(field, prectf, prectf_perc, ctf, ctf_perc, precvd, precvd_perc, cvd, cvd_perc, rr, rr_perc, bushel, bushel_perc) %>% 
          bind_rows(summarize_at(., vars(-field), sum)) %>% 
          mutate(field = ifelse(is.na(field), "Total", field))
        
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
        #st_papers <- pr_outcomes %>% filter(!covid) %>% nrow()
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
      all_indicators <- melbourne %>% select(claim_id, melb = pc1) %>% 
        left_join(keyw %>% select(claim_id, kw = pc1), by = "claim_id") %>% 
        left_join(twosix %>% select(claim_id, ts = pc1), by = "claim_id") %>% 
        left_join(usc %>% select(claim_id, u = pc1), by = "claim_id") %>% 
        left_join(psu %>% select(claim_id, p = pc1), by = "claim_id") %>% 
        left_join(texas %>% select(claim_id, a = pc1), by = "claim_id") %>% 
        left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
        mutate(across(-claim_id, function(x) (x*1)))
      
      custom_scatter <- function(data, mapping, ...) {
        ggplot(data, mapping) +
          geom_point(alpha = 0.05, color = "steelblue") +  
          geom_smooth(method = "lm", se = TRUE, color = "#e07a5f", fill = "#b45b3e") +
          scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
          scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
          theme_minimal()
      }
      
      no_axis_diag <- function(data, mapping, ...) {
        ggally_densityDiag(data, mapping, fill = "steelblue", alpha = 0.3) +
          scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
          scale_y_continuous(labels = NULL, breaks = NULL) +
          theme(
            panel.background = element_rect(fill = "#f7fafd", color = NA),
            plot.background = element_rect(fill = "#f7fafd", color = NA)
          )
      }
      
      cor_with_ci <- function(data, mapping, ...) {
        x <- eval_data_col(data, mapping$x)
        y <- eval_data_col(data, mapping$y)
        
        xy <- tibble(x, y)
        
        cor_test <- cor.test(xy$x, xy$y, method = "pearson", use = "pairwise.complete.obs")
        
        r <- formatC(cor_test$estimate, format = "f", digits = 2) %>% 
          str_replace_all(., "^0\\.", ".") %>% str_replace_all(., "^-0\\.", "-.")
        ci_low <- formatC(cor_test$conf.int[1], format = "f", digits = 2) %>% 
          str_replace_all(., "^0\\.", ".") %>% str_replace_all(., "^-0\\.", "-.")
        ci_high <- formatC(cor_test$conf.int[2], format = "f", digits = 2) %>% 
          str_replace_all(., "^0\\.", ".") %>% str_replace_all(., "^-0\\.", "-.")
        
        cor_lab <- glue("       {r}
                  [{ci_low}, {ci_high}]")
        
        ggplot(data, mapping) +
          annotate("text", x = 0.5, y = 0.5, label = cor_lab, size = 7, hjust = 0.5, fontface = "bold") +
          theme_void() +
          theme(
            panel.background = element_rect(fill = "#f7fafd", color = NA),
            plot.background = element_rect(fill = "#f7fafd", color = NA)
          )
      }
    }
    
    
    # Plot generation (ggplot object)
    {
      plot <- all_indicators %>% 
        select(-claim_id) %>% 
        rename(
          `Structured\nelicitations` = melb,
          `Markets` = kw,
          `A+` = ts,
          MACRO = u, 
          `Synthetic A` = p,
          `Synthetic B` = a,
          Replicability = score
        ) %>% 
        ggpairs(
          lower = list(continuous = wrap(custom_scatter)),
          diag = list(continuous = no_axis_diag),
          upper = list(continuous = cor_with_ci)
        ) %>% 
        { 
          # histogram for replicability
          hist_plot <- ggplot(all_indicators, aes(x = score)) +
            geom_histogram(
              aes(y = after_stat(count / sum(count))),
              fill = "steelblue", bins = 20
            ) +
            scale_x_continuous(breaks = c(0, 1)) +
            ylim(0, 1) +
            theme(
              panel.background = element_rect(fill = "#f7fafd", color = NA),
              plot.background = element_rect(fill = "#f7fafd", color = NA)
            )
          
          
          .[7, 7] <- hist_plot
          .
        } +
        theme(
          axis.text = element_text(size = 14),
          strip.background = element_blank(),
          strip.text = element_text(size = 14, face = "bold")
        )
    }
    
    # Assign plot to list of plots to export
    {
      assign(plotname,plot)
      plotlist <- append(plotlist, list(get(plotname)), after = length(plotlist))
      names(plotlist)[[length(plotlist)]] <- plotname
    }
  }
  
  # Figure X. _____
  {
    plotname <- "figure_x"
    
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
