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
    library(targets)
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
    
    # run tar_make for data updates
    tar_make()
    
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
    
    
    ### Best variants
    melb_p1_bv <- melb_p1 %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m7)
    
    melb_p2_bv <- melb_p2 %>% 
      rename(ta2_pid = paper_id) %>% 
      left_join(p2_id_key %>% select(paper_id, ta2_pid), by = "ta2_pid") %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m5)
    
    melb_bushel_bv <- melb_bushel %>% 
      filter(!is.na(evidence_id)) %>% 
      filter(signal == "deviation_replicability") %>% 
      left_join(stitched_claims %>% select(claim4_id, evidence_id = ta2_claim4_id), by = "evidence_id") %>% 
      mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>% 
      select(claim_id, cs = cs_m4)
    
    melb_bv <- melb_p1_bv %>% bind_rows(melb_p2_bv) %>% bind_rows(melb_bushel_bv)
    
    kw_bv <- kw_scores %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m9)
    
    ts_s1_bv <- ts_s1 %>% 
      rename(paper_id = ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m1)
    
    ts_s2_bv <- ts_s2 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      select(-ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m1)
    
    ts_s3_bv <- ts_s3 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      select(-ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m2)
    
    ts_bushel_bv <- ts_bushel %>% 
      left_join(stitched_claims %>% select(paper_id, claim4_id, ta3_claim4_id), by = "ta3_claim4_id") %>% 
      mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>% 
      select(claim_id, cs = cs_m5)
    
    ts_bv <- ts_s1_bv %>% bind_rows(ts_s2_bv) %>% bind_rows(ts_s3_bv) %>% bind_rows(ts_bushel_bv)
    
    usc_s1_bv <- usc_s1 %>% 
      rename(paper_id = ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m1)
    
    usc_s2_bv <- usc_s2 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      select(-ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m3)
    
    usc_s3_bv <- usc_s3 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      select(-ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m3)
    
    usc_bushel_bv <- usc_bushel %>% 
      select(-paper_id) %>% 
      left_join(stitched_claims %>% select(paper_id, claim4_id, ta3_claim4_id), by = "ta3_claim4_id") %>% 
      mutate(claim_id = select(., c(paper_id, claim4_id)) %>% apply(1, function(x) str_c(x, collapse = "_"))) %>% 
      select(claim_id, cs = cs_m2)
    
    usc_bv <- usc_s1_bv %>% bind_rows(usc_s2_bv) %>% bind_rows(usc_s3_bv) %>% bind_rows(usc_bushel_bv)
    
    psu_s1_bv <- psu_s1 %>% 
      rename(paper_id = ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m5)
    
    psu_s2_bv <- psu_s2 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      select(-ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m6)
    
    psu_s3_bv <- psu_s3 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      select(-ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m3)
    
    psu_bv <- psu_s1_bv %>% bind_rows(psu_s2_bv) %>% bind_rows(psu_s3_bv)
    
    am_s1_bv <- psu_s1 %>% 
      rename(paper_id = ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m8)
    
    am_s2_bv <- psu_s2 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      select(-ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m8)
    
    am_s3_bv <- psu_s3 %>% 
      left_join(p2_id_key %>% select(paper_id, ta3_pid), by = "ta3_pid") %>% 
      select(-ta3_pid) %>% 
      mutate(across(contains("cs_m"), function(x) ifelse(x == 0.5, NA, x))) %>% 
      mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
      select(claim_id, cs = cs_m7)
      
    am_bv <- am_s1_bv %>% bind_rows(am_s2_bv) %>% bind_rows(am_s3_bv)
    
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
        
        set.seed(6389)
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
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
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
        # NOTE!!!!!!!!!: round to whole numbers for deduplication purposes
        stevens_misc <- oa_metadata %>% 
          mutate(popularity = log(g_score)) %>% 
          select(paper_id, gdp_pc, popularity) %>% 
          mutate(across(c(gdp_pc, popularity), function(x) round(x))) %>% 
          distinct()
        
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
          left_join(ds, by = "paper_id") %>% 
          rename(pv = p)
        
        set.seed(5132)
        table <- tab4_data %>% 
          left_join(
            replicability %>% full_join(outcome %>% select(-paper_id), by = "claim_id"),
            by = "paper_id"
          ) %>% 
          select(-claim_id) %>% 
          pivot_longer(cols = -c(paper_id, supported, pr, score, reproduced), names_to = "var", values_to = "value") %>% 
          drop_na(value) %>% 
          pivot_longer(
            cols = -c(paper_id, var, value),
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
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
            }
          ) %>% 
          ungroup() %>% 
          pivot_wider(names_from = outcome, values_from = value) %>% 
          mutate(var = as_factor(var) %>% fct_relevel(., "sc", "ss", "ds_pr", "r_approx", "pv", "hedge", "humility", "gdp_pc", "cite_age", "popularity", "cs", "log_count")) %>% 
          arrange(var) %>% 
          mutate(
            var = case_match(
              var,
              "sc" ~ "StatCheck",
              "ss" ~ "SciScore",
              "ds_pr" ~ "DataSeer",
              "r_approx" ~ "Original effect size",
              "pv" ~ "Original p-value",
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
          mutate(across(-claim_id, function(x) (x*1))) %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first()))
        
        tab5_data <- extracted_claims %>% 
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
          rename(pv = p)
        
        set.seed(4801)
        table <- tab5_data %>% 
          full_join(all_indicators, by = "paper_id") %>% 
          pivot_longer(cols = -c(all_of(names(all_indicators))), names_to = "var", values_to = "value") %>% 
          drop_na(value) %>% 
          select(-claim_id) %>% 
          pivot_longer(
            cols = -c(paper_id, var, value),
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
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
            }
          ) %>% 
          ungroup() %>% 
          select(-name) %>% 
          pivot_wider(names_from = outcome, values_from = value) %>% 
          mutate(var = as_factor(var) %>% fct_relevel(., "sc", "ss", "ds_pr", "r_approx", "pv", "hedge", "humility", "gdp_pc", "cite_age", "popularity", "cs", "log_count")) %>% 
          arrange(var) %>% 
          mutate(
            var = case_match(
              var,
              "sc" ~ "StatCheck",
              "ss" ~ "SciScore",
              "ds_pr" ~ "DataSeer",
              "r_approx" ~ "Original effect size",
              "pv" ~ "Original p-value",
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
          bind_rows(summarize_at(., vars(-field), function(x) round(sum(x)))) %>% 
          mutate(field = ifelse(is.na(field), "Total", field))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
      }
      
      # Table S4
      {
        table_name <- "table_s4"
        
        table <- status %>% 
          filter(prepool) %>% 
          left_join(CR_metadata_prepool %>% select(paper_id, year = pub_year), by = "paper_id") %>% 
          mutate(year = as.character(year)) %>% 
          group_by(year) %>% 
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
              .cols = -year,
              .fns = function(x) ((x/sum(x))*100) %>% round(1),
              .names = "{.col}_perc"
            )
          ) %>% 
          select(year, prectf, prectf_perc, ctf, ctf_perc, precvd, precvd_perc, cvd, cvd_perc, rr, rr_perc, bushel, bushel_perc) %>% 
          bind_rows(summarize_at(., vars(-year), sum)) %>% 
          mutate(year = ifelse(is.na(year), "Total", year))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S5
      {
        table_name <- "table_s5"
        
        table <- CR_metadata_prepool %>% 
          semi_join(status %>% filter(p1_delivery | p2_delivery), by = "paper_id") %>% 
          select(paper_id, year = pub_year, publication_standard) %>% 
          left_join(publications %>% select(publication_standard, field = COS_pub_category), by = "publication_standard") %>% 
          group_by(field) %>% 
          mutate(j = length(unique(publication_standard))) %>% 
          ungroup() %>% 
          group_by(year, field, j) %>% 
          dplyr::summarize(t = n()) %>% 
          ungroup() %>% 
          pivot_wider(names_from = "year", values_from = "t") %>% 
          mutate(Total = select(., c(`2009`:`2018`)) %>% apply(1, sum)) %>% 
          mutate(p = (100*(Total/sum(Total))) %>% round(1)) %>% 
          mutate(field = str_to_sentence(field)) %>% 
          bind_rows(summarize_at(., vars(-field), sum)) %>% 
          mutate(field = ifelse(is.na(field), "Total", field))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S6
      {
        table_name <- "table_s6"
        
        # prepare the claims columns separately
        b_claims <- stitched_claims %>% 
          left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id") %>% 
          group_by(field) %>% 
          dplyr::summarize(claims = n()) %>% 
          ungroup()
        
        table <- CR_metadata_prepool %>% 
          semi_join(status %>% filter(p1_delivery | p2_delivery), by = "paper_id") %>% 
          select(paper_id, publication_standard) %>% 
          left_join(publications %>% select(publication_standard, field = COS_pub_category), by = "publication_standard") %>% 
          group_by(field) %>% 
          mutate(j = length(unique(publication_standard)), .after = paper_id) %>% 
          ungroup() %>% 
          select(-publication_standard) %>% 
          left_join(status, by = "paper_id") %>% 
          group_by(field, j) %>% 
          dplyr::summarize(
            annotation = sum(p1_delivery | p2_delivery),
            evidence = sum(RR),
            bushel = sum(bushel)
          ) %>% 
          ungroup() %>% 
          left_join(b_claims, by = "field") %>% 
          mutate(
            across(
              .cols = -c(field, j),
              .fns = function(x) ((x/sum(x))*100) %>% round(1),
              .names = "{.col}_perc"
            )
          ) %>% 
          select(field, j, annotation, annotation_perc, evidence, evidence_perc, bushel, bushel_perc, claims, claims_perc) %>% 
          bind_rows(summarize_at(., vars(-field), function(x) round(sum(x)))) %>% 
          mutate(field = ifelse(is.na(field), "Total", field)) %>% 
          mutate(field = str_to_sentence(field))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S7
      {
        table_name <- "table_s7"
        
        table <- status %>% 
          filter(p1_delivery | p2_delivery) %>% 
          select(paper_id) %>% 
          left_join(paper_metadata %>% select(paper_id, publication_standard, pub_year, field = COS_pub_category), by = "paper_id") %>% 
          select(-paper_id) %>% 
          group_by(publication_standard, field, pub_year) %>% 
          dplyr::summarize(t = n()) %>% 
          ungroup() %>% 
          pivot_wider(names_from = "pub_year", values_from = "t") %>% 
          mutate(field = str_to_sentence(field)) %>% 
          mutate(total = select(., c(`2009`:`2018`)) %>% apply(1, function(x) sum(x, na.rm = T))) %>% 
          bind_rows(summarize_at(., vars(-c(publication_standard, field)), function(x) sum(x, na.rm = T))) %>% 
          mutate(publication_standard = ifelse(is.na(publication_standard), "Total", publication_standard)) %>% 
          mutate(
            across(
              .cols = everything(),
              .fns = function(x) ifelse(is.na(x), "--", x)
            )
          )
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S11
      {
        table_name <- "table_s11"
        
        table <- status %>% 
          filter(prepool) %>% 
          left_join(paper_fields, by = "paper_id") %>% 
          group_by(field) %>% 
          dplyr::summarize(
            rr_samp = sum(RR),
            bushel_samp = sum(bushel)
          ) %>% 
          ungroup() %>% 
          mutate(rr_prop = round(100*rr_samp/sum(rr_samp), 1), .after = rr_samp) %>% 
          mutate(bush_prop = round(100*bushel_samp/sum(bushel_samp), 1)) %>% 
          bind_rows(summarize_at(., vars(-field), function(x) sum(x, na.rm = T))) %>% 
          mutate(field = ifelse(is.na(field), "Total", field)) %>% 
          mutate(across(contains("prop"), ~as.character(glue_data(list(x = .x), "{x}%"))))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
        
      }
        
      # Table S12
      {
        table_name <- "table_s12"
        
        table <- stitched_claims %>% 
          left_join(paper_fields, by = "paper_id") %>% 
          group_by(field) %>% 
          dplyr::summarize(
            papers = length(unique(paper_id)),
            n_claims = n(),
            n_focal = sum(focal)
          ) %>% 
          mutate(claims = round(n_claims/papers, 1)) %>% 
          mutate(focal = round(n_focal/papers, 1)) %>% 
          select(-contains("n_"))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}

      }
      
      # Table S16
      {
        table_name <- "table_s16"
        
        set.seed(9396)
        table <- repli_binary %>% 
          rename_with(., function(x) str_remove_all(x, "repli_"), everything()) %>% 
          left_join(repli_outcomes %>% select(report_id, claim_id, binary_score = repli_score_criteria_met), by = "report_id") %>% 
          select(-report_id) %>% 
          relocate(claim_id, .before = binary_analyst) %>% 
          left_join(replicability %>% select(-paper_id), by = "claim_id") %>% 
          full_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
          full_join(robust, by = "paper_id") %>% 
          full_join(process, by = "paper_id") %>% 
          pivot_longer(cols = contains("binary"), names_to = "measure", values_to = "outcome") %>% 
          pivot_longer(cols = c(score, supported, pr, reproduced), names_to = "indicator", values_to = "value") %>% 
          group_by(paper_id, measure, indicator) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup() %>%
          group_by(measure, indicator) %>% 
          group_modify(
            ~ {bootstrap.clust(
              data = .,
              FUN = function(x) {
                weightedCorr(x$outcome, x$value, method = "Pearson", weights = x$weight)
              },
              clustervar = "paper_id",
              keepvars = c("outcome", "value", "weight"),
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
            }
          ) %>% 
          ungroup() %>% 
          select(-name) %>% 
          mutate(measure = select(., measure) %>% apply(1, function(x) str_remove_all(x, "binary_"))) %>% 
          pivot_wider(names_from = indicator, values_from = value) %>% 
          select(measure, score, supported, pr, reproduced) %>% 
          mutate(score = ifelse(measure == "score", "--", score)) %>% 
          mutate(
            measure = case_match(
              measure,
              "analyst" ~ "Analyst interpretation",
              "bayes_rep" ~ "Replication Bayes factor",
              "bf_result" ~ "Bayes factor",
              "bma_result" ~ "Bayesian meta-analysis",
              "correspondence" ~ "Correspondence test",
              "meta_success" ~ "Meta-analysis",
              "orig_wthn" ~ "Orig. in rep. CI",
              "rep_wthn" ~ "Rep. in orig. CI",
              "score" ~ "Sig. + pattern",
              "skep_p" ~ "Skeptical p-value",
              "sum_p" ~ "Sum of p-values",
              "telescopes" ~ "Small telescopes",
              "wthn_pi" ~ "Prediction interval"
            )
          )
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S17
      {
        table_name <- "table_s17"
        
        tab_s17_data <- melbourne %>% select(claim_id, melb = pc1) %>% 
          left_join(keyw %>% select(claim_id, kw = pc1), by = "claim_id") %>% 
          left_join(twosix %>% select(claim_id, ts = pc1), by = "claim_id") %>% 
          left_join(usc %>% select(claim_id, u = pc1), by = "claim_id") %>% 
          left_join(psu %>% select(claim_id, p = pc1), by = "claim_id") %>% 
          left_join(texas %>% select(claim_id, a = pc1), by = "claim_id") %>% 
          left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
          pivot_longer(cols = -claim_id, names_to = "measure", values_to = "value") %>% 
          drop_na() %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
          mutate(weight = 1)
        
        table <- tab_s17_data %>%  
          group_by(measure) %>% 
          dplyr::summarize(
            N = round(sum(weight), 1),
            Median = round(weighted.median(value, weight), 2),
            Mean = round(weighted.mean(value, weight), 2),
            SD = round(sqrt(wtd.var(value, weight)), 2)
          ) %>% 
          left_join(
            tab_s17_data %>% 
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
      
      # Table S18
      {
        table_name <- "table_s18"
        
        set.seed(1401)
        t_s18_m <- replicability %>% select(-paper_id) %>% 
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
          pivot_longer(-c(paper_id, field, claim_id), names_to = "metric", values_to = "outcome") %>% 
          drop_na(outcome) %>% 
          mutate(weight = 1) %>% 
          group_by(field, metric) %>% 
          group_modify(
            ~ {bootstrap.clust(
              data = .,
              FUN = function(x) {
                weighted.mean(x$outcome, x$weight)
              },
              clustervar = "claim_id",
              keepvars = c("outcome", "weight"),
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% enframe()
            }
          ) %>% 
          ungroup() %>% 
          select(-name)
        
        t_s18_n <- replicability %>% select(-paper_id) %>% 
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
          mutate(weight = 1) %>% 
          group_by(field, metric) %>% 
          dplyr::summarize(N = sum(weight)) %>% 
          ungroup()
        
        table <- t_s18_n %>% left_join(t_s18_m, by = c("field", "metric")) %>% 
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
      
      # Table S19
      {
        table_name <- "table_s19"
        
        tab_s19_data <- extracted_claims %>% 
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
        
        set.seed(7670)
        table <- replicability %>% 
          full_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          full_join(tab_s19_data, by = "paper_id") %>% 
          pivot_longer(cols = -c(paper_id, claim_id, score, supported, pr, reproduced), names_to = "var", values_to = "value") %>% 
          drop_na(value) %>% 
          pivot_longer(
            cols = -c(paper_id, claim_id, var, value),
            names_to = "outcome", values_to = "result"
          ) %>% 
          drop_na(result) %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
          mutate(weight = 1) %>% 
          group_by(var, outcome) %>% 
          group_modify(
            ~ {bootstrap.clust(
              data = .,
              FUN = function(x) {
                weightedCorr(x$value, x$result, method = "Pearson", weights = x$weight)
              },
              clustervar = "claim_id",
              keepvars = c("value", "result", "weight"),
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
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
      
      # Table S20
      {
        table_name <- "table_s20"
        
        all_indicators <- melbourne %>% select(claim_id, melb = pc1) %>% 
          left_join(keyw %>% select(claim_id, kw = pc1), by = "claim_id") %>% 
          left_join(twosix %>% select(claim_id, ts = pc1), by = "claim_id") %>% 
          left_join(usc %>% select(claim_id, u = pc1), by = "claim_id") %>% 
          left_join(psu %>% select(claim_id, p = pc1), by = "claim_id") %>% 
          left_join(texas %>% select(claim_id, a = pc1), by = "claim_id") %>% 
          left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
          mutate(across(-claim_id, function(x) (x*1))) %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first()))
        
        set.seed(8121)
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
          rename(pv = p) %>% 
          left_join(all_indicators, by = "paper_id") %>% 
          pivot_longer(cols = -c(all_of(names(all_indicators))), names_to = "var", values_to = "value") %>% 
          drop_na(value) %>% 
          pivot_longer(
            cols = -c(claim_id, paper_id, var, value),
            names_to = "outcome", values_to = "result"
          ) %>% 
          drop_na(result) %>% 
          mutate(weight = 1) %>% 
          group_by(var, outcome) %>% 
          group_modify(
            ~ {bootstrap.clust(
              data = .,
              FUN = function(x) {
                weightedCorr(x$value, x$result, method = "Pearson", weights = x$weight)
              },
              clustervar = "claim_id",
              keepvars = c("value", "result", "weight"),
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
            }
          ) %>% 
          ungroup() %>% 
          select(-name) %>% 
          pivot_wider(names_from = outcome, values_from = value) %>% 
          mutate(var = as_factor(var) %>% fct_relevel(., "sc", "ss", "ds_pr", "r_approx", "pv", "hedge", "humility", "gdp_pc", "cite_age", "popularity", "cs", "log_count")) %>% 
          arrange(var) %>% 
          mutate(
            var = case_match(
              var,
              "sc" ~ "StatCheck",
              "ss" ~ "SciScore",
              "ds_pr" ~ "DataSeer",
              "r_approx" ~ "Original effect size",
              "pv" ~ "Original p-value",
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
      
      # Table S22
      {
        table_name <- "table_s22"
        
        tab_s22_data <- melb_bv %>% select(claim_id, melb = cs) %>% 
          left_join(kw_bv %>% select(claim_id, kw = cs), by = "claim_id") %>% 
          left_join(ts_bv %>% select(claim_id, ts = cs), by = "claim_id") %>% 
          left_join(usc_bv %>% select(claim_id, u = cs), by = "claim_id") %>% 
          left_join(psu_bv %>% select(claim_id, p = cs), by = "claim_id") %>% 
          left_join(am_bv %>% select(claim_id, a = cs), by = "claim_id") %>% 
          left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
          pivot_longer(cols = -claim_id, names_to = "measure", values_to = "value") %>% 
          drop_na() %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
          group_by(paper_id, measure) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup()

        table <- tab_s22_data %>%  
          group_by(measure) %>% 
          dplyr::summarize(
            N = round(sum(weight), 1),
            Median = round(weighted.median(value, weight), 2),
            Mean = round(weighted.mean(value, weight), 2),
            SD = round(sqrt(wtd.var(value, weight)), 2)
          ) %>% 
          left_join(
            tab_s22_data %>% 
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
      
      # Table S23
      {
        table_name <- "table_s23"
        
        bv_indicators <- melb_bv %>% select(claim_id, melb = cs) %>% 
          left_join(kw_bv %>% select(claim_id, kw = cs), by = "claim_id") %>% 
          left_join(ts_bv %>% select(claim_id, ts = cs), by = "claim_id") %>% 
          left_join(usc_bv %>% select(claim_id, u = cs), by = "claim_id") %>% 
          left_join(psu_bv %>% select(claim_id, p = cs), by = "claim_id") %>% 
          left_join(am_bv %>% select(claim_id, a = cs), by = "claim_id") %>% 
          left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
          mutate(across(-claim_id, function(x) (x*1))) %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first()))
        
        set.seed(1154)
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
          rename(pv = p) %>% 
          left_join(bv_indicators, by = "paper_id") %>% 
          pivot_longer(cols = -c(all_of(names(bv_indicators))), names_to = "var", values_to = "value") %>% 
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
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
            }
          ) %>% 
          ungroup() %>% 
          select(-name) %>% 
          pivot_wider(names_from = outcome, values_from = value) %>% 
          mutate(var = as_factor(var) %>% fct_relevel(., "sc", "ss", "ds_pr", "r_approx", "pv", "hedge", "humility", "gdp_pc", "cite_age", "popularity", "cs", "log_count")) %>% 
          arrange(var) %>% 
          mutate(
            var = case_match(
              var,
              "sc" ~ "StatCheck",
              "ss" ~ "SciScore",
              "ds_pr" ~ "DataSeer",
              "r_approx" ~ "Original effect size",
              "pv" ~ "Original p-value",
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
      
      # Table S29
      {
        table_name <- "table_s29"
        
        scholarcy <- scholarcy %>% 
          mutate(paper_id = select(., Filename) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% last())) %>% 
          select(paper_id, data_statement = `Data_NLP?`, code_statement = `Code_NLP?`, open_reg = `Registration_hardcoded?`) %>% 
          mutate(across(-paper_id, ~ . == "y")) %>% 
          select(-code_statement)
        
        ksu <- ksu %>% 
          mutate(paper_id = ifelse(paper_id == "12", "12E0", paper_id)) %>% 
          mutate(paper_id = ifelse(paper_id == "2300000", "23E5", paper_id)) %>% 
          mutate(paper_id = ifelse(paper_id == "300000000", "03e8", paper_id)) %>% 
          mutate(paper_id = ifelse(paper_id == "8E+075", "8E75", paper_id)) %>% 
          select(paper_id, power_ratio = `Actual/80% power samples`, rep_pred = `Replicability Prediction`) %>% 
          mutate(power_ratio = as.numeric(power_ratio))
        
        # filter to one row per country
        stevens <- oa_metadata %>% 
          mutate(
            democ_avg = select(., democracy) %>% 
              apply(1, function(x) str_remove_all(x, "['\\[\\]]") %>% str_split(., ", ") %>% unlist() %>% 
                      as.numeric %>% mean(., na.rm = T))
          ) %>% 
          mutate(
            indust_avg = select(., industrialization) %>% 
              apply(1, function(x) str_remove_all(x, "['\\[\\]]") %>% str_split(., ", ") %>% unlist() %>% 
                      as.numeric %>% mean(., na.rm = T))
          ) %>% 
          mutate(
            educ_avg = select(., education_index) %>% 
              apply(1, function(x) str_remove_all(x, "['\\[\\]]") %>% str_split(., ", ") %>% unlist() %>% 
                      as.numeric %>% mean(., na.rm = T))
          ) %>% 
          mutate(
            rank_avg = select(., gdp_ranking) %>% 
              apply(1, function(x) str_remove_all(x, "['\\[\\]]") %>% str_split(., ", ") %>% unlist() %>% 
                      as.numeric %>% mean(., na.rm = T))
          ) %>% 
          select(paper_id, num_countries, developed, unemployment, num_countries_western, democ_avg, indust_avg,
                 educ_avg, cpi = CPI, rank_avg, retraction_probability, retractions_num_author,
                 g_score, j_score, num_references) %>% 
          group_by(paper_id) %>% 
          slice(1) %>%  
          ungroup()
        
        de_data <- extracted_claims %>% 
          filter(phase != "covid") %>% 
          select(paper_id) %>% distinct() %>% 
          left_join(robust, by = "paper_id") %>% 
          left_join(process, by = "paper_id") %>% 
          left_join(stevens, by = "paper_id") %>% 
          left_join(ksu, by = "paper_id") %>% 
          left_join(scholarcy, by = "paper_id")
        
        set.seed(9231)
        table <- de_data %>% 
          left_join(
            replicability %>% full_join(outcome %>% select(-paper_id), by = "claim_id"),
            by = "paper_id"
          ) %>% 
          pivot_longer(cols = c(all_of(names(de_data)), -paper_id, -pr, -supported), names_to = "var", values_to = "value") %>% 
          drop_na(value) %>% 
          pivot_longer(
            cols = -c(claim_id, paper_id, var, value),
            names_to = "outcome", values_to = "result"
          ) %>% 
          drop_na(result) %>% 
          select(-claim_id) %>% 
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
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
            }
          ) %>% 
          ungroup() %>% 
          select(-name) %>% 
          pivot_wider(names_from = outcome, values_from = value) %>% 
          mutate(
            var = as_factor(var) %>% 
              fct_relevel(
                "num_countries", 
                "developed", 
                "unemployment", 
                "num_countries_western", 
                "democ_avg", 
                "indust_avg",
                "educ_avg", 
                "cpi", 
                "rank_avg", 
                "retraction_probability", 
                "retractions_num_author",
                "g_score", 
                "j_score", 
                "num_references",
                "data_statement",
                "open_reg",
                "power_ratio",
                "rep_pred",
              )
          ) %>% 
          arrange(var) %>% 
          mutate(
            var = case_match(
              var,
              "num_countries" ~ "International collaboration",
              "developed" ~ "Developed*",
              "unemployment" ~ "Unemployment rate*",
              "num_countries_western" ~ "Western collaboration",
              "democ_avg" ~ "Democracy index*",
              "indust_avg" ~ "Industrialization index*",
              "educ_avg" ~ "Education index*",
              "cpi" ~ "Consumer price index*",
              "rank_avg" ~ "GDP rank*",
              "retraction_probability" ~ "Retraction frequency (genre)",
              "retractions_num_author" ~ "Retraction frequency (author)",
              "g_score" ~ "Topic popularity",
              "j_score" ~ "References' h-indices",
              "num_references" ~ "Number of references",
              "data_statement" ~ "Open data statement",
              "open_reg" ~ "Registration identified",
              "power_ratio" ~ "Sample size to 80% power (ratio)",
              "rep_pred" ~ "Replicability prediction**"
            )
          ) %>% 
          mutate(across(everything(), function(x) ifelse(str_detect(x, "NA"), "--", x))) %>% 
          select(var, score, supported, pr, reproduced)
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S30
      {
        table_name <- "table_s30"
        
        set.seed(4258)
        table <- extracted_claims %>% 
          filter(phase != "covid") %>% 
          select(paper_id) %>% distinct() %>% 
          left_join(stevens, by = "paper_id") %>% 
          left_join(ksu, by = "paper_id") %>% 
          left_join(scholarcy, by = "paper_id") %>% 
          left_join(all_indicators, by = "paper_id") %>% 
          pivot_longer(cols = -c(all_of(names(all_indicators))), names_to = "var", values_to = "value") %>% 
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
              alpha = .05, tails = "two-tailed", iters = iters, parallel = T,
              format.percent = F, digits = 2, leading.zero = F, CI.prefix = F, CI.sep = ", ", na.rm = T
            )$formatted.text %>% str_replace_all(., "\\[", "\n[") %>%  enframe()
            }
          ) %>% 
          ungroup() %>% 
          pivot_wider(names_from = outcome, values_from = value) %>% 
          mutate(
            var = as_factor(var) %>% 
              fct_relevel(
                "num_countries", 
                "developed", 
                "unemployment", 
                "num_countries_western", 
                "democ_avg", 
                "indust_avg",
                "educ_avg", 
                "cpi", 
                "rank_avg", 
                "retraction_probability", 
                "retractions_num_author",
                "g_score", 
                "j_score", 
                "num_references",
                "data_statement",
                "open_reg",
                "power_ratio",
                "rep_pred",
              )
          ) %>% 
          arrange(var) %>% 
          mutate(
            var = case_match(
              var,
              "num_countries" ~ "International collaboration",
              "developed" ~ "Developed*",
              "unemployment" ~ "Unemployment rate*",
              "num_countries_western" ~ "Western collaboration",
              "democ_avg" ~ "Democracy index*",
              "indust_avg" ~ "Industrialization index*",
              "educ_avg" ~ "Education index*",
              "cpi" ~ "Consumer price index*",
              "rank_avg" ~ "GDP rank*",
              "retraction_probability" ~ "Retraction frequency (genre)",
              "retractions_num_author" ~ "Retraction frequency (author)",
              "g_score" ~ "Topic popularity",
              "j_score" ~ "References' h-indices",
              "num_references" ~ "Number of references",
              "data_statement" ~ "Open data statement",
              "open_reg" ~ "Registration identified",
              "power_ratio" ~ "Sample size to 80% power (ratio)",
              "rep_pred" ~ "Replicability prediction**"
            )
          ) %>% 
          select(var, melb, kw, ts, u, p, a)
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S24 
      {
        table_name <- "table_s24"
        
        all_indicators <- melbourne %>% select(claim_id, melb = pc1) %>% 
          left_join(keyw %>% select(claim_id, kw = pc1), by = "claim_id") %>% 
          left_join(twosix %>% select(claim_id, ts = pc1), by = "claim_id") %>% 
          left_join(usc %>% select(claim_id, u = pc1), by = "claim_id") %>% 
          left_join(psu %>% select(claim_id, p = pc1), by = "claim_id") %>% 
          left_join(texas %>% select(claim_id, a = pc1), by = "claim_id") %>% 
          left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
          mutate(across(-claim_id, function(x) (x*1)))
        
        correlates <- extracted_claims %>% 
          filter(phase != "covid") %>% 
          select(paper_id) %>% distinct() %>% 
          left_join(statcheck, by = "paper_id") %>% 
          left_join(sciscore %>% select(paper_id, ss), by = "paper_id") %>% 
          left_join(es %>% select(-source), by = "paper_id") %>% 
          left_join(sig %>% select(paper_id, p), by = "paper_id") %>% 
          left_join(hedge, by = "paper_id") %>% 
          left_join(stevens_misc, by = "paper_id") %>% 
          left_join(oa_citations, by = "paper_id") %>% 
          mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))) %>% 
          select(claim_id, everything(), -paper_id)
        
        eigen_vals <- all_indicators %>% 
          rename(psu = p) %>% 
          full_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          full_join(robust %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))), by = "claim_id") %>% 
          full_join(process %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))), by = "claim_id") %>% 
          select(-contains("paper_id")) %>% 
          left_join(
            repli_binary %>% 
              left_join(repli_outcomes %>% select(claim_id, report_id), by = "report_id") %>% 
              select(-report_id) %>% 
              mutate(across(where(is.logical), function(x) x*1)) %>% 
              rename_with(., function(x) str_remove_all(x, "repli_binary_"), everything()),
            by = "claim_id"
          ) %>% 
          left_join(correlates, by = "claim_id") %>% 
          select(-claim_id, -contains("paper_id")) %>% 
          cor(use = "pairwise.complete.obs") %>% 
          eigen() %>% 
          pluck("values")
        
        fa_results <- all_indicators %>% 
          rename(psu = p) %>% 
          full_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          full_join(robust %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))), by = "claim_id") %>% 
          full_join(process %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))), by = "claim_id") %>% 
          select(-contains("paper_id")) %>% 
          left_join(
            repli_binary %>% 
              left_join(repli_outcomes %>% select(claim_id, report_id), by = "report_id") %>% 
              select(-report_id) %>% 
              mutate(across(where(is.logical), function(x) x*1)) %>% 
              rename_with(., function(x) str_remove_all(x, "repli_binary_"), everything()),
            by = "claim_id"
          ) %>% 
          left_join(correlates, by = "claim_id") %>% 
          select(-claim_id, -contains("paper_id")) %>% 
          cor(use = "pairwise.complete.obs") %>% 
          fa(r = ., nfactors = 6, rotate = "oblimin", fm = "pa", n.obs = 6970)
        
        table <- fa_results$Vaccounted %>% 
          as.data.frame() %>% 
          rownames_to_column() %>% 
          slice(-1) %>% 
          mutate(across(c(where(is.numeric)), function(x) formatC(x, 2, format = "f", digits = 2) %>% 
                          as.character() %>% str_replace_all(., "^0\\.", "."))) %>% 
          mutate(
            rowname = case_match(
              rowname,
              "Proportion Var" ~ "Variance explained per factor (pre-rotation)",
              "Cumulative Var" ~ "Cumulative variance explained (pre-rotation)",
              "Proportion Explained" ~ "Variance explained per factor (post-rotation)",
              "Cumulative Proportion" ~ "Cumulative variance explained (post-rotation)"
            )
          ) %>% 
          rename(" " = rowname)
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
      }
      
      # Table S25
      {
        table_name <- "table_s25"
        
        table <- fa_results$loadings %>% 
          unclass() %>% 
          as.data.frame() %>% 
          rownames_to_column("item") %>% 
          as_tibble() %>% 
          mutate(across(-item, function(x) ifelse(abs(x) < .3, NA, x %>% formatC(., format = "f", digits = 2) %>% str_replace_all(., "^0\\.", ".")))) %>% 
          select(item, PA1, PA2, PA3, PA4, PA5, PA6) %>% 
          arrange(desc(PA1), desc(PA2), desc(PA3), desc(PA4), desc(PA5), desc(PA6)) %>% 
          mutate(
            item = case_match(
              item,
              "analyst" ~ "Analyst interpretation",
              "score" ~ "Replicability",
              "sum_p" ~ "Sum of p-values",
              "skep_p" ~ "Skeptical p-value",
              "orig_wthn" ~ "Orig. in rep. CI",
              "rep_wthn" ~ "Rep. in orig. CI",
              "wthn_pi" ~ "Rep. in prediction interval",
              "meta_success" ~ "Meta-analysis",
              "bma_result" ~ "Bayesian meta-analysis",
              "telescopes" ~ "Small telescopes",
              "bf_result" ~ "Bayes factor",
              "bayes_rep" ~ "Replication Bayes factor",
              "correspondence" ~ "Correspondence test",
              "reproduced" ~ "Outcome reproducibility",
              "pr" ~ "Process reproducibility",
              "u" ~ "MACROSCORE",
              "a" ~ "Synthetic markets B",
              "supported" ~ "Robustness",
              "ts" ~ "TwoSix",
              "kw" ~ "Prediction markets",
              "melb" ~ "Structured elicitations",
              "psu" ~ "Synthetic markets A",
              "sc" ~ "StatCheck",
              "ss" ~ "SciScore",
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
          rename(" " = item) %>% 
          mutate(across(everything(), function(x) ifelse(is.na(x), "--", x)))
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S26
      {
        table_name <- "table_s26"
        
        fa_results10 <- all_indicators %>% 
          rename(psu = p) %>% 
          full_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          full_join(robust %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))), by = "claim_id") %>% 
          full_join(process %>% mutate(claim_id = select(., paper_id) %>% apply(1, function(x) str_c(x, "_single-trace", collapse = ""))), by = "claim_id") %>% 
          select(-contains("paper_id")) %>% 
          left_join(
            repli_binary %>% 
              left_join(repli_outcomes %>% select(claim_id, report_id), by = "report_id") %>% 
              select(-report_id) %>% 
              mutate(across(where(is.logical), function(x) x*1)) %>% 
              rename_with(., function(x) str_remove_all(x, "repli_binary_"), everything()),
            by = "claim_id"
          ) %>% 
          left_join(correlates, by = "claim_id") %>% 
          select(-claim_id, -contains("paper_id")) %>% 
          cor(use = "pairwise.complete.obs") %>% 
          fa(r = ., nfactors = 10, rotate = "oblimin", fm = "pa", n.obs = 6970)
        
        table <- fa_results10$Vaccounted %>% 
          as.data.frame() %>% 
          rownames_to_column() %>% 
          slice(-1) %>% 
          mutate(across(c(where(is.numeric)), function(x) formatC(x, 2, format = "f", digits = 2) %>% 
                          as.character() %>% str_replace_all(., "^0\\.", "."))) %>% 
          mutate(
            rowname = case_match(
              rowname,
              "Proportion Var" ~ "Variance explained per factor (pre-rotation)",
              "Cumulative Var" ~ "Cumulative variance explained (pre-rotation)",
              "Proportion Explained" ~ "Variance explained per factor (post-rotation)",
              "Cumulative Proportion" ~ "Cumulative variance explained (post-rotation)"
            )
          ) %>% 
          rename(" " = rowname) %>% 
          as_tibble()
        
        # Assign an object of the name of the table for later extraction
        assign(table_name,table)
        
        # Generate an object for each cell of the table for tagging in the termplate
        for (row in 1:nrow(get(table_name))){ for (col in 1:ncol(get(table_name))){
          assign(paste0(table_name,"_",row,"_",col),as.character(get(table_name)[row,col]))}}
        
      }
      
      # Table S27
      {
        table_name <- "table_s27"
        
        table <- fa_results10$loadings %>% 
          unclass() %>% 
          as.data.frame() %>% 
          rownames_to_column("item") %>% 
          as_tibble() %>% 
          mutate(across(-item, function(x) ifelse(abs(x) < .3, NA, x %>% formatC(., format = "f", digits = 2) %>% str_replace_all(., "^0\\.", ".")))) %>% 
          select(item, PA1, PA2, PA3, PA4, PA5, PA6, PA7, PA8, PA9, PA10) %>% 
          arrange(desc(PA1), desc(PA2), desc(PA3), desc(PA4), desc(PA5), desc(PA6),
                  desc(PA7), desc(PA8), desc(PA9), desc(PA10)) %>% 
          mutate(
            item = case_match(
              item,
              "analyst" ~ "Analyst interpretation",
              "score" ~ "Replicability",
              "sum_p" ~ "Sum of p-values",
              "skep_p" ~ "Skeptical p-value",
              "orig_wthn" ~ "Orig. in rep. CI",
              "rep_wthn" ~ "Rep. in orig. CI",
              "wthn_pi" ~ "Rep. in prediction interval",
              "meta_success" ~ "Meta-analysis",
              "bma_result" ~ "Bayesian meta-analysis",
              "telescopes" ~ "Small telescopes",
              "bf_result" ~ "Bayes factor",
              "bayes_rep" ~ "Replication Bayes factor",
              "correspondence" ~ "Correspondence test",
              "reproduced" ~ "Outcome reproducibility",
              "pr" ~ "Process reproducibility",
              "u" ~ "MACROSCORE",
              "a" ~ "Synthetic markets B",
              "supported" ~ "Robustness",
              "ts" ~ "TwoSix",
              "kw" ~ "Prediction markets",
              "melb" ~ "Structured elicitations",
              "psu" ~ "Synthetic markets A",
              "sc" ~ "StatCheck",
              "ss" ~ "SciScore",
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
          rename(" " = item) %>% 
          mutate(across(everything(), function(x) ifelse(is.na(x), "--", x)))
        
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
        empirical <- replicability %>% select(-paper_id) %>% 
          full_join(outcome %>% select(-paper_id), by = "claim_id") %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
          select(-claim_id) %>% 
          full_join(robust, by = "paper_id") %>% 
          full_join(process, by = "paper_id") %>% 
          mutate(across(-paper_id, ~.*1)) %>% 
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup()
        
        repli_repro <- empirical %>% 
          filter(complete.cases(score, reproduced, weight)) %>% 
          dplyr::summarize(corr = weightedCorr(score, reproduced, method = "Pearson", weights = weight)) %>% 
          pull(corr)
        
        repli_robust <- empirical %>% 
          filter(complete.cases(score, supported, weight)) %>% 
          dplyr::summarize(corr = weightedCorr(score, supported, method = "Pearson", weights = weight)) %>% 
          pull(corr)
        
        repli_process <- empirical %>% 
          filter(complete.cases(score, pr, weight)) %>% 
          dplyr::summarize(corr = weightedCorr(score, pr, method = "Pearson", weights = weight)) %>% 
          pull(corr)
        
        repro_robust <- empirical %>% 
          filter(complete.cases(reproduced, supported, weight)) %>% 
          dplyr::summarize(corr = weightedCorr(reproduced, supported, method = "Pearson", weights = weight)) %>% 
          pull(corr)
        
        repro_process <- empirical %>% 
          filter(complete.cases(reproduced, pr, weight)) %>% 
          dplyr::summarize(corr = weightedCorr(reproduced, pr, method = "Pearson", weights = weight)) %>% 
          pull(corr)
        
        robust_process <- empirical %>% 
          filter(complete.cases(supported, pr, weight)) %>% 
          dplyr::summarize(corr = weightedCorr(supported, pr, method = "Pearson", weights = weight)) %>% 
          pull(corr)
        
        emp_corr_med <- c(repli_repro, repli_robust, repli_process, repro_robust, repro_process, robust_process) %>% median() %>% round(2)
        emp_corr_low <- c(repli_repro, repli_robust, repli_process, repro_robust, repro_process, robust_process) %>% min() %>% round(2)
        emp_corr_high <- c(repli_repro, repli_robust, repli_process, repro_robust, repro_process, robust_process) %>% max() %>% round(2)
        
        ta_indicators <- melbourne %>% select(claim_id, melb = pc1) %>% 
          left_join(keyw %>% select(claim_id, kw = pc1), by = "claim_id") %>% 
          left_join(twosix %>% select(claim_id, ts = pc1), by = "claim_id") %>% 
          left_join(usc %>% select(claim_id, u = pc1), by = "claim_id") %>% 
          left_join(psu %>% select(claim_id, p = pc1), by = "claim_id") %>% 
          left_join(texas %>% select(claim_id, a = pc1), by = "claim_id") %>% 
          left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
          mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
          mutate(across(-c(claim_id, paper_id), function(x) (x*1))) %>% 
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup() %>% 
          select(-claim_id) %>% 
          select(paper_id, weight, everything()) %>% 
          pivot_longer(cols = -c(paper_id, weight), names_to = "measure", values_to = "value") %>% 
          drop_na()
        
        pred_corr_med <- ta_indicators %>% 
          inner_join(ta_indicators, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          median() %>% 
          round(2)
        
        pred_corr_low <- ta_indicators %>% 
          inner_join(ta_indicators, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          min() %>% 
          round(2)
        
        pred_corr_high <- ta_indicators %>% 
          inner_join(ta_indicators, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          max() %>% 
          round(2)
        
        t4 <- tab4_data %>% 
          left_join(
            replicability %>% full_join(outcome %>% select(-paper_id), by = "claim_id"),
            by = "paper_id"
          ) %>% 
          select(-claim_id) %>% 
          group_by(paper_id) %>% 
          mutate(weight = 1/n()) %>% 
          ungroup() %>% 
          pivot_longer(cols = -c(paper_id, weight), names_to = "measure", values_to = "value") %>% 
          drop_na()
        
        de_corr_med <- t4 %>% 
          inner_join(t4, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          mutate(corr = abs(corr)) %>% 
          pull(corr) %>% 
          median() %>% 
          round(2)
        
        de_corr_low <- t4 %>% 
          inner_join(t4, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          mutate(corr = abs(corr)) %>% 
          pull(corr) %>% 
          min() %>% 
          round(2)
        
        de_corr_high <- t4 %>% 
          inner_join(t4, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          mutate(corr = abs(corr)) %>% 
          pull(corr) %>% 
          max() %>% 
          round(2)
        
        # t5 <- tab5_data %>% 
        #   full_join(all_indicators, by = "paper_id") %>% 
        #   select(-claim_id) %>% 
        #   group_by(paper_id) %>% 
        #   mutate(weight = 1/n()) %>% 
        #   ungroup() %>% 
        #   pivot_longer(cols = -c(paper_id, weight), names_to = "var", values_to = "value") %>% 
        #   drop_na()
        # 
        # de_pred_corr_med <- t5 %>% 
        #   inner_join(t5, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
        #   filter(var.x < var.y) %>% 
        #   group_by(var.x, var.y) %>%
        #   dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
        #   mutate(corr = abs(corr)) %>% 
        #   pull(corr) %>% 
        #   median() %>% 
        #   round(2)
        # 
        # de_pred_corr_low <- t5 %>% 
        #   inner_join(t5, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
        #   filter(var.x < var.y) %>% 
        #   group_by(var.x, var.y) %>%
        #   dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
        #   mutate(corr = abs(corr)) %>% 
        #   pull(corr) %>% 
        #   min() %>% 
        #   round(2)
        # 
        # de_pred_corr_high <- t5 %>% 
        #   inner_join(t5, by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
        #   filter(var.x < var.y) %>% 
        #   group_by(var.x, var.y) %>%
        #   dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
        #   mutate(corr = abs(corr)) %>% 
        #   pull(corr) %>% 
        #   max() %>% 
        #   round(2)
      
      }
      
      
      # Intro
      {
        st_papers <- status %>% filter(p1_delivery | p2_delivery) %>% nrow()
        n_journals <- publications %>% nrow()
        p1_papers <- status %>% filter(p1_delivery) %>% nrow()
        p2_papers <- status %>% filter(p2_delivery) %>% nrow()
        rr_papers <- status %>% filter(RR) %>% nrow()
        bushel_papers <- status %>% filter(bushel) %>% nrow()
        all_papers <- status %>% nrow()
        pool_papers <- status %>% filter(pool) %>% nrow()
        all_claims <- extracted_claims %>% nrow()
        prepool_papers <- status %>% filter(prepool) %>% nrow()
        
      }
      # Results
      {
        
        ta3_corr_med <- ta_indicators %>% 
          filter(measure %in% c("a", "p", "ts", "u")) %>% 
          inner_join(ta_indicators %>% filter(measure %in% c("a", "p", "ts", "u")), by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          median() %>% 
          round(2)
        
        ta3_corr_low <- ta_indicators %>% 
          filter(measure %in% c("a", "p", "ts", "u")) %>% 
          inner_join(ta_indicators %>% filter(measure %in% c("a", "p", "ts", "u")), by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          min() %>% 
          round(2)
        
        ta3_corr_high <- ta_indicators %>% 
          filter(measure %in% c("a", "p", "ts", "u")) %>% 
          inner_join(ta_indicators %>% filter(measure %in% c("a", "p", "ts", "u")), by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          filter(measure.x < measure.y) %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          max() %>% 
          round(2)
        
        ta23_corr_med <- ta_indicators %>% 
          filter(measure %in% c("a", "p", "ts", "u")) %>% 
          left_join(ta_indicators %>% filter(measure %in% c("kw", "melb")), by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          median() %>% 
          round(2)
          
        ta23_corr_low <- ta_indicators %>% 
          filter(measure %in% c("a", "p", "ts", "u")) %>% 
          left_join(ta_indicators %>% filter(measure %in% c("kw", "melb")), by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          min() %>% 
          round(2)
        
        ta23_corr_high <- ta_indicators %>% 
          filter(measure %in% c("a", "p", "ts", "u")) %>% 
          left_join(ta_indicators %>% filter(measure %in% c("kw", "melb")), by = c("paper_id", "weight"), relationship = "many-to-many") %>% 
          group_by(measure.x, measure.y) %>%
          dplyr::summarize(corr = weightedCorr(value.x, value.y, method = "Pearson", weights = weight), .groups = "drop") %>% 
          pull(corr) %>% 
          max() %>% 
          round(2)
        
      }
    }
  }
  
  # Moderator
  {
    mod_data <- melbourne %>% select(claim_id, melb = pc1) %>% 
      left_join(keyw %>% select(claim_id, kw = pc1), by = "claim_id") %>% 
      left_join(twosix %>% select(claim_id, ts = pc1), by = "claim_id") %>% 
      left_join(usc %>% select(claim_id, u = pc1), by = "claim_id") %>% 
      left_join(psu %>% select(claim_id, p = pc1), by = "claim_id") %>% 
      left_join(texas %>% select(claim_id, a = pc1), by = "claim_id") %>% 
      left_join(replicability %>% select(claim_id, score), by = "claim_id") %>% 
      mutate(across(-claim_id, function(x) (x*1))) %>% 
      mutate(paper_id = select(., claim_id) %>% apply(1, function(x) str_split(x, "_") %>% unlist() %>% first())) %>% 
      group_by(paper_id) %>% 
      mutate(weight = 1/n()) %>% 
      ungroup() %>% 
      left_join(paper_metadata %>% select(paper_id, field = COS_pub_category), by = "paper_id")
    
    mod_data %>% 
      filter(field == "business" | field == "psychology and health") %>% 
      select(-field, -claim_id) %>% 
      select(paper_id, weight, everything()) %>% 
      pivot_longer(cols = -c(paper_id, weight, score), names_to = "measure", values_to = "value") %>% 
      drop_na() %>% 
      group_by(measure) %>% 
      dplyr::summarize(corr = weightedCorr(score, value, method = "Pearson", weights = weight), .groups = "drop") %>% 
      pull(corr) %>% max() %>% round(2)
    
    mod_data %>% 
      filter(field != "business" & field != "psychology and health") %>% 
      select(-field, -claim_id) %>% 
      select(paper_id, weight, everything()) %>% 
      pivot_longer(cols = -c(paper_id, weight, score), names_to = "measure", values_to = "value") %>% 
      drop_na() %>% 
      group_by(measure) %>% 
      dplyr::summarize(corr = weightedCorr(score, value, method = "Pearson", weights = weight), .groups = "drop") %>% 
      pull(corr) %>% max() %>% round(2)

  }
  
  # Export everything in environment
  {
    return(rev(as.list(environment())))
  }
  
}
