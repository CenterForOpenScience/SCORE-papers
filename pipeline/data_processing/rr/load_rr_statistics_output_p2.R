# Load rr_statistics_output_p2.txt
# Downloads file from OSF if it has been updated, stores the output, and 
# deletes the temp downloaded file
load_rr_statistics_output_p2 <- function(
    rr_statistics_output_p2_osf,
    rr_statistics_output_p2_mod_date
) {
  
  # This triggers tar_make() to load data if the modification date has
  # changed since last run
  if(!is.Date(as.Date(rr_statistics_output_p2_mod_date))){
    stop(simpleError("File modification date invalid."))
  }
  
  rr_statistics_output_p2_file <- rr_statistics_output_p2_osf %>%
    osf_retrieve_file() %>%
    osf_download(path = here("pipeline",
                             "data_processing",
                             "temp"),
                 conflicts = "overwrite") %>%
    pull(local_path)
  
  rr_statistics_output_p2 <- read_tsv(rr_statistics_output_p2_file,
                                      show_col_types = FALSE)
  
  file.remove(rr_statistics_output_p2_file)
  
  rr_statistics_output_p2 %>%
    select(unique_report_id,
           rr_effect_size_type_reference = rr_effect_size_type_statsteam,
           rr_effect_size_value_reference = rr_effect_size_value_statsteam,
           rr_es_ub_ci_nativeunits,
           rr_es_lb_ci_nativeunits,
           rr_pearsons_r_defined,
           rr_pearsons_r_value,
           rr_es_ub_ci_pearson,
           rr_es_lb_ci_pearson,
           pi_ub_nativeunits = comparison_es_ub_pi_nativeunits,
           pi_lb_nativeunits = comparison_es_lb_pi_nativeunits,
           pi_ub_pearson = comparison_es_ub_pi_pearson,
           pi_lb_pearson = comparison_es_lb_pi_pearson,
           rr_power_small,
           rr_power_medium,
           rr_power_50_original_effect,
           rr_power_75_original_effect,
           rr_power_100_original_effect,
           rr_power_50_original_effect_alpha_.025,
           rr_power_75_original_effect_alpha_.025,
           rr_power_100_original_effect_alpha_.025,
           rr_power_50_original_effect_design,
           rr_power_75_original_effect_design,
           rr_power_100_original_effect_design,
           rr_power_50_original_effect_alpha_.025_design,
           rr_power_75_original_effect_alpha_.025_design,
           rr_power_100_original_effect_alpha_.025_design)
}
