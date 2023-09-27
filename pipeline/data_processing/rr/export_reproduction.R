# Export Reproductions

# # Create an attempts-level summary table, applies changelog updates
# update_p2_repro_input <- function(reproduction_qa) {
# 
#   attempts_df <- reproduction_qa %>% 
#     filter(!is.na(rr_num_claims_report)) %>%
#     # Keep the latest entry for each ticket only
#     arrange(desc(as.numeric(original_index))) %>%
#     distinct(asana_ticket_name,
#              .keep_all = TRUE) %>%
#     arrange(as.numeric(original_index))
# }