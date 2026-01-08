extend_rr_projects <- function(rr_projects_raw,
                               repli_cases_projects) {
  
  rr_projects_raw %>%
    rbind(repli_cases_projects)
  
}