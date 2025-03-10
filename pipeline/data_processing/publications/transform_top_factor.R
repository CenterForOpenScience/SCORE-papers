transform_top_factor <- function(top_factor_raw) {
  
  top_factor_raw %>%
    rename(journal = Journal,
           top_factor = `Top Factor`)
  
}