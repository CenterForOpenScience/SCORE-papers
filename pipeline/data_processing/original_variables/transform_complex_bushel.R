# Transform complex bushel
transform_complex_bushel <- function(complex_bushel) {
  
  complex_bushel %>%
  mutate(unique_claim_id = str_c(paper_id, "_", claim_id),
         complex = as.logical(complex)) %>%
        select(unique_claim_id,
               bushel_complex = complex)
}
