# create version of record power variables that combine the initial versions of Tilburg variables with the design versions

library(tidyverse)
library(targets)

tar_make()
tar_load(repli_outcomes)

# for each power threshold, create a 'combined' version of the power variable
# draws on the corresponding value from the design set when it's available
# draws on the value from the initial set of power variables when the design version is unavailable
combined_power_variables <- repli_outcomes %>% 
  mutate(
    combined_power_50 = ifelse(!is.na(rr_power_50_original_effect_design), rr_power_50_original_effect_design, repli_power_for_50_effect),
    combined_power_75 = ifelse(!is.na(rr_power_75_original_effect_design), rr_power_75_original_effect_design, repli_power_for_75_effect),
    combined_power_100 = ifelse(!is.na(rr_power_100_original_effect_design), rr_power_100_original_effect_design, rr_power_100_original_effect),
  ) %>% 
  select(report_id, contains("combined_"))
