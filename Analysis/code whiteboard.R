# Startup and initialization
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  
}

# Update app data
if (FALSE){
  
  library(targets)
  tar_make() # run this to pull new targets
  
  tar_load("repli_outcomes")
  save(repli_outcomes,file="Analysis/Data exploration app/repli_outcomes.RData")

  tar_load("orig_outcomes")
  save(orig_outcomes,file="Analysis/Data exploration app/orig_outcomes.RData")
  
  tar_load("repro_outcomes")
  save(repro_outcomes,file="Analysis/Data exploration app/repro_outcomes.RData")
}

