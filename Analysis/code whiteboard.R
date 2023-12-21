# Startup and initialization
{
  rm(list=ls()) # yes I know this is bad, will get rid of later; just a convenience for now
  
}

# Update app data
if (FALSE){
  
  objects_to_load <- c("repli_outcomes","orig_outcomes","repro_outcomes")
  
  for(i in 1:length(objects_to_load)){
    assign(objects_to_load[i],readRDS(paste0("_targets/objects/",objects_to_load[i])))
    save(list=objects_to_load[i],file=paste0("Analysis/Data exploration app/",objects_to_load[i],".RData"))
  }
  

 
}

