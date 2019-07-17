# script to create combined rdf files for a set of scenarios
library(RWDataPlyr)
source("code/combineRdfHelpers.R")

# ----- USER INPUT -------
scens <- rw_scen_gen_names(
  'Jun2019_2020,ISM1988_2017,2007Dems,IG_DCP',
  paste0("Trace", 4:38),
  "DCP_Cons"
)
rdfs <- paste0(c('KeySlots','Flags','SystemConditions','OWDAnn','Res','xtraRes'),'.rdf')
#rdfs <- 'SystemConditions.rdf'
rdfs <- c("MWDICS_mon.rdf", "MWDICS.rdf")
sPath <- 'M:/Shared/CRSS/2019/Scenario/'

# path to the RiverWareBatchRdfCombiner.exe
batchPath <- "M:/Shared/CRSS/2019/results" 
# ----- END USER INPUT -------

# call the combiner for all rdfs
# callCombinerByRdf(rdf, fNames, batchDir)
lapply(
  as.list(rdfs), 
  callCombinerByRdf, 
  fNames = file.path(sPath, scens), 
  batchDir = batchPath
)
