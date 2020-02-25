# script to create combined rdf files for a set of scenarios
library(RWDataPlyr)
source("code/combineRdfHelpers.R")

# ----- USER INPUT -------
scens <- rw_scen_gen_names(
  "Feb2020_2021,ISM1988_2018,2007Dems,IG_DCP",
  paste0("Trace", 4:38)
)
rdfs <- paste0(c('KeySlots','Flags','SystemConditions','OWDAnn','Res','xtraRes', 
                 "CRSPPowerData", "LBEnergy", "LBDCP", "UBDO"),'.rdf')

sPath <- 'M:/Shared/CRSS/2020/Scenario/'

# path to the RiverWareBatchRdfCombiner.exe
batchPath <- "M:/Shared/CRSS/2020/results" 
# ----- END USER INPUT -------

# call the combiner for all rdfs
# callCombinerByRdf(rdf, fNames, batchDir)
lapply(
  as.list(rdfs), 
  callCombinerByRdf, 
  fNames = file.path(sPath, scens), 
  batchDir = batchPath
)
