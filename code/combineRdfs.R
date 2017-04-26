# script to create combined rdf files for a set of scenarios
source('code/makeScenNames.R')
source('code/combineRdfHelpers.R')

# ----- USER INPUT -------
scens <- makeAllScenNames('Apr2017_2018','DNF','2007Dems','IG',1981:2015)
rdfs <- paste0(c('KeySlots','Flags','SystemConditions','OWDAnn','Res','xtraRes'),'.rdf')
#rdfs <- 'SystemConditions.rdf'
sPath <- 'M:/Shared/CRSS/2017/Scenario/'
batchPath <- file.path(sPath, 'Apr2017_2018,DNF,2007Dems,IG') # path to the RiverWareBatchRdfCombiner.exe
# ----- END USER INPUT -------

# call the combiner for all rdfs
# callCombinerByRdf(rdf, fNames, batchDir)
lapply(as.list(rdfs), callCombinerByRdf, fNames = file.path(sPath,scens), batchDir = batchPath)
