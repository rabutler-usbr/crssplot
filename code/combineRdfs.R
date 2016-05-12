# script to create combined rdf files for a set of scenarios
source('code/makeScenNames.R')
source('code/combineRdfHelpers.R')

# ----- USER INPUT -------
scens <- makeAllScenNames('Apr2015_2016_a3','DNF','IGa3',1981:1983)
rdfs <- c('KeySlots.rdf','MPPE.rdf','SystemConditions.rdf')
sPath <- 'C:/alan/CRSS/CRSS.2016/Scenario'
batchPath <- sPath
# ----- END USER INPUT -------

# rename the folders so there are no commas
fNames <- fRename_noComma(scens, fPath = sPath, repWith = '-')

# call the combiner for all rdfs
# callCombinerByRdf <- function(rdf, fNames, batchDir)
lapply(as.list(rdfs), callCombinerByRdf, fNames = fNames[,2], batchDir = batchPath)

# rename the folders back to their original name (new to orignal)
file.rename(fNames[,2],fNames[,1])
