# script to create combined rdf files for a set of scenarios
source('code/makeScenNames.R')
source('code/combineRdfHelpers.R')

# ----- USER INPUT -------
scens <- makeAllScenNames('Apr2016_2017','DNF','2007Dems','IG',1981:2010)
#rdfs <- paste0(c('KeySlots','Flags','SystemConditions','OWDAnn','Res','xtraRes'),'.rdf')
rdfs <- 'MPPE.rdf'
sPath <- 'C:/model/CRSS/CRSS.2016/Scenario/'
batchPath <- sPath
# ----- END USER INPUT -------

# rename the folders so there are no commas
fNames <- fRename_noComma(scens, fPath = sPath, repWith = '-')

# call the combiner for all rdfs
# callCombinerByRdf <- function(rdf, fNames, batchDir)
lapply(as.list(rdfs), callCombinerByRdf, fNames = fNames[,2], batchDir = batchPath)

# rename the folders back to their original name (new to orignal)
file.rename(fNames[,2],fNames[,1])

# the following attempted to remedy the out of memory error by breaking the combination up into two part
# but did not work. 
if(FALSE){
  ### running out of memory on KeySlots, Flags, and xtraRes, so try and split into two and then do it
  ### again; each line was manually run, and then files moved so they would not be overwritten
  
  scens <- makeAllScenNames('Apr2016_2017','DNF','2007Dems','IG',1981:1990)
  rdfs <- paste0(c('KeySlots','Flags','xtraRes'),'.rdf')
  fNames <- fRename_noComma(scens, fPath = sPath, repWith = '-')
  
  # call the combiner for all rdfs
  # callCombinerByRdf <- function(rdf, fNames, batchDir)
  lapply(as.list(rdfs), callCombinerByRdf, fNames = fNames[,2], batchDir = batchPath)
  
  # rename the folders back to their original name (new to orignal)
  file.rename(fNames[,2],fNames[,1])
  
  ### *** Now move the files that were created 
  # do again for next set
  scens <- makeAllScenNames('Apr2016_2017','DNF','2007Dems','IG',1991:2000)
  rdfs <- paste0(c('KeySlots','Flags','xtraRes'),'.rdf')
  fNames <- fRename_noComma(scens, fPath = sPath, repWith = '-')
  lapply(as.list(rdfs), callCombinerByRdf, fNames = fNames[,2], batchDir = batchPath)
  file.rename(fNames[,2],fNames[,1])
  
  ### *** Now move the files that were created 
  # do again for next set
  scens <- makeAllScenNames('Apr2016_2017','DNF','2007Dems','IG',2001:2010)
  rdfs <- paste0(c('KeySlots','Flags','xtraRes'),'.rdf')
  fNames <- fRename_noComma(scens, fPath = sPath, repWith = '-')
  lapply(as.list(rdfs), callCombinerByRdf, fNames = fNames[,2], batchDir = batchPath)
  file.rename(fNames[,2],fNames[,1])
  
  scens <- paste0('temp',1:3)
  fNames <- file.path(sPath,scens)
  rdfs <- paste0(c('KeySlots','Flags'),'.rdf')
  lapply(as.list(rdfs), callCombinerByRdf, fNames = fNames, batchDir = batchPath)

}
