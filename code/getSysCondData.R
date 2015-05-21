library(RWDataPlot)
library(dplyr)
# prep data system condition table
# mmm = min/most/max
# if adding agg attribute, assumes aggFunction takes in the scenario name as its only
# arguement 
getSysCondData <- function(scens, iFolder, oFile, addAggAttribute = TRUE, aggFunction)
{
  slotAggList <- RWDataPlot::createSlotAggList('data/SysCond.csv')
  # first scens is the folder names to search, second is the names to save in the data file
  # use folder names for now
  print('starting to process scenarios...')
  print(paste('this could take some time as you are processing',length(scens),'scenarios'))
  flush.console()
  
  RWDataPlot::getDataForAllScens(scens,scens,slotAggList, iFolder, oFile)
  
  if(addAggAttribute){
    print('now reading the file in...')
    flush.console()
    zz <- read.table(oFile,header=T)
    
    print('adding attribute...')
    flush.console()
    
    zz <- dplyr::mutate(zz, Agg = aggFunction(Scenario))
      
    print('rewriting file...')
    flush.console()

    write.table(zz, oFile)
  }
}


getSritStatsData <- function(scens, iFolder, oFile, addAggAttribute = TRUE, aggFunction)
{
  slotAggList <- RWDataPlot::createSlotAggList('data/CritStatsList.csv')
  # first scens is the folder names to search, second is the names to save in the data file
  # use folder names for now
  print('starting to process scenarios...')
  print(paste('this could take some time as you are processing',length(scens),'scenarios'))
  flush.console()
  
  RWDataPlot::getDataForAllScens(scens,scens,slotAggList, iFolder, oFile)
  
  if(addAggAttribute){
    print('now reading the file in...')
    flush.console()
    zz <- read.table(oFile,header=T)
    
    print('adding attribute...')
    flush.console()
    
    zz <- dplyr::mutate(zz, Agg = aggFunction(Scenario))
    
    print('rewriting file...')
    flush.console()
    
    write.table(zz, oFile)
  }
}
