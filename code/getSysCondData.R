library(RWDataPlot)
library(dplyr)
# prep data system condition table
# mmm = min/most/max
# if adding agg attribute, assumes aggFunction takes in the scenario name as its only
# arguement 
getScenarioData <- function(scens, iFolder, oFile, addAggAttribute = TRUE, aggFunction, slaInput)
{
  slotAggList <- RWDataPlot::createSlotAggList(slaInput) # or 
  # first scens is the folder names to search, second is the names to save in the data file
  # use folder names for now
  message('starting to process scenarios...')
  message(paste('this could take some time as you are processing',length(scens),'scenarios'))
  
  # convert the scens list to a vector for processing all scenarios at once
  scensVec <- do.call(c,scens)
  
  zz <- RWDataPlot::getDataForAllScens(scensVec,scensVec,slotAggList, iFolder, oFile, TRUE)

  if(addAggAttribute){
    message('adding attribute...')
    
    if(aggFunction == 'aggFromScenList'){
      zz <- dplyr::mutate(zz, Agg = eval(call(aggFunction, Scenario, scens)))
    } else{
      zz <- dplyr::mutate(zz, Agg = eval(call(aggFunction, Scenario)))
    }
    
    message('rewriting file...')
    write_feather(zz, oFile)
  }
}

