
# prep data system condition table
# mmm = min/most/max
# if adding agg attribute, assumes aggFunction takes in the scenario name as 
# its only arguement 
getScenarioData <- function(scens, 
                            iFolder, 
                            oFile, 
                            addAggAttribute = TRUE, 
                            aggFunction, 
                            rwa)
{
  if (!RWDataPlyr::is_rwd_agg(rwa)) {
    stop("rwa should be a rwd_agg object")
  }
  
  # or 
  # first scens is the folder names to search, 
  # second is the names to save in the data file
  # use folder names for now
  
  # convert the scens list to a vector for processing all scenarios at once
  scensVec <- do.call(c,scens)
  names(scensVec) <- scensVec
  
  message('starting to process scenarios...')
  message(
    'this could take some time as you are processing ',
    length(scensVec),
    ' scenarios'
  )

  zz <- RWDataPlyr::rw_scen_aggregate(scensVec, agg = rwa, scen_dir = iFolder)
  
  if(addAggAttribute){
    message('adding attribute...')
    
    if(aggFunction == 'aggFromScenList'){
      zz <- dplyr::mutate(zz, ScenarioGroup = eval(call(aggFunction, Scenario, scens)))
    } else{
      zz <- dplyr::mutate(zz, ScenarioGroup = eval(call(aggFunction, Scenario)))
    }
    
    message('writing file...')
    feather::write_feather(zz, oFile)
  }
}

