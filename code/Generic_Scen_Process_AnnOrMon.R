# ##############################################################################
# #This script processes annual and monthly rdf files to compare two CRSS runs
# 
# #DEVELOPMENT IS ON GOING ON THIS
# 
#   Created by C. Felletter 8/2018
#   Updated by CF on 10/2018 to be a function
# ##############################################################################


generic.scen.process <- function(scen_dir,scens,timestep) { 

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. Set Up ##
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #Additional plotting functions and libraries 
  library('tidyverse') #ggplot2,dplyr,tidyr
  library('devtools')
  library(RWDataPlyr)
  source('code/Stat_emp_ExcCrv.r')
  source('code/stat-boxplot-custom.r')
  
  # some sanity checks that UI is correct:
  if(!(mainScenGroup %in% names(scens))) 
    stop(mainScenGroup, ' is not found in scens.')
  
  # check folders
  if(!file.exists(file.path(scen_dir, scens[1])) 
     | !file.exists(file.path(scen_dir, scens[2])))
    stop('scens folder(s) do not exist or scen_dir is set up incorrectly. 
         Please ensure scens is set correctly.')
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3. Process Results 
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #check slots in rdf
  if(!any(rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffile))) 
         # !any() checks if none in the vector are true 
          %in% variable)){ # %in% checks if variable is in the character vector
    stop(paste('Slot ',variable,' does not exist in given rdf'))
  } 
    
  if (floworpe == "pe"){
    if (cyorwy == "cy"){
      peperiod = "eocy"
    } else {
      peperiod = "eowy"
    }
  }
  
  #generic agg file 
  rwa1 <- rwd_agg(data.frame(
    file = rdffile,
    slot = variable, 
    period = 
      if(timestep == "annual"){
        if (floworpe == "flow"){
          cyorwy
        } else {peperiod}
      } else {"asis"}
      ,  
    summary = 
      if(timestep == "annual"){
        if (floworpe == "flow"){
          "sum"
        } else {NA}
      } else {NA}
      ,
    eval = NA,
    t_s = NA,
    variable = variable,
    stringsAsFactors = FALSE
  ))
  
  message(paste("Processing ",timestep," ",variable," in ",names(scens[1])," & ",names(scens[2])))
  
  #rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
  scen_res <- rw_scen_aggregate(
    scens,
    agg = rwa1,
    scen_dir = scen_dir
  ) 
  
  # unique(scen_res$Variable) #check variable names
  # unique(scen_res$TraceNumber) #check trace numbers 
  
  if (timestep == "monthly"){
    #get everything on a date 
    scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
    #get a numeric month number
    scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))
  }
  
  return(scen_res)

} #end function
