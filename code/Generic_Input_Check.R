# ##############################################################################
# #This script is some sanity checks that UI is correct
# 
# #DEVELOPMENT IS ON GOING ON THIS
# 
#   Created by C. Felletter 10/2018
# ##############################################################################


generic.input.check <- function(scen_dir,scens,timestep) { 

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. Set Up ##
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # ## Examples ##
  # mainScenGroup <- "Scoopity Woop" 
  # rdffile <<- "FakeNews.rdf"
  # variable <<- "Poopity.Scoop"
  # floworpe <<- "MAGA"
  # cyorwy <<- ""
  # figuretype <<- "tripolar"
  # exc_month <<- 12345
  # startyr <<- 2099 
  # endyr <<- 1922
  # customcaption <<- 1
  # custom_y_lab <<- 2
  
  # some sanity checks that UI is correct:
  if(!(mainScenGroup %in% names(scens))) 
    stop(mainScenGroup, ' is not found in scens.')
  
  # check folders
  if(!file.exists(file.path(scen_dir, scens[1])) 
     | !file.exists(file.path(scen_dir, scens[2])))
    stop('scens folder(s) do not exist or scen_dir is set up incorrectly. 
         Please ensure scens is set correctly.')
  
  
  #check slots in rdf
  if(!any(rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffile))) 
          # !any() checks if none in the vector are true 
          %in% variable)){ # %in% checks if variable is in the character vector
    stop(paste('Slot ',variable,' does not exist in given rdf'))
  } 
  
 
  if(!(cyorwy == "cy" | cyorwy == "wy")){
    stop(paste(cyorwy,'is not a supported cyorwy'))
    }
  
  if(!(floworpe == "flow" | floworpe == "pe")){
    stop(paste(floworpe,'is not a supported floworpe'))
  }
  
  if(!any(figuretype %in% 1:3)){ 
    stop(paste(figuretype,'is not a supported figuretype'))
  }
  
  if(!any(exc_month %in% 1:12)){ 
    stop(paste(exc_month,'is not a supported exc_month'))
  }
  
  if(startyr > endyr){
    stop(paste(startyr,'startyr > endyr',endyr))
  }
  
  if(!is.numeric(startyr)){
    stop(paste(startyr,'startyr must be numeric'))
  }
  
  endyr <<- "Really??"
  
  if(!is.numeric(endyr)){
    stop(paste(endyr,'endyr must be numeric'))
  }
  
  if(!is.character(customcaption)){
    stop(paste(customcaption,'customcaption must be char'))
  }
  
  if(!is.character(custom_y_lab)){
    stop(paste(custom_y_lab,'custom_y_lab must be char'))
  }

} #end function
