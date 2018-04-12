library(RWDataPlyr)
library(tidyverse)
library(readxl)
# prep data for 10/50/90 figures for April Run compare to January Run
# mmm = min/most/max

# Want to show initial conditions on 10/50/90 figures, so have to append IC for each run
# using the MTOM results

get1TraceIc <- function(icName, icFile, icMonth, traceMap) {
  # convert icName to a trace number

  icTrace <- traceMap %>% 
    filter(ic == icName)
  
  # paste Trace[N] unless it is the min/most/max
  if(as.character(icTrace$trace) %in% c("Min", "Most", "Max")) {
    icTrace <- as.character(icTrace$trace)
  } else {
    icTrace <- paste0("Trace", icTrace$trace)
  }
 
  # then read the provided file and get powell and mead PE for the provided month
  # and return it
  read_excel(icFile, sheet = icTrace) %>%
    filter(as.Date(X__1, format = "%Y-%b-%d") == as.Date(paste0(icMonth,"-01"), format = "%y-%b-%d")) %>%
    select(`Powell.Pool Elevation`, `Mead.Pool Elevation`)
}

# assumes that it is only powell and mead initial conditions
#' @param icList: a list of model run entries; the entries are either numeric, 
#' actual IC or paths to files that contain the IC
#' @param icMonth: the month/year to use as initial conditions
#' @param traceMap: named list of matrices. Null, unless one of the icList is an excel 
#' file, then provide a traceMap that maps the trace numbers found in the excel 
#' file to the initial conditions dimension label
#' @param icDimNumber: numeric dimension number for the initial condition label within
#' the full scenario name
getAndAppendIC <- function(scens, fileToAppend, oFile, icList, icMonth, 
                           addAggAttribute = TRUE, aggFunction, traceMap, icDimNumber = 5)
{
 
  res <- read_feather(fileToAppend)
  
  icSave <- data.frame()
  
  for(j in 1:length(scens)){
    groupName <- names(scens)[j]
    icData <- icList[[groupName]]
    
    # if icData is a string, then need to read the IC in; otherwise it's numeric
    # and use just those IC
    
    if(is.numeric(icData)){
      # then use the IC from icData
      if(length(scens[[groupName]]) > 1){
        stop('Consider using csv file to input the initial conditions for the ',
             groupName, ' group.')
      }
      mp <- data.frame(
        'Scenario' = scens[[groupName]], 
        'Value' = icData[2], 
        stringsAsFactors = FALSE
      )
      pp <- data.frame(
        'Scenario' = scens[[groupName]], 
        'Value' = icData[1],
        stringsAsFactors = FALSE
      )
    } else{
      
      # apply function over all i.c. dimension for the current groupName
      # get the ic dimensions
      icDim <- stringr::str_split(scens[[groupName]], pattern = ',', simplify = TRUE)
      icDim <- icDim[,icDimNumber] # get only the i.c. dimension
      icVals <- do.call(rbind, lapply(icDim, get1TraceIc, icData, icMonth[[groupName]], traceMap))
      mp <- data.frame(
        'Scenario' = scens[[groupName]], 
        'Value' = icVals$`Mead.Pool Elevation`,
        stringsAsFactors = FALSE
      )
      pp <- data.frame(
        'Scenario' = scens[[groupName]], 
        'Value' = icVals$`Powell.Pool Elevation`,
        stringsAsFactors = FALSE
      )
    }
    
    # add other attributes to data frame
    pp$Variable <- 'Powell.Pool Elevation'
    mp$Variable <- 'Mead.Pool Elevation'
    ic <- rbind(pp,mp)
    ic$TraceNumber <- 0
    ic$Year <- as.numeric(paste0('20',simplify2array(strsplit(icMonth,'-'))[1,j])) #2015
    ic$Month <- "December"
    # order ic
    ic <- select(ic, one_of(names(res)[names(res) != "Agg"]))
    
    # add same agg attribute
    if(addAggAttribute){      
      if(aggFunction == 'aggFromScenList'){
        ic <- dplyr::mutate(ic, Agg = eval(call(aggFunction, Scenario, scens)))
      } else{
        ic <- dplyr::mutate(ic, Agg = eval(call(aggFunction, Scenario)))
      }
    }
    
    icSave <- rbind(icSave, ic)
    
  }
  
  # append I.C. on to rest of April results
  res <- bind_rows(icSave, res)
  write_feather(res, oFile)
}
