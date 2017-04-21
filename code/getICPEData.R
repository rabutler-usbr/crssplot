library(RWDataPlyr)
library(tidyverse)
library(readxl)
# prep data for 10/50/90 figures for April Run compare to January Run
# mmm = min/most/max

# Want to show initial conditions on 10/50/90 figures, so have to append IC for each run
# using the MTOM results
# assumes that it is only powell and mead initial conditions
# icList: a list of model run entries; the entries are either numeric, actual IC
#         or paths to files that contain the IC
# icMonth: the month/year to use as initial conditions
getAndAppendIC <- function(scens, fileToAppend, oFile, icList, icMonth = '15-Dec', 
                           addAggAttribute = TRUE, aggFunction)
{
  
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
      mp <- data.frame('Scenario' = scens[[groupName]], 'Value' = icData[2])
      pp <- data.frame('Scenario' = scens[[groupName]], 'Value' = icData[1])
    } else{
      # use the IC from the file 
      # get all I.C. for April run, and append
      mtomP <- read.csv(icData[1], row.names = 1)
      mtomM <- read.csv(icData[2], row.names = 1)
      
      # the years of ic, i.e., the MTOM runs and  hydrology years
      # expected column names (not in order) or the files that were read in
      ic <- c(paste0('X',1981:2015),'MTOM_Min','MTOM_Most','MTOM_Max','Min','Most','Max')
      # assumes IC are the 5th dimension of scenario names
      scenIC<- simplify2array(strsplit(scens[[j]], ',', fixed = T))[5,]
      # row that contains the initial conditions
      rr <- which(rownames(mtomP) == icMonth)
      
      mp <- data.frame()
      pp <- data.frame()
      
      # loop over all MTOM runs
      for(i in 1:length(scens[[groupName]])){
        # column to grab initial conditions from
        if(is.na(as.numeric(scenIC[i]))){
          cTmp <- ic[which(ic == scenIC[i])]
        } else{
          cTmp <- ic[which(ic == paste0('X',scenIC[i]))]
        }
        if(length(cTmp) == 0)
          stop('I.C. not found in expected column names from the input file.')

        mp <- rbind(mp,data.frame('Scenario' = scens[[groupName]][i], 'Value' = mtomM[[cTmp]][rr]))
        pp <- rbind(pp,data.frame('Scenario' = scens[[groupName]][i], 'Value' = mtomP[[cTmp]][rr]))
      }
    }
    
    # add other attributes to data frame
    pp$Variable <- 'Powell.Pool Elevation'
    mp$Variable <- 'Mead.Pool Elevation'
    ic <- rbind(pp,mp)
    ic$Trace <- 0
    ic$Year <- as.numeric(paste0('20',simplify2array(strsplit(icMonth,'-'))[1,j])) #2015
    # order ic
    ic <- ic[c('Scenario','Trace','Year','Variable','Value')]
    
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
  
  res <- read_feather(fileToAppend)
  # append I.C. on to rest of April results
  res <- rbind(icSave, res)
  write_feather(res, oFile)
}

get1TraceIc <- function(icName, icFile, icMonth, traceMap) {
  # convert icName to a trace number

  icTrace <- traceMap %>% 
    filter(ic == icName)
  icTrace <- paste0("Trace", icTrace$trace)
  
  # then read the provided file and get powell and mead PE for the provided month
  # and return it
  read_excel(icFile, sheet = icTrace) %>%
    filter(as.Date(X__1, format = "%Y-%b-%d") == as.Date(paste0(icMonth,"-01"), format = "%y-%b-%d")) %>%
    select(`Powell.Pool Elevation`, `Mead.Pool Elevation`)
}

#' @param traceMap: named list of matrices. Null, unless the icList is an excel 
#' file, then provide a traceMap that maps the trace numbers found in the excel 
#' file to the initial conditions dimension label
#' @param icDimNumber: numeric dimension number for the initial condition label within
#' the full scenario name
getAndAppendIC2 <- function(scens, fileToAppend, oFile, icList, icMonth = '15-Dec', 
                           addAggAttribute = TRUE, aggFunction, traceMap, icDimNumber = 5)
{
  
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
      mp <- data.frame('Scenario' = scens[[groupName]], 'Value' = icData[2])
      pp <- data.frame('Scenario' = scens[[groupName]], 'Value' = icData[1])
    } else{
      
      # apply function over all i.c. dimension for the current groupName
      # get the ic dimensions
      icDim <- stringr::str_split(scens[[groupName]], pattern = ',', simplify = TRUE)
      icDim <- icDim[,icDimNumber] # get only the i.c. dimension
      icVals <- do.call(rbind, lapply(icDim, get1TraceIc, icData, icMonth[[groupName]], traceMap))
      mp <- data.frame('Scenario' = scens[[groupName]], 'Value' = icVals$`Mead.Pool Elevation`)
      pp <- data.frame('Scenario' = scens[[groupName]], 'Value' = icVals$`Powell.Pool Elevation`)
    }
    
    # add other attributes to data frame
    pp$Variable <- 'Powell.Pool Elevation'
    mp$Variable <- 'Mead.Pool Elevation'
    ic <- rbind(pp,mp)
    ic$Trace <- 0
    ic$Year <- as.numeric(paste0('20',simplify2array(strsplit(icMonth,'-'))[1,j])) #2015
    # order ic
    ic <- select(ic, one_of(c('Scenario','Trace','Year','Variable','Value')))
    
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
  
  res <- read_feather(fileToAppend)
  # append I.C. on to rest of April results
  res <- rbind(icSave, res)
  write_feather(res, oFile)
}
