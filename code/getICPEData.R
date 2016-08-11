library(RWDataPlot)
library(dplyr)
# prep data for 10/50/90 figures for April Run compare to January Run
# mmm = min/most/max

# Want to show initial conditions on 10/50/90 figures, so have to append IC for each run
# using the MTOM results
# assumes that it is only powell and mead initial conditions
# pIcFile: the powell PE initial conditions file
# mICFile: the mead PE initial conditions file
# icMonth: the month/year to use as initial conditions
getAndAppendIC <- function(scens, fileToAppend, pIcFile, mICFile, icMonth = '15-Dec', 
                           addAggAttribute = TRUE, aggFunction)
{
  
  # get all I.C. for April run, and append
  mtomP <- read.csv(pICFile, row.names = 1)
  mtomM <- read.csv(mICFile, row.names = 1)
  
  # the years of ic, i.e., the MTOM runs and  hydrology years
  # expected column names (not in order) or the files that were read in
  ic <- c(paste0('X',1981:2010),'MTOM_Min','MTOM_Most','MTOM_Max','Min','Most','Max')
  # assumes IC are the 5th dimension of scenario names
  scenIC<- simplify2array(strsplit(scens, ',', fixed = T))[5,]
  
  # row that contains the initial conditions
  rr <- which(rownames(mtomP) == icMonth)
  
  mp <- data.frame()
  pp <- data.frame()
  
  # loop over all MTOM runs
  for(i in 1:length(scens)){
    # column to grab initial conditions from
    if(is.na(as.numeric(scenIC[i]))){
      cTmp <- ic[which(ic == scenIC[i])]
    } else{
      cTmp <- ic[which(ic == paste0('X',scenIC[i]))]
    }
    
    mp <- rbind(mp,data.frame('Scenario' = scens[i], 'Value' = mtomM[[cTmp]][rr]))
    pp <- rbind(pp,data.frame('Scenario' = scens[i], 'Value' = mtomP[[cTmp]][rr]))
  }
  
  # add other attributes to data frame
  pp$Variable <- 'Powell.Pool Elevation'
  mp$Variable <- 'Mead.Pool Elevation'
  ic <- rbind(pp,mp)
  ic$Trace <- 0
  ic$Year <- as.numeric(paste0('20',simplify2array(strsplit(icMonth,'-'))[1])) #2015
  # order ic
  ic <- ic[c('Scenario','Trace','Year','Variable','Value')]
  
  # add same agg attribute
  if(addAggAttribute){      
    ic <- dplyr::mutate(ic, Agg = aggFunction(Scenario))
  }
  
  # append I.C. on to rest of April results
  res <- read.table(fileToAppend,header = T)
  res <- rbind(ic, res)
  write.table(res, fileToAppend)
}

