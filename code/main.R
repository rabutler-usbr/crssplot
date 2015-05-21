source('code/makeScenNames.R')
source('code/getSysCondData.R')
source('code/dataTaggingFunctions.R')
source('code/getEOCYData.R')

# script should create everything necessary for the results in order

# scenarios are orderd model,supply,demand,policy,initial conditions
scens <- makeAllScenNames('May2015_2016','DNF','2007Dems','IG',c(1981:2010,'MTOM_Most','Most'))
iFolder <- 'M:/Shared/CRSS/CRSS.2015/Scenario'
resFolder <- '../CRSS.2015/results/'
sysCondFile <- 'MaySysCond.txt'
curMonthPEFile <- 'May_MeadPowellPE.txt'
pICFile <- '../CRSS.2015/MTOM/MTOM_May15_PowellPE.csv'
mICFile <- '../CRSS.2015/MTOM/MTOM_May15_MeadPE.csv'
icMonth <- '15-Dec' # IC are from December 2015
critStatsFile <- 'May_CritStats.txt'

## System Conditions Table Data
if(FALSE){
  getSysCondData(scens, iFolder, paste0(resFolder,sysConFile),TRUE, aggBasedOnIC)
}

if(FALSE){
  ## get the Mead and Powel EOCY Data
  getPowellMeadEOCYPE(scens, iFolder, paste0(resFolder,curMonthPEFile), TRUE, aggBasedOnIC)
  ## append initial conditions onto May data
  getAndAppendIC(scens, paste0(resFolder,curMonthPEFile), pIcFile, mICFile, icMonth, 
                 TRUE, aggBasedOnIC)
}

## Get Crit Stats Data
if(TRUE){
  getSritStatsData(scens, iFolder, paste0(resFolder,critStatsFile),TRUE, aggBasedOnIC)
}

# before combining with previous month, need to denote starting month of simulation?
if(FALSE){
  # add 'Start Month' attribute to January run, and April run
  aprRes$StartMonth <- 'Apr2015'
  janRes$StartMonth <- 'Jan2015'
}

rm(list = ls())

