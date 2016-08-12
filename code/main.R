
rm(list=ls())

library(CRSSIO)
library(dplyr)
library(reshape2)
library(grid)
library(feather)
source('code/makeScenNames.R')
source('code/getSysCondData.R')
source('code/dataTaggingFunctions.R')
source('code/getICPEData.R')
source('code/plottingFunctions.R')
source('code/getCondProbs.R')
source('code/plotFirstYearShortCond.R')

# -----------------------------------------------------------------------------
#                                    USER INPUT
# -----------------------------------------------------------------------------

# script should create everything necessary for the results in order
# CRSSDIR is the CRSS_DIR environment variable that will tell the code where to
# store the intermediate data and figures/tables created here
# iFolder is a path to the top level crss directory that contains the model output
# it could be the same as CRSSDIR, but is allowed to be different so that you
# can read model output from the server, but save figures locally.
CRSSDIR <- Sys.getenv("CRSS_DIR")
iFolder <- 'M:/Shared/CRSS/2016'
# set crssMonth to the month CRSS was run. data and figures will be saved in 
# a folder with this name
crssMonth <- 'Aug'

# scenarios are orderd model,supply,demand,policy,initial conditions (if initial conditions are used)
# scens should be a list, each entry is a scenario name, and the entry is a 
# character vector of length 1 to n. 
# all of the values in each entry of the list are combined together and processed
# as one scenario. So for a run that has 30 initial conditions, all 30 runs are 
# averaged/combined together. the name of the entries in the list are used for 
# the scenario name
scens <- list(
  'Apr2016' = makeAllScenNames('Apr2016_2017','DNF','2007Dems','IG',1981:1982),
  'Aug2016' = 'Aug2016_2017,DNF,2007Dems,IG'
)

# for each group name, it should be either 2 number or 2 file paths, both ordered
# powell, then mead.
icList <- list(
  'Apr2016' = c(file.path(CRSSDIR,'MTOM','MTOM_APR16_PowellPE.csv'),
                file.path(CRSSDIR,'MTOM','MTOM_APR16_MeadPE.csv')),
  'Aug2016' = c(3605.83, 1078.93)
)

# the mainScenGroup is the scenario to use when creating the current month's 
# 5-year table, etc. In the plots, we want to show the previous months runs,
# but in the tables, we only want the current month run. This should match names
# in scens and icList
mainScenGroup <- 'Aug2016'
mainScenGroup.name <- 'August 2016'

# IC for each run
icMonth <- c('Apr2016' = '16-Dec', 'Aug2016' = '16-Dec') 

# startMonthMap includes a map for the model name (from folder names), to a string that 
# will show up on plots;
startMonthMap <- c('Apr2015_2016_a3' = 'Apr 2015 DNF','Jan2016' = 'Jan 2016 DNF',
                   'Apr2016_2017' = 'Apr 2016 DNF', 'Aug2016_2017' = 'Aug 2016 DNF')

yrs2show <- 2017:2026
peYrs <- 2016:2026

annText <- 'Results from the August 2016 CRSS Run' # text that will be added to figures

# mtom results file for creating conditions leading to shortage in 2016
mtomResFile <- paste0(CRSSDIR,'/MTOM/FirstYearCondMTOM/AprilMTOMResults.csv') #changed to may b/c jun results file DNE

# for the 5-year simple table
# names are the names that will show up in the 5-year simple table
# the values are the Scenario Group variable names that will be filtered from the
# critStats file
ss5 <- c('Apr2016' ='April CRSS', 'Aug2016' = 'August CRSS')

# years to use for the simple 5-year table
yy5 <- 2017:2021

# "switches" to create/not create different figures
createShortConditions <- FALSE
getSysCondData <- FALSE
getPeData <- FALSE
getCSData <- FALSE
createKeySlotsCsv <- FALSE
makeFiguresAndTables <- FALSE
computeConditionalProbs <- FALSE
createSimple5yrTable <- TRUE

#                               END USER INPUT
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#       SETUP DIRECTORIES AND FILENAMES
# -----------------------------------------------------------------------------

# some sanity checks that UI is correct:
if(!(mainScenGroup %in% names(scens)))
  stop(mainScenGroup, ' is not found in scens.')
if(!(mainScenGroup %in% names(icList)))
  stop(mainScenGroup, ' is not found in icList')

iFolder <- file.path(iFolder, 'Scenario') # folder with scenario folders created by RiverSMART
message('Scenario data will be read in from: ', iFolder)

# folder location to save figures and fully procssed tables
oFigs <- file.path(CRSSDIR,'results', crssMonth) 
if(!file.exists(oFigs)){
  message(paste('Creating folder:', oFigs))
  dir.create(oFigs)
}
message('Figures and tables will be saved to: ', oFigs)

# folder to save procssed text files to (intermediate processed data)
resFolder <- file.path(CRSSDIR,'results', crssMonth, 'tempData')
if(!file.exists(resFolder)){
  message(paste('Creating folder:', resFolder))
  dir.create(resFolder)
}
message('Intermediate data will be saved to: ', resFolder)

sysCondFile <- 'SysCond.feather' # file name of system conditions data
tmpPEFile <- 'tempPE.feather'
curMonthPEFile <- 'MeadPowellPE.feather' # file name of Powell and Mead PE data

critStatsFile <- 'CritStats.feather' # file name for critical stats data
# file name for the system conditions procssed file
sysCondTable <- paste0('SysTableFull',yrs2show[1],'_',tail(yrs2show,1),'.csv') 

eocyFigs <- 'MPEOCY.pdf' # file name for figure with Powell and Mead 10/50/90 EOCY elevations

critStatsProc <- 'CritStats.csv'
critFigs <- 'CritFigs2026.pdf'
condProbFile <- 'CondProbs.csv'

shortCondFig <- 'shortConditionsFig.pdf'

simple5YrFile <- '5yrSimple.pdf'

# -----------------------------------------------------------------------------
#       Process results
# -----------------------------------------------------------------------------

## System Conditions Table Data
if(getSysCondData){
  message('starting getSysCondData')
  getScenarioData(scens, iFolder, file.path(resFolder,sysCondFile),TRUE, 
                  'aggFromScenList', 'data/SysCond.csv')
  message('finished getSysCondData')
}

if(getPeData){
  ## get the Mead and Powel EOCY Data
  getScenarioData(scens, iFolder, file.path(resFolder,tmpPEFile), TRUE, 
                  'aggFromScenList', 'data/MPPE_EOCY.csv')
  ## append initial conditions onto May data
  getAndAppendIC(scens, file.path(resFolder,tmpPEFile), 
                 file.path(resFolder,curMonthPEFile), icList, icMonth, 
                 TRUE, 'aggFromScenList')
}

## Get Crit Stats Data
if(getCSData){
  message('starting getCritStats')
  getScenarioData(scens, iFolder, file.path(resFolder,critStatsFile),TRUE, 
                  'aggFromScenList', 'data/CritStatsList.csv')
  message('finished getCritStats')
}

## Create the KeySlots csv file, but only want to include data for the 30 Ensemble and not
## the Most or MTOM_Most
if(createKeySlotsCsv){
  message('Creating KeySlots csv file')
  RWDataPlot::getDataForAllScens(scens.limit,scens.limit,
                                 RWDataPlot::createSlotAggList('data/KeySlotsProcess.csv'), 
                                 iFolder, paste0(oFigs,'/KeySlots.csv'), FALSE)
  message('Done creating KeySlots csv file')
}

if(makeFiguresAndTables){
  message("starting to create figures and tables")
  message("creating system conditions table")
  ## Create tables, figures, and data behind figures
  # 1) system conditions table
  sysCond <- read_feather(file.path(resFolder,sysCondFile))
  # trim to specified years and the current main scenario group 
  sysCond <- dplyr::filter(sysCond, Year %in% yrs2show & Agg == mainScenGroup)
  # create the system cond. table
  sysTable <- CRSSIO::createSysCondTable(sysCond, yrs2show)
  # save the sys cond table
  write.csv(sysTable[['fullTable']], file.path(oFigs,sysCondTable))
  
  # 2) Plot Mead, Powell EOCY elvations and include previous month's results too.
  # read in current month data
  message("EOCY elevation figures")
  pe <- read_feather(file.path(resFolder,curMonthPEFile))
  
  # add start month attributes to both months' data
  pe <- dplyr::mutate(pe, StartMonth = addAttByScenName(Scenario, 1, startMonthMap))

  # plot
  powellPE <- plotEOCYElev(pe, peYrs, 'Powell.Pool Elevation', 
                           'Powell End-of-December Year Elevation')
  meadPE <- plotEOCYElev(pe, peYrs, 'Mead.Pool Elevation', 
                           'Mead End-of-December Year Elevation')
  
  # save figures
  pdf(file.path(oFigs,eocyFigs), width = 8, height = 6)
  print(powellPE)
  print(meadPE)
  dev.off()
  
  rm(pe, powellPE, meadPE)
  
  
  # 3) Critical elevation thresholds; figures and data table
  # have sysCond for some, and read in crit stats for others
  message("starting critical stats")
  critStats <- read_feather(file.path(resFolder,critStatsFile))
  # defaults are ok for legendTitle, legLoc, nC, and annSize
  # drop Mead LT 1025 from one plot and Mead LT 1020 from 
  # the other plot
  critStatsFig1 <- plotCritStats(dplyr::filter(critStats, Agg == mainScenGroup, 
                                               Variable != 'meadLt1020'), 
                                 yrs2show, annText)
  critStatsFig2 <- plotCritStats(dplyr::filter(critStats, Agg == mainScenGroup, 
                                               Variable != 'meadLt1025'), 
                                 yrs2show, annText)
  # create data table to save crit stats
  cs <- dplyr::filter(critStats, Year %in% yrs2show, Agg == mainScenGroup)
  
  # rename the variables to strings
  cs$vName <- 'LB Shortage'
  cs$vName[cs$Variable == 'meadLt1000'] <- 'Mead < 1,000\' in Any Month'
  cs$vName[cs$Variable == 'meadLt1020'] <- 'Mead < 1,020\' in Any Month'
  cs$vName[cs$Variable == 'meadLt1025'] <- 'Mead < 1,025\' in Any Month'
  cs$vName[cs$Variable == 'powellLt3490'] <- 'Powell < 3,490\' in Any Month'
  
  # compute the percent of traces by averaging values 
  cs <- cs %>% group_by(Year,Variable,vName) %>%
    summarise(Value = mean(Value))
  # reshape to be easier to print out
  cs <- reshape2::dcast(cs, Year ~ vName, value.var = 'Value')
  
  # shortage surplus figure
  # defaults ok for legendTitle, nC, and legLoc
  ssPlot <- plotShortageSurplus(dplyr::filter(sysCond, Variable %in% c('lbShortage', 'lbSurplus'),
                                              Agg == mainScenGroup), 
                                yrs2show, mainScenGroup.name)
    
  # stacked barplot of different shortage tiers
  # default for annSize is ok
  shortStack <- plotShortStackedBar(dplyr::filter(sysCond, Variable %in% c('lbShortageStep1',
                                    'lbShortageStep2','lbShortageStep3')), yrs2show, annText)

# save figures and table
  pdf(file.path(oFigs,critFigs),width = 8, height = 6)
  print(critStatsFig1)
  print(critStatsFig2)
  print(ssPlot)
  print(shortStack)
  dev.off()
  write.csv(cs,file.path(oFigs,critStatsProc),row.names = F)
}

if(computeConditionalProbs){
  ## CONDITIONAL PROBABILITIES
  # use sysCond
  if(is.na(match('sysCond',ls()))){
    sysCond <- read.table(paste0(resFolder,sysCondFile),header = T) 
    sysCond <- dplyr::filter(sysCond, Year %in% yrs2show & Agg == 1)
    sysTable <- CRSSIO::createSysCondTable(sysCond, yrs2show)
  }
  cp1 <- getConditionalProbs(sysCond, yrs2show[1], yrs2show[1], 'lbShortage','mer748')
  cp2 <- getConditionalProbs(sysCond, yrs2show[1], yrs2show[1], 'lbShortage','ueb823')
  cp3 <- getConditionalProbs(sysCond, yrs2show[1],yrs2show[1], 'lbShortage',c('eq','uebGt823'))
  cp4 <- getConditionalProbs(sysCond, yrs2show[2], yrs2show[1], c('lbShortage','lbShortageStep1','lbShortageStep2',
                                                 'lbShortageStep3'), 'mer748')
  cp5 <- getConditionalProbs(sysCond, yrs2show[2], yrs2show[1], c('lbShortage','lbShortageStep1','lbShortageStep2',
                                                 'lbShortageStep3'), 'ueb823')
  cp6 <- getConditionalProbs(sysCond, yrs2show[2], yrs2show[1], c('lbShortage','lbShortageStep1','lbShortageStep2',
                                                 'lbShortageStep3'), c('eq','uebGt823'))
  
  # create data table from the above values
  cpt1 <- data.frame('ChanceOf' = c(paste(yrs2show[1],names(cp1)),paste(yrs2show[2],names(cp4))),
                     'PrctChance' = c(cp1,cp4))
  rr <- which(rownames(sysTable$fullTable) == 'Mid-Elevation Release Tier - annual release = 7.48 maf')
  cc <- which(colnames(sysTable$fullTable) == yrs2show[1])
  cpt1$PowellWYRel <- paste('7.48 MAF;',sysTable$fullTable[rr,cc])
  
  cpt2 <- data.frame('ChanceOf' = c(paste(yrs2show[1],names(cp2)),paste(yrs2show[2],names(cp5))),
                     'PrctChance' = c(cp2,cp5))
  rr <- which(rownames(sysTable$fullTable) == "Upper Elevation Balancing - annual release = 8.23 maf")
  cpt2$PowellWYRel <- paste('8.23 MAF;',sysTable$fullTable[rr,cc])
  
  cpt3 <- data.frame('ChanceOf' = c(paste(yrs2show[1],names(cp3)),paste(yrs2show[2],names(cp6))),
                     'PrctChance' = c(cp3,cp6))
  rr <- which(rownames(sysTable$fullTable) == "Upper Elevation Balancing - annual release > 8.23 maf")
  rr2 <- which(rownames(sysTable$fullTable) == "Equalization - annual release > 8.23 maf")
  cpt3$PowellWYRel <- paste('> 8.23 MAF;',sysTable$fullTable[rr,cc] + sysTable$fullTable[rr2,cc])
  
  cpt1 <- rbind(cpt1,cpt2,cpt3)
  
  # rearrange columns
  cpt1 <- cpt1[c('PowellWYRel','ChanceOf','PrctChance')]
  cpt1$PrctChance <- cpt1$PrctChance*100
  write.csv(cpt1,paste0(oFigs,condProbFile),row.names = F)
}

# pulled annotation out of generic function
if(createShortConditions){
  lbLabel <- 'LB total side inflow percent\nof average (1981-2010)'
  # filterOn being set to pe shows results for traces that are <= 1077
  shortCond <- plotFirstYearShortCond(mtomResFile, filterOn = 'pe')
  shortCond <- shortCond + annotate('segment', x = 5.1, xend = 3.7, y = 1071.1, yend = 1071.35, 
           arrow = grid::arrow(length = unit(.3,'cm')),size = 1) +
    annotate('text', x = 5.2, y = 1071,label = lbLabel, size = 4, hjust = 0)
  
  pdf(paste0(oFigs,shortCondFig),width = 9, height = 6)
  print(shortCond)
  dev.off()
}

if(createSimple5yrTable){
  ## create the 5-yr simple table that compares to the previous run
  simple5Yr <- creat5YrSimpleTable(ss5, file.path(resFolder,critStatsFile), yy5)
  pdf(file.path(oFigs,simple5YrFile),width = 8, height = 8)
  print(simple5Yr)
  dev.off()
}

