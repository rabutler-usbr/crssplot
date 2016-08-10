
rm(list=ls())

library(CRSSIO)
library(dplyr)
library(reshape2)
library(grid)
source('code/makeScenNames.R')
source('code/getSysCondData.R')
source('code/dataTaggingFunctions.R')
source('code/getEOCYData.R')
source('code/plottingFunctions.R')
source('code/getCondProbs.R')
source('code/plotFirstYearShortCond.R')
source('code/get5YrTable.R')

# -------------------------------------------------------------------------------------
#                                    USER INPUT
# -------------------------------------------------------------------------------------

# script should create everything necessary for the results in order
# CRSSDIR is the CRSS_DIR environment variable that will tell the code where to
# store the intermediate data and figures/tables created here
# iFolder is a path to the top level crss directory that contains the model output
# it could be the same as CRSSDIR, but is allowed to be different so that you
# can read model output from the server, but save figures locally.
CRSSDIR <- Sys.getenv("CRSS_DIR")
iFolder <- 'M:/Shared/CRSS/2016'

# scenarios are orderd model,supply,demand,policy,initial conditions
scens <- makeAllScenNames('Apr2016_2017','DNF','2007Dems','IG',c(1981:2010,'MTOM_Most'))
# scens.limit should only include the 30 ensemble i.c. and not most/min/max runs
scens.limit <- makeAllScenNames('Apr2016_2017','DNF','2007Dems','IG',1981:2010)
iFolder <- file.path(iFolder, 'Scenario') # folder with scenario folders created by RiverSMART
resFolder <- paste0(CRSSDIR,'/results/') # folder to save procssed text files to (intermediate processed data)
sysCondFile <- 'AprSysCond_Test.txt' # file name of system conditions data
curMonthPEFile <- 'Apr_MeadPowellPE.txt' # file name of Powell and Mead PE data
pICFile <- paste0(CRSSDIR,'/MTOM/MTOM_APR16_PowellPE.csv') # input file name of MTOM results for Powell PE
mICFile <- paste0(CRSSDIR,'/MTOM/MTOM_APR16_MeadPE.csv') # input file name of MTOM results for Mead PE
icMonth <- '16-Dec' # IC are from December 2015
critStatsFile <- '/Apr2016_CritStats.txt' # file name for critical stats data
sysCondTable <- 'Apr_SysTableFull2016_2030.csv' # file name for the system conditions procssed file
prevMonthPEFile <- 'Jan/Jan_MPPE_EOCY.txt' # file name that contains the previous CRSS run PE data
# startMonthMap includes a map for the model name (from folder names), to a string that 
# will show up on plots[]
startMonthMap <- c('Apr2015_2016_a3' = 'Apr 2015 DNF','Jan2016' = 'Jan 2016 DNF')
oFigs <- paste0(CRSSDIR,'/figs/April/') # folder location to save figures and fully procssed tables
eocyFigs <- 'Apr2016_MPEOCY.pdf' # file name for figure with Powell and Mead 10/50/90 EOCY elevations
annText <- 'Results from the April 2016 CRSS Run' # text that will be added to figures
critStatsProc <- 'Apr2016_CritStats.csv'
critFigs <- 'Apr2016_CritFigs2026.pdf'
condProbFile <- 'Apr2016_CondProbs.csv'
# mtom results file for creating conditions leading to shortage in 2016
mtomResFile <- paste0(CRSSDIR,'/MTOM/FirstYearCondMTOM/AprilMTOMResults.csv') #changed to may b/c jun results file DNE
shortCondFig <- 'Apr2016_shortConditionsFig.pdf'
# for the 5-year simple table
ss5 <- c('Jan CRSS', 'April CRSS')
# should match files for critStatsFile:
critStatsIn <- paste0(CRSSDIR,c('/results/Jan/Jan_CritStats_DNF.csv',
                 '/figs/Apr2016_CritStats.csv'))
yy5 <- 2017:2021
simple5YrFile <- 'Apr2016_5yrSimple.pdf'
createShortConditions <- FALSE

# -------------------------------------------------------------------------------------
#                               END USER INPUT
# -------------------------------------------------------------------------------------

## System Conditions Table Data
if(TRUE){
  print('starting getSysCondData')
  flush.console()
  getSysCondData(scens, iFolder, paste0(resFolder,sysCondFile),TRUE, aggBasedOnIC)
  print('finished getSysCondData')
  flush.console()
}

if(TRUE){
  ## get the Mead and Powel EOCY Data
  getPowellMeadEOCYPE(scens, iFolder, paste0(resFolder,curMonthPEFile), TRUE, aggBasedOnIC)
  ## append initial conditions onto May data
  getAndAppendIC(scens, paste0(resFolder,curMonthPEFile), pIcFile, mICFile, icMonth, 
                 TRUE, aggBasedOnIC)
}

## Get Crit Stats Data
if(TRUE){
  print('starting getCritStats')
  flush.console()
  getSritStatsData(scens, iFolder, paste0(resFolder,critStatsFile),TRUE, aggBasedOnIC)
  print('finished getCrityStats')
  flush.console()
}

## Create the KeySlots csv file, but only want to include data for the 30 Ensemble and not
## the Most or MTOM_Most
print('Creating KeySlots csv file')
flush.console()
RWDataPlot::getDataForAllScens(scens.limit,scens.limit,RWDataPlot::createSlotAggList('data/KeySlotsProcess.csv'), 
                               iFolder, paste0(oFigs,'/KeySlots.txt'))
# now read in txt file and write out csv file and delete txt file (inefficient I know)
zz <- read.table(paste0(oFigs,'/KeySlots.txt'),header=T)
write.csv(zz, paste0(oFigs,'/KeySlots.csv'),row.names = F)
file.remove(paste0(oFigs,'/KeySlots.txt'))

if(TRUE){
  print("starting to create figures and tables")
  flush.console()
  print("creating system conditions table")
  flush.console()
  ## Create tables, figures, and data behind figures
  # 1) system conditions table
  sysCond <- read.table(paste0(resFolder,sysCondFile),header = T)
  # trim to 2016-2026 and 30 trace ensemble I.C.; Agg == 1 limits to using 30 ensemble I.C. 
  sysCond <- dplyr::filter(sysCond, Year %in% 2017:2030 & Agg == 1)
  # create the system cond. table
  sysTable <- CRSSIO::createSysCondTable(sysCond, 2017:2030)
  # save the sys cond table
  write.csv(sysTable[['fullTable']], paste0(oFigs,sysCondTable))
  
  # 2) Plot Mead, Powell EOCY elvations and include previous month's results too.
  # read in current month data
  print("EOCY elevation figures")
  flush.console()
  peCur <- read.table(paste0(resFolder,curMonthPEFile),header = T)
  pePrev <- read.table(paste0(resFolder,prevMonthPEFile),header = T) # read in prev. month data
  # add start month attributes to both months' data
  peCur <- dplyr::mutate(peCur, StartMonth = addAttByScenName(Scenario, 1, startMonthMap))
  pePrev <- dplyr::mutate(pePrev, StartMonth = addAttByScenName(Scenario, 1, startMonthMap))
  # combine previous and current elevations
  pe <- rbind(peCur, pePrev)
  rm(peCur, pePrev)
  # plot
  # only use the 30 ensemble (Agg = 1)
  powellPE <- plotEOCYElev(dplyr::filter(peCur, Agg == 1), 2017:2060, 'Powell.Pool Elevation', 
                           'Powell End-of-December Year Elevation')
  meadPE <- plotEOCYElev(dplyr::filter(peCur, Agg == 1), 2017:2060, 'Mead.Pool Elevation', 
                           'Mead End-of-December Year Elevation')
  
  # save figures
  pdf(paste0(oFigs,eocyFigs), width = 8, height = 6)
  print(powellPE)
  print(meadPE)
  dev.off()
  
  rm(pe, powellPE, meadPE)
  
  
  # 3) Critical elevation thresholds; figures and data table
  # have sysCond for some, and read in crit stats for others
  print("starting critical stats")
  flush.console()
  critStats <- read.table(paste0(resFolder,critStatsFile),header = T)
  # defaults are ok for legendTitle, legLoc, nC, and annSize
  # filter to only use 30 ensemble and to drop Mead LT 1025 from one plot and Mead LT 1020 from 
  # the other plot
  critStatsFig1 <- plotCritStats(dplyr::filter(critStats, Agg == 1, Variable != 'meadLt1020'), 
                                 2016:2026, annText)
  critStatsFig2 <- plotCritStats(dplyr::filter(critStats, Agg == 1, Variable != 'meadLt1025'), 
                                 2016:2026, annText)
  # create data table to save crit stats
  cs <- dplyr::filter(critStats, Year %in% 2016:2026, Agg == 1)
  
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
                                              Agg == 1), 2017:2026, 'April 2016')
    
  # stacked barplot of different shortage tiers
  # default for annSize is ok
  shortStack <- plotShortStackedBar(dplyr::filter(sysCond, Variable %in% c('lbShortageStep1',
                                    'lbShortageStep2','lbShortageStep3')), 2017:2026, annText)

# save figures and table
  pdf(paste0(oFigs,critFigs),width = 8, height = 6)
  print(critStatsFig1)
  print(critStatsFig2)
  print(ssPlot)
  print(shortStack)
  dev.off()
  write.csv(cs,paste0(oFigs,critStatsProc),row.names = F)
}

## CONDITIONAL PROBABILITIES
# use sysCond
if(is.na(match('sysCond',ls()))){
  sysCond <- read.table(paste0(resFolder,sysCondFile),header = T) 
  sysCond <- dplyr::filter(sysCond, Year %in% 2017:2027 & Agg == 1)
  sysTable <- CRSSIO::createSysCondTable(sysCond, 2017:2027)
}
cp1 <- getConditionalProbs(sysCond, 2017, 2017, 'lbShortage','mer748')
cp2 <- getConditionalProbs(sysCond, 2017, 2017, 'lbShortage','ueb823')
cp3 <- getConditionalProbs(sysCond, 2017,2017, 'lbShortage',c('eq','uebGt823'))
cp4 <- getConditionalProbs(sysCond, 2018, 2017, c('lbShortage','lbShortageStep1','lbShortageStep2',
                                               'lbShortageStep3'), 'mer748')
cp5 <- getConditionalProbs(sysCond, 2018, 2017, c('lbShortage','lbShortageStep1','lbShortageStep2',
                                               'lbShortageStep3'), 'ueb823')
cp6 <- getConditionalProbs(sysCond, 2018, 2017, c('lbShortage','lbShortageStep1','lbShortageStep2',
                                               'lbShortageStep3'), c('eq','uebGt823'))

# create data table from the above values
cpt1 <- data.frame('ChanceOf' = c(paste(2017,names(cp1)),paste(2018,names(cp4))),
                   'PrctChance' = c(cp1,cp4))
rr <- which(rownames(sysTable$fullTable) == 'Mid-Elevation Release Tier - annual release = 7.48 maf')
cc <- which(colnames(sysTable$fullTable) == 2017)
cpt1$PowellWYRel <- paste('7.48 MAF;',sysTable$fullTable[rr,cc])

cpt2 <- data.frame('ChanceOf' = c(paste(2017,names(cp2)),paste(2018,names(cp5))),
                   'PrctChance' = c(cp2,cp5))
rr <- which(rownames(sysTable$fullTable) == "Upper Elevation Balancing - annual release = 8.23 maf")
cpt2$PowellWYRel <- paste('8.23 MAF;',sysTable$fullTable[rr,cc])

cpt3 <- data.frame('ChanceOf' = c(paste(2017,names(cp3)),paste(2018,names(cp6))),
                   'PrctChance' = c(cp3,cp6))
rr <- which(rownames(sysTable$fullTable) == "Upper Elevation Balancing - annual release > 8.23 maf")
rr2 <- which(rownames(sysTable$fullTable) == "Equalization - annual release > 8.23 maf")
cpt3$PowellWYRel <- paste('> 8.23 MAF;',sysTable$fullTable[rr,cc] + sysTable$fullTable[rr2,cc])

cpt1 <- rbind(cpt1,cpt2,cpt3)

# rearrange columns
cpt1 <- cpt1[c('PowellWYRel','ChanceOf','PrctChance')]
cpt1$PrctChance <- cpt1$PrctChance*100
write.csv(cpt1,paste0(oFigs,condProbFile),row.names = F)

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

## create the 5-yr simple table that compares to the previous run
simple5Yr <- creat5YrSimpleTable(ss5, critStatsIn, yy5)
pdf(paste0(oFigs,simple5YrFile),width = 8, height = 8)
print(simple5Yr)
dev.off()


#rm(list = ls())

