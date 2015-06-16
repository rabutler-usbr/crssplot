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

# script should create everything necessary for the results in order

# -------------------------------------------------------------------------------------
#                                    USER INPUT
# -------------------------------------------------------------------------------------
# scenarios are orderd model,supply,demand,policy,initial conditions
scens <- makeAllScenNames('May2015_2016','DNF','2007Dems','IG',c(1981:2010,'MTOM_Most','Most'))
iFolder <- 'M:/Shared/CRSS/CRSS.2015/Scenario' # folder with scenario folders created by RiverSMART
resFolder <- '../CRSS.2015/results/' # folder to save procssed text files to (intermediate processed data)
sysCondFile <- 'MaySysCond.txt' # file name of system conditions data
curMonthPEFile <- 'May_MeadPowellPE.txt' # file name of Powell and Mead PE data
pICFile <- '../CRSS.2015/MTOM/MTOM_May15_PowellPE.csv' # input file name of MTOM results for Powell PE
mICFile <- '../CRSS.2015/MTOM/MTOM_May15_MeadPE.csv' # input file name of MTOM results for Mead PE
icMonth <- '15-Dec' # IC are from December 2015
critStatsFile <- 'May_CritStats.txt' # file name for critical stats data
sysCondTable <- 'May_SysTableFull2016_2026.csv' # file name for the system conditions procssed file
prevMonthPEFile <- 'April/April_MPPE_EOCY.txt' # file name that contains the previous CRSS run PE data
# startMonthMap includes a map for the model name (from folder names), to a string that 
# will show up on plots
startMonthMap <- c('May2015_2016' = 'May 2015 DNF', 'Apr2015_2016_a3' = 'Apr 2015 DNF')
oFigs <- '../CRSS.2015/figs/' # folder location to save figures and fully procssed tables
eocyFigs <- 'May2015_MPEOCY.pdf' # file name for figure with Powell and Mead 10/50/90 EOCY elevations
annText <- 'Results from the May 2015 CRSS Run' # text that will be added to figures
critStatsProc <- 'May_CritStats.csv'
critFigs <- 'May2015_CritFigs.pdf'
condProbFile <- 'May_CondProbs.csv'
createShortConditions <- FALSE
# mtom results file for creating conditions leading to shortage in 2016
mtomResFile <- '../CRSS.2015/MTOM/FirstYearCondMTOM/MayMTOMResults.csv'
shortCondFig <- 'shortConditionsFig.pdf'
# for the 5-year simple table
ss5 <- c('April CRSS', 'May CRSS')
# should match files for critStatsFile:
critStatsIn <- c('../CRSS.2015/results/April/CriticalElevationData_CRSS.Apr2015.csv',
                 '../CRSS.2015/figs/May_CritStats.csv')
yy5 <- 2016:2020
simple5YrFile <- 'May2015_5yrSimple.pdf'

# -------------------------------------------------------------------------------------
#                               END USER INPUT
# -------------------------------------------------------------------------------------

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
if(FALSE){
  getSritStatsData(scens, iFolder, paste0(resFolder,critStatsFile),TRUE, aggBasedOnIC)
}

if(FALSE){
print("starting to create figures and tables")
flush.console()
print("creating system conditions table")
flush.console()
## Create tables, figures, and data behind figures
# 1) system conditions table
sysCond <- read.table(paste0(resFolder,sysCondFile),header = T)
# trim to 2016-2026 and 30 trace ensemble I.C.; Agg == 1 limits to using 30 ensemble I.C. 
sysCond <- dplyr::filter(sysCond, Year %in% 2016:2026 & Agg == 1)
# create the system cond. table
sysTable <- CRSSIO::createSysCondTable(sysCond, 2016:2026)
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
powellPE <- plotEOCYElev(dplyr::filter(pe, Agg == 1), 2015:2026, 'Powell.Pool Elevation', 
                         'Powell End-of-December Year Elevation')
meadPE <- plotEOCYElev(dplyr::filter(pe, Agg == 1), 2015:2026, 'Mead.Pool Elevation', 
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
                                            Agg == 1), 2016:2026, 'May 2015')
  
# stacked barplot of different shortage tiers
# default for annSize is ok
shortStack <- plotShortStackedBar(dplyr::filter(sysCond, Variable %in% c('lbShortageStep1',
                                  'lbShortageStep2','lbShortageStep3')), 2016:2026, annText)

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
  sysCond <- dplyr::filter(sysCond, Year %in% 2016:2026 & Agg == 1)
  sysTable <- CRSSIO::createSysCondTable(sysCond, 2016:2026)
}
cp1 <- getConditionalProbs(sysCond, 2016, 2016, 'lbShortage','mer748')
cp2 <- getConditionalProbs(sysCond, 2016, 2016, 'lbShortage','ueb823')
cp3 <- getConditionalProbs(sysCond, 2016,2016, 'lbShortage',c('eq','uebGt823'))
cp4 <- getConditionalProbs(sysCond, 2017, 2016, c('lbShortage','lbShortageStep1','lbShortageStep2',
                                               'lbShortageStep3'), 'mer748')
cp5 <- getConditionalProbs(sysCond, 2017, 2016, c('lbShortage','lbShortageStep1','lbShortageStep2',
                                               'lbShortageStep3'), 'ueb823')
cp6 <- getConditionalProbs(sysCond, 2017, 2016, c('lbShortage','lbShortageStep1','lbShortageStep2',
                                               'lbShortageStep3'), c('eq','uebGt823'))

# create data table from the above values
cpt1 <- data.frame('ChanceOf' = c(paste(2016,names(cp1)),paste(2017,names(cp4))),
                   'PrctChance' = c(cp1,cp4))
rr <- which(rownames(sysTable$fullTable) == 'Mid-Elevation Release Tier - annual release = 7.48 maf')
cc <- which(colnames(sysTable$fullTable) == 2016)
cpt1$PowellWYRel <- paste('7.48 MAF;',sysTable$fullTable[rr,cc])

cpt2 <- data.frame('ChanceOf' = c(paste(2016,names(cp2)),paste(2017,names(cp5))),
                   'PrctChance' = c(cp2,cp5))
rr <- which(rownames(sysTable$fullTable) == "Upper Elevation Balancing - annual release = 8.23 maf")
cpt2$PowellWYRel <- paste('8.23 MAF;',sysTable$fullTable[rr,cc])

cpt3 <- data.frame('ChanceOf' = c(paste(2016,names(cp3)),paste(2017,names(cp6))),
                   'PrctChance' = c(cp3,cp6))
rr <- which(rownames(sysTable$fullTable) == "Upper Elevation Balancing - annual release > 8.23 maf")
rr2 <- which(rownames(sysTable$fullTable) == "Equalization - annual release > 8.23 maf")
cpt3$PowellWYRel <- paste('> 8.23 MAF;',sysTable$fullTable[rr,cc] + sysTable$fullTable[rr2,cc])

cpt1 <- rbind(cpt1,cpt2,cpt3)

# rearrange columns
cpt1 <- cpt1[c('PowellWYRel','ChanceOf','PrctChance')]
cpt1$PrctChance <- cpt1$PrctChance*100
write.csv(cpt1,paste0(oFigs,condProbFile),row.names = F)

# 4) Create the conditions leading to Shortage plot from MTOM results
# pulled annotation out of generic function
if(shortCondFig){
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

