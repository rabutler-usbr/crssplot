rm(list=ls())

setwd("C:\\Users\\JShirey\\Desktop\\RCodes\\Process-CRSS-Res-master")
#setwd("C:\\model\\CRSS\\Process-CRSS-Res")
#These lines are used to install CRSSIO from Alan's gitHUB
#install.packages("devtools")
#devtools::install_github("BoulderCodeHub/RWDataPlyr")
#devtools::install_github("BoulderCodeHub/CRSSIO")
library(CRSSIO)

library(reshape2)
library(grid)
library(gridExtra)
source('code\\makeScenNames.R')
source('code\\getSysCondData.R')
source('code\\dataTaggingFunctions.R')
source('code\\getEOCYData.R')
source('code\\plottingFunctions.R')
source('code\\getCondProbs.R')
source('code\\plotFirstYearShortCond.R')
source('code\\get5YrTable.R')
library(dplyr)
# script should create everything necessary for the results in order
CRSSDIR=Sys.getenv("CRSS_DIR")

# -------------------------------------------------------------------------------------
#                                    USER INPUT
# -------------------------------------------------------------------------------------
# scenarios are orderd model,supply,demand,policy,initial conditions
scens <- makeAllScenNames('DNF','CRSS','Jan2016','IG')		


#scens.limit should only include the 30 ensemble i.c. and not most/min/max runs
scens.limit <- makeAllScenNames('DNF','CRSS','Jan2016','IG')		

# folder with scenario folders created by RiverSMART
iFolder <- paste0(CRSSDIR,'\\Scenario')


resFolder <- paste0(CRSSDIR,'\\results\\') # folder to save procssed text files to (intermediate processed data)
sysCondFile <- 'JanSysCond_DNF_Test.txt' # file name of system conditions data; CODE CREATES THIS FILE		
curMonthPEFile <- 'Jan\\annualData_DNF_Jan.txt' # file name of Powell and Mead PE data; CODE CREATES THIS FILE		

icMonth <- '15-Dec' # IC are from December 2015
critStatsFile <- 'Jan_CritStats_DNF.txt' # file name for critical stats data; CODE CREATES THIS FILE		
sysCondTable <- 'Jan_SysDNFTableFull2016_2026.csv' # file name for the system conditions procssed file
prevMonthPEFile <- 'Jan\\annualData_DNF_Aug.txt' # file name that contains the previous CRSS run PE data		
#startMonthMap includes a map for the model name (from folder names), to a string that 
# will show up on plots
startMonthMap <- c('Jun2015_2016' = 'Jun 2015 VIC', 'May2015_2016' = 'May 2015 VIC', 
	'Apr2015_2016_a3' = 'Apr 2015 VIC', 'Aug2015' = 'Aug 2015 VIC DCP','Aug2015_2016' = 'Aug 2015 VIC','Aug2015_2016v2' = 'Aug 2015 VIC2')		
oFigs <- paste0(CRSSDIR,'\\figs\\01_Jan\\') # folder location to save figures and fully procssed tables
eocyFigs <- 'Jan2016_MPEOCY_DNF_Test.pdf' # file name for figure with Powell and Mead 10/50/90 EOCY elevations		
annText <- 'Results from the January 2016 CRSS Run DNF' # text that will be added to figures		
critStatsProc <- 'Jan_CritStats_DNF_Test.csv'		
critFigs <- 'Jan2016_CritFigs_DNF_Test.pdf'		
condProbFile <- 'Jan2016_CondProbs_DNF_Test.csv'		
# mtom results file for creating conditions leading to shortage in 2016
#mtomResFile <- paste0(CRSSDIR,'/MTOM/FirstYearCondMTOM/JunMTOMResults.csv')
shortCondFig <- 'shortConditionsFig_DNF_Test.pdf'		
# for the 5-year simple table
ss5 <- c('Apr2015 CRSS','Jan2016 CRSS')
# should match files for critStatsFile:
critStatsIn <- paste0(CRSSDIR,c('/results/CriticalElevationData_CRSS.Apr2015.csv',
                 '\\figs\\01_Jan\\Jan_CritStats_DNF_Test.csv'))		
yy5 <- 2016:2020
simple5YrFile <- 'Jan2016_5yrSimple_DNF_Test.pdf'		
createShortConditions <- TRUE

# -------------------------------------------------------------------------------------
#                               END USER INPUT
# -------------------------------------------------------------------------------------

## System Conditions Table Data
if(TRUE){
  print('starting getSysCondData')
  flush.console()
  getSysCondData(scens, iFolder, paste0(resFolder,sysCondFile),FALSE, aggBasedOnIC)
  print('finished getSysCondData')
  flush.console()
}

if(TRUE){
  ## get the Mead and Powel EOCY Data
  getPowellMeadEOCYPE(scens, iFolder, paste0(resFolder,curMonthPEFile), FALSE, aggBasedOnIC)
}

## Get Crit Stats Data
if(TRUE){
  print('starting getCritStats')
  flush.console()
  getCritStatsData(scens, iFolder, paste0(resFolder,critStatsFile),FALSE, aggBasedOnIC)
  print('finished getCrityStats')
  flush.console()
}

## Create the KeySlots csv file, but only want to include data for the 30 Ensemble and not
## the Most or MTOM_Most
print('Creating KeySlots csv file')
flush.console()
RWDataPlyr::getDataForAllScens(scens.limit,scens.limit,RWDataPlot::createSlotAggList('data/KeySlotsProcess.csv'), 
                               iFolder, paste0(oFigs,'KeySlots.txt'))

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
#sysCond <- dplyr::filter(sysCond, Year %in% 2016:2026 & Agg == 1)
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
powellPE <- plotEOCYElev(dplyr::filter(pe), 2015:2026, 'Powell.Pool Elevation', ###Removed Agg = 1 in dplyr::filter(pe, Agg ==1) b/c not aggregating
                         'Powell End-of-December Year Elevation')
meadPE <- plotEOCYElev(dplyr::filter(pe), 2015:2026, 'Mead.Pool Elevation', ###SAA
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
critStatsFig1 <- plotCritStats(dplyr::filter(critStats, Variable != 'meadLt1020'), ###Removed Agg = 1 in dplyr::filter(critStats, Agg ==1) b/c not aggregating
                               2016:2026, annText)
critStatsFig2 <- plotCritStats(dplyr::filter(critStats, Variable != 'meadLt1025'), ###Removed Agg = 1 in dplyr::filter(critStats, Agg ==1) b/c not aggregating
                               2016:2026, annText)
# create data table to save crit stats
cs <- dplyr::filter(critStats, Year %in% 2016:2026)###Removed Agg = 1 in dplyr::filter(critStats, Agg ==1) b/c not aggregating

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
ssPlot <- plotShortageSurplus(dplyr::filter(sysCond, Variable %in% c('lbShortage', 'lbSurplus')),
												2016:2026, 'May 2015')###Removed Agg = 1
  
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
  sysCond <- dplyr::filter(sysCond, Year %in% 2016:2026 & Agg == 1)###Removed & Agg = 1 end of parenthesis
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

# pulled annotation out of generic function


## create the 5-yr simple table that compares to the previous run
#simple5Yr <- creat5YrSimpleTable(ss5, critStatsIn, yy5)
#pdf(paste0(oFigs,simple5YrFile),width = 8, height = 8)
#print(simple5Yr)
#dev.off()


#rm(list = ls())




































