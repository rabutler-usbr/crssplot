
rm(list=ls())

library(CRSSIO)
library(dplyr)
library(reshape2)
library(grid)
library(feather)
library(tidyr)
library(stringr)
library(RWDataPlyr)
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

# ** make sure CRSS_DIR is set correctly before running

CRSSDIR <- Sys.getenv("CRSS_DIR")
iFolder <- 'M:/Shared/CRSS/2017/Scenario'
# set crssMonth to the month CRSS was run. data and figures will be saved in 
# a folder with this name
crssMonth <- 'Jan2017_2018'

# scenarios are orderd model,supply,demand,policy,initial conditions (if initial conditions are used)
# scens should be a list, each entry is a scenario name, and the entry is a 
# character vector of length 1 to n. 
# all of the values in each entry of the list are combined together and processed
# as one scenario. So for a run that has 30 initial conditions, all 30 runs are 
# averaged/combined together. the name of the entries in the list are used for 
# the scenario name

#scens <- list(
#  'Jan2018' = 'Jan2017_2018,DNF,2007Dems,IG,1981','Jan2017_2018,DNF,2007Dems,IG,1982'
#  'Jan2017_SingleRun' = 'Aug2016_2017_v25,DNF,CT,IG,DCP',
#  'Aug2017' = 'Aug2016_2017_v25,DNF,CT,IG,USMXDCP'
#)
scens <- list('Jan2018' = makeAllScenNames('Jan2017_2018','DNF','2007Dems','IG',c(1981:2015)),
                'Aug2017' = 'Aug2016_2017,DNF,2007Dems,IG'
              )              
# for each group name, it should be either 2 number or 2 file paths, both ordered
# powell, then mead.
icList <- list(
  'Jan2018' = c(paste0(CRSSDIR,'/MTOM/MTOM_JAN17_PowellPE.csv'), paste0(CRSSDIR,'/MTOM/MTOM_JAN17_MeadPE.csv')),
  'Aug2017' = c(3605.83, 1078.93)
)

# the mainScenGroup is the scenario to use when creating the current month's 
# 5-year table, etc. In the plots, we want to show the previous months runs,
# but in the tables, we only want the current month run. This should match names
# in scens and icList
mainScenGroup <- 'Jan2018'
mainScenGroup.name <- 'January Official'

# IC for each run
icMonth <- c('Jan2018' = '17-Dec', 'Aug2017' = '16-Dec') 

# startMonthMap includes a map for the model name (from folder names), to a string that 
# will show up on plots;
# this should use the folder name, no the shortened name from icMonth, or icList
startMonthMap <- c('Jan2017_2018' = 'Jan 2017 Official',
                   'Aug2016_2017' = 'Aug 2016 Official')

yrs2show <- 2018:2026 # years to show the crit stats figures
peYrs <- 2016:2026 # years to show the Mead/Powell 10/50/90 figures for

# -------------------------------
# plot a single year of Mead PE
peScatterYear <- 2017
# peScatterData should be set to either MTOM or CRSS
# if relying on combined run, then this is likely MTOM; if using a CRSS only run,
# then likely set to CRSS
peScatterData <- 'MTOM'

annText <- 'Results from January 2017 CRSS Run' # text that will be added to figures

# -------------------------------
# Conditions leading to shortage from MTOM
# mtom results file for creating conditions leading to shortage in 2016
mtomResFile <- paste0(CRSSDIR,'/MTOM/FirstYearCondMTOM/JanMTOMResults.csv') #changed to may b/c jun results file DNE
# yearToAnalyze is used in the plot labeling. This is typically the first year
# of the MTOM run, e.g., 2017 for a January 2017 MTOM run
yearToAnalyze <- 2017
shortCondTitle <- 'Conditions Leading to a Lower Basin Shortage in 2018'
shortCondSubTitle <- 'Results from the January 2017 MTOM run based on the January 17, 2017 CBRFC forecast' 

# for the 5-year simple table
# names are the names that will show up in the 5-year simple table
# the values are the Scenario Group variable names that will be filtered from the
# critStats file
# this is the order they will show up in the table, so list the newest run second
ss5 <- c('Aug2017' = 'Aug 2016 Official', 'Jan2018' = 'Jan 2017 Official')
# this should either be a footnote corresponding to one of the ss5 names or NA
tableFootnote <- ''
  
# years to use for the simple 5-year table
yy5 <- 2018:2022

# "switches" to create/not create different figures
getSysCondData <- FALSE
getPeData <- FALSE
getCSData <- FALSE
createKeySlotsCsv <- FALSE
makeFiguresAndTables <- TRUE
createShortConditions <- TRUE
computeConditionalProbs <- FALSE
createSimple5yrTable <- TRUE
addPEScatterFig <- TRUE

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

message('Scenario data will be read in from: ', iFolder)
if(!file.exists(iFolder))
  stop(iFolder, ' does not exist. Please ensure iFolder is set correctly.')

# folder location to save figures and fully procssed tables
if(!file.exists(CRSSDIR))
  stop(CRSSDIR, 
       ' does not exist. Please ensure CRSS_DIR environment variable is sest correctly')

if(!file.exists(file.path(CRSSDIR, 'results'))){
  message(paste(file.path(CRSSDIR, 'results'),
                'does not exist. Creating this folder...'))
  dir.create(file.path(CRSSDIR, 'results'))
}

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
  RWDataPlyr::getDataForAllScens(scens, scens,
                                 RWDataPlyr::createSlotAggList('data/KeySlotsProcess.csv'), 
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
                           'Powell End-of-December Elevation')
  meadPE <- plotEOCYElev(pe, peYrs, 'Mead.Pool Elevation', 
                           'Mead End-of-December Elevation')
  
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
  
  # compare crit stats; call once each for powell LT 3490, shortage, and surplus
  cs <- critStats %>%
    mutate(AggName = ss5[Agg])
  ptitle <- 'Powell: Percent of Traces Less than Power Pool\n(elevation 3,490\') in Any Water Year'
  p3490Fig <- compareCritStats(cs, yrs2show, 'powellLt3490', '', ptitle, 'Scenario')
  shortTitle <- 'Lower Basin: Percent of Traces in Shortage Conditions'
  shortFig <- compareCritStats(cs, yrs2show, 'lbShortage', '', shortTitle, 'Scenario')
  
  surpTitle <- 'Lower Basin: Percent of Traces in Surplus Conditions'
  surpFig <- compareCritStats(cs, yrs2show, 'lbSurplus', '', surpTitle, 'Scenario')
  
  # defaults are ok for legendTitle, legLoc, nC, and annSize
  # drop Mead LT 1025 from one plot and Mead LT 1020 from 
  # the other plot
  critStatsFig1 <- plotCritStats(dplyr::filter(critStats, Agg == mainScenGroup, 
                                               !(Variable %in% c('meadLt1020','lbSurplus'))), 
                                 yrs2show, annText)
  critStatsFig2 <- plotCritStats(dplyr::filter(critStats, Agg == mainScenGroup, 
                                               !(Variable %in% c('meadLt1025','lbSurplus'))), 
                                 yrs2show, annText)
  # create data table to save crit stats
  cs <- dplyr::filter(critStats, Year %in% yrs2show, Agg == mainScenGroup, Variable != 'lbSurplus')
  
  # rename the variables to strings
  cs$vName <- 'LB Shortage'
  cs$vName[cs$Variable == 'meadLt1000'] <- 'Mead < 1,000\' in Any Month'
  cs$vName[cs$Variable == 'meadLt1020'] <- 'Mead < 1,020\' in Any Month'
  cs$vName[cs$Variable == 'meadLt1025'] <- 'Mead < 1,025\' in Any Month'
  cs$vName[cs$Variable == 'powellLt3490'] <- 'Powell < 3,490\' in Any Month'
  cs$vName[cs$Variable == 'powellLt3525'] <- 'Powell < 3,525\' in Any Month'
  
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
  print(p3490Fig)
  print(shortFig)
  print(surpFig)
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
  lbLabel <- 'LB total side inflow percent\nof average (1981-2015)'
  message('Using hard coded values for the arror in the shortage conditions figure.\n',
          'You may need to update the values and re-run main.R')
  # filterOn being set to pe shows results for traces that are <= 1077
  shortCond <- plotFirstYearShortCond(mtomResFile, filterOn = 'pe', yearToAnalyze)
  shortCond <- shortCond + annotate('segment', x = 7.2, xend = 6.4, y = 1070.4, yend = 1070.7, 
           arrow = grid::arrow(length = unit(.3,'cm')),size = 1) +
    annotate('text', x = 7.3, y = 1070.3,label = lbLabel, size = 4, hjust = 0) +
    ggtitle(shortCondTitle, subtitle = shortCondSubTitle)
  
  pdf(file.path(oFigs,shortCondFig),width = 9, height = 6)
  print(shortCond)
  dev.off()
}

if(createSimple5yrTable){
  ## create the 5-yr simple table that compares to the previous run
  simple5Yr <- creat5YrSimpleTable(ss5, file.path(resFolder,critStatsFile), yy5,
                                   tableFootnote)
  pdf(file.path(oFigs,simple5YrFile),width = 8, height = 8)
  print(simple5Yr)
  dev.off()
}

if(addPEScatterFig){
  message("elevation scatter plot figure")
  
  if(peScatterData == "CRSS"){
    pe <- read_feather(file.path(resFolder,curMonthPEFile)) %>%
      filter(Agg == mainScenGroup)
  } else if(peScatterData == "MTOM"){
    pe <- read.csv(icList[[mainScenGroup]][2]) %>%
      gather(Trace, Value, -X) %>%
      mutate(Trace = getMTOMTraceNumber(Trace, t1 = 1981, tLen = 35),
             Year = as.numeric(paste0('20', stringr::str_split_fixed(X,'-',2)[,1])),
             Month = stringr::str_split_fixed(X,'-',2)[,2],
             Variable = 'Mead.Pool Elevation') %>%
      filter(Month == 'Dec')
  } else{
    stop("Invalid peScatterData variable")
  }
  scatterTitle <- paste('Lake Mead December', peScatterYear, 'Elevations from',
                        peScatterData)

  gg <- singleYearPEScatter(pe, peScatterYear, 'Mead.Pool Elevation', 
                          scatterTitle, TRUE)
  
  pdf(file.path(oFigs,paste0('meadScatterFigure_',peScatterYear,'.pdf')), width = 8, height = 6)
  print(gg)
  dev.off()
}

