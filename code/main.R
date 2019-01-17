
library(CRSSIO)
stopifnot(packageVersion("CRSSIO") >= "0.6.3")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(feather))
suppressPackageStartupMessages(library(stringr))
library(RWDataPlyr)
suppressPackageStartupMessages(library(data.table))
source('code/plot_nameFunctions.r')
source('code/getScenarioData.R')
source('code/dataTaggingFunctions.R')
source('code/getICPEData.R')
source('code/plottingFunctions.R')
source('code/getCondProbs.R')
source('code/plotFirstYearShortCond.R')

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    -------------------        USER INPUT        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# script should create everything necessary for the results in order
# CRSSDIR is the CRSS_DIR environment variable that will tell the code where to
# store the intermediate data and figures/tables created here
# iFolder is a path to the top level crss directory that contains the model 
# output it could be the same as CRSSDIR, but is allowed to be different so that 
# you can read model output from the server, but save figures locally.

# swtiches to read data. if you've already read the data in from rdfs once, 
# you may be able to set this to FALSE, so it's faster
getSysCondData <- TRUE
getPeData <- FALSE
get_crss_short_cond_data <- FALSE

# "switches" to create/not create different figures
# typical figures
makeFiguresAndTables <- TRUE
pdf_name <- 'Jan2019PowerRun.pdf'
createSimple5yrTable <- FALSE

# optional figures/tables
createShortConditions <- FALSE
computeConditionalProbs <- FALSE
addPEScatterFig <- FALSE

# ** make sure CRSS_DIR is set correctly before running

CRSSDIR <- "C:/alan/CRSS/CRSS.2019" #Sys.getenv("CRSS_DIR")
iFolder <- "M:/Shared/CRSS"
# set crssMonth to the month CRSS was run. data and figures will be saved in 
# a folder with this name
crssMonth <- "january_2019_power"

# scenarios are orderd model,supply,demand,policy,initial conditions 
# (if initial conditions are used) scens should be a list, each entry is a 
# scenario group name, and the entry is a character vector of length 1 to n of 
# individual scenarios. all of the values in each entry of the list are combined 
# together and processed as one scenario group. So for a run that has 30 initial 
# conditions, all 30 runs are averaged/combined together. The names in the scens 
# list (scenario Groups) will be the Scenario names that show up on plots.

# *** the names of scens, icList, and icMonth should all match.

# in the comma seperated scenario folder names, currently the 5th entry is the 
# initial conditions entry
# update if for some reason the scenario naming convention has changed
icDimNumber <- 5 

scens <- list(
  #"August 2018 - DNF" = "Aug2018_2019,DNF,2007Dems,IG,Most",
  # "April 2018" = 
  #   rw_scen_gen_names("Apr2018_2019", "DNF", "2007Dems", "IG", 1981:2015),
  # "April 2018 - most" = "Apr2018_2019,DNF,2007Dems,IG,MTOM_most",
  "August 2018" = "2018/Scenario/Aug2018_2019,DNF,2007Dems,IG,Most",
  "January 2019" = "2019/Scenario/Jan2019_2019,DNF,2007Dems,IG,Hist"
)

legendWrap <- 20 # setting to NULL will not wrap legend entries at all

# for each scenario group name, it should be either 2 numbers or 2 file paths, 
# both ordered powell, then mead.

icList <- list(
  "August 2018" = c(3586.55, 1079.50),
  "January 2019" = c(3581.85, 1081.46)
  # "April 2018" = file.path(
  #   CRSSDIR,
  #   "dmi/InitialConditions/april_2018/MtomToCrss_Monthly.xlsx"
  # ),
  # "April 2018 - most" =  c(3589.78, 1079.07)
)

# The month in YY-Mmm format of the intitial condtions for each scenario group
icMonth <- c(
  "August 2018" = "18-Dec",
  "January 2019" = "18-Dec"
)

# for the 5-year simple table
# value are the scenario group variable names (should be same as above)
# the names are the new names that should show up in the table in case you need 
# to add a footnote or longer name
# this is the order they will show up in the table, so list the newest run 
# second there should only be 2 scenarios
ss5 <- c(
  "August 2018" = "August 2018"
)

# this should either be a footnote corresponding to one of the ss5 names or NA
tableFootnote <- NA

# years to use for the simple 5-year table
yy5 <- 2019:2023

# the mainScenGroup is the scenario to use when creating the current month's 
# 5-year table, etc. In the plots, we want to show the previous months runs,
# but in the tables, we only want the current month run. This should match names
# in scens and icList
mainScenGroup <- "January 2019"
mainScenGroup.name <- mainScenGroup

# text that will be added to figures
annText <- 'Results from January 2019 CRSS Run' 

# how to label the color scale on the plots
colorLabel <- 'Scenario'

yrs2show <- 2019:2060 # years to show the crit stats figures
peYrs <- 2018:2060 # years to show the Mead/Powell 10/50/90 figures for

# mead pe scatter parameters -------------------------------
# plot a single year of Mead PE
peScatterYear <- 2019
# peScatterData should be set to either MTOM or CRSS
# if relying on combined run, then this is likely MTOM; if using a CRSS only 
# run, then likely set to CRSS
peScatterData <- 'CRSS'

conditionsFrom <- "CRSS" # string should be either CRSS or MTOM

# yearToAnalyze is used in the plot labeling. This is typically the first year
# of the MTOM run, e.g., 2017 for a January 2017 MTOM run, or the year before
# the first year of shortage for a CRSS run, i.e., it uses the December elev.
# at MEad for that year, and the OND release from that year from Powell
yearToAnalyze <- 2019
if (conditionsFrom == "CRSS") {
  resFile <- file.path(CRSSDIR,'results', crssMonth, 'tempData')
  # set scenario to NA if using MTOM or 
  # to the main scenario folder if using CRSS
  scenario <- scens[[mainScenGroup]]
  
  short_cond_color_var <- "mwdIcs" # WYRelease or mwdIcs
  
  # the label for the percent of average
  lbLabel <- "Total LB natural inflow percent\nof average (1906-2015)"
  shortCondSubTitle <- "Results from the August 2018 CRSS run, based on projected December 31, 2018 conditions from the August 2018 24-Month Study."
} else if (conditionsFrom == "MTOM") {
  # see doc/README for instructions for how to create this csv file
  resFile <- paste0(CRSSDIR,'/MTOM/FirstYearCondMTOM/Jan2018MTOMResults.csv') 
  scenario <- NA
  short_cond_color_var <- "WYRelease" #only WYRelease
  # the label for the percent of average
  lbLabel <- 'LB total side inflow percent\nof average (1981-2015)'
  shortCondSubTitle <- 'Results from the January 2018 MTOM run based on the January 3, 2017 CBRFC forecast' 
  
} else {
  stop("Invalid `conditionsFrom` value.")
}

shortCondTitle <- 'Conditions Leading to a Lower Basin Shortage in 2020'

#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    --------------      SETUP DIRECTORIES AND FILENAMES   -----------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# some sanity checks that UI is correct:
if(!(mainScenGroup %in% names(scens)))
  stop(mainScenGroup, ' is not found in scens.')
if(!(mainScenGroup %in% names(icList)))
  stop(mainScenGroup, ' is not found in icList')

# check that the names of scens, icList, and icMonth are all the same; they
# don't necessarily need to be in the same order, just all exist in one another
if(!all(names(scens) %in% names(icList), names(icList) %in% names(scens), 
        names(scens) %in% names(icMonth), names(icMonth) %in% names(scens),
        names(icList) %in% names(icMonth), names(icMonth) %in% names(icList)))
  stop("scenario group names do not match.",
       "\nthe names() of scens, icList, and icMonth should all be the same")

# if we made it here, we know names() of scens, icList, and icMonth all match, 
# so just check to make sure that ss5 is withing scens
if(!all(names(ss5) %in% names(scens)))
  stop("scenario goup names of ss5 must match the names found in scens")

# onlyl check if reading in data if you have to getData
if(getPeData | getSysCondData){
  message('Scenario data will be read in from: ', iFolder)
  if(!file.exists(iFolder))
    stop(iFolder, ' does not exist. Please ensure iFolder is set correctly.')
}

# folder location to save figures and fully procssed tables
if (!file.exists(CRSSDIR)) {
  stop(
    CRSSDIR, 
    ' does not exist. Please ensure CRSS_DIR environment variable is set correctly'
  )
}

if (!file.exists(file.path(CRSSDIR, 'results'))) {
  message(paste(file.path(CRSSDIR, 'results'),
                'does not exist. Creating this folder...'))
  dir.create(file.path(CRSSDIR, 'results'))
}

oFigs <- file.path(CRSSDIR,'results', crssMonth) 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', oFigs))
  dir.create(oFigs)
}
message('Figures and tables will be saved to: ', oFigs)

# folder to save procssed text files to (intermediate processed data)
resFolder <- file.path(CRSSDIR,'results', crssMonth, 'tempData')
if (!file.exists(resFolder)) {
  message(paste('Creating folder:', resFolder))
  dir.create(resFolder)
}
message('Intermediate data will be saved to: ', resFolder)

sysCondFile <- 'SysCond.feather' # file name of system conditions data
tmpPEFile <- 'tempPE.feather'
curMonthPEFile <- 'MeadPowellPE.feather' # file name of Powell and Mead PE data

# file name for the system conditions procssed file
sysCondTable <- paste0('SysTableFull',yrs2show[1],'_',tail(yrs2show,1),'.csv') 


critStatsProc <- 'CritStats.csv'
condProbFile <- 'CondProbs.csv'

shortCondFig <- 'shortConditionsFig.pdf'

simple5YrFile <- '5yrSimple.pdf'

traceMap <- read.csv('data/Trace2IcMap.csv')

# *****************************************************************************
#       Process results --------------
# *****************************************************************************

# System Conditions Table Data
if (getSysCondData) {
  message('starting getSysCondData')
  
  # system condition rwa
  sys_rwa <- CRSSIO::sys_cond_rwa()
  if (packageVersion("CRSSIO") <= "0.7.0")
    sys_rwa$period <- "eocy"
  
  getScenarioData(
    scens, 
    iFolder, 
    file.path(resFolder,sysCondFile),
    TRUE,
    'aggFromScenList', 
    sys_rwa
  )
  message('finished getSysCondData')
}

if(getPeData){
  ## get the Mead and Powel EOCY Data
  message('starting getPeData')
  pe_rwa <- rwd_agg(read.csv(
    "data/MPPEStats_sam.csv", 
    stringsAsFactors = FALSE
  ))
  getScenarioData(scens, iFolder, file.path(resFolder,tmpPEFile), TRUE, 
                  'aggFromScenList', pe_rwa)
  ## append initial conditions onto May data
  getAndAppendIC(scens, file.path(resFolder,tmpPEFile), 
                 file.path(resFolder,curMonthPEFile), icList, icMonth, 
                 TRUE, 'aggFromScenList', traceMap, icDimNumber = icDimNumber)
  
  message('finished getPeData')
}

if (get_crss_short_cond_data) {
  message("Starting to get CRSS shortage condition data...")
  
  get_shortcond_from_rdf(
    scenario = scens[[mainScenGroup]], 
    iFolder = iFolder, 
    oFolder = resFolder
  )
  
  message("Done getting CRSS shortage condition data")
}

if(makeFiguresAndTables){
  message("starting to create figures and tables")
  message("creating system conditions table")
  ## Create tables, figures, and data behind figures
  # 1) system conditions table -------------
  sysCond <- read_feather(file.path(resFolder,sysCondFile)) %>%
    # trim to specified years and the current main scenario group 
    dplyr::filter(Year %in% yrs2show & Agg == mainScenGroup)
  # create the system cond. table
  sysTable <- CRSSIO::crsso_get_sys_cond_table(sysCond, yrs2show)
  # save the sys cond table
  data.table::fwrite(
    as.data.frame(sysTable[['fullTable']]), 
    file.path(oFigs,sysCondTable), 
    row.names = TRUE
  )
  
  # 2) Plot Mead, Powell EOCY elvations -------------
  # includes previous month's results too
  # read in current month data
  message("EOCY elevation figures")
  pe <- read_feather(file.path(resFolder,curMonthPEFile)) %>%
    # The StartMonth column is used as the color variable in plotEOCYElev, and 
    # the names that should show up in the legend/differentiate scenario groups
    # are stored in the Agg Varaible. So easiest to just copy it from Agg to 
    # StartMonth for now
    dplyr::mutate(StartMonth = Agg)

  # plot
  powellPE <- plotEOCYElev(pe, peYrs, 'Powell.Pool Elevation', 
                           'Powell End-of-December Elevation', colorLabel,
                           legendWrap = legendWrap)
  meadPE <- plotEOCYElev(pe, peYrs, 'Mead.Pool Elevation', 
                           'Mead End-of-December Elevation', colorLabel, 
                         legendWrap = legendWrap)
  
  # 3) Critical elevation thresholds; figures and data table -------
  # have sysCond for some, and read in crit stats for others
  message("starting critical stats")
  
  # compare crit stats for all scenarios
  # call once each for powell LT 3490, shortage, and surplus
  # get the necessary variables by filtering from the pe and syscond data files
  cs <- pe %>%
    filter(
      Variable %in% c('meadLt1000', 'meadLt1020', 'powellLt3490', 
                      'powellLt3525', 'meadLt1025')
    ) %>%
    mutate(AggName = Agg) %>%
    select(-StartMonth)
  rm(pe) # don't need pe any longer
  
  cs <- read_feather(file.path(resFolder,sysCondFile)) %>%
    mutate(AggName = Agg) %>%
    filter(Variable %in% c('lbSurplus', 'lbShortage')) %>%
    mutate(AggName = Agg) %>%
    rbind(cs)

  ptitle <- paste(
    'Powell: Percent of Traces Less than Power Pool', 
    "(elevation 3,490\') in Any Water Year",
    sep = "\n"
  )
  
  p3490Fig <- compareCritStats(
    cs, 
    yrs2show, 
    'powellLt3490', 
    '', 
    ptitle, 
    colorLabel, 
    legendWrap = legendWrap
  )
  
  shortTitle <- 'Lower Basin: Percent of Traces in Shortage Conditions'
  shortFig <- compareCritStats(cs, yrs2show, 'lbShortage', '', shortTitle, 
                               colorLabel, legendWrap = legendWrap)
  surpTitle <- 'Lower Basin: Percent of Traces in Surplus Conditions'
  surpFig <- compareCritStats(cs, yrs2show, 'lbSurplus', '', surpTitle, 
                              colorLabel, legendWrap = legendWrap)
  
  # now create figures only for the current "main scenario"
  # defaults are ok for legendTitle, legLoc, nC, and annSize
  # drop Mead LT 1025 from one plot and Mead LT 1020 from 
  # the other plot

  critStatsFig1 <- plotCritStats(dplyr::filter(
      cs, 
      Agg == mainScenGroup, 
      !(Variable %in% c('meadLt1020','lbSurplus'))
    ), 
    yrs2show, 
    annText
  )
  
  critStatsFig2 <- plotCritStats(dplyr::filter(
      cs, 
      Agg == mainScenGroup, 
      !(Variable %in% c('meadLt1025','lbSurplus'))
    ), 
    yrs2show, 
    annText
  )

  csVars <- csVarNames()
  # create data table to save crit stats
  cs <- cs %>%
    dplyr::filter(
      Year %in% yrs2show, 
      Agg == mainScenGroup, 
      Variable != 'lbSurplus'
    ) %>%
    # compute the percent of traces by averaging values 
    group_by(Year,Variable) %>%
    summarise(Value = mean(Value)) %>%
    dplyr::mutate(vName = csVars[Variable]) %>%
    # reshape to be easier to print out
    ungroup() %>%
    select(-Variable) %>%
    tidyr::spread(vName, Value)
  
  # shortage surplus figure
  # defaults ok for legendTitle, nC, and legLoc
  ssPlot <- plotShortageSurplus(
    dplyr::filter(
      sysCond, 
      Variable %in% c('lbShortage', 'lbSurplus'),
      Agg == mainScenGroup
      ), 
    yrs2show, 
    mainScenGroup.name
  )
    
  # stacked barplot of different shortage tiers
  # default for annSize is ok
  shortStack <- plotShortStackedBar(
    dplyr::filter(
      sysCond, 
      Variable %in% c('lbShortageStep1','lbShortageStep2','lbShortageStep3'),
      Agg == mainScenGroup
    ), 
    yrs2show, 
    annText
  )

# save figures and table
  message("creating pdf")
  pdf(file.path(oFigs, pdf_name),width = 8, height = 6)
  print(powellPE)
  print(meadPE)
  print(p3490Fig)
  print(shortFig)
  print(surpFig)
  print(critStatsFig1)
  print(critStatsFig2)
  print(ssPlot)
  print(shortStack)
  dev.off()
  data.table::fwrite(cs,file.path(oFigs,critStatsProc),row.names = F)
}

if(computeConditionalProbs){
  ## CONDITIONAL PROBABILITIES
  # use sysCond
  if(is.na(match('sysCond',ls()))){
    sysCond <- read.table(paste0(resFolder,sysCondFile),header = T) 
    sysCond <- dplyr::filter(sysCond, Year %in% yrs2show & Agg == 1)
    sysTable <- CRSSIO::createSysCondTable(sysCond, yrs2show)
  }
  cp1 <- getConditionalProbs(
    sysCond, 
    yrs2show[1], 
    yrs2show[1], 
    'lbShortage',
    'mer748'
  )
  cp2 <- getConditionalProbs(
    sysCond, 
    yrs2show[1], 
    yrs2show[1], 
    'lbShortage',
    'ueb823'
  )
  cp3 <- getConditionalProbs(
    sysCond, 
    yrs2show[1],
    yrs2show[1], 
    'lbShortage',
    c('eq','uebGt823')
  )
  cp4 <- getConditionalProbs(
    sysCond, 
    yrs2show[2], 
    yrs2show[1], 
    c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
    'mer748'
  )
  cp5 <- getConditionalProbs(
    sysCond, 
    yrs2show[2], 
    yrs2show[1], 
    c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
    'ueb823'
  )
  cp6 <- getConditionalProbs(
    sysCond, 
    yrs2show[2], 
    yrs2show[1], 
    c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
    c('eq','uebGt823')
  )
  
  # create data table from the above values
  cpt1 <- data.frame(
    'ChanceOf' = c(paste(yrs2show[1],names(cp1)),paste(yrs2show[2],names(cp4))),
    'PrctChance' = c(cp1,cp4)
  )
  rr <- which(
    rownames(sysTable$fullTable) == 
      'Mid-Elevation Release Tier - annual release = 7.48 maf'
  )
  cc <- which(colnames(sysTable$fullTable) == yrs2show[1])
  cpt1$PowellWYRel <- paste('7.48 MAF;',sysTable$fullTable[rr,cc])
  
  cpt2 <- data.frame(
    'ChanceOf' = c(paste(yrs2show[1],names(cp2)),paste(yrs2show[2],names(cp5))),
    'PrctChance' = c(cp2,cp5)
  )
  rr <- which(
    rownames(sysTable$fullTable) == 
      "Upper Elevation Balancing - annual release = 8.23 maf"
  )
  cpt2$PowellWYRel <- paste('8.23 MAF;',sysTable$fullTable[rr,cc])
  
  cpt3 <- data.frame(
    'ChanceOf' = c(paste(yrs2show[1],names(cp3)),paste(yrs2show[2],names(cp6))),
    'PrctChance' = c(cp3,cp6)
  )
  rr <- which(
    rownames(sysTable$fullTable) == 
      "Upper Elevation Balancing - annual release > 8.23 maf"
  )
  rr2 <- which(
    rownames(sysTable$fullTable) == "Equalization - annual release > 8.23 maf"
  )
  cpt3$PowellWYRel <- paste(
    '> 8.23 MAF;',
    sysTable$fullTable[rr,cc] + sysTable$fullTable[rr2,cc]
  )
  
  cpt1 <- rbind(cpt1,cpt2,cpt3)
  
  # rearrange columns
  cpt1 <- cpt1[c('PowellWYRel','ChanceOf','PrctChance')]
  cpt1$PrctChance <- cpt1$PrctChance*100
  data.table::fwrite(cpt1,paste0(oFigs,condProbFile),row.names = F)
}

# conditions leading to shortage ---------------------------------
# pulled annotation out of generic function
if (createShortConditions) {
  if (length(resFile) > 1)
    stop(
      "conditions leading to shortage is only designed to work with 1 scenario"
    )
  
  message(
    'Using hard coded values for the arrow in the shortage conditions figure.',
    '\nYou may need to update the values and re-run main.R'
  )
  # filterOn being set to pe shows results for traces that are <= 1077
  shortCond <- plotFirstYearShortCond(
    conditionsFrom, 
    iFile = resFile, 
    scenario, 
    filterOn = 'pe', 
    yearToAnalyze,
    colorVar = short_cond_color_var
  )
  shortCond <- shortCond + 
    annotate('segment', x = 16.9, xend = 14.5, y = 1055, yend = 1056.4, 
           arrow = grid::arrow(length = unit(.3,'cm')),size = 1) +
    annotate(
      'text', 
      x = 17, 
      y = 1054,
      label = lbLabel, 
      size = 4, 
      hjust = 0
    ) +
    labs(title = shortCondTitle, caption = shortCondSubTitle) +
    theme(legend.title = element_text(size = 10))
  
  ggsave(
    file.path(oFigs,paste(short_cond_color_var, shortCondFig, sep = "_")),
    plot = shortCond,
    units = "in",
    width = 9, 
    height = 6
  )
}

# 5 year simple table -------------------------
if(createSimple5yrTable){
  ## create the 5-yr simple table that compares to the previous run
  message("creating 5-year simple table")
  zz <- read_feather(file.path(resFolder, sysCondFile)) %>%
    rbind(read_feather(file.path(resFolder,curMonthPEFile))) 
  simple5Yr <- create5YrSimpleTable(zz, ss5, yy5, tableFootnote)
  pdf(file.path(oFigs,simple5YrFile),width = 8, height = 8)
  print(simple5Yr)
  dev.off()
  rm(zz)
}

# mead pe scatter ------------------
if (addPEScatterFig) {
  message("elevation scatter plot figure")
  ### This did not properly compile for the January run.
  if (peScatterData == "CRSS") {
    pe <- read_feather(file.path(resFolder,curMonthPEFile)) %>%
      filter(Agg == mainScenGroup)
  } else if (peScatterData == "MTOM") {

    icDim <- 1981:2015
    tmpIcMonth <- paste(str_replace(peScatterYear, "20", ""), "Dec", sep = "-")
    decVals <- do.call(
      rbind, 
      lapply(icDim, get1TraceIc, icList[[mainScenGroup]], tmpIcMonth, traceMap)
    )
    traceNum <- traceMap$trace[match(icDim, traceMap$ic)]
    
    pe <- decVals %>%
      select(`Mead.Pool Elevation`) %>%
      rename(Value = `Mead.Pool Elevation`) %>%
      mutate(TraceNumber = as.numeric(traceNum), 
             Year = peScatterYear,
             Variable = "Mead.Pool Elevation")
    
  } else {
    stop("Invalid peScatterData variable")
  }
  scatterTitle <- paste('Lake Mead December', peScatterYear, 'Elevations from',
                        peScatterData)

  gg <- singleYearPEScatter(pe, peScatterYear, 'Mead.Pool Elevation', 
                          scatterTitle, TRUE)
  
  tpath <- file.path(oFigs, paste0('meadScatterFigure_', peScatterYear, '.pdf'))
  pdf(tpath, width = 8, height = 6)
  print(gg)
  dev.off()
}

