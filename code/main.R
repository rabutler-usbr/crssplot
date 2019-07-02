
library(CRSSIO)
stopifnot(packageVersion("CRSSIO") >= "0.6.3")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(feather))
suppressPackageStartupMessages(library(stringr))
library(RWDataPlyr)
stopifnot(packageVersion("RWDataPlyr") >= "0.6.0")
suppressPackageStartupMessages(library(data.table))
source('code/plot_nameFunctions.r')
source('code/getScenarioData.R')
source('code/dataTaggingFunctions.R')
source('code/getICPEData.R')
source('code/plottingFunctions.R')
source('code/getCondProbs.R')
source('code/plotFirstYearShortCond.R')
source("code/compute_dcp_probs.R")
source("code/system_condition_heat_map.R")
source("code/crss_res_directory_setup.R")


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    -------------------        USER INPUT        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# script should create everything necessary for the results in order
# CRSSDIR is the CRSS_DIR environment variable that will tell the code where to
# store the intermediate data and figures/tables created here
# i_folder is a path to the top level crss directory that contains the model 
# output it could be the same as CRSSDIR, but is allowed to be different so that 
# you can read model output from the server, but save figures locally.

# swtiches to read data. if you've already read the data in from rdfs once, 
# you may be able to set this to FALSE, so it's faster
getSysCondData <- FALSE
getPeData <- FALSE
get_crss_short_cond_data <- FALSE

# "switches" to create/not create different figures
# typical figures
makeFiguresAndTables <- TRUE
pdf_name <- 'june_test.pdf'
createSimple5yrTable <- FALSE

# optional figures/tables
createShortConditions <- FALSE
computeConditionalProbs <- FALSE
addPEScatterFig <- FALSE
should_plot_clouds <- FALSE

# ** make sure CRSS_DIR is set correctly before running

CRSSDIR <- "C:/alan/CRSS/CRSS.2019" #Sys.getenv("CRSS_DIR")
i_folder <- "//128.138.214.42/bor/Shared/CRSS/2019/Scenario"
# set crssMonth to the month CRSS was run. data and figures will be saved in 
# a folder with this name
crssMonth <- "june2019_test"
# inserted onto some files. Can be ''
extra_label <- "full_"

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

jun_ensemble <- rw_scen_gen_names(
  "Jun2019_2020,DNF,2007Dems,IG_DCP", 
  paste0("Trace", 4:38), 
  "DCP_Cons"
)

jan_ensemble <- rw_scen_gen_names(
  "Jan2019_2020,DNF,2007Dems,IG", 
  1981:2015,
  "No_DCP_Cons"
)

jun_st_ensemble <- rw_scen_gen_names(
  "Jun2019_2020,ISM1988_2017,2007Dems,IG_DCP",
  paste0("Trace", 4:38),
  "DCP_Cons"
)

scens <- list(
  #"August 2018 - DNF" = "Aug2018_2019,DNF,2007Dems,IG,Most",
  # "April 2018" = 
  #   rw_scen_gen_names("Apr2018_2019", "DNF", "2007Dems", "IG", 1981:2015),
  # "April 2018 - most" = "Apr2018_2019,DNF,2007Dems,IG,MTOM_most",
  #"August 2018" = "2018/Scenario/Aug2018_2019,DNF,2007Dems,IG,Most",
  "June 2019 - Most" = "Jun2019_2020,DNF,2007Dems,IG_DCP,MTOM_Most,DCP_Cons",
  #"January 2019" = jan_ensemble,
  "June 2019 - No DCP" = "Jun2019_2020,DNF,NV300,IG,Most,No_DCP_Cons",
  
  #"June 2019" = jun_ensemble,
  #"June 2019 - Stress Test" = jun_st_ensemble,
  "June 2019 - Stress Test - No DCP" = 
    "Jun2019_2020,ISM1988_2017,NV300,IG,Most,No_DCP_Cons"
)

legendWrap <- 20 # setting to NULL will not wrap legend entries at all

# for each scenario group name, it should be either 2 numbers or 2 file paths, 
# both ordered powell, then mead.

jan_path <- file.path(
  CRSSDIR,
  "dmi/InitialConditions/jan_2019/MtomToCrss_Monthly.xlsx"
)

jun_path <- file.path(
  CRSSDIR, 
  "dmi/InitialConditions/june_2019/MtomToCrss_Monthly.xlsx"
)

icList <- list(
  #"August 2018" = c(3586.55, 1079.50),
  #"January 2019" = c(3581.85, 1081.46)
  # "January 2019 - 110" = file.path(
  #   CRSSDIR,
  #   "dmi/InitialConditions/jan_2019/MtomToCrss_Monthly.xlsx"
  # ),
  "June 2019 - Most" = c(3619.82, 1088.09),
  "June 2019 - No DCP" = c(3619.56, 1085.88),
  #"January 2019" = jan_path,
  #"June 2019" = jun_path,
  #"June 2019 - Stress Test" = jun_path,
  "June 2019 - Stress Test - No DCP" = c(3619.56, 1085.88)
)

# The month in YY-Mmm format of the intitial condtions for each scenario group
icMonth <- c(
  #"August 2018" = "18-Dec",
  #"January 2019" = "18-Dec",
  "June 2019 - Most" = "19-Dec",
  "June 2019 - No DCP" = "19-Dec",
  #"January 2019" = "19-Dec",
  #"June 2019" = "19-Dec",
  #"June 2019 - Stress Test" = "19-Dec",
  "June 2019 - Stress Test - No DCP" = "19-Dec"
)

# for the 5-year simple table
# value are the scenario group variable names (should be same as above)
# the names are the new names that should show up in the table in case you need 
# to add a footnote or longer name
# this is the order they will show up in the table, so list the newest run 
# second there should only be 2 scenarios
ss5 <- c(
  "June 2019 - Most" = "June 2019 - Most",
  "June 2019 - Stress Test - No DCP" = "June 2019 - Stress Test - No DCP"
)

# use this to select which scenarios are shown in the heat map, and what those
# scenarios should be labeled as in the heatmap. The names should be existing
# scenario names, and the values are what they will be labeled as in the heatmap
heatmap_names = c(
  "June 2019 - No DCP" = "Full", 
  "June 2019 - Stress Test - No DCP" = "Stresss Test"
)

# this should either be a footnote corresponding to one of the ss5 names or NA
tableFootnote <- NA

# years to use for the simple 5-year table
yy5 <- 2020:2024

# the mainScenGroup is the scenario to use when creating the current month's 
# 5-year table, etc. In the plots, we want to show the previous months runs,
# but in the tables, we only want the current month run. This should match names
# in scens and icList
mainScenGroup <- "June 2019 - Most"
names(mainScenGroup) <- mainScenGroup

# text that will be added to figures
annText <- 'Results from June 2019 CRSS Run' 

# how to label the color scale on the plots
colorLabel <- 'Scenario'

# the scenarios to show in Mead/Powell 10/50/90 plots, and the crit stats plots
#plot_scenarios <- c("June 2019", "January 2019", "June 2019 - No DCP")
#plot_scenarios <- c("June 2019 - Stress Test", "June 2019 - Stress Test - No DCP")
plot_scenarios <- c("June 2019 - Stress Test - No DCP", "June 2019 - Most")

# set plotting colors (optional)
# use scales::hue_pal()(n) to get default ggplot colors
plot_colors <- c("#F8766D", "#00BFC4")
#plot_colors <- c("#619CFF", "#F8766D", "#00BA38")
names(plot_colors) <- plot_scenarios

end_year <- 2060

yrs2show <- 2020:2026 # years to show the crit stats figures
peYrs <- 2019:2026 # years to show the Mead/Powell 10/50/90 figures for

# More descriptive labels for hydrologies used in the cloud plots
cloudScen <- c("January 2019 DNF","January 2019 ST")
cloudLabs <-c("January 2019 DNF" = "Jan \"Full\" Hydrology",
              "January 2019 ST" = "Jan \"Stress Test\" Hydrology")

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

crss_res_check_scen_names(scens, icList, icMonth, mainScenGroup, ss5, heatmap_names)
folder_paths <- crss_res_directory_setup(
  i_folder, 
  get_pe_data = getPeData, 
  get_sys_cond_data = getSysCondData,
  CRSSDIR = CRSSDIR,
  crss_month = crssMonth
)

o_files <- crss_res_get_file_names(
  extra_label, 
  yrs = yrs2show, 
  main_pdf = pdf_name
) %>%
  crss_res_append_file_path(
    figs_folder = folder_paths$figs_folder, 
    res_folder = folder_paths$res_folder
  )

traceMap <- read.csv('data/Trace2IcMap.csv')

# *****************************************************************************
#       PROCESS RESULTS --------------
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
    i_folder, 
    o_files$sys_cond_file,
    TRUE,
    'aggFromScenList', 
    sys_rwa
  )
  message('finished getSysCondData')
}

if (getPeData) {
  ## get the Mead and Powel EOCY Data
  message('starting getPeData')
  pe_rwa <- rwd_agg(read.csv(
    "data/MPPEStats_sam.csv", 
    stringsAsFactors = FALSE
  ))
  getScenarioData(scens, i_folder, o_files$tmp_pe_file, TRUE, 
                  'aggFromScenList', pe_rwa)
  ## append initial conditions onto May data
  getAndAppendIC(scens, o_files$tmp_pe_file, 
                 o_files$cur_month_pe_file, icList, icMonth, 
                 TRUE, 'aggFromScenList', traceMap, icDimNumber = icDimNumber)
  
  message('finished getPeData')
}

if (get_crss_short_cond_data) {
  message("Starting to get CRSS shortage condition data...")
  
  get_shortcond_from_rdf(
    scenario = scens[[mainScenGroup]], 
    i_folder = i_folder, 
    oFolder = resFolder
  )
  
  message("Done getting CRSS shortage condition data")
}

if (makeFiguresAndTables) {
  message("starting to create figures and tables")
  message("creating system conditions table")
  
  ## Create tables, figures, and data behind figures
  # 1) system conditions table -------------
  sysCond <- read_feather(o_files$sys_cond_file) %>%
    # trim to specified years and the current main scenario group 
    dplyr::filter(Year %in% yrs2show & Agg == mainScenGroup)
  
  # create the IG system cond. table
  sysTable <- CRSSIO::crsso_get_sys_cond_table(sysCond, yrs2show)
  
  # save the sys cond table
  data.table::fwrite(
    as.data.frame(sysTable[['fullTable']]), 
    o_files$sys_cond_table, 
    row.names = TRUE
  )
  
  # get the DCP related probabilities
  message("... DCP Probabilities")
  pe <- read_feather(o_files$cur_month_pe_file)
  dcp_probs <- bind_rows(
    compute_mead_dcp_probs(pe, mainScenGroup, 2019:2060),
    compute_powell_dcp_probs(pe, mainScenGroup, yrs2show)
    ) %>%
    filter(Year %in% yrs2show) %>%
    format_dcp_table()
  
  # save the dcp probabilities table
  data.table::fwrite(
    dcp_probs, 
    o_files$dcp_prob_file, 
    row.names = TRUE
  )
  
  # system condition heatmap -------------------------
  message("... System conditions heatmap")
 
  lb_dcp <- compute_mead_dcp_probs(pe, names(heatmap_names), 2019:2026)
  
  mead_system_condition_heatmap2(
    lb_dcp, 
    yrs2show, 
    scen_rename = heatmap_names, 
    my_title = "Lake Mead Conditions from June 2019 CRSS"
  )
  
  ggsave(
    file.path(folder_paths$figs_folder, "mead_heat_side-by-side.png"), 
    plot = last_plot(), 
    width = 8.91, 
    height = 5.65, 
    units = "in"
  )
  
  # 2) Plot Mead, Powell EOCY elvations -------------
  # includes previous month's results too
  # read in current month data
  message("EOCY elevation figures")
  pe <- read_feather(o_files$cur_month_pe_file) %>%
    # The StartMonth column is used as the color variable in plotEOCYElev, and 
    # the names that should show up in the legend/differentiate scenario groups
    # are stored in the Agg Varaible. So easiest to just copy it from Agg to 
    # StartMonth for now
    dplyr::mutate(StartMonth = Agg) %>%
    filter(StartMonth %in% plot_scenarios)

  # plot
  powellPE <- plotEOCYElev(
    pe, 
    peYrs, 
    "powell_dec_pe", 
    'Powell End-of-December Elevation', 
    colorLabel,
    legendWrap = legendWrap,
    plot_colors = plot_colors
  )
  
  meadPE <- plotEOCYElev(
    pe, 
    peYrs, 
    "mead_dec_pe", 
    'Mead End-of-December Elevation', 
    colorLabel, 
    legendWrap = legendWrap,
    plot_colors = plot_colors
  )
  
  # plot Clouds ----------------
  if (should_plot_clouds) {
    powellCloud <- plotCloudFigs(
      cloudScen, 
      pe, 
      peYrs, 
      "powell_dec_pe",
      'Powell End-of-December Elevation', 
      colorLabel,
      legendWrap = legendWrap
    )
    
    ggsave(
      o_files$powell_cloud, 
      width = 9, 
      height = 6.5, 
      units = "in", 
      dpi = 600
    )
    
    meadCloud <- plotCloudFigs(
      cloudScen, 
      pe, 
      peYrs, 
      "mead_dec_pe", 
      'Mead End-of-December Elevation', 
      colorLabel, 
      legendWrap = legendWrap
    )
    ggsave(
      o_files$mead_cloud,
      width = 9, 
      height = 6.5, 
      units = "in", 
      dpi = 600
    )
  }
  
  # 3) Critical elevation thresholds; figures and data table -------
  # have sysCond for some, and read in crit stats for others
  message("starting critical stats")
  
  # compare crit stats for all scenarios
  # call once each for powell LT 3490, shortage, and surplus
  # get the necessary variables by filtering from the pe and syscond data files
  cs <- pe %>%
    filter(
      Variable %in% c('mead_min_lt_1000', 'mead_min_lt_1020', 
                      'powell_wy_min_lt_3490', 
                      'powell_wy_min_lt_3525', 'mead_min_lt_1025', 
                      "mead_min_lt_1025", "mead_dec_lt_1025", 
                      "powell_dec_lt_3525")
    ) %>%
    rename(AggName = Agg) %>%
    select(-StartMonth)
  #rm(pe) # don't need pe any longer
  
  cs <- read_feather(o_files$sys_cond_file) %>%
    rename(AggName = Agg) %>%
    filter(Variable %in% c('lbSurplus', 'lbShortage')) %>%
    rbind(cs) %>%
    filter(AggName %in% plot_scenarios)

  ptitle <- paste(
    'Powell: Percent of Traces Less than Power Pool', 
    "(elevation 3,490\') in Any Water Year",
    sep = "\n"
  )
  
  p_3490_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'powell_wy_min_lt_3490', 
    '', 
    ptitle, 
    colorLabel, 
    legendWrap = legendWrap,
    plot_colors = plot_colors
  )
  
  shortTitle <- 'Lower Basin: Percent of Traces in Shortage Conditions'
  shortFig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'lbShortage', 
    '', 
    shortTitle, 
    colorLabel, 
    legendWrap = legendWrap,
    plot_colors = plot_colors
  )
  
  surpTitle <- 'Lower Basin: Percent of Traces in Surplus Conditions'
  surpFig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'lbSurplus', 
    '', 
    surpTitle, 
    colorLabel, 
    legendWrap = legendWrap,
    plot_colors = plot_colors
  )
  
  # compare critical stat figures 1025, 1000, 3490, 3525 -----------------
  p_3525_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'powell_wy_min_lt_3525', 
    '', 
    "Powell: Percent of Traces Less than elevation 3,525' in Any Water Year", 
    colorLabel, 
    legendWrap = legendWrap,
    plot_colors = plot_colors
  )
  
  m_1025_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    "mead_dec_lt_1025", 
    '', 
    "Mead: Percent of Traces Less than elevation 1,025' in December", 
    colorLabel, 
    legendWrap = legendWrap,
    plot_colors = plot_colors
  )
  
  m_1000_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    "mead_min_lt_1000", 
    '', 
    "Mead: Percent of Traces Less than elevation 1,000' in Any Month", 
    colorLabel, 
    legendWrap = legendWrap,
    plot_colors = plot_colors
  )
  
  # now create figures only for the current "main scenario"
  # defaults are ok for legendTitle, legLoc, nC, and annSize
  # drop Mead LT 1025 from one plot and Mead LT 1020 from 
  # the other plot

  critStatsFig1 <- plotCritStats(dplyr::filter(
      cs, 
      AggName == mainScenGroup, 
      !(Variable %in% c('meadLt1020','lbSurplus'))
    ), 
    yrs2show, 
    annText
  )
  
  critStatsFig2 <- plotCritStats(dplyr::filter(
      cs, 
      AggName == mainScenGroup, 
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
      AggName == mainScenGroup, 
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
    names(mainScenGroup)
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
  pdf(o_files$main_pdf, width = 8, height = 6)
  print(powellPE)
  print(meadPE)
  print(p_3525_fig)
  print(p_3490_fig)
  print(m_1025_fig)
  print(m_1000_fig)
  print(shortFig)
  print(surpFig)
  print(critStatsFig1)
  print(critStatsFig2)
  print(ssPlot)
  print(shortStack)
  dev.off()
  data.table::fwrite(cs, o_files$crit_stats_proc, row.names = F)
}

if(computeConditionalProbs){
  ## CONDITIONAL PROBABILITIES
  # use sysCond
  if(is.na(match('sysCond',ls()))){
    sysCond <- read.table(o_files$sys_cond_file, header = T) 
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
  data.table::fwrite(cpt1, o_files$cond_prob_file, row.names = F)
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
    o_files$short_cond_fig,
    plot = shortCond,
    units = "in",
    width = 9, 
    height = 6
  )
}

# 5 year simple table -------------------------
if (createSimple5yrTable) {
  ## create the 5-yr simple table that compares to the previous run
  message("creating 5-year simple table")
  zz <- read_feather(o_files$sys_cond_file) %>%
    rbind(read_feather(o_files$cur_month_pe_file))
          
  simple5Yr <- create5YrSimpleTable(zz, ss5, yy5, tableFootnote)
  pdf(o_files$simple_5yr_file, width = 8, height = 8)
  print(simple5Yr)
  dev.off()
  rm(zz)
}

# mead pe scatter ------------------
if (addPEScatterFig) {
  message("elevation scatter plot figure")
  ### This did not properly compile for the January run.
  if (peScatterData == "CRSS") {
    pe <- read_feather(o_files$cur_month_pe_file) %>%
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
  
  tpath <- file.path(
    folder_paths$figs_folder, 
    paste0('meadScatterFigure_', peScatterYear, '.pdf')
  )
  pdf(tpath, width = 8, height = 6)
  print(gg)
  dev.off()
}

