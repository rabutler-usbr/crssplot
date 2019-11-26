
library(CRSSIO)
stopifnot(packageVersion("CRSSIO") >= "0.6.3")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(feather))
suppressPackageStartupMessages(library(stringr))
library(RWDataPlyr)
stopifnot(packageVersion("RWDataPlyr") >= "0.6.0")
suppressPackageStartupMessages(library(data.table))
library(CoRiverNF)
source('code/plot_nameFunctions.r')
source('code/getScenarioData.R')
source('code/dataTaggingFunctions.R')
source('code/getICPEData.R')
source('code/plottingFunctions.R')
source('code/getCondProbs.R')
source('code/plotFirstYearShortCond.R')
source('code/plotCloudFig.R')
source("code/compute_dcp_probs.R")
source("code/system_condition_heat_map.R")
source("code/crss_res_directory_setup.R")
source("code/specify_ui.R")


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    -------------------        USER INPUT        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ui <- specify_ui()

#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    --------------      SETUP DIRECTORIES AND FILENAMES   -----------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scens <- ui$scenarios$scens
icList <- ui$scenarios$ic_list
icMonth <- ui$scenarios$ic_month
mainScenGroup <- ui$scenarios$mainScenGroup
yrs2show <- ui$defaults$plot_yrs
peYrs <- ui$defaults$pe_yrs

crss_res_check_scen_names(
  scens, 
  icList, 
  icMonth, 
  mainScenGroup, 
  ui$simple_5yr$ss5, 
  ui$heatmap$scenarios
)

folder_paths <- crss_res_directory_setup(
  ui$folders$i_folder, 
  get_pe_data = ui$process_data$pe_data, 
  get_sys_cond_data = ui$process_data$crss_short_cond_data,
  CRSSDIR = ui$folders$CRSSDIR,
  crss_month = ui$folders$crss_month
)

o_files <- crss_res_get_file_names(
  ui$folders$extra_label, 
  yrs = yrs2show, 
  main_pdf = ui$folders$pdf_name
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
if (ui$process_data$sys_cond_data) {
  message('starting to get system conditions data')
  
  # system condition rwa
  sys_rwa <- CRSSIO::sys_cond_rwa()
  if (packageVersion("CRSSIO") <= "0.7.0")
    sys_rwa$period <- "eocy"
  
  getScenarioData(
    scens, 
    ui$folders$i_folder, 
    o_files$sys_cond_file,
    TRUE,
    'aggFromScenList', 
    sys_rwa
  )
  message('finished geting system conditions data')
}

if (ui$process_data$pe_data) {
  ## get the Mead and Powel EOCY Data
  message('starting to get PE data')
  
  pe_rwa <- read_rwd_agg("data/MPPEStats_sam.csv")
  
  getScenarioData(scens, ui$folders$i_folder, o_files$tmp_pe_file, TRUE, 
                  'aggFromScenList', pe_rwa)
  ## append initial conditions onto May data
  getAndAppendIC(scens, o_files$tmp_pe_file, 
                 o_files$cur_month_pe_file, icList, icMonth, 
                 TRUE, 'aggFromScenList', traceMap, 
                 icDimNumber = ui$defaults$ic_dim_number)
  
  message('finished getting PE data')
}

if (ui$process_data$crss_short_cond_data) {
  message("Starting to get CRSS shortage condition data...")
  assert_that(length(ui$shortage_conditions$scenario) == 1)
  assert_that(
    ui$shortage_conditions$scenario %in% names(scens),
    msg = "Shortage conditions specified scenario does not exist in data."
  )
  
  get_shortcond_from_rdf(
    scenario = scens[[ui$shortage_conditions$scenario]], 
    i_folder = ui$folders$i_folder, 
    oFolder = folder_paths$res_folder
  )
  
  message("Done getting CRSS shortage condition data")
}

if (ui$create_figures$standard_figures | ui$create_figures$pe_clouds) {
  pe <- read_feather(o_files$cur_month_pe_file) %>%
    # The StartMonth column is used as the color variable in plotEOCYElev, and 
    # the names that should show up in the legend/differentiate scenario groups
    # are stored in the Agg Varaible. So easiest to just copy it from Agg to 
    # StartMonth for now
    dplyr::mutate(StartMonth = Agg) %>%
    filter(StartMonth %in% ui$plot_group$plot_scenarios)
}

if (ui$create_figures$standard_figures) {
  message("starting to create figures and tables")
  message("creating system conditions table")
  
  ## Create tables, figures, and data behind figures
  # 1) system conditions table -------------
  sys_cond <- read_feather(o_files$sys_cond_file)

  # create the IG system cond. table
  sysTable <- CRSSIO::crsso_get_sys_cond_table(
    dplyr::filter(sys_cond, Year %in% yrs2show & Agg == mainScenGroup), yrs2show
  )
  
  # save the sys cond table
  data.table::fwrite(
    as.data.frame(sysTable[['fullTable']]), 
    o_files$sys_cond_table, 
    row.names = TRUE
  )
  
  # get the DCP related probabilities
  message("... DCP Probabilities")
  
  dcp_yrs <- c(min(yrs2show) - 1, yrs2show)
  
  dcp_scens <- unique(c(mainScenGroup, names(ui$heatmap$scenarios)))
  
  pe <- read_feather(o_files$cur_month_pe_file)
  lb_dcp <- compute_mead_dcp_probs(pe, dcp_scens, 2019:2026)
  ub_dcp <- compute_powell_dcp_probs(pe, dcp_scens, 2019:2026)
  
  dcp_probs <- bind_rows(
    filter(lb_dcp, Agg == mainScenGroup),
    filter(ub_dcp, Agg == mainScenGroup)
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
 
  m_heat <- mead_system_condition_heatmap(
    filter(lb_dcp, Agg %in% names(ui$heatmap$scenarios)), 
    ui$heatmap$years, 
    scen_rename = ui$heatmap$scenarios, 
    my_title = paste("Lake Mead Conditions from", ui$heatmap$title)
  )
  
  ggsave(
    file.path(folder_paths$png_out, "mead_heat.png"), 
    plot = m_heat, 
    width = 8.91, 
    height = 5.65, 
    units = "in"
  )
  
  p_heat <- powell_system_condition_heatmap(
    filter(sys_cond, Agg %in% names(ui$heatmap$scenarios)),
    ui$heatmap$years,
    scen_rename = ui$heatmap$scenarios,
    my_title = paste("Lake Powell Conditions from", ui$heatmap$title)
  )
  
  ggsave(
    file.path(folder_paths$png_out, "powell_heat.png"), 
    plot = p_heat, 
    width = 8.91, 
    height = 5.65, 
    units = "in"
  )
  
  # 2) Plot Mead, Powell EOCY elvations -------------
  # includes previous month's results too
  # read in current month data
  message("EOCY elevation figures")

  # plot
  powellPE <- plotEOCYElev(
    pe, 
    peYrs, 
    "powell_dec_pe", 
    'Powell End-of-December Elevation', 
    ui$defaults$color_label,
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  meadPE <- plotEOCYElev(
    pe, 
    peYrs, 
    "mead_dec_pe", 
    'Mead End-of-December Elevation', 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  # 3) Critical elevation thresholds ------------ 
  # figures and data table have sysCond for some, and 
  # read in crit stats for others
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
    filter(AggName %in% ui$plot_group$plot_scenarios)

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
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  shortTitle <- 'Lower Basin: Percent of Traces in Shortage Conditions'
  shortFig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'lbShortage', 
    '', 
    shortTitle, 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  surpTitle <- 'Lower Basin: Percent of Traces in Surplus Conditions'
  surpFig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'lbSurplus', 
    '', 
    surpTitle, 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  # compare critical stat figures  ---------------------
  # 1025, 1000, 3490, 3525 
  p_3525_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'powell_wy_min_lt_3525', 
    '', 
    "Powell: Percent of Traces Less than elevation 3,525' in Any Water Year", 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  m_1025_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    "mead_dec_lt_1025", 
    '', 
    "Mead: Percent of Traces Less than elevation 1,025' in December", 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  m_1000_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    "mead_min_lt_1000", 
    '', 
    "Mead: Percent of Traces Less than elevation 1,000' in Any Month", 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  # now create figures only for the current "main scenario"
  # defaults are ok for legendTitle, legLoc, nC, and annSize
  # drop Mead LT 1025 from one plot and Mead LT 1020 from 
  # the other plot

  critStatsFig1 <- plotCritStats(dplyr::filter(
      cs, 
      AggName == mainScenGroup, 
      !(Variable %in% c('mead_min_lt_1020','lbSurplus'))
    ), 
    yrs2show, 
    ui$defaults$annText
  )
  
  critStatsFig2 <- plotCritStats(dplyr::filter(
      cs, 
      AggName == mainScenGroup, 
      !(Variable %in% c('mead_dec_lt_1025','lbSurplus'))
    ), 
    yrs2show, 
    ui$defaults$annText
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
      sys_cond, 
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
      sys_cond, 
      Variable %in% c('lbShortageStep1','lbShortageStep2','lbShortageStep3'),
      Agg == mainScenGroup
    ), 
    yrs2show, 
    ui$defaults$annText
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

# plot Clouds ----------------
if (ui$create_figures$pe_clouds) {
  assert_that(
    all(ui$clouds$scenarios %in% names(ui$scenarios$scens)),
    msg = "Scenarios specified for the clouds to not match those available."
  )
  #function(zz, scenario, scen_labs, yrs, var, myTitle, 
  #         legendTitle, legendWrap = NULL)
  powellCloud <- plotCloudFigs_solo(
    pe,
    ui$clouds$scenarios, 
    ui$clouds$scen_labs, 
    peYrs, 
    "powell_dec_pe",
    'Powell End-of-December Elevation', 
    ui$defaults$color_label,
    legendWrap = ui$defaults$legend_wrap
  )
  
  ggsave(
    o_files$powell_cloud, 
    width = 9, 
    height = 6.5, 
    units = "in", 
    dpi = 600
  )
  
  meadCloud <- plotCloudFigs_solo(
    pe,
    ui$clouds$scenarios, 
    ui$clouds$scen_labs, 
    peYrs, 
    "mead_dec_pe", 
    'Mead End-of-December Elevation', 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap
  )
  ggsave(
    o_files$mead_cloud,
    width = 9, 
    height = 6.5, 
    units = "in", 
    dpi = 600
  )
}

# conditional probabilities ---------------------------
if(ui$create_figures$conditional_probs){
  warning(
    "The conditional probabilities have not been computed for a long time.\n", 
    "Please carefully review the code and results."
  )
  ## CONDITIONAL PROBABILITIES
  # use sysCond
  if(is.na(match('sysCond',ls()))){
    sysCond <- read.table(o_files$sys_cond_file, header = TRUE) 
    sysCond <- dplyr::filter(sysCond, Year %in% yrs2show & Agg == 1)
    sysTable <- CRSSIO::createSysCondTable(sysCond, yrs2show)
  }
  
  get_all_cond_probs(sysCond, sysTable, yrs2show, o_files$cond_prob_file)
}

# conditions leading to shortage ---------------------------------
# pulled annotation out of generic function
if (ui$create_figures$short_conditions) {
  assert_that(
    length(ui$shortage_conditions$res_file) == 1, 
    msg = "conditions leading to shortage is only designed to work with 1 scenario"
  )
  
  message(
    'Using hard coded values for the arrow in the shortage conditions figure.',
    '\nYou may need to update the values and re-run main.R'
  )
  # filterOn being set to pe shows results for traces that are <= 1077
  shortCond <- plotFirstYearShortCond(
    ui$shortage_conditions$model, 
    iFile = ui$shortage_conditions$res_file, 
    ui$shortage_conditions$scenario, 
    filterOn = 'pe', 
    ui$shortage_conditions$year,
    colorVar = ui$shortage_conditions$color_var
  )
  shortCond <- shortCond + 
    annotate('segment', x = 16.9, xend = 14.5, y = 1055, yend = 1056.4, 
           arrow = grid::arrow(length = unit(.3,'cm')), size = 1) +
    annotate(
      'text', 
      x = 17, 
      y = 1054,
      label = ui$shortage_conditions$lb_label, 
      size = 4, 
      hjust = 0
    ) +
    labs(
      title = ui$shortage_conditions$title, 
      caption = ui$shortage_conditions$subtitle
    ) +
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
if (ui$create_figures$simple_5yr_table) {

  ## create the 5-yr simple table that compares to the previous run
  message("creating 5-year simple table")
  zz <- read_feather(o_files$sys_cond_file) %>%
    rbind(read_feather(o_files$cur_month_pe_file))
          
  simple5Yr <- create5YrSimpleTable(
    zz, 
    ui$simple_5yr$ss5, 
    ui$simple_5yr$yy5, 
    ui$simple_5yr$tableFootnote
  )
  
  pdf(o_files$simple_5yr_file, width = 8, height = 8)
  print(simple5Yr)
  dev.off()
  rm(zz)
}

# mead pe scatter ------------------
if (ui$create_figures$pe_scatter_fig) {
  message("elevation scatter plot figure")

  if (ui$mead_pe_scatter$model == "CRSS") {
    pe <- read_feather(o_files$cur_month_pe_file) %>%
      filter(Agg == ui$mead_pe_scatter$scenario)
    
  } else if (ui$mead_pe_scatter$model == "MTOM") {

    icDim <- 1981:2015
    tmpIcMonth <- paste(str_replace(ui$mead_pe_scatter$year, "20", ""), "Dec", sep = "-")
    decVals <- do.call(
      rbind, 
      lapply(
        icDim, 
        get1TraceIc, 
        icList[[ui$mead_pe_scatter$scenario]], 
        tmpIcMonth, 
        traceMap
      )
    )
    traceNum <- traceMap$trace[match(icDim, traceMap$ic)]
    
    pe <- decVals %>%
      select(`Mead.Pool Elevation`) %>%
      rename(Value = `Mead.Pool Elevation`) %>%
      mutate(TraceNumber = as.numeric(traceNum), 
             Year = ui$mead_pe_scatter$year,
             Variable = "mead_dec_pe")
    
  } else {
    stop("Invalid peScatterData variable")
  }
  scatterTitle <- paste('Lake Mead December', ui$mead_pe_scatter$year, 
                        'Elevations from', ui$mead_pe_scatter$model)

  gg <- singleYearPEScatter(pe, ui$mead_pe_scatter$year, 'mead_dec_pe', 
                          scatterTitle, TRUE)
  
  tpath <- file.path(
    folder_paths$figs_folder, 
    paste0('meadScatterFigure_', ui$mead_pe_scatter$year, '.pdf')
  )
  pdf(tpath, width = 8, height = 6)
  print(gg)
  dev.off()
}

# DNF vs. ST boxplot ----------------------------------
# TODO: remove this, as this is not a "CRSS Result"
if (ui$create_figures$dnf_st_boxplot){
  message('starting comparison of DNF and ST boxplots')
  FullQ = as.data.frame(cyAnnTot$LeesFerry['1906/'])
  STQ = as.data.frame(cyAnnTot$LeesFerry['1988/'])
  FullQ$scen = "Full Hydrology"
  STQ$scen = "Stress Test Hydrology"
  Q = rbind(FullQ, STQ)
  Q$LeesFerry = Q$LeesFerry/1000000

  plotColors <- c("#00BFC4", "#F8766D")
  names(plotColors) <- c("Full Hydrology", "Stress Test Hydrology")
  
  gg = ggplot(Q, aes(x = scen, y = LeesFerry, fill = scen)) + 
    theme_light() + 
    stat_boxplot_custom(
      lwd = .25, 
      fatten = .7,
      outlier.size = .75
    )
  
  gg <- gg + scale_fill_manual(values =  plotColors) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    labs(
      x = "", y = "Annual Flow (million acre-feet)", 
      title = "Distrubution of Alternative Future Hydrology Scenarios",
      subtitle = "Colorado River Natural Flow at Lees Ferry Gaging Station, Arizona"
    ) + 
    theme(
      legend.position = "none",
      plot.subtitle = element_text(size = 7.4, color = "grey21"),
      plot.title = element_text(size=8.9),
      axis.text.x = element_text(color = "black", size = 7.2),
      axis.text.y = element_text(color = "black", size = 6),
      axis.title.y = element_text(size=7.8)
    ) +
    annotate("text", label = "1906-2017",  x= 1, y=16, size = 2.65) + 
    annotate("text", label = "1988-2017", x=2 , y=14.75, size = 2.65)
  
  ggsave(
    file.path(folder_paths$png_out,'FlowDistBoxplot.png'), 
    width = 3.5, 
    height = 3.5, 
    units = "in", 
    dpi = 600
  )
}

