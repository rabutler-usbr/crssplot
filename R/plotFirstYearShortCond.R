

getMTOMConditionsData <- function(iFile, filterOn) 
{
  zz <- utils::read.csv(iFile)
  
  if(filterOn == 'shortage'){
    # trim to only traces with shortages  
    zz <- dplyr::filter(zz, Shortage == 1)
  } else if(filterOn == 'pe'){
    zz <- dplyr::filter(zz, DecElev <= 1077)
  } else {
    stop('invalid filterOn passed to plotFirstYearShortCond')
  }
  
  zz$LBPrct <- paste0(round(zz$LBPrct,0),'%')
  zz$WYRelease <- zz$WYRelease/1000
  zz$OND.Release <- as.factor(zz$OND.Release/1000)
  zz$HydrologyYear <- as.factor(zz$HydrologyYear)
  
  zz
}

get_shortcond_from_rdf <- function(scenario, i_folder, oFolder)
{
  scenario <- unlist(scenario)
  rwd2 <- RWDataPlyr::rwd_agg(x = data.frame(
    file = "KeySlots.rdf",
    slot = "Powell.Outflow",
    period = "asis",
    summary = NA,
    eval = NA,
    t_s = NA,
    variable = "powellOut",
    stringsAsFactors = FALSE
  ))
  ann_file <- file.path(oFolder, "crssShortCond_ann.feather")
  
  # short_cond_rwa is exported by package
  RWDataPlyr::rw_scen_aggregate(scenario, agg = crssplot::short_cond_rwa, 
                                scen_dir = i_folder, 
                                file = ann_file)
  
  mon_file <- file.path(oFolder, "crssShortCond_mon.feather")
  RWDataPlyr::rw_scen_aggregate(scenario, agg = rwd2, scen_dir = i_folder, 
                                file = mon_file)
  invisible(scenario)
}

getCRSSConditionsData <- function(iFolder, scenario, filterOn, dataYear) 
{
  zz <- feather::read_feather(file.path(iFolder, "crssShortCond_ann.feather"))
  zzMon <- feather::read_feather(file.path(iFolder, "crssShortCond_mon.feather"))
  
  zz <- zz %>%
    filter(Year == dataYear, Scenario == scenario) %>%
    group_by(Scenario, Year, TraceNumber) %>%
    tidyr::spread(Variable, Value) %>%
    mutate(lbGains = nfAbvMead + nfBelowMead,
           mwdIcs = mwdPut/.95 - mwdTake) %>%
    select(-nfAbvMead, -nfBelowMead, -mwdTake, -mwdPut)
  lbAvg <- mean(zz$lbGains)
 
  zzMon <- zzMon %>%
    filter(Year == dataYear, Month %in% c("October", "November", "December"),
           Scenario == scenario) %>%
    group_by(Scenario, TraceNumber, Year, Variable) %>%
    summarise(Value = sum(Value)/1000000) %>% # now OND total release 
    group_by(Scenario, TraceNumber, Year) %>%
    tidyr::spread(Variable, Value)
  
  message(
    "Assuming that ", scenario, " uses full observed hydrology\n",
    "If this is not the case, plotFirstYearShortCond() needs to be revisited"
  )
  
  zz <- full_join(zz, zzMon, by = c("Scenario", "TraceNumber", "Year")) %>%
    ungroup() %>%
    select(-Scenario, -Year) %>%
    mutate(lbGains = paste0(round(lbGains/lbAvg*100,0), "%"),
           TraceNumber = as.factor(TraceNumber + 1905),
           powellOut = as.factor(powellOut),
           WYRelease = round(WYRelease,2),
           mwdIcs = as.factor(mwdIcs)) %>%
    rename(
      LBPrct = lbGains, 
      OND.Release = powellOut, 
      HydrologyYear = TraceNumber
    )
    
  
  if(filterOn == 'shortage'){
    # trim to only traces with shortages  
    zz <- dplyr::filter(zz, DecElev <= 1075)
  } else if(filterOn == 'pe'){
    zz <- dplyr::filter(zz, DecElev <= 1077)
  } else {
    stop('invalid filterOn passed to plotFirstYearShortCond')
  }

  zz
}

#' Scatter plot of elevation vs. trace number 
#' 
#' `plotFirstYearShortCond()` plots elevation vs. trace number for a single 
#' year. It colors the points based on Powell release for the following WY and 
#' the shape is based on either Powell October-December release or MWD's ICS
#' creation/delivery volume.
#' 
#' @param model Either "MTOM" or "CRSS"
#' 
#' @param iFile Folder path or file that contains the data. If `model` is 
#'   CRSS, then the folder should have "crssShortCond_ann.feather" and 
#'   "crssShortCond_mon.feather" files. If `model` is "MTOM", then this should
#'   be a csv file that contains the necessary data.
#' 
#' @param scenario If `model` is CRSS, then this is the scenario that will be
#'   used. 
#' 
#' @param filterOn Valid choices are 'shortage' or 'pe'. If 'pe', then will keep 
#'   all traces that are <= 1077. 
#'   
#' @param dataYear Year to plot. Data are filtered to this year.
#' 
#' @param colorVar Has to be "WYRelease" if `model` is MTOM. Otherwise, can be
#' "WYRelease" or any other variable in the feather files. Typically, the other
#' value is "mwdIcs". 
#' 
#' @return `gg` object.
#' 
#' @export
plotFirstYearShortCond <- function(model, iFile, scenario, 
                                   filterOn = 'shortage', dataYear, 
                                   colorVar = "WYRelease")
{
  if(model == "MTOM"){
    zz <- getMTOMConditionsData(iFile, filterOn)
    colorVar <- "WYRelease"
  } else if(model == "CRSS"){
    zz <- getCRSSConditionsData(iFile, scenario, filterOn, dataYear)
  } else {
    stop("invalid model passed to plotFirstYearShortCond")
  }
  
  gradLabs <- round(range(zz$WYRelease),1)
  gradLabs <- round(seq(gradLabs[1],gradLabs[2],length = 5),1)

  # label points with LB prct of avg.
  gg <- ggplot(zz, aes_string(
    "HydrologyYear", 
    "DecElev", 
    shape = "OND.Release", 
    color = colorVar,
    label = "LBPrct"
  ))
  
  color_lab <- if (colorVar == "WYRelease") {
    "Powell WY Release"
  } else if (colorVar == "mwdIcs") {
    "MWD ICS (negative is take)"
  } else {
    colorVar
  }
  
  gg <- gg + 
    geom_point(size = 4) + 
    geom_hline(aes(yintercept = 1075),linetype = 2) + 
    geom_text(hjust = .4, vjust = -0.8,size = 3.5) + 
    labs(
      shape = paste0('Powell Oct-Dec\n',dataYear,' Release (maf)'), 
      x = paste0(dataYear,' Hydrology from Year'),
      y = paste0('Mead End-of-December ',dataYear,' elevation (feet)'),
      color = color_lab
    ) +
    scale_y_continuous(
      minor_breaks = 900:1200, 
      breaks = seq(900,1200,1), 
      labels = scales::comma
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(
      ylim = c(round(min(zz$DecElev), 0) - 1.2, max(c(1075, zz$DecElev))+ 0.5))
  
  if (model == "MTOM") {
    legName <- paste0('Powell WY ',dataYear,'\nRelease (maf)')
    gg <- gg + 
      scale_color_gradient(legName, low = 'red', high = 'blue')#, breaks = gradLabs,
                           #labels = paste(gradLabs, "maf"))
  }
  gg
}

create_short_condition_figure <- function(ui, folder_paths)
{
  message(
    'Using hard coded values for the arrow in the shortage conditions figure.',
    '\nYou may need to update the values and re-run\n',
    "Values are specified through UI"
  )
  gg_out <- list()
 
  for (i in seq_along(ui[["ind_plots"]][["shortage_conditions"]])) {
    if (ui[["ind_plots"]][["shortage_conditions"]][[i]][["create"]]) {
      tmp_scen <- ui[["ind_plots"]][["shortage_conditions"]][[i]]
      sl <- tmp_scen[["segment_locs"]]
      txl <- tmp_scen[["annotation_loc"]]
      
      # filterOn being set to pe shows results for traces that are <= 1077
      
      shortCond <- plotFirstYearShortCond(
        tmp_scen[["model"]],
        iFile = folder_paths[["res_folder"]], 
        names(ui[["ind_plots"]][["shortage_conditions"]])[i], 
        filterOn = 'pe', 
        tmp_scen[["year"]],
        colorVar = tmp_scen[["color_var"]]
      )
      
      shortCond <- shortCond + 
        annotate('segment', x = sl[1], xend = sl[2], y = sl[3], yend = sl[4], 
                 arrow = grid::arrow(length = unit(.3,'cm')), size = 1) +
        annotate(
          'text', 
          x = txl[1], 
          y = txl[2],
          label = tmp_scen[["lb_label"]], 
          size = 4, 
          hjust = 0
        ) +
        labs(
          title = tmp_scen[["title"]], 
          caption = tmp_scen[["subtitle"]]
        ) +
        theme(legend.title = element_text(size = 10))
      
      gg_out[[names(ui[["ind_plots"]][["shortage_conditions"]])[i]]] <- 
        gg_list("ind_shortage_conditions" = shortCond)
    }
  } 
  
  pgs_out(gg_out)
}
