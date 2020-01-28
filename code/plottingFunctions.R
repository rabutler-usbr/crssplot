# new plotting functions
library(tidyverse) # dplyr, ggplot
library(grid)
library(gridExtra)
library(scales)
library(stringr)
library(cowplot)
library(imager)
library(assertthat)

theme_set(theme_grey())

plotEOCYElev <- function(zz, yrs, var, myTitle, legendTitle, legendWrap = NULL,
                         plot_colors = NULL)
{
  zz <- zz %>%
    dplyr::filter(Year %in% yrs, Variable == var) %>%
    # compute the 10/50/90 and aggregate by start month
    dplyr::group_by(StartMonth, Year, Variable) %>%
    dplyr::summarise('50th' = median(Value), '10th' = quantile(Value,.1), 
                     '90th' = quantile(Value,.9)) %>%
    ungroup() %>%
    select(-Variable) %>%
    tidyr::gather(Percentile, Value, -StartMonth, -Year)
  
  # ploting values
  qLt <- c(3,1,2)
  names(qLt) <- c('10th','50th','90th')
  
  if (length(yrs) < 15) {
    myLabs <- 1990:3000
  } else{
    myLabs <- seq(1990,3000,5)
  }
  
  # determine colors
  plot_colors <- determine_plot_colors(plot_colors, unique(zz$StartMonth))
  
  # wrap legend lables, if specified
  if (!is.null(legendWrap)) {
    zz <- zz %>%
      mutate(StartMonth = stringr::str_wrap(StartMonth, width = legendWrap))
    
    # also update the plot color names
    names(plot_colors) <- str_wrap(names(plot_colors), width = legendWrap)
  }
  
  # plot
  gg <- ggplot(zz, aes(Year,Value, color = StartMonth, linetype = Percentile))
  gg <- gg + geom_line(size = 1) + 
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myLabs,
                       labels = myLabs) + 
    scale_y_continuous(labels = scales::comma) +
    theme(panel.grid.minor = element_line(color = 'white', size = .4),
          panel.grid.major = element_line(color = 'white', size = .6)) +
    labs(y = 'feet', title = myTitle) +
    theme(
      legend.key.height = unit(2,'line'), 
      legend.key.width = grid::unit(2, 'line')
    ) +
    scale_color_manual(values = plot_colors, guide = guide_legend(title = legendTitle)) +
    scale_linetype_manual(values = qLt)
  gg
}

singleYearPEScatter <- function(zz, yr, var, myTitle, caption = NULL, 
                                addThreshStats)
{
  zz <- zz %>% filter(Year == yr, Variable == var) %>%
    mutate(TheColor = ifelse(Value <= 1075, '<= 1,075\'', 
                             ifelse(Value <= 1076,"1,075'-1,076'",
                                    ifelse(Value <= 1077, "1,076'-1,077'",
                                           "> 1,077'"))))
  
  myCols <- c('<= 1,075\'' = '#b2182b',
              "1,075'-1,076'" = '#ef8a62',
              "1,076'-1,077'" = '#9970ab',
              "> 1,077'" = '#2166ac')
  zz$TheColor <- factor(zz$TheColor, levels = names(myCols))
  
  gg <- ggplot(zz, aes(TraceNumber, Value, color = TheColor)) + 
    geom_point(size = 3, shape = 18) +
    labs(
      x = 'Trace Number', y = 'Pool Elevation [ft]', 
      title = myTitle, caption = caption
    ) + 
    scale_y_continuous(
      label = scales::comma, 
      minor_breaks = seq(800, 1200, 5)
    ) +
    scale_color_manual(values = myCols) +
    theme(legend.title = element_blank())
  
  if (addThreshStats) {
    nn <- zz %>%
      mutate(lt1075 = ifelse(Value <= 1075, 1, 0),
             lt1076 = ifelse(Value <= 1076 & Value > 1075, 1, 0),
             lt1077 = ifelse(Value <= 1077 & Value > 1075, 1, 0)) %>%
      ungroup() %>%
      summarise(
        lt1075 = sum(lt1075), 
        lt1076 = sum(lt1076), 
        lt1077 = sum(lt1077)
      )
    
    myText <- paste(nn$lt1075, 'runs are below 1,075 ft\n','an additional',
                    nn$lt1076, 'runs are within 1 ft of being below 1,075 ft\n',
                    nn$lt1077, 'runs are within 2 ft of being below 1,075 ft')
    
    gg <- gg + geom_hline(yintercept = 1075, color = 'red', size = 1) +
      annotate(
        geom = 'text', 
        x = 1, 
        y = max(zz$Value) - 5, 
        label = myText, 
        hjust = 0
      )
  }
  gg
}

compare_crit_stats <- function(zz, yrs, variable, annText, plotTitle, 
                             legendTitle = '', 
                             legLoc = 'right', nC = 1, annSize = 3, 
                             legendWrap = NULL, plot_colors = NULL)
{

  yL <- c(0,1)
  
  if (length(yrs) < 15) {
    myLabs <- 1990:3000
  } else {
    myLabs <- seq(1990,3000,5)
  }
  
  zz <- zz %>%
    filter(Year %in% yrs, Variable == variable) %>%
    group_by(Year, AggName) %>%
    summarise(Value = mean(Value))
  
  # determine plotting colors
  plot_colors <- determine_plot_colors(plot_colors, unique(zz$AggName))
  
  if (!is.null(legendWrap)) {
    aggsN <- as.character(as.factor(zz$AggName))
    aggs <- stringr::str_wrap(aggsN, width = legendWrap)
    names(aggs) <- aggsN
    zz <- zz %>%
      mutate(AggName = aggs[AggName])
    
    names(plot_colors) <- str_wrap(names(plot_colors), width = legendWrap)
  }
  
  ggplot(zz, aes(Year, Value, color = AggName)) +
    geom_line(size = 1) + 
    coord_cartesian(ylim = yL) +
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myLabs, labels = myLabs) + 
    scale_y_continuous(
      minor_breaks = seq(yL[1],yL[2],0.05), 
      breaks = seq(yL[1],yL[2],0.10),
      labels = scales::percent
    ) + 
    theme(
      panel.grid.minor = element_line(color = 'white', size = .4),
      panel.grid.major = element_line(color = 'white', size = .6),
      legend.position = legLoc, 
      legend.key.size = unit(2, "line")
    ) +
    scale_color_manual(
      guide = guide_legend(title = legendTitle,ncol = nC),
      values = plot_colors
    ) + 
    annotate(
      'text', 
      x = min(yrs), 
      y = 0.95, 
      label = annText, 
      vjust=0, 
      hjust=0,
      size = annSize
    ) + 
    labs(y = 'Percent of Traces', title = plotTitle)
}

# annText is text that's added to annotation
# legendTitle 
# legLoc is the location of the legend
# nC is number of columns in legend
# annSize is the size of the annotation
plotCritStats <- function(zz, yrs, annText, legendTitle = '', legLoc = 'bottom', nC = 4)
{
  varName <- stringr::str_wrap(csVarNames(), 14)
  names(varName) <- names(csVarNames())

  zz <- zz %>% 
    dplyr::filter(Year %in% yrs) %>%
    # compute the percent of traces by averaging values 
    dplyr::group_by(Year,Variable) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    # rename the variables to strings
    mutate(vName = varName[Variable])
  
  yL <- c(0,1)
  
  if(length(yrs) < 15){
    myLabs <- 1990:3000
  } else{
    myLabs <- seq(1990,3000,5)
  }
  
  gg <- ggplot(zz, aes(Year, Value, color = vName)) +
    geom_line(size = 1) + 
    coord_cartesian(ylim = yL) +
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myLabs, labels = myLabs) + 
    scale_y_continuous(
      minor_breaks = seq(yL[1],yL[2],0.05), 
      breaks = seq(yL[1],yL[2],0.10),
      labels = scales::percent
    ) + 
    theme(
      panel.grid.minor = element_line(color = 'white', size = .4),
      panel.grid.major = element_line(color = 'white', size = .6),
      legend.position = legLoc, 
      legend.key.size = unit(2, "line")
    ) +
    scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
    #annotate('text', x = min(yrs), y = .95, label = annText, vjust=0, hjust=0,size = annSize) + 
    labs(y = 'Percent of Traces', caption = annText)
  gg
}

# monthRun will be added to the title
# legLoc is the location of the legend
# nC is number of columns in legend
plotShortageSurplus <- function(zz, yrs, monthRun, legendTitle = '', nC = 2, legLoc = 'bottom')
{
  varName <- c("lbShortage" = 'Shortage of Any Amount',
               "lbSurplus" = 'Surplus of Any Amount')
  zz <- zz %>%
    dplyr::filter(Year %in% yrs) %>%
    # compute the chances of shortage/surplus
    # averaging accross the traces results in total % of traces
    dplyr::group_by(Year, Variable) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    dplyr::mutate(vName = varName[Variable])
  
  # plot:
  gg <- ggplot(zz, aes(Year, Value, color = vName))
  
  yL <- c(0,1)
  
  if(length(yrs) < 15){
    myLabs <- 1990:3000
  } else{
    myLabs <- seq(1990,3000,5)
  }
 
  myTitle <- paste('Percent of Traces with Lower Basin Surplus or Shortage\nResults from the',
                    monthRun, 'CRSS Run*')
  
  gg <- gg + geom_line(size = 1) + 
    coord_cartesian(ylim = yL) +
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myLabs, labels = myLabs) + 
    scale_y_continuous(
      minor_breaks = seq(yL[1],yL[2],0.05), 
      breaks = seq(yL[1],yL[2],0.10),
      labels = scales::percent
    ) + 
    theme(
      panel.grid.minor = element_line(color = 'white', size = .4),
      panel.grid.major = element_line(color = 'white', size = .6),
      legend.position = legLoc
    ) +
    scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
    labs(x = 'Year', y = 'Percent of Traces', title = myTitle)
  
  gg
}


plotShortStackedBar <- function(zz, yrs, annText)
{
  varName <- c("lbShortageStep1" = "Step 1 Shortage",
             "lbShortageStep2" = "Step 2 Shortage",
             "lbShortageStep3" = "Step 3 Shortage")
    
  zz <- zz %>% 
    dplyr::filter(Year %in% yrs) %>%
    dplyr::group_by(Year,Variable) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    dplyr::mutate(VName = varName[Variable])
 
  yL <- c(0,1)
  
  if(length(yrs) < 15){
    myLabs <- 1990:3000
  } else{
    myLabs <- seq(1990,3000,5)
  }
  
  gg <- ggplot(zz,aes(Year,Value,fill = VName))
  
  gg <- gg + geom_bar(stat = 'identity') + 
    coord_cartesian(ylim = yL) + 
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myLabs, labels = myLabs) + 
    scale_y_continuous(
      minor_breaks = seq(yL[1],yL[2],0.05), 
      breaks = seq(yL[1],yL[2],0.1),
      labels = scales::percent
    ) + 
    theme(
      panel.grid.minor = element_line(color = 'white', size = .4),
      panel.grid.major = element_line(color = 'white', size = .6),
      legend.position = "bottom"
    ) +
    scale_fill_discrete(guide = guide_legend(title = '')) + 
    labs(x = 'Year', y = 'Percent of Traces', title = 'Lower Basin Shortages by Tier',
         caption = annText)
  gg
}

# assumes zz is data already read in and will return one variable for the given yrs
# rownames of zz should be years, and colnames should be variable names
getSingleVarData <- function(zz, yrs, var)
{
  rr <- match(yrs, rownames(zz))
  cc <- which(colnames(zz) == var)
  zz[rr,cc]
}

formatSimpleTable <- function(zz, scenNames, yrs)
{
  zzRound <- round(zz,0)
  zzRound[3,] <- zzRound[2,] - zzRound[1,]

  zzRound <- matrix(paste0(zzRound,'%'),nrow = nrow(zz), byrow = F)

  # check to see if values are non-zero, but rounded to zero
  # if they are, replace with '< 1%'
  for(i in 1:nrow(zz)){
    for(j in 1:ncol(zz)){
      if(zz[i,j] > 0 & zzRound[i,j] == '0%'){
        zzRound[i,j] <- '< 1%'
      } else if(zz[i,j] < 0 & zzRound[i,j] == '0%'){
        zzRound[i,j] <- '< -1%'
      }
    }
  }
  rownames(zzRound) <- c(scenNames, 'Difference')
  colnames(zzRound) <- yrs
  
  zzRound <- as.data.frame(zzRound)
  zzRound
}

#' @param iData data frame that contains shortage and powell < 3490 variables
#' @param scenNames a named character vector; names are the names that will show 
#'   up in the finished table and the entries are the Scenario Group variable
#'   names that will be used to filter the scenarios
#' @param yrs the years to show in the table
# Assumes that there are only two scenarios to process
create5YrSimpleTable <- function(iData, scenNames, yrs, addFootnote = NA, ofile)
{
  assert_that(
    length(scenNames) == 2, 
    msg = paste0(
      'Invalid number of scenarios passed to create5YrSimpleTable.\n',
      'Please ensure scenNames have only two scenarios.'
    )
  )

  assert_that(
    all(c('lbShortage', 'powell_wy_min_lt_3490') %in% unique(iData$Variable)),
    msg = "shortage and powell < 3490 variables are not found in iData"
  )
  
  i1 <- iData %>%
    filter(Year %in% yrs) %>%
    filter(Variable %in% c('lbShortage', 'powell_wy_min_lt_3490'), 
           Agg %in% names(scenNames)) %>%
    mutate(ScenName = scenNames[Agg]) %>%
    group_by(Year, Variable, ScenName) %>%
    # multiply by 100 to display as percent instead of decimal
    dplyr::summarise(PrctTraces = mean(Value)*100) 
  
  shortTable <- i1 %>%
    filter(Variable == 'lbShortage') %>%
    ungroup() %>%
    select(-Variable) %>%
    tidyr::spread(Year, PrctTraces) %>%
    slice(match(scenNames, ScenName))
  
  rns <- c(shortTable$ScenName)
  
  shortTable <- select(shortTable, -ScenName)
  shortTable <- round(as.matrix(shortTable),0)
    
  shortTable <- as.matrix(rbind(shortTable, shortTable[2,] - shortTable[1,]))
  shortTable <- formatSimpleTable(shortTable, rns, yrs)
  
  pTable <-  i1 %>%
    filter(Variable == 'powell_wy_min_lt_3490') %>%
    ungroup() %>%
    select(-Variable) %>%
    tidyr::spread(Year, PrctTraces) %>%
    slice(match(scenNames, ScenName))
  rns <- c(pTable$ScenName)
  
  pTable <- select(pTable, -ScenName)
  pTable <- round(as.matrix(pTable),0)
  
  pTable <- as.matrix(rbind(pTable, pTable[2,] - pTable[1,]))
  pTable <- formatSimpleTable(pTable, rns, paste('WY',yrs))
  
  myTheme <- gridExtra::ttheme_default(
    gpar.coltext = gpar(cex = 1), 
    gpar.rowtext = gpar(cex = 1), show.hlines = T,
    core.just = 'right'
  )
  
  shortGrob <- gridExtra::tableGrob(shortTable, theme = myTheme)
  pGrob <- gridExtra::tableGrob(pTable, theme = myTheme)
  
  shortLabel <- '% Traces with Lower Basin Shortage'
  pLabel <- '% Traces below 3,490 feet (minimum power pool) at Lake Powell'
  
  gg <- qplot(1:7,1:7,geom = 'blank') + theme_bw() +
    theme(line = element_blank(), text = element_blank()) +
    annotation_custom(grob = pGrob, xmin = 0, ymin = 2,xmax = 7, ymax = 6) + 
    annotation_custom(
      grob = shortGrob, 
      xmin = 0, 
      ymin = 4,
      xmax = 6, 
      ymax = 7.2
    ) +
    annotate(
      "text", 
      x = 1.5, 
      y = 4.65, 
      label = pLabel, 
      hjust = 0, 
      size = 4, 
      fontface = "bold"
    ) +
    annotate(
      "text", 
      x = 1.5, 
      y = 6.25, 
      label = shortLabel, 
      hjust = 0, 
      size = 4, 
      fontface = "bold"
    )
  
  if(!is.na(addFootnote)){
    gg <- gg +
      annotate('text', x = 1.5, y = 3.4, label = addFootnote, hjust = 0, size = 2)
  }
    
  pdf(ofile, width = 8, height = 8)
  print(gg)
  dev.off()
  
  invisible(iData)
}

create_all_simple_5yr <- function(zz, ui, folder_paths)
{
  for (i in seq_along(ui[["plot_group"]])) {
    if (ui[["plot_group"]][[i]][["simple_5yr"]][["create"]]) {
      
      ofile <- construct_file_name(
        ui, folder_paths, i, "figs_folder", '5yrSimple.pdf'
      )
      
      create5YrSimpleTable(
        zz, 
        ui[["plot_group"]][[i]][["simple_5yr"]][["scen_names"]], 
        ui[["plot_group"]][[i]][["simple_5yr"]][["years"]], 
        ui[["plot_group"]][[i]][["simple_5yr"]][["footnote"]],
        ofile
      )
    }
  }
}

determine_plot_colors <- function(plot_colors, col_vars)
{
  if (is.null(plot_colors)) {
    plot_colors <- scales::hue_pal()(length(col_vars))
    names(plot_colors) <- col_vars
  } else {
    # check that there are specified colors for each Variable
    assert_that(
      all(col_vars %in% names(plot_colors)), 
      msg = "Scenario names not found in `plot_colors`"
    )
  }
  
  plot_colors
}

create_mead_pe_scatter <- function(ui, o_files, traceMap)
{
  gg_out <- list()
  for (i in seq_along(ui[["ind_plots"]][["mead_pe_scatter"]])) {
    message("   ... ", names(ui[["ind_plots"]][["mead_pe_scatter"]])[i])
    
    tmp_model <- ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["model"]]
    
    if (tmp_model == "CRSS") {
      pe <- read_feather(o_files$cur_month_pe_file) %>%
        filter(Agg == names(ui[["ind_plots"]][["mead_pe_scatter"]])[i])
      
    } else if (tmp_model == "MTOM") {
      
      icDim <- 1981:2015
      tmpIcMonth <- paste(
        str_replace(
          ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]], "20", ""
        ), 
        "Dec", 
        sep = "-"
      )
      
      decVals <- do.call(
        rbind, 
        lapply(
          icDim, 
          get1TraceIc, 
          icList[[names(ui[["ind_plots"]][["mead_pe_scatter"]])[i]]], 
          tmpIcMonth, 
          traceMap
        )
      )
      traceNum <- traceMap$trace[match(icDim, traceMap$ic)]
      
      pe <- decVals %>%
        select(`Mead.Pool Elevation`) %>%
        rename(Value = `Mead.Pool Elevation`) %>%
        mutate(
          TraceNumber = as.numeric(traceNum), 
          Year = ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]],
          Variable = "mead_dec_pe"
        )
      
    } else {
      stop("Invalid peScatterData variable")
    }
    scatterTitle <- paste(
      'Lake Mead December', 
      ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]], 
      'Elevations from', 
      ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["model"]]
    )
    
    gg <- singleYearPEScatter(
      pe, 
      ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]], 
      'mead_dec_pe', 
      scatterTitle, 
      caption = ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["ann_text"]],
      addThreshStats = ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["add_threshold_stats"]]
    )
    gg_out[[i]] <- gg
  }
  
  gg_out
}
