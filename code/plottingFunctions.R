# new plotting functions
library(tidyverse) # dplyr, ggplot
library(grid)
library(gridExtra)
library(scales)
library(stringr)
library(cowplot)
library(imager)
theme_set(theme_grey())

plotEOCYElev <- function(zz, yrs, var, myTitle, legendTitle, legendWrap = NULL)
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
  
  if(length(yrs) < 15){
    myLabs <- 1990:3000
  } else{
    myLabs <- seq(1990,3000,5)
  }
  
  if(!is.null(legendWrap)) {
    zz <- zz %>%
      mutate(StartMonth = stringr::str_wrap(StartMonth, width = legendWrap))
  }
  
  # plot
  gg <- ggplot(zz, aes(Year,Value, color = StartMonth, linetype = Percentile))
  gg <- gg + geom_line(size = 1) + 
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myLabs,
                       labels = myLabs) + 
    scale_y_continuous(labels = scales::comma) +
    theme(panel.grid.minor = element_line(color = 'white', size = .4),
          panel.grid.major = element_line(color = 'white', size = .6)) +
    labs(y = '[feet]', title = myTitle) +
    theme(legend.key.height = unit(2,'line'), legend.key.width = grid::unit(2, 'line')) +
    scale_color_discrete(guide = guide_legend(title = legendTitle)) +
    scale_linetype_manual(values = qLt)
  gg
}

singleYearPEScatter <- function(zz, yr, var, myTitle, addThreshStats)
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
    labs(x = 'Trace Number', y = 'Pool Elevation [ft]', title = myTitle) + 
    scale_y_continuous(
      label = scales::comma, 
      minor_breaks = seq(800, 1200, 5)
    ) +
    scale_color_manual(values = myCols) +
    theme(legend.title = element_blank())
  
  if(addThreshStats){
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

compareCritStats <- function(zz, yrs, variable, annText, plotTitle, 
                             legendTitle = '', 
                             legLoc = 'right', nC = 1, annSize = 3, 
                             legendWrap = NULL)
{

  yL <- c(0,1)
  
  if(length(yrs) < 15){
    myLabs <- 1990:3000
  } else{
    myLabs <- seq(1990,3000,5)
  }
  
  zz <- zz %>%
    filter(Year %in% yrs, Variable == variable) %>%
    group_by(Year, AggName) %>%
    summarise(Value = mean(Value))
  
  if(!is.null(legendWrap)) {
    aggsN <- as.character(as.factor(zz$AggName))
    aggs <- stringr::str_wrap(aggsN, width = legendWrap)
    names(aggs) <- aggsN
    zz <- zz %>%
      mutate(AggName = aggs[AggName])
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
    scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
    annotate('text', x = min(yrs), y = 0.95, label = annText, vjust=0, hjust=0,size = annSize) + 
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
create5YrSimpleTable <- function(iData, scenNames, yrs, addFootnote = NA)
{
  if(length(scenNames) != 2){
    stop(paste0('Invalid number of scenarios passed to create5YrSimpleTable.\n',
               'Please ensure scenNames have only two scenarios.'))
  }

  if(!(all(c('lbShortage','powellLt3490') %in% levels(as.factor(iData$Variable)))))
    stop("shortage and powell < 3490 variables are not found in iData")
  
  i1 <- iData %>%
    filter(Year %in% yrs) %>%
    filter(Variable %in% c('lbShortage','powellLt3490'), 
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
    filter(Variable == 'powellLt3490') %>%
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
    
  gg
}

plotCloudFigs <- function(scenario, zz, yrs, var, myTitle, legendTitle, legendWrap = NULL)
{
# Used to generate cloud figures.  Commented out are colors used for plots in DCP presentations
# and the median projections from the 07' Interim Guidelines (shown with double hash ##)
  
  zz <- zz %>%
    dplyr::filter(StartMonth %in% scenario, Year %in% yrs, Variable == var) %>%
    # compute the 10/50/90 and aggregate by start month
    dplyr::group_by(StartMonth, Year, Variable) %>%
    dplyr::summarise('Med' = median(Value), 'Min' = quantile(Value,.1), 
                     'Max' = quantile(Value,.9)) 
  
  # Set tick marks for x and y axis
  myXLabs <- seq(1990,3000,5)
  myYLabs <- seq(900,4000,50)
  
  #  Pulling historical Pool Elevation data
  if(var == 'Powell.Pool Elevation'){
    hist <- read.csv('data/HistPowellPE.csv')
    hist$Variable <- 'Powell.Pool Elevation'
    
    # Adding switch to allow plotting of correct IG important elevations
    Switch <- T
    EQLine <- as.data.frame(read.csv('data/EQLine.csv'))
    EQLine$StartMonth <- 'Historical Elevation'
    
    ##IGProj <- read.csv('C:/RCodes/Process-CRSS-Res-TribalWaterStudy/data/IGMedProjections_Powell.csv')
    ##IGProj$Variable <- 'Powell.Pool Elevation'
  }else{
    hist <- read.csv('data/HistMeadPE.csv')  
    hist$Variable <- 'Mead.Pool Elevation'
    
    # Adding switch to allow plotting of correct IG important elevations
    Switch <- F
    
    ##IGProj <- read.csv('C:/RCodes/Process-CRSS-Res-TribalWaterStudy/data/IGMedProjections_Mead.csv')
    ##IGProj$Variable <- 'Mead.Pool Elevation'
  }
  
  # Formatting data frame to match zz
  hist$StartMonth <- 'Historical Elevation'
  hist$Med <- hist$Min <- hist$Max <- hist$EOCYPE
  hist <- within(hist, rm(EOCYPE))
  hist <- hist[c("StartMonth","Year","Variable","Med","Min","Max")]
  
  # Formatting Interim Guidelines data frame to match zz
  ##IGProj$StartMonth <- 'Median Interim Guidelines FEIS'
  ##IGProj$Med <- IGProj$Min <- IGProj$Max <- IGProj$EOCYPE
  ##IGProj <- within(IGProj, rm(EOCYPE))
  ##IGProj <- IGProj[c("StartMonth","Year","Variable","Med","Min","Max")]
  
  # Getting all scenarios passed to fxn
  addIC <- unique(zz$StartMonth)
  
  # Appending last historical year pool elevation for each scenario
  for(i in 1:length(addIC)){
    zz <- bind_rows(zz, hist[length(hist[,1]),])
    zz$StartMonth[length(zz$StartMonth)] <- addIC[i]
  }
  
  # Appending historical data
  zz <- bind_rows(hist,zz)
  ##zz <- bind_rows(zz,IGProj)
  
  # Setting colors for graph- ensures historical data is black on plot
  colorNames <- unique(zz$StartMonth)
  #DCP colors (to match AZ Big Bang slides)"#54FF9F","#F4A460"
  #Grey for Interim Guidelines Projections (if included) #8B8682. Add to end.
  plotColors <- c("#000000", "#00BFC4","#F8766D")
  names(plotColors) <- colorNames
  
  # Adding factors so ggplot does not alphebetize legend
  zz$StartMonth = factor(zz$StartMonth, levels=colorNames)
  
  # Generating labels for the lines in ggplot
  histLab = "Historical Elevation"
  ##IGLab = "\"2007 Projections\""
  names(histLab) = "Historical Elevation"
  ##names(IGLab) = "\"2007 Projections\""
  histLab = append(histLab, cloudLabs)
  ##histLab = append(histLab, IGLab)
  
  # Read in Reclamation logo png
  im <- load.image('logo/660LT-TK-flush.png')
  im_rast <- grid::rasterGrob(im, interpolate = T)
  
  # Parameters for cloud plot customization (line thicknesses, text size, etc.)
  # Have been pulled out for convenience
  #Text
  TitleSize = 13
  AxisText = 11
  LegendLabText = 9.5
  
  AxisLab = 9
  LabSize = 2.9
  LegendText = 8
  
  #Lines
  IGStartLine = .8
  OpsLines = .6
  Medians = 1
  GridMaj = .25
  GridMin = .25
  
  #Y axis limits
  yaxmin = floor(min(zz$Min)/50)*50
  yaxmax = ceiling(max(zz$Max)/50)*50
  
  #Other
  LegendWidth = 1
  LegendHeight = 2.5
  
  
  # Start making the plot
  gg <- ggplot(zz, aes(x=Year, y=Med, color=StartMonth, group=StartMonth)) +  theme_light()
  
  # Generate plot a to make ribbon legend
  name <- str_wrap("10th to 90th percentile of full range",20)
  gga <- gg + geom_ribbon(data = subset(zz,StartMonth %in% rev(addIC)),aes(ymin=Min, ymax=Max, fill = StartMonth), 
                          alpha = 0.5, linetype = 2, size = 0.5*Medians) +
    scale_fill_manual(name, 
                      values = plotColors, guide = guide_legend(order=1),
                      labels = str_wrap(cloudLabs, 15)) + scale_color_manual(name,
                                                                             values = plotColors, guide = guide_legend(order=1),
                                                                             labels = str_wrap(cloudLabs, 15))  +
    theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
          legend.box.margin = margin(0,0,0,0)) 
  legenda <- get_legend(gga)
  
  # Generate plot b to take medians legend
  ggb <- gg + geom_line(size=Medians) + 
    scale_color_manual(name = str_wrap("Historical and Median Projected Pool Elevation",20),
                       values = plotColors, labels = str_wrap(histLab, 15)) +
    theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
          legend.box.margin = margin(0,0,0,0)) 
  legendb <- get_legend(ggb)
  
  # Make legend grob.  4 rows used to make legend close together and in the middle with respects to the vertical
  gglegend <- plot_grid(NULL, legenda,legendb, NULL, align = 'hv', nrow=4)
  
  # Generate plot
  gg <- gg + geom_vline(xintercept=2007, size = IGStartLine, color = '#707070') + 
    annotate("text", x=2007.1, y = yaxmin, 
             label = 'Adoption of the 2007\nInterim Guidelines', size = LabSize, hjust = 0,
             fontface = "bold") + 
             {if(Switch)geom_line(data=EQLine, aes(x = Year, y = EQLine), size = OpsLines,
                                  color = '#707070', linetype = 5)} +   
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myXLabs,
                       labels = myXLabs, expand = c(0,0)) +
    scale_y_continuous(minor_breaks = seq(900,4000,25), 
                       breaks = myYLabs, labels = comma, limits = c(yaxmin, yaxmax)) +
    geom_ribbon(data = subset(zz,StartMonth %in% addIC),aes(ymin=Min, ymax=Max, fill = StartMonth), 
                alpha = 0.5, linetype = 2, size = 0.5*Medians) + #, colour = NA) + #Orig alpha =0.3
    geom_line(size=Medians) +
    scale_fill_manual(str_wrap("10th to 90th percentile of full range",20),
                      values = plotColors, guide = FALSE,
                      labels = str_wrap(cloudLabs, 15)) + 
    scale_color_manual(name = str_wrap("Historical and Median Projected Pool Elevation",20),
                       values = plotColors, guide = FALSE,
                       labels = str_wrap(histLab, 15)) +
    labs(y = 'feet', title = myTitle, x = '') + 
    theme(plot.title = element_text(size = TitleSize),
          ## axis.text.x = element_text(size = AxisLab),
          axis.text.y = element_text (size =AxisLab),
          axis.title = element_text(size=AxisText),
          panel.grid.minor = element_line(size = GridMin),
          panel.grid.major = element_line(size = GridMaj)) +
    guides(fill=FALSE) +
    
    # Adding lines for Mead ops - plot only if Switch = False
    {if(Switch!=TRUE)geom_segment(x=2007, y=1075, xend =2026, yend = 1075, size = OpsLines, 
          color ='#707070', linetype = 5)} + 
    {if(Switch!=TRUE)annotate("text", x = 2007.1, y = 1070, label = "Level 1 Shortage Condition", 
          size = LabSize, hjust = 0, fontface = "italic")} +
    {if(Switch!=TRUE)geom_segment(x=2007, y=1050, xend =2026, yend = 1050, size = OpsLines, 
          color ='#707070', linetype = 5)} +
    {if(Switch!=TRUE)annotate("text", x = 2007.1, y = 1045, label = "Level 2 Shortage Condition", 
          size = LabSize, hjust = 0, fontface = "italic")} +
    {if(Switch!=TRUE)geom_segment(x=2007, y=1025, xend =2026, yend = 1025, size = OpsLines, 
          color ='#707070', linetype = 5)} +
    {if(Switch!=TRUE)annotate("text", x = 2007.1, y = 1020, label = "Level 3 Shortage Condition", 
          size = LabSize, hjust = 0, fontface = "italic")} +
    {if(Switch!=TRUE)geom_segment(x=2007, y=1145, xend =2026, yend = 1145, size = OpsLines, 
          color ='#707070', linetype = 5)} +
    {if(Switch!=TRUE)annotate("text", x = 2007.1, y = 1140, label = "Normal or ICS Surplus Condition", 
          size = LabSize, hjust = 0, fontface = "italic")} +
    {if(Switch!=TRUE)annotate("text", x = 2007.1, y = yaxmax, label = "Surplus Condition", 
          size = LabSize, hjust = 0, fontface = "italic")} +
    
    # Adding lines and annotation for Powell ops - plot only if Switch = True
    {if(Switch)annotate("text", x = 2007.1, y = yaxmax, label = "Equalization Tier", 
          size = LabSize, hjust = 0, fontface = "italic")} +
    ##{if(Switch)geom_segment(x=1998, y=3490, xend =2026, yend = 3490, size = OpsLines, 
    ##     color ='#707070', linetype = 5)} + 
    ##{if(Switch)annotate("text", x = 1999.5, y = 3485, label = "Minimum Power Pool", 
    ##     size = LabSize, hjust = 0, fontface = "italic")} +
    {if(Switch)geom_segment(x=2007, y=3525, xend =2026, yend = 3525, size = OpsLines, 
          color ='#707070', linetype = 5)} + 
    {if(Switch)annotate("text", x = 2007.1, y = 3520, label = "Lower Elevation Balancing Tier", 
          size = LabSize, hjust = 0, fontface = "italic")} +    
    {if(Switch)geom_segment(x=2007, y=3575, xend =2026, yend = 3575, size = OpsLines, 
          color ='#707070', linetype = 5)} + 
    {if(Switch)annotate("text", x = 2007.1, y = 3570, label = "Mid Elevation Release Tier", 
          size = LabSize, hjust = 0, fontface = "italic")} + 
    {if(Switch)annotate("text", x = 2007.1, y = 3582, label = "Upper Elevation Balancing Tier", 
          size = LabSize, hjust = 0, fontface = "italic")} + 
    
    # Add BOR Logo
    annotation_custom(im_rast, ymin = yaxmin, ymax = yaxmin + 12, xmin = 1999, xmax = 2006) 
  gg <- plot_grid(gg, gglegend, rel_widths = c(2,.4))
  gg
}
