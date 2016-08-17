# new plotting functions
library(dplyr)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)

plotEOCYElev <- function(zz, yrs, var, myTitle)
{
  zz <- dplyr::filter(zz, Year %in% yrs, Variable == var)
  
  # compute the 10/50/90 and aggregate by start month
  zz <- zz %>%
    dplyr::group_by(StartMonth, Year, Variable) %>%
    dplyr::summarise('50th' = median(Value), '10th' = quantile(Value,.1), 
                     '90th' = quantile(Value,.9))
  
  # reshape in format to easily plot
  #zz <- dplyr::select(zz, StartMonth, '10th', '50th', '90th')
  zz <- reshape2::melt(zz, value.name = 'Value', measure.vars = c('10th','50th','90th'), 
                       id.vars = c('StartMonth','Year'), variable.name = 'Percentile')
  
  # ploting values
  qLt <- c(3,1,2)
  names(qLt) <- c('10th','50th','90th')
  
  if(length(yrs) < 15){
    myLabs <- 1990:3000
  } else{
    myLabs <- seq(1990,3000,5)
  }
  
  # plot
  gg <- ggplot(zz, aes(Year,Value, color = StartMonth, linetype = Percentile))
  gg <- gg + geom_line(size = 1) + 
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myLabs,
                       labels = myLabs) + 
    theme(panel.grid.minor = element_line(color = 'white', size = .4),
          panel.grid.major = element_line(color = 'white', size = .6)) +
    labs(y = '[feet]', title = myTitle) +
    theme(legend.key.height = unit(2,'line'), legend.key.width = grid::unit(2, 'line')) +
    scale_color_discrete(guide = guide_legend(title = 'Start Month')) +
    scale_linetype_manual(values = qLt)
}

# annText is text that's added to annotation
# legendTitle 
# legLoc is the location of the legend
# nC is number of columns in legend
# annSize is the size of the annotation
plotCritStats <- function(zz, yrs, annText, legendTitle = '', legLoc = 'bottom', nC = 4,
                          annSize = 3)
{
  zz <- dplyr::filter(zz, Year %in% yrs)
  
  # rename the variables to strings
  zz$vName <- 'LB Shortage'
  zz$vName[zz$Variable == 'meadLt1000'] <- 'Mead < 1,000\'\nin Any Month'
  zz$vName[zz$Variable == 'meadLt1020'] <- 'Mead < 1,020\'\nin Any Month'
  zz$vName[zz$Variable == 'meadLt1025'] <- 'Mead < 1,025\'\nin Any Month'
  zz$vName[zz$Variable == 'powellLt3490'] <- 'Powell < 3,490\'\nin Any Month'
  
  # compute the percent of traces by averaging values 
  zz <- zz %>% dplyr::group_by(Year,Variable,vName) %>%
    dplyr::summarise(Value = mean(Value))
  
  yL <- c(0,100)
  
  gg <- ggplot(zz, aes(Year, Value, color = vName))
  gg <- gg + geom_line(size = 1) + 
    coord_cartesian(ylim = yL) +
    scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,1)) + 
    scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
    theme(panel.grid.minor = element_line(color = 'white', size = .4),
          panel.grid.major = element_line(color = 'white', size = .6)) +
    scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
    theme(legend.position = legLoc, axis.text.x = element_text(angle = 90,vjust=.5)) +
    annotate('text', x = min(yrs), y = 95, label = annText, vjust=0, hjust=0,size = annSize) + 
    labs(y = 'Percent of Traces [%]')
  gg
}

# monthRun will be added to the title
# legLoc is the location of the legend
# nC is number of columns in legend
plotShortageSurplus <- function(zz, yrs, monthRun, legendTitle = '', nC = 2, legLoc = 'bottom')
{
  zz <- dplyr::filter(zz, Year %in% yrs)
  
  # compute the chances of shortage/surplus
  # averaging accross the traces results in total % of traces
  zz <- zz %>%
    dplyr::group_by(Year, Variable) %>%
    dplyr::summarise(prob = mean(Value)*100)
  zz$vName <- 'Shortage of Any Amount'
  zz$vName[zz$Variable == 'lbSurplus'] <- 'Surplus of Any Amount'
  # plot:
  gg <- ggplot(zz, aes(Year, prob, color = vName))
  
  yL <- c(0,100)
 
  myTitle <- paste('Percent of Traces with Lower Basin Surplus or Shortage\nResults from the',
                    monthRun, 'CRSS Run*')
  
  gg <- gg + geom_line(size = 1) + 
    coord_cartesian(ylim = yL) +
    scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,1)) + 
    scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
    theme(panel.grid.minor = element_line(color = 'white', size = .4),
          panel.grid.major = element_line(color = 'white', size = .6)) +
    scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
    theme(legend.position = legLoc) +
    labs(x = 'Year', y = '[%]', title = myTitle)
  
  gg
}


plotShortStackedBar <- function(zz, yrs, annText, annSize = 4)
{

  zz <- dplyr::filter(zz, Year %in% yrs)
  zz <- zz %>% 
    dplyr::group_by(Year,Variable) %>%
    dplyr::summarize(prob = mean(Value)*100)
  
  # rename variables for plotting
  zz$VName<- 'Step 1 Shortage'
  zz$VName[zz$Variable == 'lbShortageStep2'] <- 'Step 2 Shortage'
  zz$VName[zz$Variable == 'lbShortageStep3'] <- 'Step 3 Shortage'
  
  yL <- c(0,100)
  
  gg <- ggplot(zz,aes(Year,prob,fill = VName))
  
  gg <- gg + geom_bar(stat = 'identity') + 
    coord_cartesian(ylim = yL) + 
    scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,1)) + 
    scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
    theme(panel.grid.minor = element_line(color = 'white', size = .4),
          panel.grid.major = element_line(color = 'white', size = .6)) +
    scale_fill_discrete(guide = guide_legend(title = '')) + 
    theme(legend.position = 'bottom') +
    labs(x = 'Year', y = '[%]', title = 'Lower Basin Shortages by Tier') +
    annotate('text', x = min(zz$Year), y = 95, label = annText, vjust=0, hjust=0,size = annSize)
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

#' @param scenNames a named character vector; names are the names that will show up in
#'            the finished table and the entries are the Scenario Group variable
#'            names that will be used to filter the scenarios
#' @param iFile character vector with path to the critStatsData
#' @param yrs the years to show in the table
# Assumes that there are only two scenarios to process
creat5YrSimpleTable <- function(scenNames, iFile, yrs, addFootnote = NA)
{
  if(length(scenNames) != 2){
    stop(paste0('Invalid number of scenarios passed to create5YrSimpleTable.\n',
               'Please ensure scenNames and iFiles have two scenarios each.'))
  }
 
  i1 <- read_feather(iFile) %>% filter(Year %in% yrs) %>%
    filter(Variable %in% c('lbShortage','powellLt3490'), Agg %in% names(scenNames)) %>%
    mutate(ScenName = scenNames[Agg]) %>%
    group_by(Year, Variable, ScenName) %>%
    dplyr::summarise(PrctTraces = mean(Value))
  
  shortTable <- i1 %>%
    filter(Variable == 'lbShortage') %>%
    ungroup() %>%
    select(-Variable) %>%
    tidyr::spread(Year, PrctTraces) %>%
    slice(match(scenNames, ScenName))
  
  rns <- c(shortTable$ScenName)
  
  shortTable <- select(shortTable, -ScenName)
    
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
    annotation_custom(grob = shortGrob, xmin = 0, ymin = 4,xmax = 6, ymax = 7.2) +
    annotate('text', x = 1.5, y = 4.65, label = pLabel, hjust = 0, size = 4, fontface = 'bold') +
    annotate('text', x = 1.5, y = 6.25, label = shortLabel, hjust = 0, size = 4, fontface = 'bold')
  
  if(!is.na(addFootnote)){
    gg <- gg +
      annotate('text', x = 1.5, y = 3.4, label = addFootnote, hjust = 0, size = 2)
  }
    
  gg
}
