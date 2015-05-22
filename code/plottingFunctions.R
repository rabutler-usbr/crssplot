# new plotting functions
library(dplyr)
library(reshape2)
library(ggplot2)
library(grid)

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
  
  # plot
  gg <- ggplot(zz, aes(Year,Value, color = StartMonth, linetype = Percentile))
  gg <- gg + geom_line(size = 1) + 
    scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,5)) + 
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
  
  gg <- ggplot(zz2, aes(Year, Value, color = vName))
  gg <- gg + geom_line(size = 1) + 
    coord_cartesian(ylim = yL) +
    scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,1)) + 
    scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
    theme(panel.grid.minor = element_line(color = 'white', size = .4),
          panel.grid.major = element_line(color = 'white', size = .6)) +
    scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
    theme(legend.position = legLoc, axis.text.x = element_text(angle = 90,vjust=.5)) +
    annotate('text', x = min(yy), y = 95, label = annText, vjust=0, hjust=0,size = annSize) + 
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
  
}
