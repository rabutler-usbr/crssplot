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

