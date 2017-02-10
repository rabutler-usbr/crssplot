
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(grid)

# filterOn: valid choices are 'shortage', 'pe'
# if filterOn == pe, then will keep all traces that are <= 1077
plotFirstYearShortCond <- function(iFile, filterOn = 'shortage', dataYear)
{
  zz <- read.csv(iFile)
  
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

  gradLabs <- round(range(zz$WYRelease),1)
  gradLabs <- round(seq(gradLabs[1],gradLabs[2],length = 5),1)

  # label points with LB prct of avg.
  gg <- ggplot(zz, aes(HydrologyYear, DecElev, shape = OND.Release, color = WYRelease,
                       label = LBPrct))
  gg <- gg + geom_point(size = 4) + 
    geom_hline(aes(yintercept = 1075),linetype = 2) + 
    scale_color_gradient(paste0('Powell WY ',dataYear,'\nRelease [MAF]'), low = 'red', 
                         high = 'blue', breaks = gradLabs) + 
    geom_text(hjust = .4, vjust = -.6,size = 3.5) + 
    labs(shape = paste0('Powell Oct-Dec\n',dataYear,' Release [MAF]'), x = paste0(dataYear,' Hydrology from Year'),
         y = paste0('Mead End-of-December ',dataYear,' elevation [ft]')) +
    scale_y_continuous(minor_breaks = 900:1200, breaks = seq(900,1200,1), label = comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(round(min(zz$DecElev),0)-1.2,max(c(1075,zz$DecElev))+.5))
  gg
}
