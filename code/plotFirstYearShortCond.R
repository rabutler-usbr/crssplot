

library(ggplot2)
library(scales)
library(reshape2)
library(grid)

#setwd("~/R/RProjects/FirstYearCondMTOM")

dataYear = "2015"
shortageYear = "2016"

mtomPath <- 'MTOM/FirstYearCondMTOM/'

iFile = paste0(mtomPath, dataYear,"Data",shortageYear,"Shortage.csv")
oFile = paste0(mtomPath, dataYear,"Data",shortageYear,"Shortage.pdf")

  zz <- read.csv(iFile)
  
  # trim to only traces with shortages
  zz <- zz[zz$Shortage == 1,]
  zz$LBPrct <- paste0(round(zz$LBPrct,0),'%')
  zz$WYRelease <- zz$WYRelease/1000000
  zz$OND.Release <- as.factor(zz$OND.Release/1000000)
  zz$HydrologyYear <- as.factor(zz$HydrologyYear)
  
  lbLabel <- 'LB total side inflow percent\nof average (1981-2010)'

  # this will plot with lables showing the LB inflow rank. 
  if(FALSE){
    gg <- ggplot(zz, aes(HydrologyYear, DecElev, shape = OND.Release, color = WYRelease,
                         label = LBRank))
    gg <- gg + geom_point(size = 4) + scale_color_continuous() + geom_text(hjust = -.2, vjust = 0)
  }
  gradLabs <- round(range(zz$WYRelease),1)
  gradLabs <- round(seq(gradLabs[1],gradLabs[2],length = 5),1)

  # label points with LB prct of avg.
  gg <- ggplot(zz, aes(HydrologyYear, DecElev, shape = OND.Release, color = WYRelease,
                       label = LBPrct))
  gg <- gg + geom_point(size = 4) + 
    scale_color_gradient(paste0('Powell WY ',dataYear,'\nRelease [MAF]'), low = 'red', 
                         high = 'blue', breaks = gradLabs) + 
    geom_text(hjust = .4, vjust = -.6) + 
    labs(shape = paste0('Powell Oct-Dec\n',dataYear,' Release [MAF]'), x = paste0(dataYear,' Hydrology from Year'),
         y = paste0('Mead End-of-December ',dataYear,' elevation [ft]')) +
    scale_y_continuous(minor_breaks = 900:1200, breaks = seq(900,1200,1), label = comma) +
    annotate('segment', x = 5, xend = 5.8, y = 1070.2, yend = 1069.85, 
             arrow = grid::arrow(length = unit(.3,'cm')),size = 1) +
    annotate('text', x = 4.5, y = 1070.4,label = lbLabel, size = 4, hjust = 0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(round(min(zz$DecElev),0)-1.2,1075+.2))
  
if(TRUE){
  pdf(oFile, width = 11, height = 7)
  print(gg)
  dev.off()
}