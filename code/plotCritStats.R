
#source('C:/alan/GeneralCode/getDataFromRdf.R')
source('C:/alan/GeneralCode/generalPlotting.R')
#source('C:/alan/CRSS/code/get5YrTable.R')
library(reshape2)
library(dplyr)
library(ggplot2)

zz <- read.table('results/critStats.txt',header = T)

yy <- 2016:2026
zz <- filter(zz, Year %in% yy & Agg == 1)

zz$vName <- 'LB Shortage'
zz$vName[zz$Variable == 'meadLt1000'] <- 'Mead < 1,000\'\nin Any Month'
zz$vName[zz$Variable == 'meadLt1020'] <- 'Mead < 1,020\'\nin Any Month'
zz$vName[zz$Variable == 'meadLt1025'] <- 'Mead < 1,025\'\nin Any Month'
zz$vName[zz$Variable == 'powellLt3490'] <- 'Powell < 3,490\'\nin Any Month'

zz2 <- filter(zz, Variable != 'meadLt1020')
zz2 <- zz2 %>% group_by(Year,Variable,vName) %>%
  summarise(Value = mean(Value))

yL <- c(0,100)
annText <- 'Results from the April 2015 CRSS run.'
legendTitle <- ''
legLoc <- 'bottom'
nC <- 4
annSize <- 3

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

zz2 <- filter(zz, Variable != 'meadLt1025')
zz2 <- zz2 %>% group_by(Year,Variable,vName) %>%
  summarise(Value = mean(Value))

gg2 <- ggplot(zz2, aes(Year, Value, color = vName))
gg2 <- gg2 + geom_line(size = 1) + 
  coord_cartesian(ylim = yL) +
  scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,1)) + 
  scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
  theme(panel.grid.minor = element_line(color = 'white', size = .4),
        panel.grid.major = element_line(color = 'white', size = .6)) +
  scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
  theme(legend.position = legLoc, axis.text.x = element_text(angle = 90,vjust=.5)) +
  annotate('text', x = min(yy), y = 95, label = annText, vjust=0, hjust=0,size = annSize) + 
  labs(y = 'Percent of Traces [%]')

pdf('figs/critStatsApr2015.pdf',width = 8, height = 6)
print(gg)
print(gg2)
dev.off()