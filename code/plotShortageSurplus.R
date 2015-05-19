# LB Shortage and Surplus Plot
# stats from
library(dplyr)
library(reshape2)
library(ggplot2)

res <- read.table('results/SysConditions.txt', header = T)
res <- tbl_df(res)

yy <- 2014:2026

# Agg == 1 is the suite of 30
zz <- filter(res, Year %in% yy & Variable %in% c('lbShortage', 'lbSurplus') & Agg == 1)


# compute the chances of shortage/surplus
zz <- zz %>%
  group_by(Year, Variable) %>%
  summarise(prob = mean(Value)*100)
zz$vName <- 'Shortage of Any Amount'
zz$vName[zz$Variable == 'lbSurplus'] <- 'Surplus of Any Amount'
# plot:
gg <- ggplot(zz, aes(Year, prob, color = vName))
# averaging accross the traces results in total % of traces
yL <- c(0,100)
legendTitle <- ''
nC <- 2
legLoc <- 'bottom'
gg <- gg + geom_line(size = 1) + 
  coord_cartesian(ylim = yL) +
  scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,1)) + 
  scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
  theme(panel.grid.minor = element_line(color = 'white', size = .4),
        panel.grid.major = element_line(color = 'white', size = .6)) +
  scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
  theme(legend.position = legLoc) +
  labs(x = 'Year', y = '[%]', 
       title = 'Percent of Traces with Lower Basin Surplus or Shortage\nResults from the April 2015 CRSS Run*')
  
pdf('figs/ShortSurplusApr2015.pdf', width = 8, height = 6)
print(gg)
dev.off()