# plot shortage as stacked bar

library(dplyr)
library(reshape2)
library(ggplot2)

res <- read.table('results/SysConditions.txt', header = T)
res <- tbl_df(res)
res <- filter(res, Agg == 1)

zz <- filter(res, Variable %in% c('lbShortageStep1','lbShortageStep2','lbShortageStep3') &
               Year %in% 2016:2026)

zz <- zz %>% group_by(Year,Variable) %>%
  summarize(prob = mean(Value)*100)
zz$VName<- 'Step 1 Shortage'
zz$VName[zz$Variable == 'lbShortageStep2'] <- 'Step 2 Shortage'
zz$VName[zz$Variable == 'lbShortageStep3'] <- 'Step 3 Shortage'

annText <- 'Results from the April 2015 CRSS run.'
yL <- c(0,100)
annSize <- 4

gg <- ggplot(zz,aes(Year,prob,fill = VName))
# add position = 'dodge' option
gg1 <- gg + geom_bar(stat = 'identity') + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,1)) + 
  scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
  theme(panel.grid.minor = element_line(color = 'white', size = .4),
        panel.grid.major = element_line(color = 'white', size = .6)) +
  scale_fill_discrete(guide = guide_legend(title = '')) + 
  theme(legend.position = 'bottom') +
  labs(x = 'Year', y = '[%]', title = 'Lower Basin Shortages by Tier') +
  annotate('text', x = min(zz$Year), y = 95, label = annText, vjust=0, hjust=0,size = annSize)
  

gg2 <- gg + geom_bar(stat = 'identity', position = 'dodge') + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,1)) + 
  scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
  theme(panel.grid.minor = element_line(color = 'white', size = .4),
        panel.grid.major = element_line(color = 'white', size = .6)) +
  scale_fill_discrete(guide = guide_legend(title = '')) + 
  theme(legend.position = 'bottom') +
  labs(x = 'Year', y = '[%]', title = 'Lower Basin Shortages by Tier') +
  annotate('text', x = min(zz$Year), y = 95, label = annText, vjust=0, hjust=0,size = annSize)


pdf('figs/shortByTier.pdf',width = 8, height = 6)
print(gg1)
print(gg2)
dev.off()

