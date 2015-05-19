# plot using code from normal 10/50/90 code, but use Start Month as the attribute to seperate
# stats from
library(dplyr)
library(reshape2)
library(ggplot2)

res <- read.table('results/JanApr_MPPE_EOCY.txt', header = T)
res <- dplyr::tbl_df(res)

yy <- 2014:2026

#zz <- res[res$Year %in% yy,]
zz <- dplyr::filter(res, Year %in% yy)

# remove mmm and mtom mmm
#zz <- zz[zz$Scenario %in% c('DNF,2007Dems,IG',paste0('Apr2015_2016_a3,DNF,IGa3,',1981:2010)),]
zz <- dplyr::filter(zz, Scenario %in% c('DNF,2007Dems,IG',
                                        paste0('Apr2015_2016_a3,DNF,IGa3,',1981:2010)))


# compute the 10/50/90 and aggregate by start month
zzQ <- zz %>%
        dplyr::group_by(StartMonth, Year, Variable) %>%
        dplyr::summarise(q50 = median(Value), q10 = quantile(Value,.1), q90 = quantile(Value,.9))

pp <- dplyr::filter(zzQ, Variable == 'Powell.Pool Elevation')
pp <- dplyr::select(pp, StartMonth, q10, q50, q90)
pp <- reshape2::melt(pp, value.name = 'Value', measure.vars = c('q10','q50','q90'), 
                      id.vars = c('StartMonth','Year'))

mp <- dplyr::filter(zzQ, Variable == 'Mead.Pool Elevation')
mp <- dplyr::select(mp, StartMonth, q10, q50, q90)
mp <- reshape2::melt(mp, value.name = 'Value', measure.vars = c('q10','q50','q90'), 
                     id.vars = c('StartMonth','Year'))

# ploting values
qLt <- c(3,1,2)
names(qLt) <- c('10th','50th','90th')
mp$Percentile <- '10th'
mp$Percentile[mp$variable == 'q50'] <- '50th'
mp$Percentile[mp$variable == 'q90'] <- '90th'
pp$Percentile <- '10th'
pp$Percentile[pp$variable == 'q50'] <- '50th'
pp$Percentile[pp$variable == 'q90'] <- '90th'
mp$StartMonth <- as.character(mp$StartMonth)
mp$StartMonth[mp$StartMont == 'Apr2015'] <- 'Apr. 2015 DNF'
mp$StartMonth[mp$StartMont == 'Jan2015'] <- 'Jan. 2015 DNF'
mp$StarMonth <- as.factor(mp$StartMonth)
pp$StartMonth <- as.character(pp$StartMonth)
pp$StartMonth[pp$StartMont == 'Apr2015'] <- 'Apr. 2015 DNF'
pp$StartMonth[pp$StartMont == 'Jan2015'] <- 'Jan. 2015 DNF'
pp$StarMonth <- as.factor(pp$StartMonth)

gg <- ggplot(pp, aes(Year,Value, color = StartMonth, linetype = Percentile))
gg <- gg + geom_line(size = 1) + 
  scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,5)) + 
  theme(panel.grid.minor = element_line(color = 'white', size = .4),
        panel.grid.major = element_line(color = 'white', size = .6)) +
  labs(y = '[feet]', title = 'Powell End of the Calendar Year Elevation') +
  theme(legend.key.height = unit(2,'line'), legend.key.width = unit(2, 'line')) +
  scale_color_discrete(guide = guide_legend(title = 'Start Month')) +
  scale_linetype_manual(values = qLt)

gg2 <- ggplot(mp, aes(Year,Value, color = StartMonth, linetype = Percentile))
gg2 <- gg2 + geom_line(size = 1) + 
  scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,5)) + 
  theme(panel.grid.minor = element_line(color = 'white', size = .4),
        panel.grid.major = element_line(color = 'white', size = .6)) +
  labs(y = '[feet]', title = 'Mead End of the Calendar Year Elevation') +
  theme(legend.key.height = unit(2,'line'), legend.key.width = unit(2, 'line')) +
  scale_color_discrete(guide = guide_legend(title = 'Start Month')) +
  scale_linetype_manual(values = qLt)

pdf('figs/Apr2015_MPEOCY.pdf', width = 8, height = 6)
print(gg)
print(gg2)
dev.off()
