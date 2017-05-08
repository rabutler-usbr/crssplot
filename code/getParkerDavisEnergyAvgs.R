library(dplyr)
library(reshape2)
library(zoo)

# **** need to add in a call before this to getDataForAllScens
# **** can use the LBEnergy.rdf file (all slots)
# **** and probably better to save as .feather instead of .txt; then the rest
# **** should be the same

zz <- read.table('results/ParkerDavisEnergy.txt', header = T)

zz <- filter(zz, Year %in% 2016:2026)

zz <- zz %>% group_by(Month,Year,Variable) %>%
  summarize(Avg = mean(Value))

zz$MonthNum <- NA
zz$MonthNum <- sapply(zz$Month,function(x) match(tolower(x), tolower(month.abb)))

zz$YearMon <- paste(zz$Year,zz$MonthNum,sep = '-')

zz2 <- zz
zz2$Month <- NULL
zz2$Year <- NULL
zz2$MonthNum <- NULL
zz3 <- dcast(zz2, YearMon ~ Variable, value.var = 'Avg')
zz3$YearMon <- as.yearmon(zz3$YearMon)

write.csv(zz3, 'results/ParkerDavisEnergyAvg.csv',row.names = F)
