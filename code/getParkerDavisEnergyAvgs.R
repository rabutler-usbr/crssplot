
library(dplyr)
library(zoo)
library(data.table)

# **** need to add in a call before this to getDataForAllScens
# **** can use the LBEnergy.rdf file (all slots)
# **** and probably better to save as .feather instead of .txt; then the rest
# **** should be the same

zz <- data.table::fread('results/ParkerDavisEnergy.txt', header = T, sep = '\t', data.table = FALSE)

zz <- zz %>%
  filter(Year %in% 2016:2026) %>%
  group_by(Month,Year,Variable) %>%
  summarize(Avg = mean(Value))

zz$MonthNum <- NA
zz$MonthNum <- sapply(zz$Month,function(x) match(tolower(x), tolower(month.abb)))

zz$YearMon <- paste(zz$Year,zz$MonthNum,sep = '-')

zz2 <- zz
zz2$Month <- NULL
zz2$Year <- NULL
zz2$MonthNum <- NULL
stop("change dcast to tidyr::spread")
zz3 <- dcast(zz2, YearMon ~ Variable, value.var = 'Avg')
zz3$YearMon <- as.yearmon(zz3$YearMon)

data.table::fwrite(zz3, 'results/ParkerDavisEnergyAvg.csv',row.names = F)
