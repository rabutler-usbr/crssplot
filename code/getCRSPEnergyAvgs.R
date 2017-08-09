
library(dplyr)
library(zoo)
library(data.table)

zz <- read.table('results/CRSPEnergyData.txt', header = T)

#zz <- filter(zz, Year %in% 2016:2026)

# compute the median accross all traces and scenarios
zz <- zz %>% group_by(Month,Year,Variable) %>%
  summarize(Med = median(Value))

# convert from a month and year column to a single year-month column
zz$MonthNum <- NA
zz$MonthNum <- sapply(zz$Month,function(x) match(tolower(x), tolower(month.abb)))

zz$YearMon <- paste(zz$Year,zz$MonthNum,sep = '-')

zz2 <- zz
# remove unnecessary columns
zz2$Month <- NULL
zz2$Year <- NULL
zz2$MonthNum <- NULL

# change to a tabular format with variables as columns
stop("convert dcast to tidyr::spread")
zz3 <- dcast(zz2, YearMon ~ Variable, value.var = 'Med')
zz3$YearMon <- as.yearmon(zz3$YearMon)

data.table::fwrite(zz3, 'results/CRSPEnergyMedian.csv',row.names = F)
