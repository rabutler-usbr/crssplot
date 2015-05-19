library(CRSSIO)
library(dplyr)

zz <- read.table('results/SysConditions.txt', header = T)

# trim to 2016-2026 and 30 trace ensemble
zz <- filter(zz, Year %in% 2016:2026 & Agg == 1)

sysTable <- createSysCondTable(zz, 2016:2026)

write.csv(sysTable[['fullTable']], 'results/SysTableFull2016_2026.csv')
