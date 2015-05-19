# get conditional probabilities

library(dplyr)
library(reshape2)

res <- read.table('results/SysConditions.txt', header = T)
res <- tbl_df(res)
res <- filter(res, Agg == 1)

# P(2016 Short | WY 2016 Rel = 7.48)
zz <- filter(res, Year == 2016, Variable %in% c('lbShortage', 'mer748'))
zz <- zz %>% select(Value, Variable, Trace, Scenario) %>%
  mutate(lName = paste(Scenario,Trace,sep = '_')) %>%
  select(Value,Variable,lName)
zz <- dcast(zz, lName ~ Variable, value.var = 'Value')
zz2 <- filter(zz, mer748 == 1) 
print(paste('P(2016 Short | WY 2016 Rel = 7.48)',mean(zz2$lbShortage)))

# P(2017 Short (by tier) | WY 2016 Rel = 7.48)
zz <- filter(res,Year == 2016, Variable == 'mer748')
zz2 <- filter(res,Year == 2017, Variable %in% c('lbShortage','lbShortageStep1','lbShortageStep2','lbShortageStep3'))
zz <- rbind(zz,zz2)
zz <- zz %>% select(Value, Variable, Trace, Scenario) %>%
  mutate(lName = paste(Scenario,Trace,sep = '_')) %>%
  select(Value,Variable,lName)
zz <- dcast(zz, lName ~ Variable, value.var = 'Value')
zz2 <- filter(zz,mer748 == 1)
print(paste('P(2017 Short 1 | WY 2016 Rel = 7.48)',mean(zz2$lbShortageStep1)))
print(paste('P(2017 Short 2 | WY 2016 Rel = 7.48)',mean(zz2$lbShortageStep2)))
print(paste('P(2017 Short 3 | WY 2016 Rel = 7.48)',mean(zz2$lbShortageStep3)))
print(paste('P(2017 Short | WY 2016 Rel = 7.48)',mean(zz2$lbShortage)))

# P(2017 Short (by tier) | WY 2016 Rel = 8.23)
zz <- filter(res,Year == 2016, Variable == 'ueb823')
zz2 <- filter(res,Year == 2017, Variable %in% c('lbShortage','lbShortageStep1','lbShortageStep2','lbShortageStep3'))
zz <- rbind(zz,zz2)
zz <- zz %>% select(Value, Variable, Trace, Scenario) %>%
  mutate(lName = paste(Scenario,Trace,sep = '_')) %>%
  select(Value,Variable,lName)
zz <- dcast(zz, lName ~ Variable, value.var = 'Value')
zz2 <- filter(zz,ueb823 == 1)
print(paste('P(2017 Short 1 | WY 2016 Rel = 8.23)',mean(zz2$lbShortageStep1)))
print(paste('P(2017 Short 2 | WY 2016 Rel = 8.23)',mean(zz2$lbShortageStep2)))
print(paste('P(2017 Short 3 | WY 2016 Rel = 8.23)',mean(zz2$lbShortageStep3)))
print(paste('P(2017 Short | WY 2016 Rel = 8.23)',mean(zz2$lbShortage)))

# P(2017 Short (by tier) | WY 2016 Rel > 8.23)
zz <- filter(res,Year == 2016, Variable == c('eq'))
zz3 <- filter(res, Year == 2016, Variable == 'uebGt823')
zz2 <- filter(res,Year == 2017, Variable %in% c('lbShortage','lbShortageStep1','lbShortageStep2','lbShortageStep3'))
zz <- rbind(zz,zz2,zz3)
zz <- zz %>% select(Value, Variable, Trace, Scenario) %>%
  mutate(lName = paste(Scenario,Trace,sep = '_')) %>%
  select(Value,Variable,lName)
zz <- dcast(zz, lName ~ Variable, value.var = 'Value')
zz2 <- filter(zz,uebGt823 == 1 | eq == 1)
print(paste('P(2017 Short 1 | WY 2016 Rel > 8.23)',mean(zz2$lbShortageStep1)))
print(paste('P(2017 Short 2 | WY 2016 Rel > 8.23)',mean(zz2$lbShortageStep2)))
print(paste('P(2017 Short 3 | WY 2016 Rel > 8.23)',mean(zz2$lbShortageStep3)))
print(paste('P(2017 Short | WY 2016 Rel > 8.23)',mean(zz2$lbShortage)))
