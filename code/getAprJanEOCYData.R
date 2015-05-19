library(RWDataPlot)
# prep data for 10/50/90 figures for April Run compare to January Run
# mmm = min/most/max

# get all CRSS results through 2026 Mead and Powell EOCY elevations January Run
aprScens <- c('Apr2015_2016_a3,DNF,IGa3,')
aprScens <- paste0(aprScens,c(1981:2010,'MTOM_Min','MTOM_Most','MTOM_Max','Min','Most','Max'))
slotAggList <- RWDataPlot::createSlotAggList('MPPE_EOCY.csv')
RWDataPlot::getDataForAllScens(aprScens,aprScens,slotAggList,'Scenario/','results/MPPE_EOCY.txt')

# get Mead/Powell EOCY elevations from January run
slotAggList <- RWDataPlot::createSlotAggList(matrix(c('KeySlots.rdf','Mead.Pool Elevation',
                                                      'EOCY',NA, 'Mead.Pool Elevation',
                                                      'KeySlots.rdf','Powell.Pool Elevation',
                                                      'EOCY',NA,'Powell.Pool Elevation'),
                                                    nrow = 2,byrow = T))
RWDataPlot::getDataForAllScens('DNF,2007Dems,IG/','DNF,2007Dems,IG',slotAggList, 
                               'C:/alan/CRSS/CRSS.Jan2015/Scenario/','results/JanMPPE_EOCY.txt')

# append I.C. for January run
janIC <- data.frame('Scenario' = rep('DNF,2007Dems,IG',2), 'Trace' = rep(0,2), 
                    'Year' = rep(2014,2), 
                    'Variable' = c('Powell.Pool Elevation','Mead.Pool Elevation'), 
                    'Value' = c(3597.75, 1087.79))

janRes <- read.table('results/JanMPPE_EOCY.txt',header = T)
janRes <- rbind(janIC, janRes)
write.table(janRes, 'results/JanMPPE_EOCY.txt')

# get all I.C. for April run, and append
mtomP <- read.csv('MTOM/MTOM_APR15_PowellPE.csv', row.names = 1)
mtomM <- read.csv('MTOM/MTOM_APR15_MeadPE.csv', row.names = 1)
ic <- 1981:2010
icMonth <- '15-Dec'
rr <- which(rownames(mtomP) == icMonth)
scen <- 'Apr2015_2016_a3,DNF,IGa3,'
mp <- data.frame()
pp <- data.frame()
for(i in 1:length(ic)){
  sTmp <- paste0(scen,ic[i])
  cTmp <- paste0('X',ic[i])
  mp <- rbind(mp,data.frame('Scenario' = sTmp, 'Value' = mtomM[[cTmp]][rr]))
  pp <- rbind(pp,data.frame('Scenario' = sTmp, 'Value' = mtomP[[cTmp]][rr]))
}
# add in mtom mmm and mmm
mp <- rbind(mp, data.frame('Scenario' = c('Apr2015_2016_a3,DNF,IGa3,MTOM_Max',
                               'Apr2015_2016_a3,DNF,IGa3,MTOM_Most',
                               'Apr2015_2016_a3,DNF,IGa3,MTOM_Min'), 
            'Value' = c(mtomM[['Max']][rr],mtomM[['Most']][rr],mtomM[['Min']][rr])))
mp <- rbind(mp, data.frame('Scenario' = c('Apr2015_2016_a3,DNF,IGa3,Max',
                               'Apr2015_2016_a3,DNF,IGa3,Most',
                               'Apr2015_2016_a3,DNF,IGa3,Min'),
            'Value' = c(1081.48, 1080.3, 1073.0)))
pp <- rbind(pp, data.frame('Scenario' = c('Apr2015_2016_a3,DNF,IGa3,MTOM_Max',
                               'Apr2015_2016_a3,DNF,IGa3,MTOM_Most',
                               'Apr2015_2016_a3,DNF,IGa3,MTOM_Min'), 
            'Value' = c(mtomP[['Max']][rr],mtomP[['Most']][rr],mtomP[['Min']][rr])))
pp <- rbind(pp, data.frame('Scenario' = c('Apr2015_2016_a3,DNF,IGa3,Max',
                               'Apr2015_2016_a3,DNF,IGa3,Most',
                               'Apr2015_2016_a3,DNF,IGa3,Min'),
            'Value' = c(3599.29, 3575.21, 3568.33)))

pp$Variable <- 'Powell.Pool Elevation'
mp$Variable <- 'Mead.Pool Elevation'
aprIC <- rbind(pp,mp)
aprIC$Trace <- 0
aprIC$Year <- 2015
aprIC <- aprIC[c('Scenario','Trace','Year','Variable','Value')]

# append I.C. on to rest of April results
aprRes <- read.table('results/MPPE_EOCY.txt',header = T)
aprRes <- rbind(aprIC, aprRes)
write.table(aprRes, 'results/MPPE_EOCY.txt')

# add 'Start Month' attribute to January run, and April run
# in april run, there are 6 extra scenarios, that we may need to denote differently
# maybe use 'Agg' attribute, that has 30 Ensemble, MTOM-MMM, and MMM? 
aprRes$StartMonth <- 'Apr2015'
janRes$StartMonth <- 'Jan2015'
aprRes$Agg <- 1
aprRes$Agg[aprRes$Scenario %in% c('Apr2015_2016_a3,DNF,IGa3,MTOM_Max',
                                  'Apr2015_2016_a3,DNF,IGa3,MTOM_Most',
                                  'Apr2015_2016_a3,DNF,IGa3,MTOM_Min')] <- 2
aprRes$Agg[aprRes$Scenario %in% c('Apr2015_2016_a3,DNF,IGa3,Max',
                                  'Apr2015_2016_a3,DNF,IGa3,Most',
                                  'Apr2015_2016_a3,DNF,IGa3,Min')] <- 3
janRes$Agg <- 3

# combine Jan and Apr. runs
aprRes <- rbind(aprRes, janRes)
write.table(aprRes, 'results/JanApr_MPPE_EOCY.txt')
