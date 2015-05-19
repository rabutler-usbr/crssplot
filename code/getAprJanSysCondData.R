library(RWDataPlot)
# prep data for 5-year table
# mmm = min/most/max

# get all CRSS results through 2026 Mead and Powell EOCY elevations January Run
aprScens <- c('Apr2015_2016_a3,DNF,IGa3,')
aprScens <- paste0(aprScens,c(1981:2010,'MTOM_Min','MTOM_Most','MTOM_Max','Min','Most','Max'))
slotAggList <- RWDataPlot::createSlotAggList('SysCond.csv')
RWDataPlot::getDataForAllScens(aprScens[1],aprScens[1],slotAggList,'Scenario/','results/SysConditions.txt')

aprRes <- read.table('results/SysConditions.txt',header=T)

aprRes$Agg <- 1
aprRes$Agg[aprRes$Scenario %in% c('Apr2015_2016_a3,DNF,IGa3,MTOM_Max',
                                  'Apr2015_2016_a3,DNF,IGa3,MTOM_Most',
                                  'Apr2015_2016_a3,DNF,IGa3,MTOM_Min')] <- 2
aprRes$Agg[aprRes$Scenario %in% c('Apr2015_2016_a3,DNF,IGa3,Max',
                                  'Apr2015_2016_a3,DNF,IGa3,Most',
                                  'Apr2015_2016_a3,DNF,IGa3,Min')] <- 3

write.table(aprRes, 'results/SysConditions.txt')
