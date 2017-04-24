
source('code/generalPlotting.R')

library(reshape2)
library(plyr)
library(RWDataPlyr)

getDataAndPlot <- function(resPath, scenFolders, scenNames, resFile, dataPath,figPath,
	pdfName,slotAggList, yy, newScenNum = 1, forceColors = FALSE)
{
	# get data 
	srA <- RWDataPlyr::createSlotAggList(slotAggList)
	RWDataPlyr::getDataForAllScens(scenFolders, scenNames, srA, resPath, paste(dataPath,resFile,sep = ''))
	
	# plot data
	gridSize <- .4
	annualD = read.table(paste(dataPath,resFile,sep = ''), header = T)
	alts <- scenNames
	gg8 <- line105090(annualD, 'Powell.Pool Elevation_EOCY_1',alts, forceColors,yy)
	gg9 <- line105090(annualD, 'Mead.Pool Elevation_EOCY_1',alts, forceColors,yy)
	gg12 <- probPlot(annualD, 'Powell.Pool Elevation_WYMinLTE_3490',alts, forceColors,yy)
	gg13 <- probPlot(annualD, 'Shortage.ShortageFlag_EOCY_100',alts, forceColors,yy)
	gg14 <- probPlot(annualD, 'Surplus.SurplusFlag_EOCY_100',alts, forceColors,yy)
	gg1 <- probPlotMultVars(annualD, c('Shortage.ShortageFlag_EOCY_100',
		'Surplus.SurplusFlag_EOCY_100'), alts[newScenNum], forceColors, c('Shortage of Any Amount',
		'Surplus of Any Amount'), yy,legLoc = 'bottom', nC = 2)
	
	gg8 <- labelFigure(gg8, 'Powell End of the Calendar Year Elevation','Year','[feet]')
	gg9 <- labelFigure(gg9, 'Mead End of the Calendar Year Elevation','Year','[feet]')
	gg12 <- labelFigure(gg12, 'Powell: Percent of Futures Less than Power Pool\n(elevation 3,490\') in Any Water Year','Water Year','[%]') +
		scale_y_continuous(breaks = seq(0,100,10)) + 
		theme(panel.grid.minor = element_line(color = 'white', size = gridSize),
		panel.grid.major = element_line(color = 'white', size = gridSize)) 
	gg13 <- labelFigure(gg13, 'Lower Basin: Percent of Futures in Shortage Conditions','Year','[%]') +
		scale_y_continuous(breaks = seq(0,100,10)) + 
		theme(panel.grid.minor = element_line(color = 'white', size = gridSize),
		panel.grid.major = element_line(color = 'white', size = gridSize)) 
	gg14 <- labelFigure(gg14, 'Lower Basin: Percent of Futures in Surplus Conditions','Year','[%]')
		scale_y_continuous(breaks = seq(0,100,10)) + 
		theme(panel.grid.minor = element_line(color = 'white', size = gridSize),
		panel.grid.major = element_line(color = 'white', size = gridSize)) 
	gg1 <- labelFigure(gg1, paste('Percent of Traces with Lower Basin Surplus or Shortage\nResults from the ',
		alts[newScenNum],' Run*',sep = ''), 'Year','[%]') + 
		scale_y_continuous(breaks = seq(0,100,10)) + 
		theme(panel.grid.minor = element_line(color = 'white', size = gridSize),
		panel.grid.major = element_line(color = 'white', size = gridSize)) 
	pdf(paste(figPath,pdfName,sep = ''), width = 8, height = 6)
	print(gg8)
	print(gg9)
	#print(gg10)
	#print(gg11)
	print(gg12)
	print(gg13)
	print(gg14)
	print(gg1)
	dev.off()
}