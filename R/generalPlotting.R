
# specify colors by name
getMyCols <- function(scens, manualCols)
{
	if(manualCols){
		allScens <- c('Current \"Official\"', 'CP-Pre Powell Change',
	'New \"Official\" w/Basin\nStudy CP Demands','Current \"Official\"\nw/New UB Logic',
	'Current \"Official\"\nw/New UB Logic and\nPowell Additions =\nNew \"Official\"',
	'New \"Official\"\nw/2007 Demands','New \"Official\"',
	'UCRC w/CP-Pre\nPowell Change','UCRC w/CP',
	'New \"Official\" w/Basin\nStudy CP Demands \nand CMIP3 Hydrology','21 traces',
	'105 traces','112 CMIP3 traces',
	'Baseline',"Scen. C1", "Scen. C2", "Scen. R1", "Scen. R2", "Scen. R3")
	
		cols <- c('#377eb8','#4daf4a','#984ea3','#ffff33','#ff7f00','#ff7f00','#ff7f00',
		'#f781bf','#4daf4a','#a65628','#244A9F','#ff7f00','#f781bf',
		'#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf')
		cc <- cols[match(scens, allScens)]
	} else{
		cc <-  brewer.pal(length(scens) + 1, 'Set1')
		cc <- cc[2:length(cc)]
	}
	names(cc) <- scens
	cc
}

standardXTS <- function()
{
	rr <- scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,5)) + 
		theme(panel.grid.minor = element_line(color = 'white', size = .5),
		panel.grid.major = element_line(color = 'white', size = .7)) 
	rr
}

# X is a data frame, v is variable to plot
# default is to plot by scenarios for all time
# XXX
# update later to specify different time windows
monthlyBP <- function(X, v, alts = character(0), manualCols = FALSE)
{
	if(length(alts) == 0){
		alts <- levels(X$Scenario)
	}
	cc <- getMyCols(alts, manualCols)
	
	xx <- X[X$Variable == v & X$Scenario %in% alts,]
	xx$Month <- factor(xx$Month, levels = month.abb) # change the order to Jan-Feb
	xx$Scenario <- factor(xx$Scenario, levels = alts)
	
	gg <- ggplot(xx, aes(Month, Value, fill = Scenario))
	gg <- gg + geom_boxplot() + scale_fill_manual(values = cc)
	
	gg
}

# X is a data frame, v is variable to plot
# default is to plot by scenarios for all time
# assumes it is annual data already; 
# XXX
# also update to be able to do monthly probability plots
probPlot <- function(X, v, alts = character(0), manualCols = FALSE, 
	years = numeric(0), yL = c(0,100))
{
	# XXX
	# can likely generalize the beginning of these functions as they are the same for
	# all plotting type
	if(length(alts) == 0){
		alts <- levels(X$Scenario)
	}
	if(length(years) == 0){
		years <- range(X$Year)
		years <- seq(years[1], years[2], 1)
	}
	cc <- getMyCols(alts, manualCols)

	xx <- X[X$Variable == v & X$Scenario %in% alts & X$Year %in% years,]
	xx$Scenario <- factor(xx$Scenario, levels = alts) # change order of scenarios
	
	gg <- ggplot(xx, aes(Year, Value, color = Scenario))
	# averaging accross the traces results in total % of traces
	gg <- gg + stat_summary(fun.y = mean, geom = 'line', size = 1) + 
		scale_color_manual(values = cc) + coord_cartesian(ylim = yL) +
		scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,5)) + 
		theme(panel.grid.minor = element_line(color = 'white', size = .4),
		panel.grid.major = element_line(color = 'white', size = .6)) 
	
	gg
}

probPlotMultVars <- function(X, v,alts, manualCols = FALSE, vReplace = character(0),
	years = numeric(0), yL = c(0,100), legendTitle = '',legLoc = 'right', nC = 1)
{
	# XXX
	# can likely generalize the beginning of these functions as they are the same for
	# all plotting type
	if(length(alts) != 1){
		warning(paste('values will be averaged over',length(alts),'scenarios.'))
	}

	xx <- X[X$Variable %in% v & X$Scenario %in% alts & X$Year %in% years,]
	xx$Scenario <- factor(xx$Scenario, levels = alts) # change order of scenarios
	
	if(length(vReplace) > 0){
		if(length(v) != length(vReplace)){
			stop('v and vReplace do not have same lengths')
		}
		tmp = as.character(xx$Variable)
		for(i in 1:length(v)){
			tmp[tmp == v[i]] <- vReplace[i]
		}
		xx$Variable <- tmp
		xx$Variable <- factor(xx$Variable,levels = vReplace)
	}
	
	gg <- ggplot(xx, aes(Year, Value, color = Variable))
	# averaging accross the traces results in total % of traces
	gg <- gg + stat_summary(fun.y = mean, geom = 'line', size = 1) + 
		coord_cartesian(ylim = yL) +
		scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,5)) + 
		scale_y_continuous(minor_breaks = seq(yL[1],yL[2],5), breaks = seq(yL[1],yL[2],10)) + 
		theme(panel.grid.minor = element_line(color = 'white', size = .4),
		panel.grid.major = element_line(color = 'white', size = .6)) +
		scale_color_discrete(guide = guide_legend(title = legendTitle,ncol = nC)) + 
		theme(legend.position = legLoc)
	
	gg
}


labelFigure <- function(gg, myTitle, xlab, ylab)
{
	gg <- gg + labs(x = xlab, y = ylab, title = myTitle) + 
		theme(legend.key.height = unit(2,'line'), legend.key.width = unit(2, 'line'))
	gg
}

# assumes annual data
timeSeriesBP <- function(X, v, alts = character(0), manualCols = FALSE, years = numeric(0))
{
	if(length(alts) == 0){
		alts <- levels(X$Scenario)
	}
	# XXX
	# if I change years default to be the specification below does it work?
	if(length(years) == 0){
		years <- range(X$Year)
		years <- seq(years[1], years[2], 1)
	}
	cc <- getMyCols(alts, manualCols)
	
	xx <- X[X$Variable == v & X$Scenario %in% alts & X$Year %in% years,]
	xx$Scenario <- factor(xx$Scenario, levels = alts)

	gg <- ggplot(xx, aes(factor(Year), Value, fill = Scenario))
	gg <- gg + geom_boxplot() + scale_fill_manual(values = cc) 
	gg
}

computeTag105090 <- function(alt, X)
{
	xx <- X[X$Scenario == alt,]
	xx$Scenario <- factor(xx$Scenario)
	yr = range(X$Year)
	yr = seq(yr[1],yr[2],1)
	xx = subset(xx, select = c('Trace','Year','Value'))
	# transfor matrix back to Year x Trace to easily compute percentiles w/time
	stop("need to convert dcast and melt to tidyr::spread in code. Please re-run")
	xx <- reshape2::dcast(xx, Year ~ Trace, value.var = 'Value')
	xx <- apply(xx, 1, quantile, c(.1,.5,.9))
	xx <- t(xx)
	colnames(xx) <- c('10th','50th','90th')
	rownames(xx) <- yr
	
	xx <- reshape2::melt(xx, value.name = 'Value', varnames = c('Year','Percentile'))
	xx$Scenario = rep(alt, nrow(xx))
	xx
}

computeTagAll105090 <- function(X)
{
	alts <- levels(X$Scenario)
	xx <- apply(matrix(alts,nrow = 1),2,computeTag105090,X)
	xx <- do.call(rbind, lapply(xx, function(X) X))
	xx
}

lt105090 <- function()
{
	r <- c(3,1,2)
	names(r) <- c('10th','50th','90th')
	r
}

# 10-50-90 time series figures
# assumes annual data
line105090 <- function(X, v, alts = character(0), manualCols = FALSE, years = numeric(0))
{

	if(length(alts) == 0){
		alts <- levels(X$Scenario)
	}
	# XXX
	# if I change years default to be the specification below does it work?
	if(length(years) == 0){
		years <- range(X$Year)
		years <- seq(years[1], years[2], 1)
	}
	cc <- getMyCols(alts, manualCols)
	
	xx <- X[X$Variable == v & X$Scenario %in% alts & X$Year %in% years,]
	xx$Scenario <- factor(xx$Scenario, levels = alts)
	xx <- computeTagAll105090(xx)
	
	gg <- ggplot(xx, aes(Year, Value, color = Scenario))
	gg <- gg + geom_line(aes(linetype = Percentile),size = 1) + scale_color_manual(values = cc) +
		scale_linetype_manual(values = lt105090()) +
		scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,5)) + 
		theme(panel.grid.minor = element_line(color = 'white', size = .4),
		panel.grid.major = element_line(color = 'white', size = .6)) 
	
	gg
}

aggregateBP <- function(X, v, alts = character(0), manualCols = FALSE, years = numeric(0))
{
	if(length(alts) == 0){
		alts <- levels(X$Scenario)
	}
	# XXX
	# if I change years default to be the specification below does it work?
	if(length(years) == 0){
		years <- range(X$Year)
		years <- seq(years[1], years[2], 1)
	}
	cc <- getMyCols(alts, manualCols)
	
	xx <- X[X$Variable == v & X$Scenario %in% alts & X$Year %in% years,]

	xx$Scenario <- factor(xx$Scenario, levels = alts)

	gg <- ggplot(xx, aes(Scenario, Value, fill = Scenario))
	gg <- gg + geom_boxplot() + scale_fill_manual(values = cc) + theme(legend.position = 'none')
	
	gg
}

