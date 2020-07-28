
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
	
	xx <- X[X$Variable == v & X$Scenario %in% alts,]
	xx$Month <- factor(xx$Month, levels = month.abb) # change the order to Jan-Feb
	xx$Scenario <- factor(xx$Scenario, levels = alts)
	
	gg <- ggplot(xx, aes(Month, Value, fill = Scenario))
	gg <- gg + geom_boxplot()
	
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

	xx <- X[X$Variable == v & X$Scenario %in% alts & X$Year %in% years,]
	xx$Scenario <- factor(xx$Scenario, levels = alts) # change order of scenarios
	
	gg <- ggplot(xx, aes(Year, Value, color = Scenario))
	# averaging accross the traces results in total % of traces
	gg <- gg + stat_summary(fun.y = mean, geom = 'line', size = 1) + 
    coord_cartesian(ylim = yL) +
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
	
	xx <- X[X$Variable == v & X$Scenario %in% alts & X$Year %in% years,]
	xx$Scenario <- factor(xx$Scenario, levels = alts)

	gg <- ggplot(xx, aes(factor(Year), Value, fill = Scenario))
	gg <- gg + geom_boxplot() 
	gg
}


lt105090 <- function()
{
	r <- c(3,1,2)
	names(r) <- c('10th','50th','90th')
	r
}


aggregateBP <- function(X, v, alts = character(0), manualCols = FALSE, 
                        years = numeric(0))
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
	
	xx <- X[X$Variable == v & X$Scenario %in% alts & X$Year %in% years,]

	xx$Scenario <- factor(xx$Scenario, levels = alts)

	gg <- ggplot(xx, aes(Scenario, Value, fill = Scenario))
	gg <- gg + geom_boxplot() + 
	  theme(legend.position = 'none')
	
	gg
}

