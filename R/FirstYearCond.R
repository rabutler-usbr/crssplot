# format a data table as follows:
# TRACE | SHORTAGE_2016 | HYDROLOGY START YEAR | 2015 WY RELEASE | OND 2015 RELEASE | 
# LB SIDE INFLOWS MAGNITUDE | LB SIDE INFLOWS RANK | LB SIDE INFLOWS PRCT OF AVG

# iYear is an index to the year of concern, e.g., if it is 1, then will look at conditions
# in the first year, and whether that leads to a shortage in the second year
formatData <- function(iFolder, iYear, oFile)
{
	# read in necesary data
	key <- read.rdf(paste(iFolder, '/KeySlots.rdf',sep = ''))
	short <- rdfSlotToMatrix(key, 'Shortage.ShortageFlag')
	wyRel <- rdfSlotToMatrix(key, 'PowellOperation.PowellWYRelease')
	mead <- rdfSlotToMatrix(key, 'Mead.Pool Elevation')
	crsp <- read.rdf(paste(iFolder, '/CRSPPowerData.rdf', sep = ''))
	pOut <- rdfSlotToMatrix(crsp, 'Powell.Outflow')
	check <- read.rdf(paste(iFolder, '/Check.rdf', sep = ''))
	md <- rdfSlotToMatrix(check, 'TotVal.Mead')
	mhv <- rdfSlotToMatrix(check, 'TotVal.Mohave')
	hvs <- rdfSlotToMatrix(check, 'TotVal.Havasu')
	imprl <- rdfSlotToMatrix(check, 'TotVal.Imperial')
	hyd <- 1906:2010
	hyd <- c(hyd,hyd)
	hyd <- hyd[iYear:(iYear+104)]
	# traces
	tt <- paste('Trace',1:dim(short)[2])
	
	# annualize all the data
	short <- short[seq(12, dim(short)[1], 12),]
	short[is.nan(short)] <- 0
	mead <- mead[seq(12,dim(mead)[1], 12),]
	wyRel <- wyRel[seq(12, dim(wyRel)[1], 12),]
	
	pOut <- apply(pOut[(iYear*12-2):(iYear*12),], 2, sum)
	
	lbIn <- md + mhv + hvs + imprl
	lbIn <- sumMonth2Annual(lbIn)
	
	# select the year of concern
	short <- short[iYear + 1,]
	wyRel <- wyRel[iYear,]
	mead <- mead[iYear,]
	lbIn <- lbIn[iYear,]/1000
	lbRank <- match(lbIn, sort(lbIn, decreasing = TRUE))
	lbPrctAvg <- lbIn/mean(lbIn)*100
	
	oo <- data.frame('Trace' = tt, 'DecElev' = mead, 'Shortage' = short, 'HydrologyYear' = hyd, 
		'WYRelease' = wyRel, 'OND Release' = pOut, 'LBSideIn' = lbIn, 
		'LBRank' = lbRank, 'LBPrct' = lbPrctAvg)
	
	data.table::fwrite(oo, oFile, row.names = F)
}

plotCondForShortage <- function(iFile, oFile)
{
	zz <- read.csv(iFile)
	
	# trim to only traces with shortages
	zz <- zz[zz$Shortage == 1,]
	zz$LBPrct <- paste0(round(zz$LBPrct,0),'%')
	zz$WYRelease <- zz$WYRelease/1000000
	zz$OND.Release <- as.factor(zz$OND.Release/1000000)
	zz$HydrologyYear <- as.factor(zz$HydrologyYear)
	
	# this will plot with lables showing the LB inflow rank. 
	if(FALSE){
		gg <- ggplot(zz, aes(HydrologyYear, DecElev, shape = OND.Release, color = WYRelease,
			label = LBRank))
		gg <- gg + geom_point(size = 4) + scale_color_continuous() + geom_text(hjust = -.2, vjust = 0)
	}
	
	# label points with LB prct of avg.
	gg <- ggplot(zz, aes(HydrologyYear, DecElev, shape = OND.Release, color = WYRelease,
		label = LBPrct))
	gg <- gg + geom_point(size = 4) + scale_color_gradient('Powell 2015 WY\nRelease [MAF]', low = 'red', high = 'blue') + 
		geom_text(hjust = .4, vjust = -.6) + 
		labs(shape = 'Powell Oct-Dec\n2015 Release [MAF]', x = '2015 Hydrology from Year',
			y = 'Mead End-of-December 2015 elevation [ft]') +
		scale_y_continuous(minor_breaks = 900:1200, breaks = seq(900,1200,5), label = comma) +
		annotate('segment',x = 15, xend = 15.7, y=1063, yend=1062.4, arrow = grid::arrow(length = unit(.3,"cm")),size = 1) +
		annotate('text', x = 13.8, y = 1063.4, label = 'LB natural inflow percent\nof average (1906-2010)', size = 3.5,hjust=0) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))
	
	pdf(oFile, width = 11, height = 7)
	print(gg)
	dev.off()
}


# conditional probs
# use 5-year table code as much as possible
# X is data frame returned by getDataForAllScens
get5YrStats <- function(X,oFile, nYrs = 5)
{
	zz2 <- ddply(X, .(Year,Variable), summarize,mean = mean(Value))
	yr <- min(zz2$Year)
	yr <- yr:(yr+(nYrs-1)) # 5 year window from the first year
	zz2 <- zz2[zz2$Year %in% yr,]
	stop("need to convert dcast to tidyr::spread")
	zz <- reshape2::dcast(zz2, Year~Variable, value.var = 'mean')
	
	# change names and arange in the correct order
	rr <- names(zz)[2:ncol(zz)]
	rr[match(vNames(),rr)] <- vShort()
	names(zz)[2:ncol(zz)] <- rr
	yrs <- zz$Year
	zz$eqAll <- zz$eq + zz$eq823
	zz$uebAll <- zz$uebGT823 + zz$ueb823 + zz$uebLT823
	zz$merAll <- zz$mer823 + zz$mer748
	zz$lebAll <- zz$lebGT823 + zz$leb823 + zz$lebLT823
	zz <- subset(zz,select = shortOrderFull())
	zzLimit <- subset(zz, select = shortOrderLimit())
	
	# change to full descriptions and transpose the matrix
	rr <- names(zz)
	ii <- match(rr,vShortAll())
	rr <- vDescAll()[ii]
	names(zz) <- rr
	zz <- t(zz)
	colnames(zz) <- yrs
	
	rr <- names(zzLimit)
	ii <- match(rr,vShortAll())
	rr <- vDescAll()[ii]
	names(zzLimit) <- rr
	zzLimit <- t(zzLimit)
	colnames(zzLimit) <- yrs
	
	data.table::fwrite(zz, oFile, row.names = T)
}

getCond5YrData <- function(scen, scenPath, oPath, getData = TRUE)
{
	if(getData){
		srA <- RWDataPlyr::createSlotAggList('C:/alan/CRSS/CRSS.Jan2015/5YearDataInList.csv')
		x <- RWDataPlyr::getDataForAllScens(scen, scen, srA, scenPath, paste(oPath,'/5yrData.txt',sep = ''))
	}
	
	zz <- read.table(paste(oPath,'/5yrData.txt',sep = ''), header =T)
	
	# subset table by WY 2016 release of 7.48 MAF and 8.23 MAF
	
	
	# get traces w/WY release == 8.23 and 7.48
	tmp <- zz[zz$Year == 2016 & zz$Variable == "PowellOperation.PowellWYRelease_EOCY_1e-06",]
	# round the WY release
	tmp$Value <- round(tmp$Value,2)
	tt748 <- tmp$Trace[tmp$Value == 7.48]
	tt823 <- tmp$Trace[tmp$Value == 8.23]
	
	zz823 <- zz[zz$Trace %in% tt823,]
	zz748 <- zz[zz$Trace %in% tt748,]
	
	#remove powell ops
	zz823 <- zz823[zz823$Variable != "PowellOperation.PowellWYRelease_EOCY_1e-06",]
	zz748 <- zz748[zz748$Variable != "PowellOperation.PowellWYRelease_EOCY_1e-06",]
	
	# get the 5 yr tables
	get5YrStats(zz823, paste(oPath, '/Cond5YrTable_823.csv',sep = ''))
	get5YrStats(zz748, paste(oPath, '/Cond5YrTable_748.csv',sep = ''))
}