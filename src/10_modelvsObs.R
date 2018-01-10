##most of beginning part is copied from analysis/sandbox_cl/modelResults/02_readData.R
rm(list=ls())
load('thetab.rda')
source('filterAndSpectrumParameters.R')

angle <- -( angleFC + 90)

#read in locations of lon,lat and depth of model
loc <- read.table('/data/modelResults/flemishCap/lons_lats.dat', header=FALSE, col.names=c('longitude','latitude','level'))

#lon, lat of mooring sites
looklon <- c(m1adcp[[1]][['longitude']]*-1, m2adcp[[1]][['longitude']], m3adcp[[1]][['longitude']]*-1)
looklat <- c(m1adcp[[1]][['latitude']], m2adcp[[1]][['latitude']], m3adcp[[1]][['latitude']])

#test : use just first mooring
dataObs <- c(m1adcp, m1rcm)
	#depth of observations
depthObs <- unlist(lapply(dataObs, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))

dataTS <- m1mc
depthTS <- unlist(lapply(m1mc, function(stn) mean(stn[['pressure']], na.rm=T)))

#find which model runs match mooring location
#NOTE: 	cannot do which.min(...) b/c only one location will
#		be returned
lonmin <- min(abs(loc$longitude - looklon[1]))
filelon <- which(abs(loc$longitude - looklon[1]) == lonmin)
latmin <- min(abs(loc$latitude - looklat[1]))
filelat <- which(abs(loc$latitude - looklat[1]) == latmin)

look <- intersect(filelon, filelat)
files <- paste("00",look,'.DAT', sep='') # FIXME: needs to be though about for files > 100
path <- '/data/modelResults/flemishCap/processedData/'
pf <- paste(path,files, sep='')

#get depth of model runs 
depthModel <- unlist(lapply(pf, function(k) read.table(k, header=TRUE)$depth[1] ))

#find closest observation to model
lk <- unlist(lapply(depthModel, function(k) which.min(abs(depthObs - k))))
lkTS <- unlist(lapply(depthModel, function(k) which.min(abs(depthTS - k))))

#read in data
for (i in 1:length(pf)){
	print(i)
	model <- read.table(pf[i], header=TRUE)
	obs <- dataObs[[lk[i]]]
	#find model overlap with obs dates
	obsStart <- obs[['time']][1]
	obsEnd <- obs[['time']][length(obs[['time']])]
	obsY <- obsStart$year + 1900
	obsM <- obsStart$mon + 1
	obsD <- obsStart$mday
	obsH <- obsStart$hour
	timeModel <- as.POSIXlt(model$time, origin='1970-01-01', tz='UTC')
	modY <- timeModel$year + 1900
	modM <- timeModel$mon + 1
	modD <- timeModel$mday 
	modH <- timeModel$hour
	
	yi <- which(modY == obsY)
	mi <- which(modM == obsM)
	di <- which(modD == obsD)
	hi <- which(modH == obsH)
	start <- intersect(di, intersect(mi,yi))[1]
	
	obsY <- obsEnd$year + 1900
	obsM <- obsEnd$mon + 1
	obsD <- obsEnd$mday
	obsH <- obsEnd$hour
	
	yi <- which(modY == obsY)
	mi <- which(modM == obsM)
	di <- which(modD == obsD)
	hi <- which(modH == obsH)
	end <- intersect(di, intersect(mi,yi))
	end <- end[length(end)]
	
	obsVel <- RCMvelocity(obs)
	tobs <- obs[['time']]	
	obsrt <- rotateDetideButterfilter(obsVel$u, obsVel$v, tobs, angle=angle, butterfilter=FALSE)
	obsUrt <- obsrt$urt
	obsVrt <- obsrt$vrt
	nnaobs <- which(!is.na(obsUrt))
	obsUrt <- obsUrt[nnaobs]
	obsVrt <- obsVrt[nnaobs]
	modUr <- rotateVelocity(u=model$u[start:end], v=model$v[start:end], angle=angle)
	modU <- modUr$u
	modV <- modUr$v
	#obsV <- obsVel$v
	#obsU <- sqrt(obsU^2 + obsV^2)
	#modV <- model$v[start:end]
	#modU <- sqrt(modU^2 + modV^2)

	tmodel <- timeModel[start:end]
	tmodel <- tmodel[-722] # remove weird new year day run
	modU <- modU[-722]
	modV <- modV[-722]
	nmod <- 24 / 6
	Nyquistobs <- 2*1 #samp freq of 1 hour
	Nyquistmod <- 2*6 #samp freq of 6 hours
	lf <- 1/60/24 #low freq filter of 60 days
	hf <- 1/3/24 #high freq filter of 3 days
	fo <- butter(2, W=c(lf,hf)*Nyquistobs, "pass")

	
	obsUtt <- ts(obsUrt, start = as.Date(tobs[1]), frequency=1) #hourly 'season' nyquist should be 1/2
	print(paste('obsts',i))
	modUts <- ts(modU, start = as.Date(tmodel[1]), frequency= 1/6) #hourly 'season' nyquist should be 1/12
	print(paste('modts',i))
	
	so <- spectrum(obsUtt, spans=spans, plot=FALSE)
	#sm <- spectrum(modUtt, spans=spans, plot=FALSE)
	
	#so <- spectrum(obsUts, spans=spans, plot=FALSE)
	sm <- spectrum(modUts, spans=spans, plot=FALSE)	
	if(!interactive()) png(paste('10_specNfcmoor_',round(depthObs[lk[i]]),'m.png', sep=''), width=7, height=3, units='in', res=200, pointsize=11)
	par(mar=c(3.5,3.5,1,1))
		plot(so$freq, so$spec, type='l', xlab="", ylab="", lty=1,xlim=c(0,1/5/24))
	mtext("Frequency [cph]", side=1, line=2)
	mtext(expression(paste('variance [m'^2,'s'^-2,'cph'^-1,']')), side=2, line=2)
	#mtext(paste('Observations at:',round(depthObs[lk[i]]),'m'), side=3, adj=1)
	lines(sm$freq, sm$spec, lty=2)
	legend('topright', lty=c(1,2), legend=c('Observations','Model'))
	#plot(sm$freq, sm$spec, type='l', xlab="", ylab="", lty=1, xlim=c(0,1/5/24), ylim=c(0,0.15))
	#mtext("Frequency [cph]", side=1, line=2)
	#mtext(expression(paste('variance [m'^2,'s'^-2,'cph'^-1,']')), side=2, line=2)
	#mtext(paste('Model run at:', round(depthModel[i]),'m'), side=3, adj=1)
	
	
	
}
		
