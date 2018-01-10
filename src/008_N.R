rm(list=ls())
source('filterAndSpectrumParameters.R')

options(oceEOS="unesco")

d <- m1rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth1 <- depth[o]

d <- m2rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth2 <- depth[o]

latm <- latm[1:2]
lonm <- lonm[1:2]


N13 <- N14 <- N <- Nn <- vector(length=length(latm))
Nse13 <- Nse14 <- Nse <- vector(length=length(latm))
if(!interactive()) png('N.png', width=7, height=3, units='in', res=200, pointsize=12)
par(mfrow=c(1,2))
for (i in 1:length(lonm)){
	lon <- lonm[i]
	lat <- latm[i]
	distance <- geodDist(lonctd, latctd, lon, lat)
	o <- order(distance)
	ctd <- FlemishCap[o[1:6]]
	ctdyear <- unlist(lapply(ctd, function(k) k[['date']]$year + 1900))
	ctd13 <- ctd[ctdyear == 2013]
	ctd14 <- ctd[ctdyear == 2014]
 	
	#subset data
	#d1 <- subset(ctd[[1]], pressure < ifelse(i==1, depth1[length(depth1)], depth2[length(depth2)]) & pressure > ifelse(i==1, depth1[(length(depth1)-1)], depth2[(length(depth2)-1)]))
	
	d1 <- lapply(ctd, function(k) subset(k, pressure < ifelse(i==1, depth1[length(depth1)], depth2[length(depth2)]) & pressure > ifelse(i==1, depth1[(length(depth1)-1)], depth2[(length(depth2)-1)])))
		
	d13 <- lapply(ctd13, function(k) subset(k, pressure < ifelse(i==1, depth1[length(depth1)], depth2[length(depth2)]) & pressure > ifelse(i==1, depth1[(length(depth1)-1)], depth2[(length(depth2)-1)])))
	
	d14 <- lapply(ctd14, function(k) subset(k, pressure < ifelse(i==1, depth1[length(depth1)], depth2[length(depth2)]) & pressure > ifelse(i==1, depth1[(length(depth1)-1)], depth2[(length(depth2)-1)])))

	rho <- unlist(lapply(d1, function(k)
		swSigma1(k)))
	pressure <- unlist(lapply(d1, function(k)
		k[['pressure']]))

	rho13 <- unlist(lapply(d13, function(k)
		swSigma1(k)))
	pressure13 <- unlist(lapply(d13, function(k)
		k[['pressure']]))
		
	rho14 <- unlist(lapply(d14, function(k)
		swSigma1(k)))
	pressure14 <- unlist(lapply(d14, function(k)
		k[['pressure']]))
	
	
	#rho <- swSigma1(d1)
	#pressure <- d1[['pressure']]
	g <- 9.8
	
	m <- nls(rho ~ N^2/(g^2 * 1e-4)*(pressure) + rho0, start=list(N=1e-3, rho0=1020))
	m13 <- nls(rho13 ~ N^2/(g^2 * 1e-4)*(pressure13) + rho0, start=list(N=1e-3, rho0=1020))
	m14 <- nls(rho14 ~ N^2/(g^2 * 1e-4)*(pressure14) + rho0, start=list(N=1e-3, rho0=1020))
	
	N[i] <- unname(coef(m)[1])
	Nn[i] <- summary(m)$df[2]
	N13[i] <- unname(coef(m13)[1])
	N14[i] <- unname(coef(m14)[1])
	
	Nse[i] <- unname(coef(summary(m))[1,2])
	Nse13[i] <- unname(coef(summary(m13))[1,2])
	Nse14[i] <- unname(coef(summary(m14))[1,2])
	
	par(mar=c(3,3,1,1))
	ok <- which(diff(pressure13) < 1)
	top <- c(1, ok+1)
	bottom <- c(ok, length(pressure13))
	xlim <- range(c(rho13, rho14))
	xlim <- c(27.73, 27.770)
	lwd <-1.4
	plot(rho13[top[1]:bottom[1]], pressure13[top[1]:bottom[1]], ylim=rev(range(pressure13)), type='l', xlim=xlim, ylab='', xlab='', lwd=lwd)
	mtext('Pressure [dbar]', side=2, line=2)
	mtext(expression(paste(sigma[1],'[kg/',m^3,'] ')), side=1, line=2)
	lines(rho13[top[2]:bottom[2]], pressure13[top[2]:bottom[2]],lwd=lwd)
	lines(predict(m13)[top[1]:bottom[1]], pressure13[top[1]:bottom[1]], col='red', lwd=lwd)
	
	lines(rho14[top[1]:bottom[1]], pressure14[top[1]:bottom[1]], lty=3, lwd=lwd)
	lines(rho14[top[2]:bottom[2]], pressure14[top[2]:bottom[2]], lty=3, lwd=lwd)
	lines(predict(m14)[top[1]:bottom[1]], pressure14[top[1]:bottom[1]], col='red', lty=3, lwd=lwd)
	lines(predict(m)[top[1]:bottom[1]], pressure[top[1]:bottom[1]], lty=1, col='blue', lwd=lwd)	
	
	legend(ifelse(i==1,'bottomleft','topright'), lty=c(1,1,3,3), col=rep(c('black','red'),2), legend=c('2013','2013 fit', '2014', '2014 fit'))
}

if(!interactive()) dev.off()

save(N, Nse,N13, Nse13, N14, Nse14, Nn, file='N.rda')

