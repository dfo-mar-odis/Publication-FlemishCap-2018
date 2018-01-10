rm(list=ls())
#load('rotationangles.rda')
load('thetab.rda')
source('filterAndSpectrumParameters.R')
source('specConfFn.R')

## SACKVILLE SPUR SPECTRA
angle <- -( angleSS + 90 )

##order microcat data to get upper depths for ADCP
d <- m2mc
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
Hyddepth <- depth[o]
 
#adcp data
d <- m2adcp
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
look1 <- which.min(abs(Hyddepth[1] - depth))
look2 <- which.min(abs(Hyddepth[2] - depth))
adpd <- d[c(look1,look2)]

##order current meter data
d <- m2rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
mcd <- d[o]

data <- c(adpd, mcd) #adcp data and current meter
depth <- getDepth(data)


Nyquist <- 2*1 #samp freq of 1hour
lf <- 1/60/24 #low freq filter of 60 days
hf <- 1/3/24 #high freq filter of 3 days
f <- butter(2, W=c(lf,hf)*Nyquist, "pass")

if(!interactive()) png('14_specFCandSS.png', width=7.5, height=3*2, res=200, units='in', pointsize=10)
spans <- c(5,3)
col <- oceColorsJet(length(depth)+1)
col[6] <- 'darkgoldenrod2'
m <- matrix(c(1,1,2,3,3,4), nrow = 2, byrow = TRUE)
layout(m)
par(mar=c(4,4,3,1))
spec21 <- spec21cil <- spec21ciu <- f20u <- f20l <- NULL
for (i in 1:length(data)){
	c <- data[[i]]
	u <- RCMvelocity(data[[i]])$u
	v <- RCMvelocity(data[[i]])$v
	time <- data[[i]][['time']]
	rtb <- rotateDetideButterfilter(u , v, time, angle, deltat=deltat, cutoff=cutoff)
	ur <- rtb$ur
	urm <- rtb$urm
	urt <- rtb$urt
	urtb <- rtb$urtb
	vr <- rtb$vr
	vrt <- rtb$vrt
	vrm <- rtb$vrm
	vrtb <- rtb$vrtb
	obs <- ts(urt, start = as.Date(time[1]), frequency = 1)
	#s <- spectrum(urt, spans=spans, plot=FALSE)
	s <- spectrum(obs, spans = spans, plot = FALSE)
	findvar <- findVariance(s)
	f20u[i] <- findvar$f20u
	f20l[i] <- findvar$f20l
	#print(spec.ci(s, coverage=0.95))

	if(i == 1){
		plot(s$freq, s$spec, type='l', xlab="", ylab="", col=col[i], xlim=c(0,1/5/24), ylim=c(0,.6), lwd=1.5, lty = 3)
		mtext("Frequency (cph)", side=1, line=2, cex = 0.9)
		mtext(expression(paste('Variance (m'^2,'s'^-2,'cph'^-1,')')), side=2, line=2, cex = 0.9)
		#drawSpectrumLimits(s, lty=2)
		dayaxis <- c(64,32,16,8,4)

		#axis(3, at = 1/dayaxis/24, labels = paste(dayaxis,'d',sep=""))

	}
	if(i > 1){
		lines(s$freq, s$spec, col=col[i], lwd=1.5,
			  lty = ifelse(i == 2, 4, 1))
	}
	CI <- spec.ci(s, coverage = 0.95)
	look21 <- which.min(abs(s$freq - 1/21/24))
	spec21[i] <- s$spec[look21]
	spec21cil[i] <- s$spec[look21]*CI[1]
	spec21ciu[i] <- s$spec[look21]*CI[2]	
	if(i == length(data)){
		#abline(v=1/21/24, col='grey55', lty=2)
		#abline(v=1/13.5/24, col='grey55', lty=2)
		#abline(v=mean(f20u, na.rm=TRUE), col='grey55', lty=2)
		#abline(v=mean(f20l, na.rm=TRUE), col='grey55', lty=2)
		#mtext('21 d', side=3, at=1/21/24)
		#mtext('13.5 d', side=3, at=1/13.5/24)
		#mtext(paste(round(1/mean(f20u, na.rm=TRUE)/24),'d', sep=" "), side=3, at = mean(f20u, na.rm=TRUE), cex=0.8)
		#mtext(paste(round(1/mean(f20l, na.rm=TRUE)/24),'d', sep=" "), side=3, at = mean(f20l, na.rm=TRUE), cex =0.8)
		lines(s$freq[1] + c(-.5,.5) * s$bandwidth, rep(.55, 2), col='black', lwd=1.5)
		arrows(1/80/24, 0.255, 1/80/24, 0.23, length = 0.05)
		text(1/80/24, 0.275, '80d')
		arrows(1/21/24, 0.48 + 0.025, 1/21/24, 0.48, length = 0.05)
		text(1/21/24, 0.48 + 0.025 + 0.02, '21d *')
		arrows(1/13.1/24, 0.31 + 0.025, 1/13.1/24, 0.31, length = 0.05)
		text(1/13.1/24, 0.31 + 0.025 + 0.02, '13.1d')
		arrows(1/9.3/24, 0.23 + 0.025, 1/9.3/24, 0.23, length = 0.05)
		text(1/9.3/24, 0.23 + 0.025 + 0.02, '9.3d *')
		
	}

	
}
legend('topright', 
	lty=c(3,4, rep(1, length(depth)-2)), col=col, legend=c(paste(round(depth), 'm')), lwd=1.5, ncol=2, title = "Sackville Spur")

plot(spec21,depth, xlim=c(0,1.6), ylim=rev(range(depth)), pch=20, col=col, ylab="", xlab="")
for (i in 1:length(depth)){
	lines(c(spec21ciu[i], spec21cil[i]), rep(depth[i],2), col=col[i])
}
mtext(expression(paste('Variance (m'^2,'s'^-2,'cph'^-1,')')), side=1, line=2.5, cex = 0.9)
mtext('Depth (m)', side=2, line=2, cex = 0.9)


###FLEMISH CAP
angle <- -( angleFC + 90 )

##order microcat data to get upper depths for ADCP
d <- m1mc
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
Hyddepth <- depth[o]
 
#adcp data
d <- m1adcp
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
look1 <- which.min(abs(Hyddepth[1] - depth))
look2 <- which.min(abs(Hyddepth[2] - depth))
adpd <- d[c(look1,look2)]

##order current meter data
d <- m1rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
mcd <- d[o]

data <- c(adpd, mcd) #adcp data and current meter
depth <- getDepth(data)


Nyquist <- 2*1 #samp freq of 1hour
lf <- 1/60/24 #low freq filter of 60 days
hf <- 1/3/24 #high freq filter of 3 days
f <- butter(2, W=c(lf,hf)*Nyquist, "pass")


spans <- c(5,3)
col <- oceColorsJet(length(depth)+1)
col[6] <- 'darkgoldenrod2'
spec21 <- spec21cil <- spec21ciu <- f20u <- f20l <- NULL
for (i in 1:length(data)){
	c <- data[[i]]
	u <- RCMvelocity(data[[i]])$u
	v <- RCMvelocity(data[[i]])$v
	time <- data[[i]][['time']]
	rtb <- rotateDetideButterfilter(u , v, time, angle, deltat=deltat, cutoff=cutoff)
	ur <- rtb$ur
	urm <- rtb$urm
	urt <- rtb$urt
	urtb <- rtb$urtb
	vr <- rtb$vr
	vrt <- rtb$vrt
	vrm <- rtb$vrm
	vrtb <- rtb$vrtb
	
	obs <- ts(urt, start = as.Date(time[1]), frequency = 1)
	#s <- spectrum(urt, spans=spans, plot=FALSE)
	s <- spectrum(obs, spans = spans, plot = FALSE)
	findvar <- findVariance(s)
	f20u[i] <- findvar$f20u
	f20l[i] <- findvar$f20l
	#print(spec.ci(s, coverage=0.95))
	par(mar=c(4,4,2,1))
	if(i == 1){
		plot(s$freq, s$spec, type='l', xlab="", ylab="", col=col[i], xlim=c(0,1/5/24), ylim=c(0,.6), lwd=1.5, lty = 3)
		mtext("Frequency (cph)", side=1, line=2, cex=0.9)
		mtext(expression(paste('Variance (m'^2,'s'^-2,'cph'^-1,')')), side=2, line=2, cex=0.9)
		dayaxis <- c(64,32,16,8,4)
		#axis(3, at = 1/dayaxis/24, labels = paste(dayaxis,'d',sep=""))

	}
	if(i > 1){
		lines(s$freq, s$spec, col=col[i], lwd=1.5,
			  lty = ifelse(i == 2, 4, 1))
	}
	CI <- spec.ci(s, coverage = 0.95)
	look21 <- which.min(abs(s$freq - 1/21/24))
	spec21[i] <- s$spec[look21]
	spec21cil[i] <- s$spec[look21]*CI[1]
	spec21ciu[i] <- s$spec[look21]*CI[2]	
	if(i == length(data)){
		#abline(v=1/21/24, col='grey55', lty=2)
		#abline(v=1/11/24, col='grey55', lty=2)
		#abline(v=mean(f20u), col='grey55', lty=2)
		#abline(v=mean(f20l), col='grey55', lty=2)	
		#mtext('21 d', side=3, at=1/21/24, cex=0.9)
		#mtext('11 d', side=3, at=1/11/24, cex=0.9)
		#mtext(paste(round(1/mean(f20u)/24),'d', sep=" "), side=3, at = mean(f20u), cex=0.8)
		#mtext(paste(round(1/mean(f20l)/24),'d', sep=" "), side=3, at = mean(f20l), cex =0.8)
		lines(s$freq[1] + c(-.5,.5) * s$bandwidth, rep(.55, 2), col='black', lwd=1.5)
		arrows(1/80/24, 0.4 + 0.025, 1/80/24, 0.4, length = 0.05)
		text(1/80/24, 0.4 + 0.025 + 0.02, '80d')
		arrows(1/21/24, 0.5 + 0.025, 1/21/24, 0.5, length = 0.05)
		text(1/21/24, 0.5 + 0.025 + 0.02, '21d *')
		arrows(1/11/24, 0.24 + 0.025, 1/11/24, 0.24, length = 0.05)
		text(1/11/24, 0.24 + 0.025 + 0.02, '11d *')
		
	}

	
}
legend('topright', 
lty=c(3,4, rep(1, length(depth)-2)), col=col, legend=c(paste(round(depth), 'm')), lwd=1.5, ncol=2, title = 'Flemish Cap')

plot(spec21,depth, xlim=c(0,1.6), ylim=rev(range(depth)), pch=20, col=col, ylab="", xlab="")
for (i in 1:length(depth)){
	lines(c(spec21ciu[i], spec21cil[i]), rep(depth[i],2), col=col[i])
}
mtext(expression(paste('Variance (m'^2,'s'^-2,'cph'^-1,')')), side=1, line=3, cex=0.9)
mtext('Depth (m)', side=2, line=2, cex=0.9)

if(!interactive()) dev.off()




