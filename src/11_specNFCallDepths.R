rm(list=ls())
#load('rotationangles.rda')
load('thetab.rda')
source('filterAndSpectrumParameters.R')
source('specConfFn.R')

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

if(!interactive()) png('11_specFC.png', width=7.5, height=3, res=200, units='in', pointsize=10)
spans <- c(5,3)
col <- oceColorsJet(length(depth)+1)
col[6] <- 'darkgoldenrod2'
m <- cbind(1,1,2)
layout(m)
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
	
	s <- spectrum(urt, spans=spans, plot=FALSE)
	findvar <- findVariance(s)
	f20u[i] <- findvar$f20u
	f20l[i] <- findvar$f20l
	#print(spec.ci(s, coverage=0.95))
	par(mar=c(4,4,1.5,1))
	if(i == 1){
		plot(s$freq, s$spec, type='l', xlab="", ylab="", col=col[i], xlim=c(0,1/5/24), ylim=c(0,.8), lwd=1.5)
		mtext("Frequency (cph)", side=1, line=2.5, cex=0.9)
		mtext(expression(paste('Variance (m'^2,'s'^-2,'cph'^-1,')')), side=2, line=2, cex=0.9)
		#drawSpectrumLimits(s, lty=2)
	}
	if(i > 1){
		lines(s$freq, s$spec, col=col[i], lwd=1.5)
	}
	CI <- spec.ci(s, coverage = 0.95)
	look21 <- which.min(abs(s$freq - 1/21/24))
	spec21[i] <- s$spec[look21]
	spec21cil[i] <- s$spec[look21]*CI[1]
	spec21ciu[i] <- s$spec[look21]*CI[2]	
	if(i == length(data)){
		abline(v=1/21/24, col='grey55', lty=2)
		abline(v=1/11/24, col='grey55', lty=2)
		abline(v=mean(f20u), col='grey55', lty=2)
		abline(v=mean(f20l), col='grey55', lty=2)		
		mtext('21 d', side=3, at=1/21/24, cex=0.9)
		mtext('11 d', side=3, at=1/11/24, cex=0.9)
		mtext(paste(round(1/mean(f20u)/24),'d', sep=" "), side=3, at = mean(f20u), cex=0.8)
		mtext(paste(round(1/mean(f20l)/24),'d', sep=" "), side=3, at = mean(f20l), cex =0.8)
		#lines(rep(s$freq[look21],2), c(s$spec[look21], s$spec[look21]*CI[1]), col=col[i])
		#lines(rep(s$freq[look21],2), c(s$spec[look21], s$spec[look21]*CI[2]), col=col[i])
		#lines(s$freq[look21] + c(-.5,.5) * s$bandwidth, rep(s$spec[look21],2), col=col[i])
		lines(s$freq[1] + c(-.5,.5) * s$bandwidth, rep(.78, 2), col='black', lwd=1.5)
		
	}

	
}
#abline(v=1/21/24)
#abline(v=1/11/24)
#mtext('21 d', side=3, at=1/21/24)
#mtext('11 d', side=3, at=1/11/24)
#mtext('u-component NFC mooring', side=3, adj=1)
legend('topright', lty=1, col=col, legend=c(paste(round(depth), 'm')), lwd=1.5, ncol=2)

plot(spec21,depth, xlim=range(c(spec21cil, spec21ciu)), ylim=rev(range(depth)), pch=20, col=col, ylab="", xlab="")
for (i in 1:length(depth)){
	lines(c(spec21ciu[i], spec21cil[i]), rep(depth[i],2), col=col[i])
}
mtext(expression(paste('Variance (m'^2,'s'^-2,'cph'^-1,')')), side=1, line=3, cex=0.9)
mtext('Depth ( m )', side=2, line=2, cex=0.9)

lm <- lm(spec21 ~ depth)
dz <- mean(diff(depth))
slope <- coef(lm)[2] * dz
save(slope, file='linearSlopePeak.rda')
if(!interactive()) dev.off()

spans <- c(7,3)
if(!interactive()) png('11_specFCerrorBar.png', width=7.5, height=3, res=200, units='in', pointsize=10)
par(mar=c(3.5,3.5,1,1))
s <- spectrum(urt, spans=spans, xlim=c(0,1/5/24), main='', ylab='', xlab='', ylim=c(5e-3, 5e-1), col=col[(length(col)-1)], lwd=1.5) 
#plot(s$freq, log10(s$spec), type='l', xlab="", ylab="", col=col[i],, lwd=1.5)
mtext("Frequency [cph]", side=1, line=2, cex=0.9)
mtext(expression(paste('variance [m'^2,'s'^-2,'cph'^-1,']')), side=2, line=2, cex=0.9)
plotSpecConf(s, ci=0.95, ci.col='blue', conf.x = 1/5/24 - s$bandwidth)
abline(v=1/21/24, col='grey55', lty=2)
abline(v=1/11/24, col='grey55', lty=2)
mtext('21 d', side=3, at=1/21/24, cex=0.9)
mtext('11 d', side=3, at=1/11/24, cex=0.9)
if(!interactive()) dev.off()
