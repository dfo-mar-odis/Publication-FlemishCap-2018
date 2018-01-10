rm(list=ls())
load('thetab.rda')
source('filterAndSpectrumParameters.R')
source('specConfFn.R')

angle <- -( angleFC + 90)

##order microcat data to get upper depths for ADCP
d <- m1mc
depth <- getDepth(d, order=FALSE)
o <- order(depth)
Hyddepth <- depth[o]
 
#adcp data
d <- m1adcp
depth <- getDepth(d, order=FALSE)
look1 <- which.min(abs(Hyddepth[1] - depth))
look2 <- which.min(abs(Hyddepth[2] - depth))
adpd <- d[c(look1,look2)]

##order current meter data
d <- m1rcm
depth <- getDepth(d, order=FALSE)
o <- order(depth)
depth <- depth[o]
mcd <- d[o]

data <- c(adpd, mcd) #adcp data and current meter
depth <- getDepth(data)


if(!interactive()) png('05_specNFCseasonSplit.png', width=7.5, height=6, res=200, units='in', pointsize=10)
par(mfrow=c(2,1))
spans <- c(5,3)
col <- oceColorsJet(length(depth)+1)
col[6] <- 'darkgoldenrod2'
for (i in 1:length(data)){
	c <- data[[i]]
	vel <- RCMvelocity(c)
	time1 <- c[['time']]
	looktime <- time1 < '2014-01-01'
	time <- time1[looktime]
	u <- vel$u[looktime]
	v <- vel$v[looktime]
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
	par(mar=c(4,4,2,1))
	if(i == 1){
		plot(s$freq, s$spec, type='l', xlab="", ylab="", col=col[i], xlim=c(0,1/5/24), ylim=c(0,.75), lwd=1.5, lty = 2)
		mtext("Frequency (cph)", side=1, line=2)
		mtext(expression(paste('Variance (m'^2,'s'^-2,'cph'^-1,')')), side=2, line=2)
		dayaxis <- c(64,32,16,8,4)
		axis(3, at = 1/dayaxis/24, labels = paste(dayaxis,'d',sep=""))
	}
	if(i > 1){
		lines(s$freq, s$spec, col=col[i], lwd=1.5,
		     lty = ifelse(i == 2, 2, 1))
	}
}
#abline(v=1/21/24, col='grey55', lty=2)
#abline(v=1/11/24, col='grey55', lty=2)
#mtext('21 d', side=3, at=1/21/24)
#mtext('11 d', side=3, at=1/11/24)
#mtext('u-component NFC mooring', side=3, adj=1)
legend('topright', lty=c(rep(2,2), rep(1, length(depth)-2)), col=col, legend=c(paste(round(depth), 'm')), lwd=1.5, ncol=2)



for (i in 1:length(data)){
	c <- data[[i]]
	vel <- RCMvelocity(c)
	time1 <- c[['time']]
	looktime <- time1 > '2014-01-01'
	time <- time1[looktime]
	u <- vel$u[looktime]
	v <- vel$v[looktime]
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
	par(mar=c(4,4,2,1))
	if(i == 1){
		plot(s$freq, s$spec, type='l', xlab="", ylab="", col=col[i], xlim=c(0,1/5/24), ylim=c(0,.75), lwd=1.5, lty = 2)
		mtext("Frequency (cph)", side=1, line=2)
		mtext(expression(paste('Variance (m'^2,'s'^-2,'cph'^-1,')')), side=2, line=2)
		dayaxis <- c(64,32,16,8,4)
		axis(3, at = 1/dayaxis/24, labels = paste(dayaxis,'d',sep=""))
	}
	if(i > 1){
		lines(s$freq, s$spec, col=col[i], lwd=1.5,
		      lty = ifelse(i == 2, 2, 1))
	}
}
#abline(v=1/21/24, col='grey55', lty=2)
#abline(v=1/11/24, col='grey55', lty=2)
#mtext('21 d', side=3, at=1/21/24)
#mtext('11 d', side=3, at=1/11/24)
#mtext('u-component NFC mooring', side=3, adj=1)
legend('topright', lty=c(rep(2,2), rep(1, length(depth)-2)), col=col, legend=c(paste(round(depth), 'm')), lwd=1.5, ncol=2)

if(!interactive()) dev.off()
