rm(list=ls())
#load('rotationangles.rda')
load('thetab.rda')
source('filterAndSpectrumParameters.R')
source('specConfFn.R')
source('find80varForAdcp.R')

angle <- -( angleFC + 90 )

#adcp data
data <- m1adcp
depth <- unlist(lapply(data, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
data <- data[o]
depth <- depth[o]

Nyquist <- 2*1 #samp freq of 1hour
lf <- 1/60/24 #low freq filter of 60 days
hf <- 1/3/24 #high freq filter of 3 days
f <- butter(2, W=c(lf,hf)*Nyquist, "pass")

spans <- c(5,3)
col <- oceColorsJet(length(depth)+1)
var <- spec80 <- NULL
if(!interactive()) png('16_FCadcpSpecAllbins.png',
	width = 7.5, height = 3, res = 200, units ='in',
	pointsize = 10)
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
	#findvar <- findVariance(s)
	#f20u[i] <- findvar$f20u
	#f20l[i] <- findvar$f20l
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
	if(i == length(data)){
		abline(v= 1/80/24)
		abline(v= 1/45/24)		
		lines(s$freq[1] + c(-.5,.5) * s$bandwidth, rep(.78, 2), col='black', lwd=1.5)
		
	}
	look80 <- which.min(abs(s$freq - 1/80/24))
	spec80[i] <- s$spec[look80]
	var[i] <- find80var(s = s)$var
	
}

legend('topright', lty=1, col=col, legend=c(paste(round(depth), 'm')), lwd=1.5, ncol=3)
if(!interactive()) dev.off()

var80lm <- lm(var ~ depth)
var80lm <- lm(spec80 ~ depth)
dz <- mean(diff(depth))
slope80 <- coef(var80lm)[2] * dz
save(slope80, file = 'adcplm.rda')
