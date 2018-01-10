rm(list=ls())
library(biwavelet)
load('thetab.rda')
source('filterAndSpectrumParameters.R')

angle <- -( angleFC + 90)

periodToY <- function(period, label)
    log2(period)

timeToX <- function(time, start)
    (as.numeric(time) - as.numeric(start)) / 24 / 3600


#' Add horiz line at given period, plus note in margin 4
periodLabel <- function(tau, label)
{
	rug(x = periodToY(tau), side = 4)
    #abline(h=periodToY(tau), lty = 2)
    if (missing(label))
        label <- tau
    mtext(label, side=4, at=periodToY(tau), line=0)
}

##mooring location
lat <- m1adcp[[1]][['latitude']]
lon <- m1adcp[[1]][['longitude']]*-1
L <- 60

data <- c(orderByDepth(m2rcm)[length(m2rcm)], orderByDepth(m1rcm)[length(m1rcm)])
depth <- getDepth(data)

if(!interactive()) png('04_waveletDetideSSandFC.png', width=6, height=6, units='in', res=200, pointsize=10)

par(mfrow=c(2,1))	
for(i in 1:length(data)) {
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
	depth <- data[[i]][['depthMax']]
	stn <- data[[i]][['station']]

	par(mar=c(2, 3, 1, 1), mgp=c(2, 0.7, 0))
## Kludgy way to get time axis
	tu <- cbind(seq.int(0, by=1/24, length.out=length(urt)),urt)
	WT <- wt(tu)
	plot(WT, xaxt='n', xlab="", ylab="Period [days]", ncol=100, bw=TRUE, plot.phase=FALSE, alpha.coi=1, ylim=c(5,136)) # do not draw x axis
	## whiteout
	n <- length(WT$t)
	bottom <- 2^par('usr')[3]
	X <- c(WT$t[1], WT$t, WT$t[n], WT$t[1])
	Y <- log2(c(bottom, WT$coi, bottom, bottom))
	polygon(X, Y, col='white')
	
	if(i == 1){
		periodLabel(80)
		periodLabel(21)
		periodLabel(13)
		periodLabel(9)
	}
	if(i == 2){
		periodLabel(80)
		periodLabel(21)
		periodLabel(11)
	}
	timeLabels <- pretty(time, 15)
	start <- time[1]
	day <- timeToX(time, start)
	at <- timeToX(timeLabels, start)
	labels <- format(timeLabels, "%b")
	axis(side=1, at=at, labels=labels)
newYearsDay <- timeToX(as.POSIXct("2014-01-01 00:00:00", tz="UTC"), start)
	#abline(v=newYearsDay, lwd=2)
	if ( i == 1){
	mtext("2013 ", side=3, at=newYearsDay, adj=1)
	mtext(" 2014", side=3, at=newYearsDay, adj=0)
	mtext("|", side=3, at=newYearsDay)}
	#mtext(paste(depth[i],'m'), side=3, adj=1)

}
	if(!interactive()) dev.off()
