rm(list=ls())
library(smatr)
source('filterAndSpectrumParameters.R')

##order current meter data
d <- m1rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
data <- d[o]

eigenAngle <- function(x, y){
		e <- eigen(cov(data.frame(x, y)))
		a <- atan2(e$vectors[2,1], e$vectors[1,1])*180/pi
		ifelse(a > 0, a-180, a+180)
}

seSlopeSma <- function(m){
	slope <- as.numeric(m$coef[[1]][2,1])
	ci <- as.numeric(m$coef[[1]][2, 2:3])
	n <- as.numeric(m$groupsummary[2])
	diff(ci) / 2 / qt(0.95, df = n -1)
}

thetav <- thetav2 <- thetavsd <- thetavn <- vector(length=length(data))

#utb <- vtb <- timetb <- vector(mode='list', length=length(data))
for (i in 1:length(data)){
	d <- data[[i]]
	u <- RCMvelocity(d)$u
	v <- RCMvelocity(d)$v
	time <- d[['time']]
	rtb <- rotateDetideButterfilter(u , v, time, rotate=FALSE, deltat=deltat, cutoff=cutoff)
	
	urtb <- rtb$urtb
	vrtb <- rtb$vrtb
	
	m <- sma(vrtb ~ urtb)
	slope <- as.numeric(m$coef[[1]][2,1])
	thetav[i] <- atan(slope) * 180/pi
	sd <- seSlopeSma(m)
	thetavsd[i] <- sd / (1 + slope^2)
	thetavn[i] <- unlist(m$n)
	#thetav[i] <- eigenAngle(urtb , vrtb)
	#plot(urtb, vrtb, asp=1)
	#abline(m)
	#utb[[i]] <- urtb
	#vtb[[i]] <- vrtb
	#timetb[[i]] <- time
	}

#u <- utb
#v <- vtb
#time <- timetb

#save(u,v,time,depth, file='detidedLowpassedForDK.rda')
	
thetavFC <- thetav
thetavsdFC <- thetavsd
thetavnFC <- thetavn

save(thetavFC, thetavsdFC, thetavnFC, file='thetavFC.rda')