rm(list=ls())
angleFromIsobath <- TRUE

## Rossby lengths
km <- 1e3
LLSS <- 0.86e-3*1454/1.09e-4/km        # SS Rossby length [km] using table 3 (11.47193 km)
LLFC <- 0.97e-3*1485/1.1e-4/km         # FC Rossby length [km] using table 3 (13.095 km)
span <- 3                              # span in Rossby lengths
#FAC <- 1.25                             # we look +-FAC*LLSS etc along isobath to get angle
## FAC <- 1 # FC bad


library('KernSmooth')
load('thetab.rda')
load('thetavFC.rda')
load('thetavSS.rda')
source('filterAndSpectrumParameters.R')

angle <- angleFC + 90
lon <- lonm[1]
lat <- latm[1]

angle2 <- angleSS + 90
lonss <- lonm[2]
latss <- latm[2]

##order flemish cap current meter data
d <- m1rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
data <- d[o]

##order sackville spur data
d <- m2rcm
depth2 <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth2)
depth2 <- depth2[o]
data2 <- d[o]



fc <- seq(-1000, -2000, length.out=6)
 if(!interactive()) png('12_uvOnMap_dk.png', width=8.5, height=6, units='in', res=200, pointsize=12)
par(mar=c(1.5,1.5,1.2,0.5), mfrow=c(2,2))
 	
for (i in 4:5){

	d <- data2[[i]]
	u <- RCMvelocity(d)$u
	v <- RCMvelocity(d)$v
	time <- d[['time']]
	rt <- rotateDetideButterfilter(u , v, time, rotate=FALSE, angle=angle, butterfilter=TRUE, deltat=deltat, cutoff=cutoff)
	
	urtb <- rt$urtb
	vrtb <- rt$vrtb
	Urtb <- sqrt(urtb^2 + vrtb^2)
	theta <- atan2(vrtb, urtb)*180/pi
	
	p <- dist2pos(lonss, latss, L = 2e5 * Urtb, phi = theta)
	bp <- dist2pos(lonss, latss, L=3e4, phi = angle2)
	vp <- dist2pos(lonss, latss, L=3e4, phi = thetavSS[i])

	b <- bkde2D(cbind(p$lonend, p$latend), bandwidth=c(0.015, 0.015), gridsize=c(200,200))
	
	coltopo <- grey.colors(length(fc), start=.1, end=.3)
	latlim <- c(48.15,48.6)
	lonlim <- c(-46.8,-46.1)
	mapPlot(coastlineWorldFine, proj="+proj=merc", longitudelim=lonlim,latitudelim=latlim, col='grey', grid=c(.25,.25))
	g <- 0.2


	#mapPoints(p$lonend, p$latend, pch=20, col=rgb(rep(g,length(p$lonend)), rep(g+.1, length(p$lonend)), rep(g, length(p$lonend)),alpha=.1), cex=.3)
	mapPoints(p$lonend, p$latend, pch=20, col='grey', cex=.3)
        mapContour(longitude, latitude, z, levels=fc, col=coltopo, lwd=.8)	
                                        #mapContour(b$x1, b$x2, b$fhat, col='blue', lwd=1.4, nlevels=5)  
	  
	mapArrows(lonss,latss, bp$lonend, bp$latend, length=0.05, lwd=3.5*1.4, col='white')
	mapArrows(lonss,latss, bp$lonend, bp$latend, length=0.05, lwd=1.5*1.4, col='red')
	mapArrows(lonss,latss, vp$lonend, vp$latend, length=0.05, lwd=3.5*1.4, col='white')
	mapArrows(lonss,latss, vp$lonend, vp$latend, length=0.05, lwd=1.5*1.4, col='blue')
	#mapLines(c(lonss, vp$lonend),c(latss, vp$latend), col='white', lty=2, lwd=1.5*1.4)
	mtext(paste('SS',round(depth2[i]),'m'), adj=1)
        if (angleFromIsobath) {
            a <- contourLines(longitude, latitude, z, levels=-1454)
            alen <- unlist(lapply(a, function(x) length(x$x)))
            aa <- a[[which.max(alen)]] # pick the longest trace, ignoring little squiggles
            dist <- geodDist(aa$x, aa$y, lonss, latss)
            lookingDistance <- 0.5 * span * LLSS
            message("SS: lookingDistance=", lookingDistance, " km")
            look <- dist <= lookingDistance
            aaa <- list(x=aa$x[look], y=aa$y[look])
            xy <- lonlat2map(longitude=aaa$x, latitude=aaa$y)
            lines(xy$x, xy$y, lwd=5)
            points(lon, lat, pch=20, cex=2) # so DK can see it
            evec <- eigen(cov(cbind(xy$x, xy$y)))$vectors[,1]
            angleDKe <- atan2(evec[2], evec[1])*180/pi+180
            m <- lm(y~x,data=xy)
            abline(m, col='forestgreen', lwd=2, lty='dotted')
            angleDK <- atan(coef(m)[2])*180/pi
            mtext(sprintf("span=%.1f; angles DK=%.2f DKe=%.2f text=%.2f",
                          span, angleDK, angleDKe, angle2),
                  side=3, adj=0, col='magenta', cex=0.8)
        }

	
	
	
	d <- data[[i]]
	u <- RCMvelocity(d)$u
	v <- RCMvelocity(d)$v
	time <- d[['time']]
	rt <- rotateDetideButterfilter(u , v, time, rotate=FALSE, angle=angle, butterfilter=TRUE, deltat=deltat, cutoff=cutoff)
	
	urtb <- rt$urtb
	vrtb <- rt$vrtb
	Urtb <- sqrt(urtb^2 + vrtb^2)
	theta <- atan2(vrtb, urtb)*180/pi
	
	p <- dist2pos(lon, lat, L = 2e5 * Urtb, phi = theta)
	bp <- dist2pos(lon, lat, L=3e4, phi = angle)
	vp <- dist2pos(lon, lat, L=3e4, phi = thetavFC[i])
	cl <- dist2pos(lon, lat, L=6e4, phi = 40)
	cl2 <- dist2pos(lon, lat, L=6e4, phi = 40 + 180)


	coltopo <- grey.colors(length(fc), start=.1, end=.3)
	latlim <- c(48.7,49)
	lonlim <- c(-46,-45.2)
	mapPlot(coastlineWorldFine, proj="+proj=merc", longitudelim=lonlim,latitudelim=latlim, col='grey', grid=c(.25,.25))
	g <- 0.2
	#mapPoints(p$lonend, p$latend, pch=20, col=rgb(rep(g,length(p$lonend)), rep(g+.1, length(p$lonend)), rep(g, length(p$lonend)),alpha=.1), cex=.8)
	mapPoints(p$lonend, p$latend, pch=20, col='grey', cex=.3)
	mapContour(longitude,latitude,z,levels=fc ,col=coltopo, lwd=.8)
	mapArrows(lon,lat, bp$lonend, bp$latend, length=0.05, lwd=3.5*1.4, col='white')
	mapArrows(lon,lat, bp$lonend, bp$latend, length=0.05, lwd=1.5*1.4, col='red')
	mapArrows(lon,lat, vp$lonend, vp$latend, length=0.05, lwd=2*1.4, col='black')
	mapArrows(lon,lat, vp$lonend, vp$latend, length=0.05, lwd=1.5*1.4, col='blue')
	mapArrows(lon,lat, vp$lonend, vp$latend, length=0.05, lwd=1.5*1.4, col='blue')
	mapArrows(lon,lat, cl$lonend, cl$latend, length=0, lwd=1.5*1.4, col='red', lty=3)
	mapArrows(lon,lat, cl2$lonend, cl2$latend, length=0, lwd=1.5*1.4, col='red', lty=3)
	#mapLines(c(lon, vp$lonend),c(lat, vp$latend), col='white', lty=2, lwd=2)
	mtext(paste('FC',round(depth[i]),'m'), adj=1)	
        if (angleFromIsobath) {
            a <- contourLines(longitude, latitude, z, levels=-1485)
            alen <- unlist(lapply(a, function(x) length(x$x)))
            aa <- a[[which.max(alen)]] # pick the longest trace, ignoring little squiggles
            dist <- geodDist(aa$x, aa$y, lon, lat)
            lookingDistance <- 0.5 * span * LLFC
            message("FC: lookingDistance=", lookingDistance, " km")
            look <- dist <= lookingDistance
            aaa <- list(x=aa$x[look], y=aa$y[look])
            xy <- lonlat2map(longitude=aaa$x, latitude=aaa$y)
            lines(xy$x, xy$y, lwd=5)
            points(lon, lat, pch=20, cex=2) # so DK can see it
            evec <- eigen(cov(cbind(xy$x, xy$y)))$vectors[,1]
            angleDKe <- atan2(evec[2], evec[1])*180/pi+180
            m <- lm(y~x,data=xy)
            abline(m, col='forestgreen', lwd=2, lty='dotted')
            angleDK <- atan(coef(m)[2])*180/pi
            mtext(sprintf("span=%.1f; angles DK=%.2f DKe=%.2f text=%.2f",
                          span, angleDK, angleDKe, angle),
                  side=3, adj=0, col='magenta', cex=0.8)
            ##browser()
        }

}

if(!interactive()) dev.off()
