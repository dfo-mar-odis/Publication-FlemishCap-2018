rm(list=ls())
source('filterAndSpectrumParameters.R')
load('webTideUandV.rda')
options(oceEOS="unesco")

proj <- "+proj=merc"
fill <- 'lightgray'

plotStationLocations <- function(distance, plabel, xlim){
	ok <- distance < xlim[2]
	par(xpd=NA)
	lines(range(distance[ok]), rep(plabel,2), lwd=5, col='white')
	points(distance[ok], rep(plabel, length(distance[ok])), pch=25, bg='black', cex=0.8)
	par(xpd=FALSE)
}

fc <- c(-100,-200,-300,-400, -500, -1000, -1500,-2000,-2500,-3000) #topo contours
col <- grey.colors(length(fc),start=0, end=0.6)
latlim <- c(46.8,49.5)
lonlim <- c(-47.5,-44.5)
colDepth <- c('darkred', 'plum', 'green', 'orange')

cex <- 2/3
transectbeg <- c(2,27,30,41,48)
transectend <- c(13,15,40,47,61)

section <- vector(mode='list', length=5)
section[[1]] <- seq(2,13,1)
section[[2]] <- rev(seq(21,27,1))
section[[3]] <- seq(30,40,1)
section[[4]] <- seq(41,47,1)
section[[5]] <- c(48, 50, 51, 52, 49, 53, 57)
sctd <- vector(mode='list', length=5)
sctd[[1]] <- seq(2,13, 1)
sctd[[2]] <- rev(seq(21,27,1))
sctd[[3]] <- seq(30,40,1)
sctd[[4]] <- seq(52,58,1)
sctd[[5]] <- c(59, 61, 62, 63, 60, 64, 68)
vvector <- vector(mode='list', length=5)
vvector[[1]] <- mround(getDepth(m3adcp)[c(22,32,38,44)],5)
vvector[[2]] <- mround(getDepth(m2mc)[c(1,3,5,7)],5)
vvector[[3]] <- mround(getDepth(m1mc)[c(1,3,5,7)],5)
vvector[[4]] <- vvector[[3]]
vvector[[5]] <- vvector[[2]]

mooringlon <- c(lonm[3], lonm[2], lonm[1], lonm[1], lonm[2])
mooringlat <- c(latm[3], latm[2], latm[1], latm[1], latm[2])
stime <- slongitude <- slatitude <- vector(mode='list', length=5)

sectionseq <- c(2,5,3,4)
trans <- c('SS', 'SS', 'FC', 'FC')
urlist <- plist <- distancelist <- mooringladp <- urt <- vrt <- vector(mode='list', length=4)
angles <- vector(length=4)

for (i in sectionseq){	
	if (i == 2 | i == 3){
		print(i)
		if(!interactive()) 	png(paste('01_sectionTransect',trans[i],'.png',sep=""), width=9, height=3.5*2, units='in', res=200, type='cairo', pointsize=13)
		par(mfrow=c(2,3), mar=c(3,3,1,1))
	}
	
	data <- ladcp[section[[i]]]
	webTideu <- utide[[i]]
	webTidev <- vtide[[i]] 
	s <- as.section(data) #ladcp
	#find angle of transect
	slon <- s[['longitude', 'byStation']]
	slat <- s[['latitude', 'byStation']]
	mlon <- mooringlon[i]
	mlat <- mooringlat[i]
	lookloc <- which.min(geodDist(slon, slat, mlon, mlat))

	longitudeRef <- -52
	latitudeRef <- 52
	xy <- geodXy(slon,slat, longitudeRef = longitudeRef, latitudeRef= latitudeRef)
	lm <- lm(xy$y ~ xy$x)
	newx <- seq(xy$x[1], xy$x[length(xy$x)], by=200)
	newy <- coef(lm)[1] + newx*coef(lm)[2]
	lonlat <- geodXyInverse(newx, newy, longitudeRef = longitudeRef, latitudeRef = latitudeRef)
	bottomdepth <- interp.surface(list(x=longitude, y=latitude, z=z), cbind(lonlat$longitude, lonlat$latitude))
	bottomdepthdistance <- geodDist(lonlat$longitude, lonlat$latitude,lonlat$longitude[1], lonlat$latitude[1], alongPath=TRUE)
	print(range(bottomdepthdistance))
	#abline(lm)

	a <- atan(coef(lm)[2])*180/pi #upslope angle
	print(angle)	
	#print(angle)
	angle <- a + 90
	print(paste('along-slope angle',angle))
	#rotate velocity, add to data var
	ok <- which(i == sectionseq)
	for (k in 1:length(data)){
		u <- data[[k]][['u']]
		v <- data[[k]][['v']]
		ut <- webTideu[k]
		vt <- webTidev[k]
		rU <- rotateVelocity(u=u,v=v, angle=-angle)
		rUtide <- rotateVelocity(u=ut, v=vt, angle=-angle)
		data[[k]]@data$ur <- rU$u - rUtide$u
		data[[k]]@data$vr <- rU$v - rUtide$v
		urt[[ok]][k] <- unname(rUtide$u)
		vrt[[ok]][k] <- unname(rUtide$v)
	}
	
	sr <- as.section(data) #make rotate var a section
	sc <- as.section(FlemishCap[sctd[[i]]]) #ctd
	if (i ==5){
		sc@data$station[[2]]@metadata$waterDepth <- 2437
	}
	sg <- sectionGrid(sc)
	p <- sg[['station', 1]][['pressure']]
	distance <- sg[['distance', "byStation"]]
	salinity <- matrix(sg[["salinity"]], nrow=length(distance), ncol=length(p), byrow=TRUE)
	temperature <- matrix(sg[["temperature"]], nrow=length(distance), ncol=length(p), byrow=TRUE)
	pressure <- matrix(sg[["pressure"]], nrow=length(distance), ncol=length(p), byrow=TRUE)
	sigmaTheta <- swSigmaTheta(salinity, temperature, pressure)

	srr <- sectionGrid(sr)
	rdistance <- srr[['distance', 'byStation']]
	rp <- srr[['station',1]][['pressure']]
	ur <- matrix(srr[["ur"]], nrow=length(rdistance), ncol=length(rp), byrow=TRUE)
	vr <- matrix(srr[["vr"]], nrow=length(rdistance), ncol=length(rp), byrow=TRUE)
	dapprox <- approx(distance, n=length(distance)*2-1)
	vline <- dapprox$y[which(lookloc == dapprox$x)]
	
	Tlim <- c(-2,10)
	Slim <- c(32,35)
	xlim <- c(0,35)
	plot(sr, which='ur', ztype='image', zbreaks=seq(-0.4, 0.4, 0.05), zcol=oceColorsTwo, showBottom=FALSE, xlim=xlim)
	contour(rdistance, rp, ur, add=TRUE, levels=c(0,.2,.4), lwd=c(.5,.5,.5*1.4), lty=c(2,1,1), xlim=c(0,35))
	abline(v=vline, lty=2)
	contour(distance, p, sigmaTheta, add=TRUE, lwd=1.4*1.4, labcex=0.7, levels=c(27.68, 27.74, 27.8), xlim=c(0,35))
	bdd <- bottomdepthdistance
	bd <- bottomdepth*-1
	xpoly <- c(bdd[1], bdd, ifelse(max(rdistance) < max(distance), max(distance), max(rdistance)), ifelse(max(rdistance) < max(distance), max(distance), max(rdistance)), bdd[1], bdd[1])
	ypoly <- c(bd[1], bd, bd[length(bd)], max(bd), max(bd), bd[1])
	polygon(xpoly, ypoly, col='lightgrey')
	axis(1)
	
	plotStationLocations(distance = rdistance, plabel = -50, xlim=xlim)
	legend('bottomright', legend=expression(u[r]), bg='white', x.intersp=0, y.intersp=0.5)


	plot(sr, which='vr', ztype='image', zbreaks=seq(-0.4, 0.4, 0.05), zcol=oceColorsTwo, showBottom=FALSE, xlim=c(0,35))
	contour(rdistance, rp, vr, add=TRUE, levels=c(0,-.2), lwd=c(.5,.5), lty=c(2,1), xlim=c(0,35))
	contour(distance, p, sigmaTheta, add=TRUE, lwd=1.4*1.4, labcex=0.7, levels=c(27.68, 27.74, 27.8), xlim=c(0,35))
	abline(v=vline, lty=2)	
	polygon(xpoly, ypoly, col='lightgrey')
	axis(1)
	plotStationLocations(distance = rdistance, plabel = -50, xlim=xlim)
	legend('bottomright', legend=expression(v[r]), bg='white', x.intersp=0, y.intersp=0.5)

	lon <- s[['longitude', 'byStation']]
	lat <- s[['latitude', 'byStation']]
	latlim <- range(lat) + c(-.3,.3)
	lonlim <- range(lon) + c(-.3,.3)
	mapPlot(coastlineWorldFine,latitudelim=latlim, longitudelim=lonlim, proj=proj, grid=c(.25,.25), col=fill)
	mapContour(longitude,latitude,z, levels=rev(fc), col=col, lwd=.8)
	mapPoints(mlon, mlat, pch=20, col = 'red', cex = 2)
	lookDepth <- vvector[[i]]
	okstns <- which(rdistance < xlim[2])
	for(j in okstns){
		for (m in 3){
			d <- data[[j]]
			if(max(d[['pressure']] >= lookDepth[[m]])){
			look <- which.min(abs(d[['pressure']] - lookDepth[[m]]))
			mua <- mean(d[['u']][(look-2):(look+2)])
			mva <- mean(d[['v']][(look-2):(look+2)])
			mapDirectionField(longitude = lon[[j]], latitude=lat[[j]], u=mua, v=mva, col='black', length=0.05, lwd=1.4)}
					}
		}
		if (i == 1){
		mapDirectionField(longitude=lonlim[2]-.5, latitude=round(latlim[1],1), u=.25, v=0, length=0.05)
		mapText(longitude= lonlim[2]-.3, latitude=round(latlim[1],1)+.09, labels='0.25 m/s')
		}
		else{
		mapDirectionField(longitude=lonlim[1] + .05, latitude=round(latlim[1],1) + .1, u=.25, v=0, length=0.05)
		mapText(longitude= lonlim[1]+.25, latitude=round(latlim[1],1)+.07, labels='0.25 m/s')
		#mapCoordinateSystem(slon[1], slat[1], phi=a)
		}
	if(i == 5 | i == 4){	
	if(!interactive()) dev.off()}
	stime[[i]] <- srr[['time', 'byStation']]
	slongitude[[i]] <- srr[['longitude', 'byStation']]
	slatitude[[i]] <- srr[['latitude', 'byStation']]
	urlist[[i]] <- ur
	plist[[i]] <- rp
	distancelist[[i]] <- rdistance
	angles[which(i == sectionseq)] <- angle
	mooringladp[which(i == sectionseq)] <- sr@data$station[[lookloc]]
	}
	
ladpspeed <- lapply(mooringladp, function(k) sqrt(k[['u']]^2 + k[['v']]^2))
ladpmeanspeed <- lapply(ladpspeed, mean)
ladpmeanspeedsd <- lapply(ladpspeed, sd)
ladpur <- lapply(mooringladp, function(k) k[['ur']])
ladpurmean <- lapply(ladpur, mean)
ladpursd <- lapply(ladpur, sd)
ladpurrange <- lapply(ladpur, range)
ladpvr <- lapply(mooringladp, function(k) k[['vr']])
ladpvrmean <- lapply(ladpvr, mean)
ladpvrsd <- lapply(ladpvr, sd)

if(!interactive()){
save(mooringladp, trans, file='mooringladp.rda')
save(urlist, plist, distancelist, file='vrPDistance.rda')
save(urt, vrt, trans, file = 'rotatedTidesBySec.rda')
}



