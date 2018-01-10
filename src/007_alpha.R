rm(list=ls())
source('filterAndSpectrumParameters.R')
load('thetab.rda')

angle <- c(angleFC, angleSS)
alpha <- alphase <- vector(length=2)
lookdist <- c(60,60)
for (i in 1:2){

	lengths <- seq(1,lookdist[i],length.out=100)*1000 #going to want to do a loop starting here
	delH.1 <- dist2pos(lonm[i],latm[i],L=rev(lengths), phi=angle[i])
	delH.2 <- dist2pos(lonm[i], latm[i],L=lengths, phi=angle[i] + 180)

	latH <- c(delH.1$latend,latm[i], delH.2$latend)
	lonH <- c(delH.1$lonend,lonm[i], delH.2$lonend)

	
	#Hmoor <- interp.surface(list(x=x,y=y,z=z), cbind(lon[1], lat[1]))
	slopeDepth <- interp.surface(list(x=longitude,y=latitude,z=z), cbind(lonH, latH))
	dist <- geodDist(longitude2=lonH[1], latitude2=latH[1], longitude1= lonH, latitude1 = latH, alongPath=TRUE) * 1000

	d <- lm(slopeDepth ~ dist)
	alpha[i] <- unname(d$coef[2]) # alpha using linear, see alpha.R for graph
	alphase[i] <- unname(coef(summary(d))[2,2])
	alphan <- length(dist)
	
	plot(dist, slopeDepth)
	abline(d)
	mapPlot(coastlineWorldFine, longitudelim = c(-48, -44), latitudelim=c(45,50), projection='+proj=merc')
	mapContour(longitude, latitude, z)
	mapPoints(lonH, latH)
	
}
alpha <- abs(alpha)
save(alpha, alphase, alphan, file='Alpha.rda')