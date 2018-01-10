rm(list=ls())
library(oce)
library(ocedata)
library(cl)
library(fields)
data(coastlineWorldFine)
#source('filterAndSpectrumParameters.R')
#load('rotationangles.rda')
load('thetab.rda')
load("/data/floats/RAFOSexp.rda")
load('/data/topography/topoMaritimes.rda')
load('/data/flemishCap/CTD/CTDall_ODFQC.rda')
load('/data/flemishCap/CTD/CTDen492.rda')
load('/data/flemishCap/moorings/mooringMC.rda')
load('/data/flemishCap/moorings/mooringRCM.rda')
load('/data/flemishCap/moorings/mooringADCP.rda')
load('/data/flemishCap/msm27_ctd/msmCTD.rda')

fileList <- '/data/nafoClosure/2015_Closures_sponge_coral.shp'
files <- scan(text=fileList, what=character())
cl <- vector("list", length(files))
for (ifile in seq_along(files)) {
    cl[[ifile]] <- read.coastline.shapefile(files[ifile])
}



## get latitude and longitude of ctd stations and moorings
latfc <- unlist(lapply(FlemishCap, function(stn) stn[['latitude']][1])) #stn latitude
lonfc <- unlist(lapply(FlemishCap, function(stn) stn[['longitude']][1])) #stn longitude

#lat and lon of moorings from adcp data
latmoor <- c(m1adcp[[1]][['latitude']], m2adcp[[1]][['latitude']], m3adcp[[1]][['latitude']])
lonmoor <- c(m1adcp[[1]][['longitude']]*-1, m2adcp[[1]][['longitude']], m3adcp[[1]][['longitude']]*-1)

# set map plot options
proj <- "+proj=merc"
fill <- 'lightgray'

fc <- c(-150,-200, -550, -1000, -1500, -2000,-4000) #topo contours
col <- grey.colors(length(fc), start=0, end=.6)#topo colours

if(!interactive()) png('09_map.png', width=7, height=3.5, res=200, units='in', pointsize=12)

par(mar=c(2.5,2.5,.5,.5))
m <- matrix(c(1,1,2), nrow=1, ncol=3)
layout(m)
#latlim <- c(46.5,49.5)
latlim <- c(47.8,49.5)
lonlim <- c(-47,-44)
# plot map
mapPlot(coastlineWorldFine,latitudelim=latlim, longitudelim=lonlim, 
	proj=proj, col=fill, grid=c(1,.5), lonlabel=T)
mapContour(longitude,latitude,z, levels=rev(fc), col=col, lwd=0.7)
mapText(-44.84, 48.18, '550 m', srt = -30)
mapText(-44.69, 48.47, '1000 m', srt = -30)
mapText(-44.62, 48.66, '1500 m', srt = -30)
mapText(-44.44, 48.91, '2000 m', srt = -30)


for (ifile in seq_along(files)) {
    #xy <- lonlat2map(cl[[ifile]][["longitude"]], cl[[ifile]][["latitude"]])
    #x <- xy$x
    #y <- xy$y
    mapPolygon(cl[[ifile]][["longitude"]], cl[[ifile]][["latitude"]], col='grey80', lwd=1.2, border=0)   
}

mapPoints(lonfc, latfc, bg='grey',col='black',  pch=4 ,cex=1.4)
#mapPoints(lonen492, laten492, bg='grey37', col='black',  pch=21, cex=1.4)
mapPoints(lonmoor, latmoor, bg='black',col='grey', pch=21, cex=1.8)
mapText(lonmoor, latmoor, labels=c('FC','SS', 'FP'), adj=c(0,-1.3))
## rotated axis for FC mooring
axislength <- 60
mapCoordinateSystem(lonmoor[1], latmoor[1], L = axislength, phi= angleFC + 90, length=0.1, lwd=1.8)
mapText(-44.95, 49.13, labels= 'x', srt=angleFC+90)
mapText(-46, 49.25, labels='y', srt=angleFC+90)
## rotated axis for SS mooring
axislength <- 60
mapCoordinateSystem(lonmoor[2], latmoor[2], L = axislength, phi= angleSS + 90, length=0.1, lwd=1.8)
mapText(-45.87, 48.68, labels = 'x', srt = angleSS + 90)
mapText(-46.88, 48.83, labels = 'y', srt = angleSS + 90)




ssarrowlat <- c(48.56,48.17)
ssarrowlon <- c(-47.29,-46.67)
#mapArrows(ssarrowlon[1], ssarrowlat[1], ssarrowlon[2], ssarrowlat[2], length=0.05,lwd=1)
#mapText(-47.57,48.63,'Sackville Spur')
mapText(-45, 48, 'Flemish Cap')
mapText(-46.4, 47.8, 'Flemish Pass')
mapText(-46.96, 48.05, 'Sackville Spur', srt=30, adj=.3)

###depths of mooring instruments
#depths of madcp readings
depths <- NULL
depths[[1]] <- as.numeric(lapply(m1adcp, function(stn) stn[['depthMax']]))
depths[[2]] <- as.numeric(lapply(m2adcp, function(stn) stn[['depthMax']]))
depths[[3]] <- as.numeric(lapply(m3adcp, function(stn) stn[['depthMax']]))


#depths of microcat instrument
dmc <- NULL
dmc[[1]] <- as.numeric(lapply(m1mc, function(stn) stn[['depthMax']]))
dmc[[2]] <- as.numeric(lapply(m2mc, function(stn) stn[['depthMax']]))
dmc[[3]] <- as.numeric(lapply(m3mc, function(stn) stn[['depthMax']]))
#depths of rcm instrument
drcm <- NULL
drcm[[1]] <- as.numeric(lapply(m1rcm, function(stn) stn[['depthMax']]))
drcm[[2]] <- as.numeric(lapply(m2rcm, function(stn) stn[['depthMax']]))
drcm[[3]] <- NA

depthmax <- which.max(depths[[1]])
depthmin <- which.min(abs(depths[[1]] - min(dmc[[1]])))
depths[[1]] <- depths[[1]][c(depthmax, depthmin)]

depthmax <- which.max(depths[[2]])
depthmin <- which.min(abs(depths[[2]] - min(dmc[[2]])))
depths[[2]] <- depths[[2]][c(depthmax, depthmin)]

latm <- latmoor
lonm <- lonmoor
lonsub <- latsub <- NULL
par(mar=c(3,3,.5,1))
cexpt <- 1.2*2
for (i in 2){
	Ldist <- 60
	angle <- angleSS
	lengths <- seq(1,30,length.out=100)*1000
	delH.1 <- dist2pos(lonm[i], latm[i],L=rev(lengths), phi=angle)
	delH.2 <- dist2pos(lonm[i], latm[i],L=lengths, phi=angle-180)
	latH <- c(delH.1$latend, delH.2$latend)
	lonH <- c(delH.1$lonend, delH.2$lonend)
	depth <- interp.surface(list(x=longitude,y=latitude,z=z), cbind(lonH, latH))
	dist <- geodDist(longitude2=lonH[1], latitude2=latH[1], longitude1= lonH, latitude1 = latH, alongPath=TRUE) * 1000
	plot(dist, depth, type='l',ylim=c(min(depth), 0), xlab='', ylab='', xaxt='n', yaxs='i', xaxs='i')
	polyx <- c(dist, dist[length(dist)], dist[1], dist[1])
	polyy <- c(depth, depth[length(depth)], depth[length(depth)], depth[1])
	polygon(polyx, polyy, col='grey87', border=NA)
	lines(dist,depth)
	points(rep(dist[101], length(depths[[i]])), -depths[[i]], pch=1, cex=cexpt)
	points(dist[114], -200, pch=2, cex=1.2)	
	points(rep(dist[101], length(dmc[[i]])), -dmc[[i]], pch=4, lwd=1.3)

	points(rep(dist[101], length(drcm[[i]])), -drcm[[i]], pch=22, cex=cexpt)
	axis(1, at = seq(0,60000,10000), labels = seq(0,60000,10000)/1000)
	mtext('Distance (km)', side=1, line =1.9, cex=0.8)
	#mtext('Longitude', side=1,line=2, cex=0.8)
	if(i==2){mtext('Depth (m)', side=2,line=2, cex=2/3)}
	#mtext(paste(ifelse(i==1, 'Flemish Cap', ifelse(i==2, 'Sackville Spur', 'Flemish Pass'))), side=3,adj=1, cex=2/3)
	legend('topleft', col=c('black','black','black','black'), pch=c(1,4,2,22), legend=c('ADCP','MC','float','RCM'), bty='n', cex=1)	
}
if(!interactive()) dev.off()
