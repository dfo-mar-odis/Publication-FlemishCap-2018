rm(list=ls())
library(oce)
library(cl)
library(fields)
library(ocedata)
library(RColorBrewer)
data(coastlineWorldFine)
load('/data/topography/topoMaritimes.rda')
load('/data/flemishCap/CTD/CTDall_ODFQC.rda')
load('/data/flemishCap/CTD/CTDen492.rda')
load('/data/flemishCap/moorings/mooringMC.rda')
load('/data/flemishCap/moorings/mooringRCM.rda')
load('/data/flemishCap/moorings/mooringADCP.rda')
load('/data/flemishCap/msm27_ctd/msmCTD.rda')
load("/data/floats/RAFOSexp.rda")

praf <- lapply(p, function(f) replace(f, is.infinite(f), NA))
praflist <- unlist(lapply(praf, function(f) mean(f, na.rm=TRUE)))
praflook <- which(praflist < 1000 & praflist > 10)
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}




if (!interactive()) png('06_rafosTracks.png', width=7.5, height=3.5, res=200, unit='in', pointsize=10)

lon <- longitude #topo longitude
lat <- latitude #topo latitude

latlim <- c(46,49)
lonlim <- c(-47,-44)

proj <- "+proj=merc"
fill <- 'lightgray'

#fc <- c(-140,-250, -550, -1100) #topo contours
fc <- c(-200, -550, -1000, -2000) #topo contours
col <- grey.colors(length(fc), start=.4, end=.7)#topo colours

lonlabel <- -47.92
latlabel <- 47.27
srt <- -77
fcl <- fc*-1

scaleLat <- 48.85
scaleLon <- -47.5
lengthb <- 100

#m <- rbind(c(1,1,2,2,2), c(1,1,3,4,5))
#m <- cbind(c(1,1,1), c(1,1,1), c(2,3,4))
#m <- cbind(c(2,3,4), c(1,1,1), c(1,1,1))
#m <- cbind(c(2,1,1), c(3,1,1), c(4,1,1))
#m <- cbind(c(1,1,1), c(2,3,4))
#layout(m)

#lat and lon of moorings from adcp data
latm <- c(m1adcp[[1]][['latitude']], m2adcp[[1]][['latitude']], m3adcp[[1]][['latitude']])
lonm <- c(m1adcp[[1]][['longitude']]*-1, m2adcp[[1]][['longitude']], m3adcp[[1]][['longitude']]*-1)

morlat <- latm[1:2]
morlon <- lonm[1:2]

par(mar=c(2.5,2.5,1,1), mfrow=c(1,2))
mapPlot(coastlineWorldFine,latitudelim=latlim, longitudelim=lonlim, proj=proj, col=fill, grid=c(.5,.5), lonlabel=T)
mapContour(lon,lat,z, levels=rev(fc), col=col)

keep <- c(1,2,10,26,30,32,36,49) #floats that go through fp or across SS
keep2 <- outersect(keep, praflook)
keep2 <- keep2[-7] #this float stopped sending signal around SS
set.seed(19900114)
samp <- sample(keep2, length(keep))
colpal <- oce.colorsJet(length(keep))
colpal[5] <- 'goldenrod3'
colpal[4] <- 'olivedrab4'
datekeep <- as.POSIXlt(unlist(lapply(date[keep], function(stn) stn[1])), origin='1970-01-01')
datekeep2 <- as.POSIXlt(unlist(lapply(date[keep2], function(stn) stn[1])), origin='1970-01-01')

yrkeep <- datekeep$year + 1900
yrkeep2 <- datekeep2$year + 1900

monkeep <- datekeep$mon + 1
monkeep2 <- datekeep2$mon + 1

y1pal <- brewer.pal(9, 'Reds')
y2pal <- brewer.pal(9, 'Purples')
y3pal <- brewer.pal(9, 'Greens')
y4pal <- brewer.pal(9, 'Blues')
y5pal <- brewer.pal(9, 'Purples')

##FP floats
keepcol <- vector(length=length(keep))
for (i in 1:length(keep)){
	if(yrkeep[i] == 2003){
		colpal <- y1pal
	} 
	if(yrkeep[i] == 2004){
		colpal <- y2pal
	}
	if(yrkeep[i] == 2005){
		colpal <- y3pal
	} 
	if(yrkeep[i] == 2006){
		colpal <- y4pal
	} 
	if(yrkeep[i] == 2007){
		colpal <- y5pal
	}
	mon <- monkeep[i]
	#keepcol[i] <- ifelse(mon == 1 | mon == 2, colpal[(9-5)], 					ifelse(mon == 4 | mon == 5, colpal[(9-3)],
	#				ifelse(mon == 7, colpal[(9-2)], colpal[9])))
	keepcol[i] <- colpal[7]  
}

##FC floats
keepcol2 <- vector(length=length(keep2))
for (i in 1:length(keep2)){
	if(yrkeep2[i] == 2003){
		colpal <- y1pal
	} 
	if(yrkeep2[i] == 2004){
		colpal <- y2pal
	}
	if(yrkeep2[i] == 2005){
		colpal <- y3pal
	} 
	if(yrkeep2[i] == 2006){
		colpal <- y4pal
	} 
	if(yrkeep2[i] == 2007){
		colpal <- y5pal
	}
	mon <- monkeep2[i]
	#keepcol2[i] <- ifelse(mon == 1 | mon == 2, colpal[(9-5)], 					ifelse(mon == 4 | mon == 5, colpal[(9-3)],
	#				ifelse(mon == 7, colpal[(9-2)], colpal[9])))
	keepcol2[i] <- colpal[7]    
}






for (i in 1:length(keep)) {
#for (i in 4) {
	#keep <- rafoslon[[i]] < (-43)
	#rlon <- rafoslon[[i]][keep]
	#rlat <- rafoslat[[i]][keep]
    mapLines(rafoslon[[keep[i]]], rafoslat[[keep[i]]], type='l', col= keepcol[i], lwd=1.8*1.4)
    print(i)
}

legend('bottomright', col=c(y1pal[7], y2pal[7], y3pal[7], y4pal[7]), legend=unique(yrkeep2), lty=1, lwd=3, bty='n')


scale <- dist2pos(scaleLat, scaleLon, L=lengthb*1000, phi=0)
mapContour(lon, lat, z, levels=-700, col='black', lwd=1.4)

mapPoints(morlon, morlat, bg='black',col='white', pch=21, cex=1.8, lwd=2)
mapText(-43.32 + 0.02, latlabel, labels=paste(fcl[length(fc)], 'm'), srt=srt)
mapText(-43.63 - 0.01, latlabel, labels=paste(fcl[(length(fc)-1)], 'm'), srt=srt)
mapText(-43.91 - 0.02, latlabel, labels=paste(fcl[(length(fc)-2)], 'm'), srt=srt)
mapText(-44.38, latlabel, labels=paste(fcl[(length(fc)-3)], 'm'), srt=srt + 5)
#mapText(lonlabel, latlabel, labels=paste(fc[(length(fc)-4)], 'm'), srt=srt)
#mapLines(c(scaleLon, scale$lonend), c(scaleLat, scale$latend))
#mapText(mean(c(scaleLon, scale$lonend)), scaleLat + .08, paste(lengthb,'km'))




mapPlot(coastlineWorldFine,latitudelim=latlim, longitudelim=lonlim, proj=proj, col=fill, grid=c(.5,.5), lonlabel=T)
mapContour(lon,lat,z, levels=rev(fc), col=col)
#keep2 <- keep2[-7] #this float stopped sending signal around SS
cpal2 <- rep(colpal, 3)
#for (i in 1:length(samp)) {
for (i in 1:length(keep2)) {
#for (i in 4) {
	#keep <- rafoslon[[i]] < (-43)
	#rlon <- rafoslon[[i]][keep]
	#rlat <- rafoslat[[i]][keep]
    #mapLines(rafoslon[[samp[i]]], rafoslat[[samp[i]]], type='l', col= colpal[i], lwd=1.8*1.4)
    mapLines(rafoslon[[keep2[i]]], rafoslat[[keep2[i]]], type='l', col= keepcol2[i], lwd=1.8*1.4)
    print(i)
}

mapContour(lon, lat, z, levels=-700, col='black', lwd=1.4)

mapPoints(morlon, morlat, bg='black',col='white', pch=21, cex=1.8, lwd=2)
mapText(-43.32 + 0.02, latlabel, labels=paste(fcl[length(fc)], 'm'), srt=srt)
mapText(-43.63 - 0.01, latlabel, labels=paste(fcl[(length(fc)-1)], 'm'), srt=srt)
mapText(-43.91 - 0.02, latlabel, labels=paste(fcl[(length(fc)-2)], 'm'), srt=srt)
mapText(-44.38, latlabel, labels=paste(fcl[(length(fc)-3)], 'm'), srt=srt + 5)
#mapText(lonlabel, latlabel, labels=paste(fc[(length(fc)-4)], 'm'), srt=srt)
#mapLines(c(scaleLon, scale$lonend), c(scaleLat, scale$latend))
#mapText(mean(c(scaleLon, scale$lonend)), scaleLat + .08, paste(lengthb,'km'))
if(!interactive()) dev.off()



latlim <- c(48,52)
lonlim <- c(-50,-47)
mapPlot(coastlineWorldFine,latitudelim=latlim, longitudelim=lonlim, proj=proj, col=fill, grid=c(.5,.5), lonlabel=T)
mapContour(lon,lat,z, levels=rev(fc), col=col)
cpal2 <- rep(colpal, 3)
#for (i in 1:length(samp)) {
for (i in 1:length(keep)) {
#for (i in 4) {
	#keep <- rafoslon[[i]] < (-43)
	#rlon <- rafoslon[[i]][keep]
	#rlat <- rafoslat[[i]][keep]
    #mapLines(rafoslon[[samp[i]]], rafoslat[[samp[i]]], type='l', col= colpal[i], lwd=1.8*1.4)
    mapPoints(rafoslon[[keep[i]]][1], rafoslat[[keep[i]]][1],pch=20, col= keepcol[i], lwd=1.8*1.4)
    print(date[[keep[i]]][1])
}




mapPlot(coastlineWorldFine,latitudelim=latlim, longitudelim=lonlim, proj=proj, col=fill, grid=c(.5,.5), lonlabel=T)
mapContour(lon,lat,z, levels=rev(fc), col=col)
cpal2 <- rep(colpal, 3)
#for (i in 1:length(samp)) {
for (i in 1:length(keep2)) {
#for (i in 4) {
	#keep <- rafoslon[[i]] < (-43)
	#rlon <- rafoslon[[i]][keep]
	#rlat <- rafoslat[[i]][keep]
    #mapLines(rafoslon[[samp[i]]], rafoslat[[samp[i]]], type='l', col= colpal[i], lwd=1.8*1.4)
    mapPoints(rafoslon[[keep2[i]]][1], rafoslat[[keep2[i]]][1],pch=20, col= keepcol2[i], lwd=1.8*1.4)
    print(date[[keep2[i]]][1])
}










