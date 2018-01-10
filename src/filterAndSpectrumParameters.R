library(cl)
library(oce)
library(fields)
library(signal)
library(plyr)
library(ocedata)
library(xtable)
library(pracma)
data(coastlineWorldFine)
load("/data/floats/RAFOSexp.rda")
load('/data/topography/topoMaritimes.rda')
load('/data/flemishCap/moorings/mooringADCP.rda')
load('/data/flemishCap/moorings/mooringMC.rda')
load('/data/flemishCap/moorings/mooringRCM.rda')
load("/data/flemishCap/LADCP/LADCPall.rda")
load('/data/flemishCap/CTD/CTDall_ODFQC.rda')

latm <- c(m1adcp[[1]][['latitude']], m2adcp[[1]][['latitude']], m3adcp[[1]][['latitude']])
lonm <- c(m1adcp[[1]][['longitude']]*-1,m2adcp[[1]][['longitude']], m3adcp[[1]][['longitude']]*-1)


H <- interp.surface(list(x = longitude, y = latitude, z = z), cbind(lonm,latm))

save(H, file = 'H.rda')

latctd <- unlist(lapply(FlemishCap, function(stn) stn[['latitude']][1]))

lonctd <- unlist(lapply(FlemishCap, function(stn) stn[['longitude']][1]))

mooringPosition <- cbind(lonm, latm)
f <- coriolis(latm)
save(f, file='f.rda')
L <- 60
thetab <- unlist(lapply(1:length(latm), function(k) eigenMethod(longitude, latitude, z, lonm[k], latm[k], Ldist=L)$angle+90 - 360))

library(signal)
#spectral parameters
spans <- c(5,3)
#filter parameters
W <- 2/(4*24)
deltat <- 1/24 #hourly data
cutoff <- 4 #4 day cutoff
bf <- butter(1, W)	

W2 <- (2*5)/(1*24*60) #one day
bf2 <- butter(1,W2)


gaussfilter <- function(x, deltat , cutoff){
	n <- cutoff / deltat
    gauss <- gausswin(n = n, w = 2)
    gauss <- gauss/sum(gauss)
    xx <- stats::filter(x, gauss)
    xxx <- rev(xx)
    xxxx <- stats::filter(xxx, gauss)
    xxxxx <- rev(xxxx)
    xxxxx}
    
