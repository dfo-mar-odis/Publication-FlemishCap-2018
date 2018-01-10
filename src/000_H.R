library(cl)
library(oce)
library(fields)
library(signal)
library(plyr)
library(ocedata)
library(xtable)
library(pracma)

load('/data/topography/topoMaritimes.rda')
load('/data/flemishCap/moorings/mooringADCP.rda')


latm <- c(m1adcp[[1]][['latitude']], m2adcp[[1]][['latitude']], m3adcp[[1]][['latitude']])
lonm <- c(m1adcp[[1]][['longitude']]*-1,m2adcp[[1]][['longitude']], m3adcp[[1]][['longitude']]*-1)

H <- interp.surface(list(x = longitude, y = latitude, z = z), cbind(lonm,latm))