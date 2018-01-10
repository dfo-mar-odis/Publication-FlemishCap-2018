rm(list=ls())
angleFromIsobath <- TRUE

## Rossby lengths
km <- 1e3
LLSS <- 0.86e-3*1454/1.09e-4/km        # SS Rossby length [km] using table 3 (11.47193 km)
LLFC <- 0.97e-3*1485/1.1e-4/km         # FC Rossby length [km] using table 3 (13.095 km)
span <- 3                              # span in Rossby lengths
#FAC <- 1.25                             # we look +-FAC*LLSS etc along isobath to get angle
## FAC <- 1 # FC bad
LL <- c(LLFC, LLSS)

seSlopeSma <- function(m){
	slope <- as.numeric(m$coef[[1]][2,1])
	ci <- as.numeric(m$coef[[1]][2, 2:3])
	n <- as.numeric(m$groupsummary[2])
	diff(ci) / 2 / qt(0.95, df = n -1)
}


library('KernSmooth')
library('smatr')
load('H.rda')
source('filterAndSpectrumParameters.R')

thetac <- thetacsd <- NULL
contourline <- vector(mode = 'list', length = 2)
for (i in 1:2){
	a <- contourLines(longitude, latitude, z, levels=H[i])
    alen <- unlist(lapply(a, function(x) length(x$x)))
    aa <- a[[which.max(alen)]] # pick the longest trace, ignoring little squiggles
    dist <- geodDist(aa$x, aa$y, lonm[i], latm[i])
    lookingDistance <- 0.5 * span * LL[i]
    message("SS: lookingDistance=", lookingDistance, " km")
    look <- dist <= lookingDistance
    aaa <- list(x=aa$x[look], y=aa$y[look])
    zone <- lonlat2utm(longitude = lonm[i], latitude = latm[i])$zone
    xy <- lonlat2utm(longitude=aaa$x, latitude=aaa$y, zone = zone)
    sma <- sma(northing ~ easting, data = xy)
    slope <- as.numeric(sma$coef[[1]][2,1])
    sd <- seSlopeSma(sma)
    thetac[i] <- atan(slope) * 180/pi
    thetacsd[i] <- sd / (1 + slope^2)
    contourline[[i]] <- aaa
}

save(thetac, thetacsd, file = 'thetac.rda')
save(contourline, file = 'uvMapcontour.rda')