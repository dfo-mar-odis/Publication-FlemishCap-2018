rm(list=ls())
library(smatr)
source('filterAndSpectrumParameters.R')

section <- vector(mode='list', length=5)
section[[1]] <- seq(2,13,1)
section[[2]] <- rev(seq(21,27,1))
section[[3]] <- seq(30,40,1)
section[[4]] <- seq(41,47,1)
section[[5]] <- c(48, 50, 51, 52, 49, 53, 57)

sectionseq <- c(2,5,3,4)
trans <- c('SS', 'SS', 'FC', 'FC')

FCindex <- unlist(section[sectionseq[trans == 'FC']])
SSindex <- unlist(section[sectionseq[trans == 'SS']])

dataFC <- ladcp[FCindex]
dataSS <- ladcp[SSindex]

sFC <- as.section(dataFC)
sSS <- as.section(dataSS)

slonFC <- sFC[['longitude','byStation']]
slatFC <- sFC[['latitude', 'byStation']]

slonSS <- sSS[['longitude', 'byStation']]
slatSS <- sSS[['latitude', 'byStation']]

xyFC <- lonlat2utm(longitude = slonFC, latitude = slatFC)
xySS <- lonlat2utm(longitude = slonSS, latitude = slatSS)

mFC <- sma(northing ~ easting, data=xyFC)
mSS <- sma(northing ~ easting, data=xySS)

seSlopeSma <- function(m){
	slope <- as.numeric(m$coef[[1]][2,1])
	ci <- as.numeric(m$coef[[1]][2, 2:3])
	n <- as.numeric(m$groupsummary[2])
	diff(ci) / 2 / qt(0.95, df = n -1)
}
#below commented out can be used to test sd calc
#slopeFC <- as.numeric(mFC$coef[[1]][2,1])
#ciFC <- as.numeric(mFC$coef[[1]][2, 2:3])
#nFC <- as.numeric(mFC$groupsummary[2])

sdFC <- seSlopeSma(mFC)
sdSS <- seSlopeSma(mSS)

slopeFC <- as.numeric(mFC$coef[[1]][2,1])
slopeSS <- as.numeric(mSS$coef[[1]][2,1])

angleFC <- atan(slopeFC) * 180/pi
angleFCsd <- sdFC / (1 + slopeFC^2)
angleSS <- atan(slopeSS) * 180/pi
angleSSsd <- sdSS / (1 + slopeSS^2)

save(angleFC, angleFCsd, angleSS, angleSSsd, file='thetab.rda')


