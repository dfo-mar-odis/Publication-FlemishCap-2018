rm(list=ls())
library(cl)
library(oce)
library(fields)
library(xtable)
source('filterAndSpectrumParameters.R')
source('coherencyFunctions.R')

##order microcat data to get upper depths for ADCP
d <- m2mc
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
Hyddepth <- depth[o]
 
#adcp data
d <- m2adcp
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
look1 <- which.min(abs(Hyddepth[1] - depth))
look2 <- which.min(abs(Hyddepth[2] - depth))
adpd <- d[c(look1,look2)]

##order current meter data
d <- m2rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
mcd <- d[o]

data <- c(adpd, mcd) #adcp data and current meter

#####


ci <- coherencyStats(data, type='U', pngname='002_SpecCohPhaseUSS', plot=TRUE, whichplot=length(data))
par(mar=c(3,3,1,1))
plot(ci$coh,ci$p[-(length(ci$p))], ylim=rev(range(ci$p[-(length(ci$p))])), xlim=c(0,1), xlab="", ylab="")
for (i in 1:length(ci$coh)){
	lines(c(ci$cohcil[i], ci$coh[i]),c(ci$p[i], ci$p[i]))
		lines(c(ci$cohciu[i], ci$coh[i]),c(ci$p[i], ci$p[i]))
}
points(ci$coh,ci$p[-(length(ci$p))], pch=21, bg='white')
mtext('Squared coherency', side=1, line=2, cex=2/3)
mtext('Pressure [dbar]', side=2, line=2, cex=2/3)

plot(ci$phase,ci$p[-(length(ci$p))], ylim=rev(range(ci$p[-(length(ci$p))])), xlim=c(-pi,pi), xlab="", ylab="")
for (i in 1:length(ci$coh)){
	lines(c(ci$phasecil[i], ci$phase[i]),c(ci$p[i], ci$p[i]))
	lines(c(ci$phaseciu[i], ci$phase[i]),c(ci$p[i], ci$p[i]))
}
points(ci$phase,ci$p[-(length(ci$p))], pch=21, bg='white')
mtext('Phase [rad]', side=1, line=2, cex=2/3)
mtext('Pressure [dbar]', side=2, line=2, cex=2/3)
if(!interactive()) dev.off()

save(ci, file='SScoherency.rda')

