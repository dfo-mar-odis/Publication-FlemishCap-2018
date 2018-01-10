rm(list=ls())
library(cl)
library(oce)
library(fields)
library(xtable)
source('filterAndSpectrumParameters.R')
load('/data/topography/topoMaritimes.rda')
load('/data/flemishCap/moorings/mooringADCP.rda')
load('/data/flemishCap/moorings/mooringRCM.rda')
load('/data/flemishCap/moorings/mooringMC.rda')
source('coherencyFunctions.R')

##order microcat data to get upper depths for ADCP
d <- m1mc
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
Hyddepth <- depth[o]
 
#adcp data
d <- m1adcp
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
look1 <- which.min(abs(Hyddepth[1] - depth))
look2 <- which.min(abs(Hyddepth[2] - depth))
adpd <- d[c(look1,look2)]

##order current meter data
d <- m1rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
mcd <- d[o]

data1 <- c(adpd, mcd) #adcp data and current meter

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

data2 <- c(adpd, mcd) #adcp data and current meter


for (i in 1:length(data1)){
	s <- coherencyStats(c(data1[i], data2[i]), type="U", plot=FALSE)
	if(i == 1){
		coh <- s$coh
		ph <- s$phase
		cohciu <- s$cohciu
		cohcil <- s$cohcil
		phciu <- s$phaseciu
		phcil <- s$phasecil
	}
	else{
		coh <- c(coh,s$coh)
		ph <- c(ph,s$phase)
		cohciu <- c(cohciu,s$cohciu)
		cohcil <- c(cohcil,s$cohcil)
		phciu <- c(phciu,s$phaseciu)
		phcil <- c(phcil,s$phasecil)
	}
}

if(!interactive()) png('004_lateralCohPhase.png', width=5.5, height=3, units='in', res=200, pointsize=11)
par(mar=c(3,3,1,1), mfrow=c(1,2))
p <- mround(getDepth(data2),5)
plot(coh,p, ylim=rev(range(p)), xlim=c(0,1))
for(i in 1:length(cohcil)){
	lines(c(cohcil[i], coh[i]), c(p[i],p[i]))
	lines(c(cohciu[i], coh[i]), c(p[i], p[i]))
}
points(coh, p, pch=21, bg='white')
mtext('Coherency squared', side=1, line=2, cex=4/5)
mtext('Pressure [dbar]', side=2, line=2, cex=4/5)

plot(ph,p, ylim=rev(range(p)), xlim=c(-pi,pi))
for(i in 1:length(cohcil)){
	lines(c(phcil[i], ph[i]), c(p[i],p[i]))
	lines(c(phciu[i], ph[i]), c(p[i], p[i]))
}
points(ph, p, pch=21, bg='white')
mtext('Phase [rad]', side=1, line=2, cex=4/5)
mtext('Pressure[dbar]', side=2, line=2, cex=4/5)
if(!interactive()) dev.off()

save(coh, cohcil, cohciu, ph, phcil, phciu, p, file='lateralCoherency.rda')

