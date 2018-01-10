rm(list=ls())
load('thetab.rda')
load('thetac.rda')
load('N.rda')
load('H.rda')
load('Alpha.rda')
load('thetavSS.rda')
load('kappaSS.rda')
source('filterAndSpectrumParameters.R')

# set values
N <- N[2]
Nse <- Nse[2]
H <- abs(H[2])
f <- f[2]
alpha <- alpha[2]
alphase <- alphase[2]
thetab <- (angleSS + 90)
#thetab <- thetac[2]
thetabse <- angleSSsd
#thetabse <- thetacsd[2]
thetav <- mean(thetavSS)
#thetav <- mean(thetavSS[4:6]) #for testing
thetavsd <- sd(thetavSS)/ sqrt(length(thetavSS)) # check to see if this is right, I think it's suitable.
kappa <- kappauSS
kappase <- kappauseSS

# theta = thetav - thetab propagate error
theta <- thetav - thetab
thetasd <- sqrt(thetavsd^2 + thetabse^2)

# gamma = kappa * N * H / f propagate error
gamma <- (kappa * N * H) / f
gammasd <- gamma * sqrt((kappase/kappa)^2 + (Nse/N)^2)


omega <- alpha * N * sin(theta * pi / 180) * coth(gamma)
omega <- abs(omega)
#propagate error for omega
dwda <- N * sin(theta * pi / 180) * coth(gamma)
dwdN <- alpha * sin(theta * pi / 180) * coth(gamma)
dwdt <- alpha * N * cos(theta * pi / 180) * coth(gamma)
dwdg <- -alpha * N * sin(theta * pi / 180) * csch(gamma)^2

#Q for DK : does thetasd have to be in radians ??
#A yes
omegasd <- sqrt(dwda^2 * alphase^2 + dwdN^2 * Nse^2 
				+ dwdt^2 * (thetasd*pi/180)^2 + dwdg^2 * gammasd^2)
				
				
period <- 2*pi/omega/86400 #in days
periodsd <- period * sqrt((omegasd/omega)^2)

omegaSS <- omega
omegasdSS <- omegasd
periodSS <- period
periodsdSS <- periodsd

if(!interactive()) save(omegaSS, omegasdSS, periodSS, periodsdSS, file='omegaSS.rda')



