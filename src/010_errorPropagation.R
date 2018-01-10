rm(list=ls())
load('thetab.rda')
load('N.rda')
load('H.rda')
load('Alpha.rda')
load('thetavFC.rda')
load('kappaFC.rda')
load('thetac.rda')
source('filterAndSpectrumParameters.R')


# set values
N <- N[1]
Nse <- Nse[1]
H <- abs(H[1])
f <- f[1]
alpha <- alpha[1]
alphase <- alphase[1]
thetab <- (angleFC + 90)
#thetab <- 40
#thetab <- thetac[1]
thetabse <- angleFCsd
#thetabse <- thetacsd[1]
thetav <- mean(thetavFC)
#thetav <- mean(thetavFC[4:6]) #test for revision
thetavsd <- sd(thetavFC)/sqrt(length(thetavFC)) # check to see if this is right, I think it's suitable.
kappa <- kappauFC
#kappa <- 2*pi / (140 * 1000) #for test 100 km wavelength
kappase <- kappauseFC

# theta = thetav - thetab propagate error
theta <- thetav - thetab
#theta <- theta - 2.5 # for test
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
#A : yes
omegasd <- sqrt(dwda^2 * alphase^2 + dwdN^2 * Nse^2 
				+ dwdt^2 * (thetasd*pi/180)^2 + dwdg^2 * gammasd^2)
				
				
period <- 2*pi/omega/86400 #in days
periodsd <- period * sqrt((omegasd/omega)^2)

omegaFC <- omega
omegasdFC <- omegasd
periodFC <- period
periodsdFC <- periodsd

if(!interactive()) save(omegaFC, omegasdFC, periodFC, periodsdFC, file='omegaFC.rda')



