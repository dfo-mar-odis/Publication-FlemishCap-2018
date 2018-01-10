rm(list=ls())
load('thetab.rda')
load('N.rda')
load('H.rda')
load('Alpha.rda')
load('thetavSS.rda')
source('filterAndSpectrumParameters.R')


N <- N[2]
Nse <- Nse[2]
H <- H[2]
f <- f[2]
alpha <- alpha[2]
alphase <- alphase[2]
angle <- -(angleSS + 90)
anglese <- angleSSsd


##order current meter data
d <- m2rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
data <- d[o]



#anglevec <- rnorm(50, mean = angle, sd = anglese)

uvar <- vvar <- Uvar <- uvarg5d <- uvarl5d <- theta <- vector(length=length(data))

#uvar <- vvar <- Uvar <- uvarg5d <- uvarl5d <- theta <- vector(mode='list', length=length(anglevec))

#for(j in 1:length(anglevec)){
#	print(j)
#	ang <- anglevec[j]
for (i in 1:length(data)){
	d <- data[[i]]
	u <- RCMvelocity(d)$u
	v <- RCMvelocity(d)$v
	time <- d[['time']]
	rt <- rotateDetideButterfilter(u , v, time, rotate=TRUE, angle=angle, butterfilter=FALSE)
	
	ur <- rt$ur
	urm <- rt$urm
	urt <- rt$urt
	vr <- rt$vr
	vrm <- rt$vrm
	vrt <- rt$vrt
	Urt <- sqrt(urt^2 + vrt^2)
	
	
		#first rotated detided along-slope
		su <- spectrum(urt, spans=spans, plot=FALSE)
		fvu <- findVariance(su)
		#uvar[[j]][i] <- fvu$var
		uvar[i] <- fvu$var
		uvarg5d[i] <- fvu$varg5d
		uvarl5d[i] <- fvu$varl5d
		
		#sv <- spectrum(vrt, spans=spans, plot=FALSE)
		#fvv <- findVariance(sv)
		#vvar[[j]][i] <- fvv$var
	
		#sU <- spectrum(Urt, spans=spans, plot=FALSE)
		#fvU <- findVariance(sU)
		#Uvar[[j]][i] <- fvU$var	
		}
#	}


#Nvec <- rnorm(50, mean=N, sd= Nse)
#seqlen <- seq_len(length(Nvec))
#kappau <- lapply(seqlen, function(k){
#	m20u <- try(nls(uvar[[k]] ~ sigma0 * cosh((kappa * Nvec[k] * depth)/f), start = list(sigma0 = 5e-4, kappa = 2*pi/100/1000)))
#	ifelse(inherits(m20u, 'try-error'), NA, coef(m20u)[2])
#}
#)

m20u <- try(nls(uvar ~ sigma02 * cosh((kappa * N * depth)/f)^2, start = list(sigma02 = 5e-4, kappa = 2*pi/100/1000)))
kappau <- ifelse(inherits(m20u, 'try-error'), NA, coef(m20u)[2])
kappause <- coef(summary(m20u))[2,2]
kappaun <- summary(m20u)$df[2]

#mtheta <- lapply(theta, mean)
#medtheta <- lapply(theta, median)
#sdtheta <- lapply(theta, function(k) sd(k)/sqrt(6))

#m20u <- try(nls(uvar ~ sigma0 * cosh((kappa * N * depth)/f), start = list(sigma0 = 5e-4, kappa = 2*pi/100/1000)))
#kappau <- ifelse(inherits(m20u, 'try-error'), NA, coef(m20u)[2])



#early tests indicate not going to work for v and U
#m20v <- try(nls(vvar ~ sigma0 * cosh((kappa * N * depth)/f), start = list(sigma0 = 5e-4, kappa = 2*pi/100/1000)))
#kappav <- ifelse(inherits(m20v, 'try-error'), NA, coef(m20v)[2])

#m20U <- try(nls(Uvar ~ sigma0 * cosh((kappa * N * depth)/f), start = list(sigma0 = 5e-4, kappa = 2*pi/100/1000)))
#kappaU <- ifelse(inherits(m20U, 'try-error'), NA, coef(m20U)[2])

uvarg5dSS <- uvarg5d
uvarl5dSS <- uvarl5d
uvarsumSS <- uvarg5dSS + uvarl5dSS
uvarSS <- uvar
pmSS <- predict(m20u)
depthSS <- depth

save(uvarSS, uvarsumSS, depthSS, pmSS, file='varianceSS.rda')

kappauSS <- kappau
kappauseSS <- kappause
kappaunSS <- kappaun
save(kappauSS, kappauseSS, kappaunSS, file='kappaSS.rda')



