rm(list=ls())
load('thetab.rda')
source('filterAndSpectrumParameters.R')

angle <- -( angleSS + 90)


mround <- function(x,base){ 
        base*round(x/base) 
}

rms <- function(x){
	sqrt(mean(x^2, na.rm=TRUE))
}

cohci <- function(x, ci=0.95, ci.lty=3, ci.col='blue'){
	nser <- NCOL(x$spec)
    gg <- 2/x$df
    se <- sqrt(gg/2)
    z <- -qnorm((1 - ci)/2)
    coh <- pmin(0.99999, sqrt(x$coh))
    lines(x$freq, (tanh(atanh(coh) + z * se))^2, lty = ci.lty, col = ci.col)
    lines(x$freq, (pmax(0, tanh(atanh(coh) - z * se)))^2, lty = ci.lty, col = ci.col)
	
}

lon <- lonm[2]
lat <- latm[2]
L <- 60
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

p <- unlist(lapply(data, function(stn) ifelse(length(stn[['pressure']]) !=0, rep(mround(mean(stn[['pressure']], na.rm=TRUE),5), length(stn[['pressure']])), rep(mround(stn[['depthMax']],5), length(stn[['time']])))))

p <- unlist(lapply(data, function(stn) ifelse(length(stn[['pressure']]) !=0, rep(round(mean(stn[['pressure']], na.rm=TRUE)), length(stn[['pressure']])), rep(round(stn[['depthMax']]), length(stn[['time']])))))

u <- unlist(lapply(data, function(stn) RCMvelocity(stn)$u))
v <- unlist(lapply(data, function(stn) RCMvelocity(stn)$v))
t <- lapply(data, function(stn) trunc(stn[['time']],'hours')) #adcp is every hour and 3 min
len <- unlist(lapply(t, function(k) length(k)))
look <- which.max(len)
deftime <- t[[look]]
tmat <- lapply(t, function(k) intersect(deftime,k))
r1 <- unlist(lapply(tmat, function(k) which(deftime == k[1])))
r2 <- unlist(lapply(tmat, function(k) which(deftime == k[length(k)])))
urms <- utrms <- ubfrms <- vector(length=length(data))


for (i in 1:length(data)){
	u <- rep(NA, length(deftime))
	v <- rep(NA, length(deftime))
	u[r1[i]:r2[i]] <- RCMvelocity(data[[i]])$u
	v[r1[i]:r2[i]] <- RCMvelocity(data[[i]])$v
	if (i ==1){
		m <- u
		q <- v
	}
	else{
	m <- cbind(m, u)
	q <- cbind(q, v)}
}
colnames(m) <- p
colnames(q) <- p
deftime <- as.POSIXct(deftime)
if(!interactive()) png('08_stackedPlotSSur.png', height=4, width=6, units='in', res=250, pointsize=11)
ylim <- c(-0.2, 0.4)
layout(matrix(c(0,seq(1,length(data)),0), ncol=1),height=c(1,rep(3,length(data)),2))
#layout(cbind(c(0,seq(1,length(data)),0), c(0, seq(length(data)+1, length(data)*2), 0)), height=rep(c(1,rep(3,length(data)),1.5),2))
par(mar=c(0,3.25,0,3))
for(i in 1:length(data)){
#for (i in 1){
	print(p[i])
	check <- i == seq(2,length(data),2)
	print(length(check[check]))
	u <- m[,i]
	v <- q[,i]
rtb <- rotateDetideButterfilter(u , v, deftime, angle, deltat=deltat, cutoff=cutoff, constituents=c('standard', '-SA', '-SSA'))
	ur <- rtb$ur
	urm <- rtb$urm
	urt <- rtb$urt
	urtb <- rtb$urtb
	vr <- rtb$vr
	vrm <- rtb$vrm
	vrtb <- rtb$vrtb
	urms[i] <- rms(ur-urm)
	utrms[i] <- rms(urt[!is.na(ur)])
	ubfrms[i] <- rms(urtb)
	print(paste('for',p[i],'m'))
	print(paste('the rms for the mean remove raw u is', round(urms,4)))
	print(paste('the rms for the detided u is', round(utrms,4)))
	print(paste('the rms for the detided butter filter u is', round(ubfrms,4)))
	print(paste('the difference between raw and detided u rms is', round(round(urms,4) - round(utrms,4),4)))
	print(paste('the difference between detided and butter filtered u rms is', round(round(utrms,4) - round(ubfrms,4),4)))

	
	if (length(check[check]) == 0){
	plot(deftime, ur, type='l', xaxt="n", yaxt="n", ylim=ylim, ylab="", col='grey', lwd=0.6)
	lines(deftime, urtb + urm, col='black', lty=1)
	abline(v=as.POSIXct("2014-01-01 00:00:00", tz="UTC"))
	abline(h=0, lwd=0.8)
	text(x=1374590133, y=-0.1481697, labels=paste(p[i],'m'))
	axis(2, at=c(-0.2, 0, 0.2, 0.4))
	
	}
	
	else{
	plot(deftime, ur, type='l', xaxt="n", yaxt="n", ylim=ylim, ylab="", col='grey', lwd=0.6)
	lines(deftime, urtb + urm, col='black', lty=1)
	abline(v=as.POSIXct("2014-01-01 00:00:00", tz="UTC"))
	abline(h=0, lwd=0.8)
	text(x=1374590133, y=-0.1481697, labels=paste(p[i],'m'))
	axis(4, at=c(-0.2, 0, 0.2, 0.4))
	#grid()	
	}
	if(i == length(data)){
	timeat <- seq(as.POSIXct("2013-07-01", tz='UTC'), as.POSIXct("2014-07-01", tz='UTC'),by="month")
	axis.POSIXct(side=1, x=deftime, at=timeat,format='%b')

	}
	if(i == 1){
		newYear <- as.POSIXct("2014-01-01 00:00:00", tz="UTC")
		mtext('2013', side=3, at=as.POSIXct('2013-10-01 00:00:00', tz='UTC'),cex=2/3)
		mtext('2014', side=3, at=as.POSIXct('2014-04-01 00:00:00', tz='UTC'),cex=2/3)
		mtext("|", side=3, at=newYear, line=0, cex=2/3)
		#mtext('time', side=1, line=2, cex=2/3)
	}
	if(i==4){
		mtext(expression(paste('u'[r],'(m/s)')), adj=-1, cex=2/3, side=2, line=2)
	}
	#legend('bottomleft', legend=paste(p[i],'m'), bg='white')
}
if(!interactive()) dev.off()


urmsss <- urms
utrmsss <- utrms
ubfrmsss <- ubfrms
pss <- p
save(urmsss, utrmsss, ubfrmsss, pss=p, file='rmsValuesSS.rda')

# colnames(b) <- p
# for (i in 2:length(b[1,])){
# if(!interactive()) png(paste('10_ccfSpecCohUNFC',i,'.png',sep=""), height=4, width=6.5, units='in', res=200, pointsize=11)	
	# naunique <- intersect(which(!is.na(b[,i])), which(!is.na(b[,(i-1)])))
	# naunique2 <- intersect(which(!is.na(bfd[,i])), which(!is.na(bfd[,(i-1)])))
	# cf <- ccf(bfd[naunique2,(i)], bfd[naunique2,(i-1)], plot=FALSE, lag.max=60*24)
	# par(mfrow=c(3,1), mar=c(3,3,1,1))
	# plot(cf$lag, cf$acf, type='l')
	# mtext(paste('u-velocity',colnames(b)[i],'m and',colnames(b)[(i-1)],'m'), adj=1)
	# mtext('ccf',side=2, line=2)
	# abline(h=0)
	# maxacf <- which.max(cf$acf)
	# abline(v=cf$lag[maxacf])
	# message(paste('The u-component velocity at',colnames(b)[i],'m lags ',colnames(b)[(i-1)]),'m ,based on the maximum value of the acf, by (',cf$lag[maxacf],') hours')	
	# sp <- spectrum(cbind(b[naunique,(i)], b[naunique,(i-1)]), spans=spans, plot=FALSE)
	# plot(sp$freq, sp$spec[,1], type='l', xlim=c(0,1/2/24))
	# mtext('spectrum', side=2, line=2)
	# lines(sp$freq, sp$spec[,2], col='red')
	# sp1max <- which.max(sp$spec[,1])
	# sp2max <- which.max(sp$spec[,2])
	# abline(v=sp$freq[sp1max], lty=2, col='black')
	# abline(v=sp$freq[sp2max], lty=2, col='red')
	# plot(sp$freq, sp$coh, type='l', xlim=c(0,1/2/24))
	# cohci(sp)
	# mtext('coh', side=2, line=2)
	# abline(v=sp$freq[sp1max], lty=2, col='black')
	# abline(v=sp$freq[sp2max], lty=2, col='red')
	# message('The spectrum for the u-velocity component at ',colnames(b)[i],'m has a peak at a period of ',round(1/sp$freq[sp1max]/24,2),'days. The spectrum for the u-velocity component at ',colnames(b)[(i-1)],'m  has a peak at a period of ',round(1/sp$freq[sp2max]/24,2),'days. The squared coherency at ',round(1/sp$freq[sp1max]/24,2),'days is ',round(sp$coh[sp1max],3),', and the squared coherency at ',round(1/sp$freq[sp2max]/24,2),'days is ',round(sp$coh[sp2max],3),'.')
	# if(!interactive()) dev.off()
# }


# if(!interactive()) png('10_stackedPlotVNFC.png', height=4, width=6.5, units='in', res=200, pointsize=11)
# layout(matrix(c(0,seq(1,length(data)),0), ncol=1),height=c(1,rep(3,length(data)),1.5))
# #layout(cbind(c(0,seq(1,length(data)),0), c(0, seq(length(data)+1, length(data)*2), 0)), height=rep(c(1,rep(3,length(data)),1.5),2))
# par(mar=c(0,3,0,3))
# for (i in 1:length(data)){
	# v <- rep(NA, length(deftime))
	# v[r1[i]:r2[i]] <- RCMvelocity(data[[i]])$v
	# if (i ==1){
		# m <- v
	# }
	# else{
	# m <- cbind(m, v)}
# }
# par(mar=c(0,3,0,3))
# for(i in 1:length(data)){
	# print(p[i])
	# check <- i == seq(2,length(data),2)
	# print(length(check[check]))
	# u <- m[,i]
	# tm <- tidem(deftime, m[,i])
	# ut <- u - predict(tm)
	# notna <- which(!is.na(ut))
	# na <- which(is.na(ut))
	# ut <- fillGap(ut)
	# wna <- which(is.na(ut))
	# ut[wna] <- ut[(min(wna) - 1)]
	# utb <- butterfilter(ut, deltat=deltat, cutoff=cutoff)
	# utb[na] <- NA
	# utb[wna] <- NA
	# ub <- rep(NA, length(deftime))
	# ub[r1[i]:r2[i]] <- ut
	# if(i == 1){
		# b <- ub
	# }
	# else{
		# b <- cbind(b,ub)
		# }
	# ubf <- rep(NA, length(deftime))
	# ubf[r1[i]:r2[i]] <- utb
		# if(i == 1){
		# bfd <- ubf
	# }
	# else{
		# bfd <- cbind(bfd,ubf)
		# }
	
	
	# if (length(check[check]) == 0){
	# plot(deftime, m[,i], type='l', xaxt="n", ylim=ylim, ylab="", col='grey', lwd=0.6)
	# lines(deftime, utb, col='black', lty=1)
	# abline(h=0, lwd=0.8)
	# text(x=1374590133, y=-0.1481697, labels=paste(p[i],'m'))
	
	# }
	
	# else{
	# plot(deftime, m[,i], type='l', xaxt="n", yaxt="n", ylim=ylim, ylab="", col='grey', lwd=0.6)
	# lines(deftime, utb, col='black', lty=1)
	# abline(h=0, lwd=0.8)
	# text(x=1374590133, y=-0.1481697, labels=paste(p[i],'m'))
	# axis(4)
	# #grid()	
	# }
	# if(i == length(data)){
		# axis.POSIXct(side=1, x=deftime, format='%b')

	# }
	# if(i == 1){
		# newYear <- as.POSIXct("2014-01-01 00:00:00", tz="UTC")
		# mtext('2013', side=3, at=as.POSIXct('2013-10-01 00:00:00', tz='UTC'),cex=2/3)
		# mtext('2014', side=3, at=as.POSIXct('2014-04-01 00:00:00', tz='UTC'),cex=2/3)
		# mtext("|", side=3, at=newYear, line=0, cex=2/3)
		# #mtext('time', side=1, line=2, cex=2/3)
	# }
	# if(i==4){
		# mtext('v [m/s]', adj=-1, cex=2/3, side=2, line=2)
	# }
	# #legend('bottomleft', legend=paste(p[i],'m'), bg='white')
# }
# if(!interactive()) dev.off()


# colnames(b) <- p
# for (i in 2:length(b[1,])){
	# png(paste('10_ccfSpecCohVNFC',i,'.png',sep=""), height=4, width=6.5, units='in', res=200, pointsize=11)
	# naunique <- intersect(which(!is.na(b[,i])), which(!is.na(b[,(i-1)])))
	# naunique2 <- intersect(which(!is.na(bfd[,i])), which(!is.na(bfd[,(i-1)])))
	# cf <- ccf(bfd[naunique2,(i)], bfd[naunique2,(i-1)], plot=FALSE, lag.max=60*24)
	# par(mfrow=c(3,1), mar=c(3,3,1,1))
	# plot(cf$lag, cf$acf, type='l')
	# mtext(paste('v-velocity',colnames(b)[i],'m and',colnames(b)[(i-1)],'m'), adj=1)
	# abline(h=0)
	# maxacf <- which.max(cf$acf)
	# abline(v=cf$lag[maxacf])
	# message(paste('The v-component velocity at',colnames(b)[i],'m lags ',colnames(b)[(i-1)]),'m ,based on the maximum value of the acf, by (',cf$lag[maxacf],') hours')	
	# sp <- spectrum(cbind(b[naunique,(i)], b[naunique,(i-1)]), spans=spans, plot=FALSE)
	# plot(sp$freq, sp$spec[,1], type='l', xlim=c(0,1/2/24))
	# lines(sp$freq, sp$spec[,2], col='red')
	# sp1max <- which.max(sp$spec[,1])
	# sp2max <- which.max(sp$spec[,2])
	# abline(v=sp$freq[sp1max], lty=2, col='black')
	# abline(v=sp$freq[sp2max], lty=2, col='red')
	# plot(sp$freq, sp$coh, type='l', xlim=c(0,1/2/24))
	# cohci(sp)
	# abline(v=sp$freq[sp1max], lty=2, col='black')
	# abline(v=sp$freq[sp2max], lty=2, col='red')
	# message('The spectrum for the v-velocity component at ',colnames(b)[i],'m has a peak at a period of ',round(1/sp$freq[sp1max]/24,2),'days. The spectrum for the v-velocity component at ',colnames(b)[(i-1)],'m  has a peak at a period of ',round(1/sp$freq[sp2max]/24,2),'days. The squared coherency at ',round(1/sp$freq[sp1max]/24,2),'days is ',round(sp$coh[sp1max],3),', and the squared coherency at ',round(1/sp$freq[sp2max]/24,2),'days is ',round(sp$coh[sp2max],3),'.')
	# if(!interactive()) dev.off()	
# }
