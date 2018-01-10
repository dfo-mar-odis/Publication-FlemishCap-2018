rm(list=ls())
load('thetab.rda')
source('filterAndSpectrumParameters.R')

angle <- -( angleFC + 90)


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
if(!interactive()) png('07_stackedPlotFCur.png', height=4, width=6, units='in', res=250, pointsize=11)
ylim <- c(-0.2, 0.4)
layout(matrix(c(0,seq(1,length(data)),0), ncol=1),height=c(1,rep(3,length(data)),2))
#layout(cbind(c(0,seq(1,length(data)),0), c(0, seq(length(data)+1, length(data)*2), 0)), height=rep(c(1,rep(3,length(data)),1.5),2))
par(mar=c(0,3.25,0,3))
for(i in 1:length(data)){
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
	plot(deftime, ur, type='l', xaxt="n", yaxt='n', ylim=ylim, ylab="", col='grey', lwd=0.6)
	lines(deftime, urtb + urm, col='black', lty=1)
	abline(v=as.POSIXct("2014-01-01 00:00:00", tz="UTC"))
	abline(h=0, lwd=0.8)
	text(x=1374590133, y=-0.1481697, labels=paste(p[i],'m'))
	axis(2, at=c(-0.2, 0, 0.2, 0.4))
	
	}
	
	else{
	plot(deftime, ur, type='l', xaxt="n", yaxt="n", ylim=ylim, ylab="", col='grey', lwd=0.6)
	lines(deftime, urtb + urm, col='black', lty=1)
	abline(h=0, lwd=0.8)
	abline(v=as.POSIXct("2014-01-01 00:00:00", tz="UTC"))
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

save(urms, utrms, ubfrms, p, file='rmsValues.rda')


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
