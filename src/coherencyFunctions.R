cohci <- function(x, ci=0.95, ci.lty=3, ci.col='blue'){
	nser <- NCOL(x$spec)
    gg <- 2/x$df
    se <- sqrt(gg/2)
    z <- -qnorm((1 - ci)/2)
    coh <- pmin(0.99999, sqrt(x$coh))
    lines(x$freq, (tanh(atanh(coh) + z * se))^2, lty = ci.lty, col = ci.col)
    lines(x$freq, (pmax(0, tanh(atanh(coh) - z * se)))^2, lty = ci.lty, col = ci.col)
    cohciupper <- (tanh(atanh(coh) + z * se))^2
    cohcilower <- (pmax(0, tanh(atanh(coh) - z * se)))^2
	invisible(list(cohciupper = cohciupper, cohcilower=cohcilower))
}

phaseci <- function(x, ci=0.95, ci.lty=3, ci.col='blue'){
	nser <- NCOL(x$spec)
    gg <- 2/x$df
    coh <- sqrt(x$coh)
    cl <- asin(pmin(0.9999, qt(ci, 2/gg - 2) * sqrt(gg *(coh^{-2} - 1)/(2 * (1 - gg)))))
    lines(x$freq, x$phase + cl, lty = ci.lty, col = ci.col)
    lines(x$freq, x$phase - cl, lty = ci.lty, col = ci.col)
    phaseciupper <- x$phase + cl
    phasecilower <- x$phase - cl
    invisible(list(phaseciupper=phaseciupper, phasecilower=phasecilower))
}

coherencyStats <- function(data, type='u', plot=FALSE, pngname, whichplot = NA) {
p <- unlist(lapply(data, function(stn) ifelse(length(stn[['pressure']]) !=0, rep(mround(mean(stn[['pressure']], na.rm=TRUE),5), length(stn[['pressure']])), rep(mround(stn[['depthMax']],5), length(stn[['time']])))))

t <- lapply(data, function(stn) trunc(stn[['time']],'hours')) #adcp is every hour and 3 min
len <- unlist(lapply(t, function(k) length(k)))
look <- which.max(len)
deftime <- t[[look]]
tmat <- lapply(t, function(k) intersect(deftime,k))
r1 <- unlist(lapply(tmat, function(k) which(deftime == k[1])))
r2 <- unlist(lapply(tmat, function(k) which(deftime == k[length(k)])))

for (i in 1:length(data)){
	u <- rep(NA, length(deftime))
	if(type=='u'){
	u[r1[i]:r2[i]] <- RCMvelocity(data[[i]])$u}
	if(type=='v'){
	u[r1[i]:r2[i]] <- RCMvelocity(data[[i]])$v}
	if(type=='U'){
	u[r1[i]:r2[i]] <- sqrt(RCMvelocity(data[[i]])$u^2 + RCMvelocity(data[[i]])$v^2)}
	if (i ==1){
		m <- u
	}
	else{
	m <- cbind(m, u)}
}
colnames(m) <- p
deftime <- as.POSIXct(deftime)

for(i in 1:length(data)){
	#print(p[i])
	#check <- i == seq(2,length(data),2)
	#print(length(check[check]))
	u <- m[,i]
	tm <- tidem(deftime, m[,i])
	ut <- u - predict(tm)
	notna <- which(!is.na(ut))
	na <- which(is.na(ut))
	ut <- fillGap(ut)
	wna <- which(is.na(ut))
	if(length(wna) > 0){ut[wna] <- ut[(min(wna) - 1)]}
	utb <- butterfilter(ut, deltat=deltat, cutoff=cutoff)
	utb[na] <- NA
	utb[wna] <- NA
	ub <- rep(NA, length(deftime))
	ub[r1[i]:r2[i]] <- ut
	if(i == 1){
		b <- ub
	}
	else{
		b <- cbind(b,ub)
		}
	ubf <- rep(NA, length(deftime))
	ubf[r1[i]:r2[i]] <- utb
		if(i == 1){
		bfd <- ubf
	}
	else{
		bfd <- cbind(bfd,ubf)
		}
	}	
colnames(b) <- p

coh <- cohcil <- cohciu <- phase <- phasecil <- phaseciu <- vector(length=(length(b[1,])-1))

for (i in 2:length(b[1,])){
	
	naunique <- intersect(which(!is.na(b[,i])), which(!is.na(b[,(i-1)])))
	naunique2 <- intersect(which(!is.na(bfd[,i])), which(!is.na(bfd[,(i-1)])))	
	cf <- ccf(bfd[naunique2,(i)], bfd[naunique2,(i-1)], plot=FALSE, lag.max=60*24)	
	sp <- spectrum(cbind(b[naunique,(i)], b[naunique,(i-1)]), spans=spans, plot=FALSE)
	par(mfrow=c(3,1), mar=c(3,3,1,1))
	if(is.finite(whichplot) == TRUE & (i == whichplot & plot == TRUE)){
		if(!interactive()) {png(paste(pngname,'.png',sep=""), height=4, width=7, units='in', res=200, pointsize=11)}
		par(mar=c(3,3.2,1,1))
		layoutmat <- cbind(c(1,1,2,2,3,3), c(1,1,2,2,3,3), c(4,4,4,5,5,5))
		layout(layoutmat)
	}
	plot(sp$freq, sp$spec[,1], type='l', xlim=c(0,1/2/24), xlab="", ylab="")
	mtext(expression(paste('Variance [m'^2,'s'^-2,'cph'^-1,']')), side=2, line=1.8, cex=2/3)
	lines(sp$freq, sp$spec[,2], col='red')
	legend('topright', lty=1, col=c('black', 'red'), legend=c(paste(p[i],'m'), paste(p[(i-1)],'m')))
	sp1max <- which.max(sp$spec[,1])
	sp2max <- which.max(sp$spec[,2])
	abline(v=sp$freq[sp1max], lty=2, col='black')
	abline(v=sp$freq[sp2max], lty=2, col='black')
	#mtext(paste(type,colnames(b)[i],'m and',colnames(b)[(i-1)],'m'), adj=1)
	plot(sp$freq, sp$coh, type='l', xlim=c(0,1/2/24), xlab="", ylab="")
	cohci(sp)
	spcohci <- cohci(sp)
	mtext('Squared coherency', side=2, line=2, cex=2/3)
	abline(v=sp$freq[sp1max], lty=2, col='black')
	abline(v=sp$freq[sp2max], lty=2, col='black')
	#print(sp$freq[sp1max])
	
	plot(sp$freq, sp$phase, type='l', xlim=c(0,1/2/24), xlab="", ylab="")
	spphaseci <- phaseci(sp)
	mtext('Phase [rad]', side=2, line=2, cex=2/3)
	mtext('Frequency [cph]',side=1, line=2, cex=2/3)
	abline(v=sp$freq[sp1max], lty=2, col='black')
	abline(v=sp$freq[sp2max], lty=2, col='black')
	
	freqlook <- which.min(abs(sp$freq - 0.002)) #20.83days
	coh[(i-1)] <- sp$coh[freqlook]
	cohcil[(i-1)] <- spcohci$cohcilower[freqlook]
	cohciu[(i-1)] <- spcohci$cohciupper[freqlook]
	phase[(i-1)] <- sp$phase[freqlook]
	phasecil[(i-1)] <- spphaseci$phasecilower[freqlook]
	phaseciu[(i-1)] <- spphaseci$phaseciupper[freqlook]	
	
	}
	invisible(list(coh=coh, cohcil=cohcil, cohciu=cohciu, phase=phase, phasecil=phasecil, phaseciu=phaseciu, p=p))
	}