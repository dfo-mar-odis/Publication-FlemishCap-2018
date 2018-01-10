rm(list=ls())
load('varianceFC.rda')
load('varianceSS.rda')

axis10exp <- function(side, constantExp=TRUE, at=NULL, labels=TRUE, tick=TRUE, line=NA,
                      pos=NA, outer=FALSE, font=NA, lty='solid',
                      lwd=1, lwd.ticks=lwd, col=NULL, col.ticks=NULL,
                      hadj=NA, padj=NA, ...)
{
    if (is.null(at)) at <- axTicks(side)
    atexp <- round(log10(abs(at)))
    zero <- is.infinite(atexp)
    atexp[zero] <- 0
    if (constantExp) { # find the smallest non-zero exponent
        zeroExp <- atexp == 0
        newexp <- min(atexp[!zeroExp])
        atexp[!zeroExp] <- newexp
    }
    atbase <- at/10^atexp
    atlabel <- NULL
    for (ilab in seq_along(at)) {
        if (zero[ilab]) {
            lab <- expression(0)
        } else if (atexp[ilab]==0) {
            lab <- substitute(b, list(b=atbase[ilab]))
        } else {
            lab <- substitute(b%*%10^e, list(b=atbase[ilab], e=atexp[ilab]))
        }
        atlabel <- c(atlabel, lab)
    }
    atlabel <- as.expression(atlabel)
    axis(side, at=at, labels=atlabel, tick, line,
                      pos, outer, font, lty,
                      lwd, lwd.ticks, col, col.ticks,
                      hadj, padj, ...)
}


if(!interactive()) png('13_varianceWithFitBoth.png', width=6, height=4.5, units='in', pointsize=10, res=200)
xlim <- c(1e-4, 5e-4)
par(mar=c(3.5,3,1,1), mfrow=c(1,2))
plot(uvarSS, depthSS, ylim=rev(range(depthSS)),
		ylab='', xlab='',xlim=xlim, xaxt = 'n')
axis10exp(side=1)
lines(pmSS, depthSS)
mtext(expression(paste('Variance (m'^2,'s'^-2,')')), side=1, line=2.2)
mtext('Depth [m]', side=2, line=2)

plot(uvarFC, depthFC, ylim=rev(range(depthFC)),
		ylab='', xlab='', xlim=xlim, xaxt = 'n')
axis10exp(side=1)
lines(pmFC, depthFC)
mtext(expression(paste('Variance (m'^2,'s'^-2,')')), side=1, line=2.2)
mtext('Depth [m]', side=2, line=2)

if(!interactive()) dev.off()
