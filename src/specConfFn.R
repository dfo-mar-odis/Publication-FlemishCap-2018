##got below code from looking in
##getAnywhere(plot.spec)

spec.ci <- function(spec.obj, coverage = 0.95) {
        if (coverage < 0 || coverage >= 1) 
            stop("coverage probability out of range [0,1)")
        tail <- (1 - coverage)
        df <- spec.obj$df
        upper.quantile <- 1 - tail * pchisq(df, df, lower.tail = FALSE)
        lower.quantile <- tail * pchisq(df, df)
        1/(qchisq(c(upper.quantile, lower.quantile), df)/df)
    }
 
plotSpecConf <- function(x, ci, ci.col, conf.x){ 
	if (missing(conf.x))
		conf.x <- max(x$freq) - x$bandwidth 
	conf.lim <- spec.ci(x, coverage = ci)    
	conf.y <- max(x$spec)/conf.lim[2L]

	lines(rep(conf.x, 2), conf.y * conf.lim, col = ci.col)
	lines(conf.x + c(-0.5, 0.5) * x$bandwidth, rep(conf.y, 2), col = ci.col)}
	
spec.ci.shumway <- function(spec.obj, ci){
	df <- spec.obj$df
	alpha <- 1 - ci
	L <- qchisq(1 - alpha/2, df)
	qchisq(alpha/2 , df)

}

drawSpectrumLimits <- function(spec.obj, coverage=0.95, ...)
{
    CI <- spec.ci(spec.obj, coverage)
    lines(spec.obj$freq, spec.obj$spec*CI[1], ...)
    lines(spec.obj$freq, spec.obj$spec*CI[2], ...)
}

drawSpectrumCross <- function(spec.obj, coverage=0.95, frequency,...)
{
    if (missing(frequency))
        frequency <- spec.obj$freq[length(spec.obj$freq)/2]
    CI <- spec.ci(spec.obj, coverage)
    spec <- spec.obj$spec[which.min(abs(spec.obj$freq-frequency))]
    lines(frequency+spec.obj$bandwidth*c(-0.5,0.5), rep(spec,2), ...)
    lines(rep(frequency, 2), spec*CI, ...)
}