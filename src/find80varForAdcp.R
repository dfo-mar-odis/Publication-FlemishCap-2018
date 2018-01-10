find80var <- function (s, plotcheck = FALSE) 
{
    ok <- s$freq < 1/5/24
    ok2 <- s$freq > 1/5/24
    df <- s$freq[2] - s$freq[1]
    varg5d <- sum(s$spec[ok2] * df)
    varl5d <- sum(s$spec[ok] * df)
    dspec <- diff(s$spec[ok])
    rlds <- rle(dspec < 0)
    rlen <- cumsum(rlds$lengths)
    look <- c(1, rlen[rlds$values] + 1)
    rlfr <- s$freq[look]
    rlsp <- s$spec[look]
    u80 <- which(rlfr > 1/36/24 & rlfr < 1/20/24)
    #u20 <- which(rlfr > 1/24/24 & rlfr < 1/12/24)
    {
        if (length(u80) == 0) {
            var <- NA
            #f20l <- NA
            #f20u <- NA
        }
        else {
            var <- sum(s$spec[look[1]:look[u80[1]]] * 
                df)
            #f20l <- s$freq[look[l20[length(l20)]]]
            #f20u <- s$freq[look[u20[1]]]
        }
    }
    if (plotcheck == TRUE) {
        par(mar = c(3, 3, 1, 1))
        plot(s$freq, s$spec, type = "l", xlim = range(s$freq[ok]))
        abline(v = s$freq[look[u80[1]]])
        #abline(v = c(s$freq[look[l20[length(l20)]]], s$freq[look[u20[1]]]), 
        #    col = "red")
        #abline(v = 1/21/24, lty = 2)
    }
    invisible(list(var = var))
}