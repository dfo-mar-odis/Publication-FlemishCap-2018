library(oce)
if(!exists('mooringladp')){
load('mooringladp.rda')}

ladpspeed <- lapply(mooringladp, function(k) sqrt(k[['u']]^2 + k[['v']]^2))
ladpmeanspeed <- round(unlist(lapply(ladpspeed, mean)),2)
ladpmeanspeedsd <- round(unlist(lapply(ladpspeed, sd)),2)
ladpur <- lapply(mooringladp, function(k) k[['ur']])
ladpurmean <- round(unlist(lapply(ladpur, mean)),2)
ladpursd <- round(unlist(lapply(ladpur, sd)),2)
ladpurrange <- round(unlist(lapply(ladpur, range)),2)
ladpvr <- lapply(mooringladp, function(k) k[['vr']])
ladpvrmean <- round(unlist(lapply(ladpvr, mean)),2)
look <- which(ladpvrmean == 0)
ladpvrmean <- as.character(ladpvrmean)
ladpvrmean[look] <- paste("0.00")
ladpvrsd <- round(unlist(lapply(ladpvr, sd)),2)



year <- rep(c('2013', '2014'), 2)
location <- c('Sackville Spur', '', 'Flemish Cap', '')
header <- c('Location', 'Year', '$\\overline{u_r}$ (m/s)', 'sd (m/s)', '$\\overline{v_r}$ (m/s)', 'sd (m/s)','$\\overline{U}$ (m/s)', 'sd (m/s)')
table <- cbind(location, year, ladpurmean, ladpursd, ladpvrmean, ladpvrsd, ladpmeanspeed, ladpmeanspeedsd)
colnames(table) <- header

align <- c('p{.5in}','p{.9in} |',rep('p{.3in}',7))
caption <- 'Sackville Spur and Flemish Cap velocity statistics from LADCP station nearest to the mooring location; $\\overline{u_r}$, $\\overline{v_r}$ are the mean along-isobath and cross-isobath velocity components respectively, $\\overline{U}$ is mean speed, and sd indicates standard deviation of the quantity whose mean is in the column to the left.'

