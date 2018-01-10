
if(!exists('m1mc')){
load('thetab.rda')
load('mooringladp.rda')
source('filterAndSpectrumParameters.R')}


##FlemishCapMooring
d <- m1rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
data <- d[o]

#params for rotation
lon <- m1adcp[[1]][['longitude']][1]*-1
lat <- m1adcp[[1]][['latitude']][1]
angle <- -(angleFC+90)

nfcVeldepth <- depth


nfcu <- unlist(lapply(data, function(stn) mean(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=angle)$u,na.rm=TRUE)))
nfcuper <- unlist(lapply(data, function(stn) (length(RCMvelocity(stn)$u) -length(which(is.na(RCMvelocity(stn)$u))))/ length(RCMvelocity(stn)$u)))*100
nfcusd <- unlist(lapply(data, function(stn) sd(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=angle)$u,na.rm=TRUE)))
nfcv <- unlist(lapply(data, function(stn) mean(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=angle)$v,na.rm=TRUE)))
nfcvsd <- unlist(lapply(data, function(stn) sd(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=angle)$v,na.rm=TRUE)))
nfcuper <- unlist(lapply(data, function(stn) (length(RCMvelocity(stn)$v) -length(which(is.na(RCMvelocity(stn)$v))))/ length(RCMvelocity(stn)$v)))*100
nfcU <- unlist(lapply(data, function(stn) mean(sqrt((RCMvelocity(stn)$u)^2 + (RCMvelocity(stn)$v)^2), na.rm=TRUE)))
nfcUsd <- unlist(lapply(data, function(stn) sd(sqrt((RCMvelocity(stn)$u)^2 + (RCMvelocity(stn)$v)^2), na.rm=TRUE)))

d <- m1mc
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
data <- d[o]

nfcHyddepth <- depth

nfcT <- unlist(lapply(data, function(stn) mean(stn[['temperature']], na.rm=TRUE)))
nfcTsd <- unlist(lapply(data, function(stn) sd(stn[['temperature']], na.rm=TRUE)))
nfcTper <- unlist(lapply(data, function(stn) (length(stn[['temperature']]) -length(which(is.na(stn[['temperature']]))))/ length(stn[['temperature']])))*100
nfcS <- unlist(lapply(data, function(stn) mean(stn[['salinity']], na.rm=TRUE)))
nfcSsd <- unlist(lapply(data, function(stn) sd(stn[['salinity']], na.rm=TRUE)))
nfcSper <- unlist(lapply(data, function(stn) (length(stn[['salinity']]) -length(which(is.na(stn[['salinity']]))))/ length(stn[['salinity']])))*100
nfcST <- unlist(lapply(data, function(stn) mean(stn[['sigmaTheta']], na.rm=TRUE)))
nfcSTsd <- unlist(lapply(data, function(stn) sd(stn[['sigmaTheta']], na.rm=TRUE)))


d <- m1adcp
latNFC <- d[[1]][['latitude']]
lonNFC <- d[[1]][['longitude']]*-1
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
look1 <- which.min(abs(nfcHyddepth[1] - depth))
look2 <- which.min(abs(nfcHyddepth[2] - depth))
nfcAdcpdepth <- c(depth[look1], depth[look2])
nfcuadcp <- nfcuadcpsd <- nfcvadcp <- nfcvadcpsd <- nfcUadcp <- nfcuadcpper <- nfcvadcpper <- nfcUadcpsd <- NA

#unlist(lapply(data, function(stn) mean(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=thetab)$u,na.rm=TRUE)))

nfcuadcp[1] <- mean(rotateVelocity(u=d[[look1]][['u']], v=d[[look1]][['v']], angle=angle)$u,na.rm=TRUE)
nfcuadcp[2] <- mean(rotateVelocity(u=d[[look2]][['u']], v=d[[look2]][['v']], angle=angle)$u,na.rm=TRUE)
nfcuadcpper[1] <- ((length(d[[look1]][['u']]) -length(which(is.na(d[[look1]][['u']]))))/ length(d[[look1]][['u']]))*100
nfcuadcpper[2] <- ((length(d[[look2]][['u']]) -length(which(is.na(d[[look2]][['u']]))))/ length(d[[look2]][['u']]))*100
nfcuadcpsd[1] <- sd(rotateVelocity(u=d[[look1]][['u']], v=d[[look1]][['v']], angle=angle)$u,na.rm=TRUE)
nfcuadcpsd[2] <- sd(rotateVelocity(u=d[[look2]][['u']], v=d[[look2]][['v']], angle=angle)$u,na.rm=TRUE)
nfcvadcp[1] <- mean(rotateVelocity(u=d[[look1]][['u']], v=d[[look1]][['v']], angle=angle)$v,na.rm=TRUE)
nfcvadcp[2] <- mean(rotateVelocity(u=d[[look2]][['u']], v=d[[look2]][['v']], angle=angle)$v,na.rm=TRUE)
nfcvadcpsd[1] <- sd(rotateVelocity(u=d[[look1]][['u']], v=d[[look1]][['v']], angle=angle)$v,na.rm=TRUE)
nfcvadcpsd[2] <- sd(rotateVelocity(u=d[[look2]][['u']], v=d[[look2]][['v']], angle=angle)$v,na.rm=TRUE)
nfcUadcp[1] <- mean(sqrt((d[[look1]][['u']])^2 + (d[[look1]][['v']])^2), na.rm=TRUE)
nfcUadcpsd[1] <- sd(sqrt((d[[look1]][['u']])^2 + (d[[look1]][['v']])^2), na.rm=TRUE)
nfcUadcp[2] <- mean(sqrt((d[[look2]][['u']])^2 + (d[[look2]][['v']])^2), na.rm=TRUE)
nfcUadcpsd[2] <- sd(sqrt((d[[look2]][['u']])^2 + (d[[look2]][['v']])^2), na.rm=TRUE)

okm <- which(trans == 'FC')
ladpUFC <- matrix(nrow=length(nfcHyddepth), ncol=4)

for (i in 1:length(okm)){
	dm <- mooringladp[[okm[i]]]
	for (j in 1:length(nfcHyddepth)){
		depthlook <- nfcHyddepth[j]
		pm <- dm[['pressure']]
		okd <- which.min(abs(depthlook - pm))
		ladpUFC[j,(i*2 -1)] <- dm[['ur']][okd]
		ladpUFC[j,(i*2)] <- dm[['vr']][okd]	
	}
}

##SackvilleSpurMooring
d <- m2rcm
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
data <- d[o]

#params for rotation
lon <- m2adcp[[1]][['longitude']][1]
lat <- m2adcp[[1]][['latitude']][1]
angle <- -(angleSS + 90)

ssVeldepth <- depth
ssu <- unlist(lapply(data, function(stn) mean(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=angle)$u,na.rm=TRUE)))
ssusd <- unlist(lapply(data, function(stn) sd(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=angle)$u,na.rm=TRUE)))
ssuper <- unlist(lapply(data, function(stn) (length(RCMvelocity(stn)$u) -length(which(is.na(RCMvelocity(stn)$u))))/ length(RCMvelocity(stn)$u)))*100
ssv <- unlist(lapply(data, function(stn) mean(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=angle)$v,na.rm=TRUE)))
ssvsd <- unlist(lapply(data, function(stn) sd(rotateVelocity(u=RCMvelocity(stn)$u, v=RCMvelocity(stn)$v, angle=angle)$v,na.rm=TRUE)))
ssU <- unlist(lapply(data, function(stn) mean(sqrt((RCMvelocity(stn)$u)^2 + (RCMvelocity(stn)$v)^2), na.rm=TRUE)))
ssUsd <- unlist(lapply(data, function(stn) sd(sqrt((RCMvelocity(stn)$u)^2 + (RCMvelocity(stn)$v)^2), na.rm=TRUE)))

d <- m2mc
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
o <- order(depth)
depth <- depth[o]
data <- d[o]

ssHyddepth <- depth

ssT <- unlist(lapply(data, function(stn) mean(stn[['temperature']], na.rm=TRUE)))
ssTsd <- unlist(lapply(data, function(stn) sd(stn[['temperature']], na.rm=TRUE)))
ssTper <- unlist(lapply(data, function(stn) (length(stn[['temperature']]) -length(which(is.na(stn[['temperature']]))))/ length(stn[['temperature']])))*100
ssS <- unlist(lapply(data, function(stn) mean(stn[['salinity']], na.rm=TRUE)))
ssSsd <- unlist(lapply(data, function(stn) sd(stn[['salinity']], na.rm=TRUE)))
ssST <- unlist(lapply(data, function(stn) mean(stn[['sigmaTheta']], na.rm=TRUE)))
ssSTsd <- unlist(lapply(data, function(stn) sd(stn[['sigmaTheta']], na.rm=TRUE)))

d <- m2adcp
latSS <- d[[1]][['latitude']]
lonSS <- d[[1]][['longitude']]
depth <- unlist(lapply(d, function(stn) ifelse(length(stn[['pressure']]) != 0, mean(stn[['pressure']], na.rm=TRUE), stn[['depthMax']])))
look1 <- which.min(abs(ssHyddepth[1] - depth))
look2 <- which.min(abs(ssHyddepth[2] - depth))
ssAdcpdepth <- c(depth[look1], depth[look2])
ssuadcp <- ssuadcpsd <- ssvadcp <- ssvadcpsd <- ssUadcp <- ssuadcpper <- ssUadcpsd <- NA
ssuadcp[1] <- mean(rotateVelocity(u=d[[look1]][['u']], v=d[[look1]][['v']], angle=angle)$u,na.rm=TRUE)
ssuadcp[2] <- mean(rotateVelocity(u=d[[look2]][['u']], v=d[[look2]][['v']], angle=angle)$u,na.rm=TRUE)
ssuadcpsd[1] <- sd(rotateVelocity(u=d[[look1]][['u']], v=d[[look1]][['v']], angle=angle)$u,na.rm=TRUE)
ssuadcpsd[2] <- sd(rotateVelocity(u=d[[look2]][['u']], v=d[[look2]][['v']], angle=angle)$u,na.rm=TRUE)
ssuadcpper[1] <- ((length(d[[look1]][['u']]) -length(which(is.na(d[[look1]][['u']]))))/ length(d[[look1]][['u']]))*100
ssuadcpper[2] <- ((length(d[[look2]][['u']]) -length(which(is.na(d[[look2]][['u']]))))/ length(d[[look2]][['u']]))*100
ssvadcp[1] <- mean(rotateVelocity(u=d[[look1]][['u']], v=d[[look1]][['v']], angle=angle)$v,na.rm=TRUE)
ssvadcp[2] <- mean(rotateVelocity(u=d[[look2]][['u']], v=d[[look2]][['v']], angle=angle)$v,na.rm=TRUE)
ssvadcpsd[1] <- sd(rotateVelocity(u=d[[look1]][['u']], v=d[[look1]][['v']], angle=angle)$v,na.rm=TRUE)
ssvadcpsd[2] <- sd(rotateVelocity(u=d[[look2]][['u']], v=d[[look2]][['v']], angle=angle)$v,na.rm=TRUE)
ssUadcp[1] <- mean(sqrt((d[[look1]][['u']])^2 + (d[[look1]][['v']])^2), na.rm=TRUE)
ssUadcpsd[1] <- sd(sqrt((d[[look1]][['u']])^2 + (d[[look1]][['v']])^2), na.rm=TRUE)
ssUadcp[2] <- mean(sqrt((d[[look2]][['u']])^2 + (d[[look2]][['v']])^2), na.rm=TRUE)
ssUadcpsd[2] <- sd(sqrt((d[[look2]][['u']])^2 + (d[[look2]][['v']])^2), na.rm=TRUE)


okm <- which(trans == 'SS')
ladpUSS <- matrix(nrow=length(ssHyddepth), ncol=4)

for (i in 1:length(okm)){
	dm <- mooringladp[[okm[i]]]
	for (j in 1:length(ssHyddepth)){
		depthlook <- ssHyddepth[j]
		pm <- dm[['pressure']]
		okd <- which.min(abs(depthlook - pm))
		ladpUSS[j,(i*2 -1)] <- dm[['ur']][okd]
		ladpUSS[j,(i*2)] <- dm[['vr']][okd]	
	}
}



#header <- c('Mooring','Depth (m)', 'u $\\pm$ std dev (m\\,s$^{-1}$)', 'v $\\pm$ std dev', 'T $\\pm$ std dev $^\\circ$C', 'S $\\pm$ std dev')

#header of old table with temp and salinity
#header <- c('Depth (m)', '$\\bar{u_r}$ (m/s)', 'sd (m/s)', '$\\bar{v_r}$ (m/s)', 'sd (m/s)','$\\overline{U}$ (m/s)','$\\overline{T}$ ($^\\circ$C)', 'sd ($^\\circ$C)', '$\\overline{S}$', 'sd')

header <- c('Depth (m)', '$\\overline{u_r}$ (m/s)', 'sd (m/s)', '$\\overline{v_r}$ (m/s)', 'sd (m/s)','$\\overline{U}$ (m/s)','sd (m/s)', '$\\overline{\\sigma_\\theta}$ (kg/m$^3$)' , 'sd (kg/m$^3$)')


#header <- c('Depth (m)', '$\\bar{u_r}$ (m/s)', 'sd (m/s)', '$\\bar{v_r}$ (m/s)', 'sd (m/s)','$\\overline{U}$ (m/s)','$\\bar{u_r}$ (m/s)', '$\\bar{v_r}$ (m/s)', '$\\bar{u_r}$ (m/s)', '$\\bar{v_r}$ (m/s)')

#header <- c('Depth (m)', '$\\bar{u}$ (m/s)', 'sd (m/s)', '$\\bar{v}$ (m/s)', 'sd (m/s)','$\\overline{U}$ (m/s)','\\%','$\\overline{T}$ ($^\\circ$C)', 'sd ($^\\circ$C)', '$\\overline{S}$', 'sd','\\%')

###FULL TABLE, ONLY NFC AND SS
mooring <- c('Sackville Spur', 
			rep(' ',length(ssHyddepth)-1),
			'Flemish Cap', 
			rep(' ', length(nfcHyddepth)-1))
depth <- c(round(ssHyddepth), round(nfcHyddepth))
u <- c(round(ssuadcp,2), round(ssu,2),
	   round(nfcuadcp,2), round(nfcu,2))
usd <- c(round(ssuadcpsd,2), round(ssusd,2),
		 round(nfcuadcpsd,2), round(nfcusd,2)
		)
v <- c(round(ssvadcp,2), round(ssv,2), round(nfcvadcp,2), round(nfcv,2))
look <- which(v==0)
v <- as.character(v)
v[look] <- paste("0.00")
vsd <- c(round(ssvadcpsd,2), round(ssvsd,2),
         round(nfcvadcpsd,2),round(nfcvsd,2))
U <- c(round(ssUadcp,2), round(ssU,2),
	   round(nfcUadcp,2), round(nfcU,2))
Usd <- c(round(ssUadcpsd,2), round(ssUsd,2),
         round(nfcUadcpsd,2), round(nfcUsd,2))
velocitypercent <- c(round(ssuadcpper), round(ssuper),
         			round(nfcuadcpper), round(nfcuper))
T <- c(round(ssT,2), round(nfcT,2))
Tsd <- c(round(ssTsd,2), round(nfcTsd,2))
S <- c(round(ssS,2), round(nfcS,2))
Ssd <- c(round(ssSsd, 2), round(nfcSsd, 2))
look <- which(Ssd == 0)
Ssd <- as.character(Ssd)
Ssd[look] <- paste("<0.01")
ST <- c(round(ssST,2), round(nfcST,2))
STsd <- c(round(ssSTsd, 2), round(nfcSTsd, 2))
look <- which(STsd == 0)
STsd <- as.character(STsd)
STsd[look] <- paste("<0.01")

hydropercent <- c(round(ssTper), round(nfcTper))
lat <- c(latSS, rep("", (length(c(ssu, ssuadcp))-1)), latNFC, rep("", (length(c(nfcu, nfcuadcp))-1)))
lon <- c(lonSS, rep("", (length(c(ssu, ssuadcp))-1)), lonNFC, rep("", (length(c(nfcu, nfcuadcp))-1)))

ladpFC <- round(ladpUFC,2)
ladpSS <- round(ladpUSS,2)


#table <- cbind(mooring,depth, u, usd,v, vsd, U, T, Tsd, S, Ssd)

table <- cbind(mooring,depth, u, usd,v, vsd, U, Usd, ST, STsd)

colnames(table) <- c('Mooring',header)

###JUST NFC TABLE
header <- c('Depth (m)', '$\\bar{u_r}$ (m/s)', 'sd (m/s)', '$\\bar{v_r}$ (m/s)', 'sd (m/s)','$\\overline{U}$ (m/s)','$\\overline{T}$ ($^\\circ$C)', 'sd ($^\\circ$C)', '$\\overline{S}$', 'sd')
mooring <- c(rep('NFC', length(nfcHyddepth)), rep('SS', length(ssHyddepth)))
depth <- mround(nfcHyddepth, 5)
u <- c(round(nfcu,2), round(nfcuadcp,2))
usd <- c(round(nfcusd,2), round(nfcuadcpsd,2))
v <- c(round(nfcv,2), round(nfcvadcp,2))
vsd <- c(round(nfcvsd,2), round(nfcvadcpsd,2))
U <- c(round(nfcU,2), round(nfcUadcp,2))
T <- round(nfcT,2)
Tsd <- round(nfcTsd,2)
S <- round(nfcS,2)
Ssd <- round(nfcSsd, 2)
lat <- c(latNFC, rep("", (length(c(nfcu, nfcuadcp))-1)), latSS, rep("", (length(c(ssu, ssuadcp))-1)))
lon <- c(lonNFC, rep("", (length(c(nfcu, nfcuadcp))-1)), lonSS, rep("", (length(c(ssu, ssuadcp))-1)))

tableNFC <- cbind(as.character(depth), u, usd,v, vsd, U, T, Tsd, S, Ssd)
colnames(tableNFC) <- header



##JUST SACKVILLE SPUR TABLE
depth <- mround(ssHyddepth,5)
u <- c(round(ssu,2), round(ssuadcp,2))
usd <- c(round(ssusd,2), round(ssuadcpsd,2))
v <- c(round(ssv,2), round(ssvadcp,2))
vsd <- c(round(ssvsd,2), round(ssvadcpsd,2))
U <- c(round(ssU,2), round(ssUadcp,2))
T <- round(ssT,2)
Tsd <- round(ssTsd,2)
S <- round(ssS,2)
Ssd <- round(ssSsd, 2)
lat <- c(latNFC, rep("", (length(c(nfcu, nfcuadcp))-1)), latSS, rep("", (length(c(ssu, ssuadcp))-1)))
lon <- c(lonNFC, rep("", (length(c(nfcu, nfcuadcp))-1)), lonSS, rep("", (length(c(ssu, ssuadcp))-1)))

tableSS <- cbind(as.character(depth), u, usd,v, vsd, U, T, Tsd, S, Ssd)
#colnames(tableSS) <- header

#new_table <- matrix(0, nrow=16, ncol=6)

#new_table[,3:6] <- paste0(table[, seq(3,9,2)][grep("\\d+", table[,seq(3,9,2)])], "$\\pm$", table[,seq(4,10,2)][grep("\\d+", table[,seq(4,10,2)])])
#new_table[,1] <- table[,1]
#new_table[,2] <- table[,2]
#colnames(new_table) <- header


caption <- c('Flemish Cap and Sackville Spur mooring statistics from the RCM and ADCP unfiltered hourly observations and microCAT unfiltered 5 minute observations. $\\overline{u_r}$, $\\overline{v_r}$ are the mean along-isobath and cross-isobath velocity components respectively, sd indicates standard deviation, $\\overline{U}$ is mean speed, $\\overline{T}$ is the mean temperature, and $\\overline{S}$ is the mean salinity.','Flemish Cap and Sackville Spur mooring statistics')

caption <- c('Sackville Spur and Flemish Cap mooring statistics from the RCM and ADCP unfiltered hourly observations and microCAT unfiltered 5 minute observations. Here, $\\overline{u_r}$, $\\overline{v_r}$ are the mean along-isobath and cross-isobath velocity components respectively, $\\overline{U}$ is mean speed, $\\overline{\\sigma_\\theta}$ is the mean potential density anomaly, and sd indicates standard deviation. of the quantity whose mean is in the column to the left','Flemish Cap and Sackville Spur mooring statistics')
label <-'t:moorStats'

captionNFC <- c('Flemish Cap moorings statistics from the RCM and ADCP unfiltered hourly observations and microCAT unfiltered 5 minute observations. $\\overline{u}$, $\\overline{v}$ are the mean horizontal velocity components, sd indicates standard deviation, $\\overline{U}$ is mean speed, $\\overline{T}$ is the mean temperature, and $\\overline{S}$ is the mean salinity.', 'Flemish Cap mooring statistics')
labelNFC <- 't:NFCmoorStats'

captionSS <- c('Sackville Spur moorings statistics from the RCM and ADCP unfiltered hourly observations and microCAT unfiltered 5 minute observations. $\\overline{u}$, $\\overline{v}$ are the mean horizontal velocity components, sd indicates standard deviation, $\\overline{U}$ is mean speed, $\\overline{T}$ is the mean temperature, and $\\overline{S}$ is the mean salinity.', 'Sackville Spur Cap mooring statistics')
labelSS <- 't:SSmoorStats'

align <- c('K{.5in}','K{.8in}',rep('K{.4in}',10))
#note:can't get latex to use K column type, use this for now
align <- c('p{.5in}','p{.9in} |',rep('p{.3in}',10)) #for old table with salinity and temp
align <- c('p{.5in}','p{.86in} |',rep('p{.3in}',7), rep('p{.34in}',2)) #new table with Usd and sigma theta

alignNFC <- c('K{.5in}','p{.4in}',rep('K{.4in}',9))
#align <- paste('{\\centering\\arraybackslash}', align)

#print(xtable(table, caption=caption, label=label, align=align), include.rownames = F, sanitize.colnames.function = identity,sanitize.text.function=identity, hline.after = c(0,nrow(table)))


