library(oce)
source('filterAndSpectrumParameters.R')

## first get section info to enter into webtide
## the code below to get this is nearly identical
## to some parts of 01_ladcp...

section <- vector(mode='list', length=5)
section[[1]] <- seq(2,13,1)
section[[2]] <- rev(seq(21,27,1))
section[[3]] <- seq(30,40,1)
section[[4]] <- seq(41,47,1)
section[[5]] <- c(48, 50, 51, 52, 49, 53, 57)

sectionseq <- c(2,5,3,4)
stime <- slongitude <- slatitude <- vector(mode='list', length=5)
for (i in sectionseq){
	data <- ladcp[section[[i]]]
	sr <- as.section(data)
	srr <- sectionGrid(sr)
	stime[[i]] <- srr[['time', 'byStation']]
	slongitude[[i]] <- srr[['longitude', 'byStation']]
	slatitude[[i]] <- srr[['latitude', 'byStation']]
	}

utide <- vtide <- vector(mode="list", length=length(stime))
basedir = '/Users/chantellelayton/WebTide'
{if(dir.exists(basedir)){
	for (i in 2:length(stime)){
		t <- stime[[i]]
		lon <- slongitude[[i]]
		lat <- slatitude[[i]]
		for (k in 1:length(t)){
		#ts <- as.POSIXct(t[k], tz='UTC')+seq(-96,96)*60*15 #used to test to see if timeseries looked OK and it does
			ts <- as.POSIXct(t[k], tz='UTC') + seq(-24,24)*60*15
			lo <- lon[k]
			la <- lat[k]
			prediction <- webtide("predict", longitude=lo, latitude=la, time=ts, basedir=basedir, plot=FALSE)
			look <- which.min(abs(prediction$time - t[k]))
			if(k==1) {
				u <- prediction$u[look]
				v <- prediction$v[look]
				}
			else{
				u <- c(u, prediction$u[look])
				v <- c(v, prediction$v[look])
				}
		}
		utide[[i]] <- u
		vtide[[i]] <- v
	
	}
	save(utide, vtide, file='webTideUandV.rda')
}

else{
	print("Please change the variable basedir to the directory of the WebTide application.")
	}
}



