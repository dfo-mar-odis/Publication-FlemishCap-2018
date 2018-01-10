

function [r,stationlocation] = plotUV4(stn,r,stationlocation,a)
r1=r;
data = sprintf([a.processed '/%03d/%03d.mat'],stn,stn);

data=load(data);
lat=data.dr.lat;
lon=data.dr.lon;

stationlocation=[stationlocation;  lat  lon  stn];

r=r+1;

