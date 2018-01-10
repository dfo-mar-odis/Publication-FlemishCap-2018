function a=C_aligncurrents_withbath(a) 
%===================================================
% Script Name: C_aligncurrents_withbath.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to align currents of  LADCP data 
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				Currents are aligned along the bathymetry 
%				for a 1, 5, 10 and 100 km radius around each station
% subroutines: 
%               polar2uv
%               dist2pos.m

% Modification Log:
% May 30, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================

%----------------------------------------------------------------------
% STEP 1: load Data
%----------------------------------------------------------------------

load elevations 
load lat 
load lon
load(['HUD', a.cruise_id]) 

topo=elevations; %rename elevations 1 min data
%----------------------------------------------------------
% STEP 2: smooth topo data to 2 min
%----------------------------------------------------------

s=size(topo);
rowsize=s(1);
colsize=s(2);
topo_smooth=zeros(rowsize,colsize); %matrix for smoothed data

%Inner matrix
for n=2:colsize-1;

    for m=2:rowsize-1;
        topo_smooth(m,n)=1/9*(sum(topo((m-1:m+1),n-1))+ sum(topo((m-1:m+1),n))+ sum(topo((m-1:m+1),n+1)));
        
    end
end

%first last column
for m=2:rowsize-1;
      topo_smooth(m,1)=1/6*(sum(topo((m-1:m+1),1))+ sum(topo((m-1:m+1),2)));
      topo_smooth(m,colsize)=1/6*(sum(topo((m-1:m+1),colsize))+ sum(topo((m-1:m+1),colsize-1)));
end

%first last row
for n=2:colsize-1;
      topo_smooth(1,n)=1/6*(sum(topo(1,(n-1:n+1)))+ sum(topo(2,(n-1:n+1))));
      topo_smooth(rowsize,n)=1/6*(sum(topo(rowsize,(n-1:n+1)))+ sum(topo(rowsize-1,(n-1:n+1))));
end

%corners
 topo_smooth(1,1)=1/4*(topo(1,1)+ topo(1,2)+ topo(2,1)+topo(2,2));
  topo_smooth(1,colsize)=1/4*(topo(1,colsize)+ topo(1,colsize-1)+ topo(2,colsize)+topo(2,colsize-1));
   topo_smooth(rowsize,1)=1/4*(topo(rowsize,1)+ topo(rowsize,2)+ topo(rowsize-1,1)+topo(rowsize-1,2));
    topo_smooth(rowsize,colsize)=1/4*(topo(rowsize,colsize)+ topo(rowsize,colsize-1)+ topo(rowsize-1,colsize)+topo(rowsize-1,colsize-1));

%----------------------------------------------------------
% STEP 3: Take smoothed topo data and convert to 2 min grid by removing every second value
%----------------------------------------------------------

topo_smooth_2min=topo_smooth(1:2:size(topo_smooth,1), 1:2:size(topo_smooth,2));

%----------------------------------------------------------
% STEP 4: For each station determine the gradient of the Bathy for a 1, 5,
% 10 and 100 km radius and rotate currents
%----------------------------------------------------------

for k=[1 5 10 100]
    c=1;
    for i=1:length(HUDLADCP)
    
       latcast=  HUDLADCP(i,1).lat;
       loncast=  HUDLADCP(i,1).lon;
       y=find(abs(lon(:)-loncast) <=min(abs(lon(:)-loncast)));
       x=find(abs(lat(:)-latcast) <=min(abs(lat(:)-latcast)));
       depth=mean(elevations(x,y));
  
      for j=1:1:360
         [lat_end,lon_end] = dist2pos(lat(x),lon(y)*-1,k,j);%  (lat, lon, km, heading in deg)
         y1=find(abs(lon(:)-lon_end*-1) <=min(abs(lon(:)-lon_end*-1)));
         x1=find(abs(lat(:)-lat_end) <=min(abs(lat(:)-lat_end)));
         depth2(j)=mean(topo_smooth(x1,y1));
         coord(j,:)=[lat_end  lat(x1) lon_end*-1  lon(y1)];
         gradienddepth(j)=depth2(j)-depth;
      end
        mi=find(depth2(:)==min(depth2));
        ma=find(depth2(:)==max(depth2));

        lat1=coord(ma(1),1)*pi/180; lat2=coord(mi(1),1)*pi/180; 
        lon1=coord(ma(1),3)*pi/180*-1; lon2=coord(mi(1),3)*pi/180*-1; 

         d = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));
        headingminmax = acos((sin(lat2)-sin(lat1)*cos(d))/(sin(d)*cos(lat1)))*180/pi ;
      
        pointminmax(c,:)=[coord(mi(1),1)  coord(mi(1),2) coord(mi(1),3)  coord(mi(1),4)  depth2(mi(1)) mean(mi) headingminmax coord(ma(1),1)  coord(ma(1),2) coord(ma(1),3)  coord(ma(1),4) depth2(ma(1))  mean(ma) headingminmax+360];
        %[lat_end(min depth)  lat_start(min depth) lon_end(min depth)  lon_start(min depth) depthtopo index heading  lat_end(max depth)  lat_start(max depth) lon_end(max depth)  lon_start(max depth)]    
%         drcn=HUDLADCP(i,1).dir - pointminmax(c,7);
%         [ualign valign]=polar2uv(drcn, HUDLADCP(i,1).sp);
%         valign = -sin(headingminmax*pi/180)*HUDLADCP(i,1).u + cos(headingminmax*pi/180)*HUDLADCP(i,1).v ;
%         ualign = cos(headingminmax*pi/180)*HUDLADCP(i,1).u + sin(headingminmax*pi/180)*HUDLADCP(i,1).v ;
        drcn=(HUDLADCP(i,1).dir); spd=(HUDLADCP(i,1).sp); %station current speed and direction
        drcntrans=pointminmax(c,7); % transect direction
        newdir=drcn+drcntrans;
        if newdir>360
            newdir=newdir-360;
        end
        speed=spd*cos(drcntrans*pi/180);
        [ualign valign]=polar2uv(newdir,speed);
        HUDLADCP(i,1).(['ualign',num2str(k),'km'])=ualign;
        HUDLADCP(i,1).(['valign',num2str(k),'km'])=valign;
        HUDLADCP(i,1).(['pointminmax',num2str(k),'km'])=pointminmax(c,:);
        c=c+1;
    end
 end
%----------------------------------------------------------------------
% STEP 5: Save Structure 
%----------------------------------------------------------------------

nnn='HUDLADCP';
save(['HUD', a.cruise_id],nnn)
