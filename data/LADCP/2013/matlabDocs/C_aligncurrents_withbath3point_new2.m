function a=C_aligncurrents_withbath3point_new(a,transect) 
%===================================================
% Script Name: C_aligncurrents_withbath3point.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to align currents of  LADCP data 
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				Currents are aligned along the transect line 
%				using 3 points and polynomial curve fitting method

% subroutines:  polar2uv              

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

% latloncast=a.stnlocation;
% n=1;

%----------------------------------------------------------------------
% STEP 2: For each transect align the currents
%----------------------------------------------------------------------

%create an array of just Transect stations
castnumbers=[HUDLADCP(:,1).cast]; 
for k=1:length(transect)
        transect_index(k)=find(transect(k)==castnumbers);
end

latloncasti=[HUDLADCP(transect_index,1).lat; HUDLADCP(transect_index,1).lon; HUDLADCP(transect_index,1).cast]'; %[Latitude  Longitude Cast #]
latloncastsort=sortrows(latloncasti,2); % sort by longitude

%loop to find new heading of currents using 3 point and polyfit

if size(latloncasti,1)>=3 %Check if the transect has at least 3 points
    for j=2:length(latloncasti)-1
        x=latloncastsort(j-1:j+1,2); y=latloncastsort(j-1:j+1,1); h=1;
        p = polyfit(x,y,h); %y= p(1)x + p(2)
        y1=p(1)*(latloncastsort(j,2)) + p(2);
        y2=p(1)*(latloncastsort(j+1,2)) + p(2);
        lat2=y2*pi/180; lat1=y1*pi/180; 
        lon2=x(3)*pi/180*-1; lon1=x(2)*pi/180*-1; 
        d = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));
        heading = acos((sin(lat2)-sin(lat1)*cos(d))/(sin(d)*cos(lat1)))*180/pi;
        latloncastsort(j,4)=heading; 
        latloncastsort(j,5)=y1; latloncastsort(j,7)=y2; latloncastsort(j,6)=latloncastsort(j,2); latloncastsort(j,8)=latloncastsort(j+1,2);
    end
    %first station in transect
    y1=latloncastsort(1,1); y2=latloncastsort(2,1);
    x1=latloncastsort(1,2); x2=latloncastsort(2,2);
    lat2=y2*pi/180; lat1=y1*pi/180; 
    lon2=x2*pi/180*-1; lon1=x1*pi/180*-1; 
    d = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));
    heading = acos((sin(lat2)-sin(lat1)*cos(d))/(sin(d)*cos(lat1)))*180/pi;
    latloncastsort(1,4)=heading;
     latloncastsort(1,5)=y1; latloncastsort(1,7)=y2; latloncastsort(1,6)=x1; latloncastsort(1,8)=x2;

    %last station in transect
    y1=latloncastsort(end-1,1); y2=latloncastsort(end,1);
    x1=latloncastsort(end-1,2); x2=latloncastsort(end,2);
    lat2=y2*pi/180; lat1=y1*pi/180; 
    lon2=x2*pi/180*-1; lon1=x1*pi/180*-1; 
    d = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));
    heading = acos((sin(lat2)-sin(lat1)*cos(d))/(sin(d)*cos(lat1)))*180/pi;
    latloncastsort(end,4)=heading;
    latloncastsort(end,5)=y1; latloncastsort(end,7)=y2; latloncastsort(end,6)=x1; latloncastsort(end,8)=x2;

elseif size(latloncasti,1)==2 %if the transect has 2 points
    %first station in transect    
        y1=latloncastsort(1,1); y2=latloncastsort(2,1);
        x1=latloncastsort(1,2); x2=latloncastsort(2,2);
        lat2=y2*pi/180; lat1=y1*pi/180; 
        lon2=x2*pi/180*-1; lon1=x1*pi/180*-1; 
        d = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));
        heading = acos((sin(lat2)-sin(lat1)*cos(d))/(sin(d)*cos(lat1)))*180/pi;
        latloncastsort(1,4)=heading;
        latloncastsort(1,5)=y1; latloncastsort(1,7)=y2; latloncastsort(1,6)=x1; latloncastsort(1,8)=x2;
    %last station in transect
        y1=latloncastsort(end-1,1); y2=latloncastsort(end,1);
        x1=latloncastsort(end-1,2); x2=latloncastsort(end,2);
        lat2=y2*pi/180; lat1=y1*pi/180; 
        lon2=x2*pi/180*-1; lon1=x1*pi/180*-1; 
        d = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));
        heading = acos((sin(lat2)-sin(lat1)*cos(d))/(sin(d)*cos(lat1)))*180/pi;
        latloncastsort(end,4)=heading;
        latloncastsort(end,5)=y1; latloncastsort(end,7)=y2; latloncastsort(end,6)=x1; latloncastsort(end,8)=x2;

elseif size(latloncasti,1)==1 %if the transect has 1 points
    
        HUDLADCP(transect_index,1).ualign=HUDLADCP(transect_index,1).u;
        HUDLADCP(transect_index,1).valign=HUDLADCP(transect_index,1).v;
        HUDLADCP(transect_index,1).heading=NaN;
        HUDLADCP(transect_index,1).dalign=NaN;
        HUDLADCP(lindex(k),1).spalign=HUDLADCP(transect_index,1).sp;       
        HUDLADCP(transect_index,1).point3=[latloncastsort, NaN, NaN, NaN,  NaN, NaN];     
end


%Project currents with new heading
point3=latloncastsort;  % save latloncastsort latloncastsort
m=1;

if size(latloncasti,1)>1
   for kk= 1:size(latloncastsort,1)
        lindex(kk)=find(latloncastsort(kk,3)==castnumbers);
        drcn=(HUDLADCP(lindex(kk),1).dir); spd=(HUDLADCP(lindex(kk),1).sp); %station current speed and direction
        drcntrans=latloncastsort(kk,4); % transect direction
        newdir=drcn+drcntrans;
        if newdir>360
            newdir=newdir-360;
        end
        speed=spd*cos(drcntrans*pi/180);
        [ualign valign]=polar2uv(newdir,speed);
       
        HUDLADCP(lindex(kk),1).ualign=ualign;
        HUDLADCP(lindex(kk),1).valign=valign;
        HUDLADCP(lindex(kk),1).heading=drcntrans;   
        HUDLADCP(lindex(kk),1).dalign=newdir;
        HUDLADCP(lindex(kk),1).spalign=speed;       
        HUDLADCP(lindex(kk),1).point3=latloncastsort(kk,:); 
   end        
          
end
%----------------------------------------------------------------------
% STEP 3: Save Structure 
%----------------------------------------------------------------------

nnn='HUDLADCP';
save(['HUD', a.cruise_id],nnn)
