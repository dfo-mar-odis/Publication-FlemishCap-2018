function I_SectionPlot_aligned(a,transect,transectname,radius) 
%===================================================
% Script Name: I_SectionPlot_aligned.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot sections current figure
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				Input currents were aligned with bathymetry
%				
% subroutines:  pos2dist

% Modification Log:
% August 26, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================
%----------------------------------------------------------------------
% STEP 1: load data
%----------------------------------------------------------------------
load elevations 
load lat 
load lon
load(['HUD', a.cruise_id]) 
fldnameu=['ualign',num2str(radius),'km'];
fldnamev=['valign',num2str(radius),'km'];

%----------------------------------------------------------------------
% STEP 2: Set up data arrays for each transect
%----------------------------------------------------------------------
data2=ones(1,7);

castnumbers=[HUDLADCP(:,1).cast]; 
for kk=1:length(transect) % create an array for the 
        transect_index(kk)=find(transect(kk)==castnumbers);
       
        if HUDLADCP(transect_index(kk),1).ualign~=0
          u=  HUDLADCP(transect_index(kk),1).(fldnameu);
          v=  HUDLADCP(transect_index(kk),1).(fldnamev);
          z=  HUDLADCP(transect_index(kk),1).z;
          data2=[data2;  ones(length(z),1)*HUDLADCP(transect_index(kk),1).lat  ones(length(z),1)*HUDLADCP(transect_index(kk),1).lon  u v z ones(length(z),1)*kk  ones(length(z),1)*HUDLADCP(transect_index(kk),1).cast]; 
        end
end

latloncasti=[HUDLADCP(transect_index,1).lat; HUDLADCP(transect_index,1).lon; HUDLADCP(transect_index,1).cast; HUDLADCP(transect_index,1).depthbottom]'; %[Latitude  Longitude Cast #]
latloncastsort=sortrows(latloncasti,2); % sort by longitude
depth1= latloncastsort(:,4); %depth of each station
data2(1,:)=[];
data2sort=sortrows(data2,2); % sort by longitude

%----------------------------------------------------------------------
% STEP 3: create grid for plot
%----------------------------------------------------------------------

zmin=-.6; zmax=0.6; %limits of current velocity for colour scale %dz=0.04;
   ttlEW = [['HUD', a.cruise_id], ', E W Current Speed, Aligned with Bathymetry']; %titles
   ttlNS = [['HUD', a.cruise_id], ', N S Current Speed, Aligned with Bathymetry']; 
%     x0 = (a.lonmin : 0.01 : a.lonmax)';
%    distlon = pos2dist(a.latmin,a.lonmin,a.latmax,a.lonmax,1); %convert Lat, lon locations to distance
%    distlonmax = distlon+50; %maximum value for x-axis
%    xi = 600:1:1000;%0:1:distlonmax; % x scale 
     x0 = (latloncastsort(1,2) : 0.01 : latloncastsort(end,2))';
  ymax1 = max(abs(depth1))+100; ymax = ceil(ymax1*10^(-3))/(10^(-3));%maximum value for y-axis
   yi = 0:5:ymax1;% y scale

 yi = yi';
 x  = latloncastsort(:,2); %longitude for each station in the whole section
 y  = latloncastsort(:,1); % lat
depth =data2sort(:,5);% depth for each station in the whole section

% calculate distance between two points on earth's surface given by their latitude-longitude pair.
[k,r] = polyfit(x,y,1);

   y0 = k(1)*x0 + k(2);
   xref  = x0(1);
   yref  = y0(1);

   dy20  = (y0-yref).^2;
   dx20  = ((x0-xref).*cos(0.5*(y0-yref)*3.1415/180)).^2;
   ds0   = 110.585*sqrt(dy20 + dx20);

   dy2  = (y-yref).^2;
   dx2  = ((x-xref).*cos(0.5*(y-yref)*3.1415/180)).^2;
   ds   = 110.585*sqrt(dy2 + dx2);

   dist(1)=0;
   
     for t=1:length(transect_index)
       dist = [dist;  ds(t)*ones(length(HUDLADCP(transect_index(t),1).z),1)]; 
     end
     
   dist(1)=[];   
   xi = 0:1:dist(end); % x scale 

%----------------------------------------------------------------------
% STEP 4: Create plot
%----------------------------------------------------------------------
   
    for uv=1:2  %  Create Plot for U and V

        par   = data2(:,2+uv); %U or V 
        prmi = griddata(dist,depth,par,xi,yi,'linear'); %Interpolates scattered data to produce gridded data

  figure (uv)
   clrlim = [zmin,zmax]; % limits for colour bar
   pcolor(xi,yi,prmi), hold on
   set(gca,'ydir','reverse')
   shading interp
   caxis(clrlim(1,:))
   [C,h] = contour(xi,yi,prmi,[0 0],'Color',[0.7,0.7,0.7]); clabel(C,h,'FontSize',7,'fontweight','bold','Rotation',0,'LabelSpacing',200);
   distance = ds ;
   hold on
   plot(distance,ones(size(distance)),'kv','markerfacecolor',[0,0,0]), hold on
   text(ds, ones(size(ds))-10,num2str(latloncasti(:,3)), 'FontSize',8,'FontWeight','bold')
      
   %Plot bathymetry profile
      bdat  = depth1'*-1; 
      x1  = floor(min(ds));
      xm  = ceil(max(ds));
      xii = x1:xm;
      yii = pchip(ds,bdat,xii);
      X = [xii,xii(end),xii(1),xii(1)];
      Y = [yii,ymax,ymax,yii(1)];
      Xi  = X(find(~isnan(Y)));
      Yi  = Y(find(~isnan(Y)));
      fill(Xi,Yi,[0.7,0.7,0.7]), hold on;
      set(gca,'ylim',[0 ymax1])
    ylabel('Depth (m)')
    xlabel('Distance (km)')
    load cmapSPcontour 
    colormap(cmap)
    colorbar
%     colorbar('YTick',[zmin:dz:zmax])%('YTick',[-0.05:0.025:0.05])
if uv==1
   ttl = ttlEW ; 
else
   ttl = ttlNS ; 
end
 
title (ttl)

    end
%----------------------------------------------------------------------
% STEP 5: Save figures
%----------------------------------------------------------------------

 figure(1)   
 filename=[['HUD', a.cruise_id], 'aligned_USection_', transectname,'radius',num2str(radius)];
 print('-djpeg', '-r300', filename);
 hgsave(filename);
 
figure(2)
 filename=[['HUD', a.cruise_id], 'aligned_VSection_',transectname,'radius',num2str(radius) ];
 print('-djpeg', '-r300', filename);
 hgsave(filename);

