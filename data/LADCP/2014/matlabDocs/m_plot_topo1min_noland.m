% routine to plot bathymetry data from data file topo_12.1.img from 	
% Sandwell & Smith topography and satellite derived bathymetry
% This is a collaborative effort between NGA, NOAA, NAVO and SIO
% Reference:  Smith, W. H. F., and D. T. Sandwell, Global seafloor 
             %topography from satellite altimetry and ship depth soundings, 
             %Science, v. 277, p. 1957-1962, 26 Sept., 1997.

%******INPUT:
% input file is a img format with the naming convention 'topo_12.1.img'
    % it is Downloaded from: http://topex.ucsd.edu/marine_topo/mar_topo.html
    % Note that the Smith and Sandwell data file is quite large (>700 MB)
    % The predicted depths are based on the V16.1 gravity anomaly model in an adjacent directory.
    % grid spacing in longitude is 1 minute 
    % The latitude range is +/- 80.738
    % The elevation(+)and depth(-) are stored as 2-byte integers to the nearest meter.
    % new versions are released and file name may need to be changed 
% user must specify:
    % minimum and maximum Latitude in decimal degrees
    % minimum and maximum Longitude in decimal degrees (can range from
    % 0-360 or using neg values for west)
    % vector with points to plot one with latitude and one with longitude (neg
    % for west)
    % name for output figure
    
%*****CONTENT
%reads topo_12.1.img using the extract_1m.m routine provided with input
%extract_1m.m:  [image_data,vlat,vlon] = extract_1m(region,iopt)
%     where:  region =[south north west east];
%               iopt = 1 for bathymetry (default)
%		               2 for ship tracks 
%	          image_data  
%                (for iopt = 1) - matrix of sandwell bathymetry/topography
%                (for iopt = 2) - matrix of ones and zeros, where 1 represents
%                    a ship location, 0 represents a depth based on interpolation
%           vlat - vector of latitudes associated with image_data
%           vlon - vector of longitudes

% plots map with filled contours for depth with colour bar.
% contour plot is created using M-Map A mapping package for Matlab see:
%http://www.eos.ubc.ca/~rich/map.html
% Points are plotted as red circles
% saves figure

%*****OUTPUT: 
% figure FIG file  

%*****SEE ALSO:
% extract_1m.m, m_proj, m_contourf, clabel, colorbar, colormap, m_gshhs_h, 
% m_grid, m_usercoast, findobj, facecolor, m_plot

%Diana Cardoso, October 2009
% DFO Ocean Sciences
% 1 Challenger Drive			
% Dartmouth,NS	
% B2Y 4A2,Canada	

%Tel (902) 426-9663

%Diana.Cardoso@dfo-mpo.gc.ca
%www.dfo-mpo.gc.ca

%********END OF HEADER**************************************************************************************

function topo1min = m_plot_topo1min_noland(plottitle,latmax,latmin,lonmax,lonmin,lat_points,lon_points,slist)

file='cmap_bath500.mat'; %colour range for map
load(file);

% get Bathemetry from topo_12.1.img
% [image_data,vlat,vlon] = extract_1m([south north west east],iopt)
 [elevations,lat,lon]=extract_1m([latmin latmax lonmin lonmax],1);
lon=lon-360; %change Lonmat range from 0 to 360 to -0 to -180
land=find(elevations>=0);
elevations(land)=0;

%plot
m_proj('Transverse Mercator','longitudes',[lonmin lonmax], 'latitudes',[latmin latmax]);
[cs h]=m_contourf(lon,lat,elevations,[0 -100 -200 -300 -400 -500 -1000 -1500 -2000 -2500 -3000 -3500 -4000 -4500 -5000 -5500 -6000]);
clabel(cs,h,'manual','fontsize',6, 'fontweight','bold');% [-100 -200 -500 -1000 -2000 -3000],'LabelSpacing',288
m_gshhs_f('save','mycoast');
m_grid('fontsize',12);colormap(cmap_bath500);%colorbar;
% m_usercoast('mycoast','patch',[.7 .7 .7],'linewidth',1); %shade land grey and outline coast
set(findobj('tag','m_grid_color'),'facecolor','none'); %command to see the colours in Matlab otherwise you get a white map it is a bug in MAtlab
hold on;
m_plot(lon_points(:),lat_points(:), 'ro','MarkerEdgeColor','r','MarkerFaceColor','r','MarkerSize',2) % plots given point on map 
%    for jj = 1:length(lon_points)
%        m_text(lon_points(jj)+0.09,lat_points(jj),slist(jj,:),'fontsize',4,'fontweight','bold','color',[.1 1 .1])
%    end;    
xlabel('Longitude (^oW)','FontSize',10)
ylabel('Latitude (^oN)','FontSize',10)
title(plottitle)
save elevations elevations
save lon lon
save lat lat

