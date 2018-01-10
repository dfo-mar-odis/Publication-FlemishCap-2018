function a=B_MAPcastlocation(a) 

%===================================================
% Script Name: B_MAPcastlocation.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot a map of station locations of LADCP data 
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
% subroutines: 
%               m_plot_topo1min_noland.m

% Modification Log:
% May 30, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================

%----------------------------------------------------------------------
% STEP 1: load Data
%----------------------------------------------------------------------
load(['HUD', a.cruise_id]) 

%----------------------------------------------------------------------
% STEP 2: Create an array of Latitudes, Longitudes and Cast # for all stations and save in a structure 
%----------------------------------------------------------------------

stationlocation=[HUDLADCP(:,1).lat; HUDLADCP(:,1).lon; HUDLADCP(:,1).cast]';
a.stnlocation=stationlocation;
%    save stationnames slist
%    save stationlocation stationlocation
%----------------------------------------------------------------------
% STEP 3: Plot a map of all stations 
%----------------------------------------------------------------------

plottitle=(['HUD',a.cruise_id]);
% stationlocation(1,:)=[];
slist  = num2str(stationlocation(:,3));
lat_points=stationlocation(:,1);lon_points=stationlocation(:,2);

m_plot_topo1min_noland(plottitle,a.latmax,a.latmin,a.lonmax,a.lonmin,lat_points,lon_points,slist)

%----------------------------------------------------------------------
% STEP 4: Save map 
%----------------------------------------------------------------------

filename=[plottitle,'MAP_nonum'];
 print('-djpeg', '-r300', filename);
   print('-dpng', '-r300',  filename);
   hgsave(filename);

