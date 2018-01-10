function F_MAPheading(a,stnnumber,radius) 
%===================================================
% Script Name: F_MAPheadings.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot current headings on a map
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				Input currents were aligned along the bathymetry and
%				transect
%				
% subroutines:  m_plot_topo1min_noland2 (need m_map toolbox and routine 
%                 B_MAPcastlocation must be run first)            

% Modification Log:
% August 26, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================

%----------------------------------------------------------------------
% STEP 1: load Data
%----------------------------------------------------------------------

load(['HUD', a.cruise_id]) 

%----------------------------------------------------------------------
% STEP 2: Set up Data arrays with headings from currents aligned along the bathymetry and transect
%----------------------------------------------------------------------

numfiles=length(stnnumber);
stationlocation(1,:)=[0 0 0];
fldname=['pointminmax',num2str(radius),'km'];
stationlocation=a.stnlocation; 
for n=1:numfiles
   %(n,:)= [HUDLADCP(n,1).lat  HUDLADCP(n,1).lon HUDLADCP(n,1).cast];
  if  length(HUDLADCP(n,1).point3)==8
   heading(n,:)=[HUDLADCP(n,1).point3(1,6)  HUDLADCP(n,1).point3(1,8) HUDLADCP(n,1).point3(1,5)  HUDLADCP(n,1).point3(1,7)];
  else 
   heading(n,:)=[NaN NaN NaN NaN];
  end
   heading2(n,:)=[HUDLADCP(n,1).(fldname)(1,3)  HUDLADCP(n,1).(fldname)(1,10) HUDLADCP(n,1).(fldname)(1,1)  HUDLADCP(n,1).(fldname)(1,8)];
end

%----------------------------------------------------------------------
% STEP 3: Plot heading on map
%----------------------------------------------------------------------

figure (1)

figtitle=[['HUD', a.cruise_id], ' Current Heading for Stations aligned with Transect'];
latmax=a.latmax;latmin=a.latmin;
lonmax=a.lonmax;lonmin=a.lonmin; 
slist  = num2str(stationlocation(:,3));
lat_points=stationlocation(:,1);lon_points=stationlocation(:,2);
m_plot_topo1min_noland2(figtitle,latmax,latmin,lonmax,lonmin,lat_points,lon_points,slist, heading)

orient tall
filename=[['HUD', a.cruise_id], 'headingtransect'];
 print('-djpeg', '-r300', filename);
 hgsave(filename);
  
figure (2)
figtitle=[['HUD', a.cruise_id], ' Current Heading for Stations aligned with Bathymetry'];

m_plot_topo1min_noland2(figtitle,latmax,latmin,lonmax,lonmin,lat_points,lon_points,slist, heading2)

orient tall
filename=[['HUD', a.cruise_id], 'headingbathy','radius',num2str(radius) ];
 print('-djpeg', '-r300', filename);
%    print('-dpng', '-r300',  filename);
   hgsave(filename);