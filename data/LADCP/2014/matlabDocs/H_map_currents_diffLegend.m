     
function H_map_currents(a,bins, binsize,stnnumber) 
% Script Name: H_map_currents.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot current on a map
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
load elevations 
load lat 
load lon
load(['HUD', a.cruise_id]) 

%plot overall U and V for depth bins from surface
for n=1:length(stnnumber) 
    stationlocation(n,:)= [HUDLADCP(n,1).lat  HUDLADCP(n,1).lon HUDLADCP(n,1).cast];
end

figure (1)
figtitle=[['HUD', a.cruise_id], ' Currents (not aligned with Transect or Bathy)'];
latmax=a.latmax;latmin=a.latmin;
lonmax=a.lonmax;lonmin=a.lonmin; 
slist  = num2str(stationlocation(:,3));
lat_points=stationlocation(:,1);lon_points=stationlocation(:,2);
m_plot_topo1min_noland3(figtitle,latmax,latmin,lonmax,lonmin,lat_points,lon_points,slist)
hold on;
b=0;
linecolours=[1 1 0; 0 1 0; 0 1 1; 1 0 0; 1 0 1; 0 0 1; 0 0 0; 1 1 1];
linecolours=getcolour(linecolours);
positionforlegend=[(a.latmax-a.latmin)/4 + a.latmin    (a.lonmax-a.lonmin)/4*-1 + a.lonmax];

for n=1:length(stnnumber) 
    for i=1:length(bins)
        m_quiver(stationlocation(n,2),stationlocation(n,1),HUDLADCP(n,1).binu(i),HUDLADCP(n,1).binv(i),1,'color',linecolours(i,:))
        while n==1
            m_text(positionforlegend(2),positionforlegend(1)-b,[num2str(bins(i)),'+-',num2str(binsize(i))],'color',linecolours(i,:))
            b=b+0.2;
        end
    end
end

m_quiver(positionforlegend(2),positionforlegend(1)+.5,1,0,1,'color','k','linewidth',2)
m_text(positionforlegend(2),positionforlegend(1)+.8,'1 ms^-^1','color','k')

orient tall
filename=[['HUD', a.cruise_id], 'headingbathy'];
 print('-djpeg', '-r300', filename);
 hgsave(filename);

