     
function H_map_currents(a,bins, binsize,stnnumber,radius) 
%===================================================
% Script Name: H_map_currents.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot current on a map
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				Input currents were aligned along the bathymetry and
%				transect and not aligned
%				
% subroutines:  m_plot_topo1min_noland3 (need m_map toolbox and routine 
%               B_MAPcastlocation must be run first)            
%               getcolour
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

%----------------------------------------------------------------------
% STEP 2: set up variables
%----------------------------------------------------------------------

% array of station locations
stationlocation= a.stnlocation; %[HUDLADCP(n,1).lat  HUDLADCP(n,1).lon HUDLADCP(n,1).cast];

%set up map limits and list of points to plot
latmax=a.latmax;latmin=a.latmin;
lonmax=a.lonmax;lonmin=a.lonmin; 
slist  = num2str(stationlocation(:,3));
lat_points=stationlocation(:,1);lon_points=stationlocation(:,2);
positionforlegend=[(a.latmax-a.latmin)/4 + a.latmin    (a.lonmax-a.lonmin)/4*-1 + a.lonmax];

%set up an array of RGB color indexes to specify color for each depth bin
linecolours=[1 1 0; 0 1 0; 0 1 1; 1 0 0; 1 0 1; 0 0 1; 0 0 0; 1 1 1];
linecolours=getcolour(linecolours);

%----------------------------------------------------------------------
% STEP 3: plot U and V for depth bins determined in function G_Creatbins
%----------------------------------------------------------------------

%plot map with stations
figure (1)
figtitle=[['HUD', a.cruise_id], ' Currents (not aligned with Transect or Bathy)'];
m_plot_topo1min_noland3(figtitle,latmax,latmin,lonmax,lonmin,lat_points,lon_points,slist)
hold on;

%plot currents
for n=1:length(stnnumber) 
    if n==1
       for i=1:length(bins)
        m_quiver(stationlocation(n,2),stationlocation(n,1),HUDLADCP(n,1).binu(i),HUDLADCP(n,1).binv(i),1,'color',linecolours(i,:))
        text_labels{1,i}=[num2str(bins(i)),' \pm',num2str(binsize(i))];
        end 
    else  
        for i=1:length(bins)
        m_quiver(stationlocation(n,2),stationlocation(n,1),HUDLADCP(n,1).binu(i),HUDLADCP(n,1).binv(i),1,'color',linecolours(i,:))
        end
    end
end

m_quiver(positionforlegend(2),positionforlegend(1)+.5,1,0,1,'color','k','linewidth',2)
m_text(positionforlegend(2),positionforlegend(1)+.8,'1 ms^-^1','color','k')

legend 'show'
[legend_h,object_h,plot_h,text_strings] = legend;
legend(plot_h(11:length(bins)+10),text_labels,'FontSize', 8,'location','EastOutside')  %'box','off', 'Color', 'none');

orient tall
filename=[['HUD', a.cruise_id], 'curents_notaligned'];
 print('-djpeg', '-r300', filename);
 hgsave(filename);

%----------------------------------------------------------------------
% STEP 4: plot U and V aligned with Bathy for depth bins determined in function G_Creatbins
%----------------------------------------------------------------------

%plot map with stations
figure (2)
figtitle=[['HUD', a.cruise_id], ' Currents aligned with Bathemetry'];
m_plot_topo1min_noland3(figtitle,latmax,latmin,lonmax,lonmin,lat_points,lon_points,slist)
hold on;

%plot currents
for n=1:length(stnnumber) 
     for i=1:length(bins)
        m_quiver(stationlocation(n,2),stationlocation(n,1),HUDLADCP(n,1).binubathy(i),HUDLADCP(n,1).binvbathy(i),1,'color',linecolours(i,:))
     end
end

m_quiver(positionforlegend(2),positionforlegend(1)+.5,1,0,1,'color','k','linewidth',2)
m_text(positionforlegend(2),positionforlegend(1)+.8,'1 ms^-^1','color','k')

legend 'show'
[legend_h,object_h,plot_h,text_strings] = legend;
legend(plot_h(11:length(bins)+10),text_labels,'FontSize', 8,'location','EastOutside')  %'box','off', 'Color', 'none');

orient tall
filename=[['HUD', a.cruise_id], 'curents_alignedBathy',num2str(radius)];
 print('-djpeg', '-r300', filename);
 hgsave(filename);
 
 %----------------------------------------------------------------------
% STEP 5: plot U and V aligned with Transect for depth bins determined in function G_Creatbins
%----------------------------------------------------------------------

%plot map with stations
figure (3)
figtitle=[['HUD', a.cruise_id], ' Currents aligned with transect'];
m_plot_topo1min_noland3(figtitle,latmax,latmin,lonmax,lonmin,lat_points,lon_points,slist)
hold on;

%plot currents
for n=1:length(stnnumber) 
     for i=1:length(bins)
        m_quiver(stationlocation(n,2),stationlocation(n,1),HUDLADCP(n,1).binutran(i),HUDLADCP(n,1).binvtran(i),1,'color',linecolours(i,:))
     end
end

m_quiver(positionforlegend(2),positionforlegend(1)+.5,1,0,1,'color','k','linewidth',2)
m_text(positionforlegend(2),positionforlegend(1)+.8,'1 ms^-^1','color','k')

legend 'show'
[legend_h,object_h,plot_h,text_strings] = legend;
legend(plot_h(11:length(bins)+10),text_labels,'FontSize', 8,'location','EastOutside')  %'box','off', 'Color', 'none');

orient tall
filename=[['HUD', a.cruise_id], 'curents_alignedTran'];
 print('-djpeg', '-r300', filename);
 hgsave(filename);

