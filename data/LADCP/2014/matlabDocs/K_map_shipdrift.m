function K_map_shipdrift(a,bins,binsize) 
%===================================================
% Script Name: K_map_shipdrift.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot currents and ship drift of LADCP
%               data Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%								
% subroutines:  m_plot_topo1min_noland3
%               getcolour
%               cart2pol
%               polar2uv
% Modification Log:
% August 26, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================
%----------------------------------------------------------------------
% STEP 1: load data
%----------------------------------------------------------------------
load(['HUD', a.cruise_id])    
load elevations 
load lat 
load lon

%----------------------------------------------------------------------
% STEP 2: set up variables
%----------------------------------------------------------------------

stationlocation=a.stnlocation; %array of station locations

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
for n=1:length(HUDLADCP) 
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

m_quiver(positionforlegend(2),positionforlegend(1)+.5,1,0,1,'color','k','linewidth',2) % plot a scale on map
m_text(positionforlegend(2),positionforlegend(1)+.8,'1 ms^-^1','color','k')

legend 'show'
[legend_h,object_h,plot_h,text_strings] = legend;
legend(plot_h(11:length(bins)+10),text_labels,'FontSize', 8,'location','EastOutside')  %'box','off', 'Color', 'none');

%plot ship drift
for h=1:length(HUDLADCP)
  if isnan(HUDLADCP(h,1).shiplon)==0
    dd2=datevec(HUDLADCP(h,1).tim(end)); dd1=datevec(HUDLADCP(h,1).tim(1));
    e = etime(dd2, dd1);
    %heading
    [THETA,RHO] = cart2pol(HUDLADCP(h,1).xship(end),HUDLADCP(h,1).yship(end));
    anglecounterclock=THETA*180/pi;
    if anglecounterclock<90
        heading(h)=90-anglecounterclock;
    elseif anglecounterclock>=90 && anglecounterclock<180
        heading(h)=(180-anglecounterclock)+270;
    elseif anglecounterclock>=180 && anglecounterclock<270
        heading(h)=(270-anglecounterclock)+180;
    elseif anglecounterclock>=270 
        heading(h)=(360-anglecounterclock)+90;
    end
        
    %distance between start and end point of ship % Output dist is in km.
    dist(h) = pos2dist(HUDLADCP(h,1).shiplat(1),HUDLADCP(h,1).shiplon(1),HUDLADCP(h,1).shiplat(end),HUDLADCP(h,1).shiplon(end),1);
    dist(h) = dist(h)*1000; % convert to meters
    vel(h)  = dist(h)/e;
    [Uship(h)  Vship(h)] = polar2uv(heading(h),vel(h));
    m_plot([HUDLADCP(h,1).lon HUDLADCP(h,1).shiplon(end)], ...
    [HUDLADCP(h,1).lat HUDLADCP(h,1).shiplat(end)],'color',linecolours(i+5,:), 'LineWidth',1.5)
  else
    heading(h)= NaN ;
    dist(h) = NaN ;
    dist(h) = NaN ; 
    vel(h)  = NaN ;
    Uship(h)  = NaN ;
    Vship(h) = NaN ;
  end
end

m_quiver(stationlocation(:,2)',stationlocation(:,1)',Uship,Vship,0,'color',linecolours(i+6,:), 'LineWidth',1.5,'linestyle',':')
% m_quiver(stationlocation(:,2)',stationlocation(:,1)',Uship,Vship,0,'color',[.5 .2 .6], 'LineWidth',2,'linestyle',':')
m_text(lonmin+2,latmax-2,'Ship Drift','color',linecolours(i+5,:))
m_text(lonmin+2,latmax-2,'Ship Distance','color',linecolours(i+6,:))

orient tall
filename=[['HUD', a.cruise_id], 'curentshipdrift_notaligned'];
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
for n=1:length(HUDLADCP) 
     for i=1:length(bins)
        m_quiver(stationlocation(n,2),stationlocation(n,1),HUDLADCP(n,1).binubathy(i),HUDLADCP(n,1).binvbathy(i),1,'color',linecolours(i,:))
     end
end

m_quiver(positionforlegend(2),positionforlegend(1)+.5,1,0,1,'color','k','linewidth',2)
m_text(positionforlegend(2),positionforlegend(1)+.8,'1 ms^-^1','color','k')

legend 'show'
[legend_h,object_h,plot_h,text_strings] = legend;
legend(plot_h(11:length(bins)+10),text_labels,'FontSize', 8,'location','EastOutside')  %'box','off', 'Color', 'none');

%plot ship drift
for h=1:length(HUDLADCP)
  if isnan(HUDLADCP(h,1).shiplon)==0
    m_plot([HUDLADCP(h,1).lon HUDLADCP(h,1).shiplon(end)], ...
    [HUDLADCP(h,1).lat HUDLADCP(h,1).shiplat(end)],'color',linecolours(i+3,:), 'LineWidth',1.5)
  end
end

m_quiver(stationlocation(:,2)',stationlocation(:,1)',Uship,Vship,0,'color',linecolours(i+4,:), 'LineWidth',1.5,'linestyle',':')
m_text(lonmin+2,latmax-2,'Ship Drift','color',linecolours(i+4,:))
m_text(lonmin+2,latmax-2,'Ship Distance','color',linecolours(i+3,:))

orient tall
filename=[['HUD', a.cruise_id], 'curentshipdrift_alignedBathy'];
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
for n=1:length(HUDLADCP) 
     for i=1:length(bins)
        m_quiver(stationlocation(n,2),stationlocation(n,1),HUDLADCP(n,1).binutran(i),HUDLADCP(n,1).binvtran(i),1,'color',linecolours(i,:))
     end
end

m_quiver(positionforlegend(2),positionforlegend(1)+.5,1,0,1,'color','k','linewidth',2)
m_text(positionforlegend(2),positionforlegend(1)+.8,'1 ms^-^1','color','k')

legend 'show'
[legend_h,object_h,plot_h,text_strings] = legend;
legend(plot_h(11:length(bins)+10),text_labels,'FontSize', 8,'location','EastOutside')  %'box','off', 'Color', 'none');

%plot ship drift
for h=1:length(HUDLADCP)
  if isnan(HUDLADCP(h,1).shiplon)==0
    m_plot([HUDLADCP(h,1).lon HUDLADCP(h,1).shiplon(end)], ...
    [HUDLADCP(h,1).lat HUDLADCP(h,1).shiplat(end)],'color',linecolours(i+3,:), 'LineWidth',1.5)
  end
end

m_quiver(stationlocation(:,2)',stationlocation(:,1)',Uship,Vship,0,'color',linecolours(i+4,:), 'LineWidth',1.5,'linestyle',':')
m_text(lonmin+2,latmax-2,'Ship Drift','color',linecolours(i+4,:))
m_text(lonmin+2,latmax-2,'Ship Distance','color',linecolours(i+3,:))

orient tall
filename=[['HUD', a.cruise_id], 'curentshipdrift_alignedTran'];
 print('-djpeg', '-r300', filename);
 hgsave(filename);