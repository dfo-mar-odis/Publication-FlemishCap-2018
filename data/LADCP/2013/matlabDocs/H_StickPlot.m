function H_StickPlot(a,bins, binsize, stnnumber) 
%===================================================
% Script Name: H_StickPlot.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot stick current figure
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				Input currents were original and NOT aligned along the
%				bathymetry or transect
%				
% subroutines:  sticks.m, mtit.m

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

% array of station locations
stationlocation= a.stnlocation; 

%----------------------------------------------------------------------
% STEP 2: Set up Data arrays 
%----------------------------------------------------------------------


uvdepthbins=[0 0 0 0 0];%[u,  v,  sp,  dir,  depth of bin]
for n=1:length(stnnumber) 
    depthbottom(n,:)=HUDLADCP(n,1).depthbottom*-1;
    hh=[HUDLADCP(n,1).binu;  HUDLADCP(n,1).binv; HUDLADCP(n,1).binspd;  HUDLADCP(n,1).bindrc; HUDLADCP(n,1).depthbins];
    uvdepthbins =[uvdepthbins; hh'];
end
    uvdepthbins(1,:) =[];
    
    xlims = [ -48 max(max(stationlocation(:,2)))]; %min(min(stationlocation(:,2)))
    ylims = [min(min(stationlocation(:,1))) max(max(stationlocation(:,1)))]; %ylims = [-1 1]*max(max(ylat)); %ylims = [-1 1]*0.5; %
    ylims = [-1 1]*max(uvdepthbins(:,3));


%----------------------------------------------------------------------
% STEP 2: Plot stick plots
%----------------------------------------------------------------------
figure (2)

for k=1:length(bins)
    subplot(ceil(length(bins)/2),2,k)
    hl2 =line(stationlocation(:,2),depthbottom,'Color','k','Marker','.','linestyle','none');    
    set(gca,'YDir','reverse','xlim',[ -48 max(max(stationlocation(:,2)))]) %min(min(stationlocation(:,2)))
    ax1 = get(gcf,'CurrentAxes');
    set(ax1,'XColor','k','YColor','k')
    ax2 = axes('YLim',ylims,'XLim',xlims,'Position',get(ax1,'Position'), 'XAxisLocation','top',...
               'YAxisLocation','right', 'Color','none',...
               'XColor',[.8 .1 .8],'YColor',[.8 .1 .8]); %'YLim',ylims,'XLim',xlims,
    hold on;
    
    mag=uvdepthbins(find(uvdepthbins(:,5)==bins(k)),3);
    theta=uvdepthbins(find(uvdepthbins(:,5)==bins(k)),4);

    h = sticks(stationlocation(:,2),mag,theta) ;%h = sticks(x,mag,theta,xlims,ylims,magref,u)

text_labels=[num2str(bins(k)),' \pm',num2str(binsize(k)),  ' m'];%['HUD', a.cruise_id],' ',

 tt=title(text_labels);
 ttpos=get(tt,'Position'); set(tt, 'position',[ttpos(1) ttpos(2)/3 ttpos(3)])

end
    ttt = [['HUD', a.cruise_id],' ','Current Stick plots'];
    P = mtit(ttt,'fontsize',10); %place a title at top centered over subplots
   set(P.ah, 'position',[P.pos(1) P.pos(2)*1.2 P.pos(3) P.pos(4)])

orient tall
filename=[['HUD', a.cruise_id], 'curent_stickplot'];
 print('-djpeg', '-r300', filename);
 hgsave(filename);