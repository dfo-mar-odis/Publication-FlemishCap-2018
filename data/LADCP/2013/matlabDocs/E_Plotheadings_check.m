function E_Plotheadings_check(a,radius) 
%===================================================
% Script Name: E_Plotheadings_check.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot headings for Comparison of current 
%               velocity aligned with bathy using the given radius size around original point
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				
% subroutines:   mtit.m         

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
% STEP 2: plot heading
%----------------------------------------------------------------------

%determine how many Subplot and figues, 10 subplots per figure
plotnum=length(HUDLADCP);  nrc=plotnum/10;
numsubplots=ceil(nrc);
fldname=['pointminmax',num2str(radius),'km']; 
% HUDLADCP(h,1).pointminmax=[lat_end(min depth)  lat_start(min depth) lon_end(min depth)  lon_start(min depth) depthtopo index heading  lat_end(max depth)  lat_start(max depth) lon_end(max depth)  lon_start(max depth)]

if numsubplots==1  %for one subplot figure 
figure (1)

    for h=1:plotnum 
        subplot(plotnum,2,h)
        plot(HUDLADCP(h,1).lon, HUDLADCP(h,1).lat,'ko')
        hold on;
        plot([HUDLADCP(h,1).lon HUDLADCP(h,1).(fldname)(1,3)], [HUDLADCP(h,1).lat HUDLADCP(h,1).(fldname)(1,1)],'r-')%slope from station to min depth
        plot([HUDLADCP(h,1).lon HUDLADCP(h,1).(fldname)(1,10)], [HUDLADCP(h,1).lat HUDLADCP(h,1).(fldname)(1,8)],'c-')%slope from station to max depth
        plot([HUDLADCP(h,1).(fldname)(1,3)  HUDLADCP(h,1).(fldname)(1,10)], [HUDLADCP(h,1).(fldname)(1,1)  HUDLADCP(h,1).(fldname)(1,8)],'k')%heading for slope of bathy
        plot(HUDLADCP(h,1).(fldname)(1,4),  HUDLADCP(h,1).(fldname)(1,2),'yo')
        plot(HUDLADCP(h,1).(fldname)(1,11),  HUDLADCP(h,1).(fldname)(1,9),'yo')
        text (HUDLADCP(h,1).(fldname)(1,3),HUDLADCP(h,1).(fldname)(1,1),['depthmin ',num2str(round(HUDLADCP(h,1).(fldname)(1,5)))], 'color','r')
        text (HUDLADCP(h,1).(fldname)(1,10),HUDLADCP(h,1).(fldname)(1,8)-0.05,['depthmax ',num2str(round(HUDLADCP(h,1).(fldname)(1,12)))], 'color','c')
        text (HUDLADCP(h,1).lon,HUDLADCP(h,1).(fldname)(1,8),['heading ',num2str(round(HUDLADCP(h,1).(fldname)(1,7)))], 'color','k')
        title([['HUD', a.cruise_id], ' cast',num2str(HUDLADCP(h,1).cast)])
    end
    ttt = ['Heading of current aligned with Bathemetry for ', num2str(radius),' km radius'];
    P = mtit(ttt,'fontsize',10); %place a title at top centered over subplots
   set(P.ah, 'position',[P.pos(1) P.pos(2)*1.05 P.pos(3) P.pos(4)])% change position of title
    orient tall
    print('-djpeg', '-r300',[['HUD', a.cruise_id], 'Heading']);% save figures
    hgsave([['HUD', a.cruise_id], 'Heading'])
else %for multiple subplot figures
    m=0;
    for g=1:numsubplots-1;  
        figure(g)
        m=m+1;sb=1;
        for h=m:m+9
            subplot(5,2,sb)
         plot(HUDLADCP(h,1).lon, HUDLADCP(h,1).lat,'ko') %station location
        hold on;
        plot([HUDLADCP(h,1).lon HUDLADCP(h,1).(fldname)(1,3)], [HUDLADCP(h,1).lat HUDLADCP(h,1).(fldname)(1,1)],'r-') %slope from station to min depth
        plot([HUDLADCP(h,1).lon HUDLADCP(h,1).(fldname)(1,10)], [HUDLADCP(h,1).lat HUDLADCP(h,1).(fldname)(1,8)],'c-')%slope from station to max depth
        plot([HUDLADCP(h,1).(fldname)(1,3)  HUDLADCP(h,1).(fldname)(1,10)], [HUDLADCP(h,1).(fldname)(1,1)  HUDLADCP(h,1).(fldname)(1,8)],'k') %heading for slope of bathy
        plot(HUDLADCP(h,1).(fldname)(1,4),  HUDLADCP(h,1).(fldname)(1,2),'yo')
        plot(HUDLADCP(h,1).(fldname)(1,11),  HUDLADCP(h,1).(fldname)(1,9),'yo')
        text (HUDLADCP(h,1).(fldname)(1,3),HUDLADCP(h,1).(fldname)(1,1),['depthmin ',num2str(round(HUDLADCP(h,1).(fldname)(1,5)))], 'color','r')
        text (HUDLADCP(h,1).(fldname)(1,10),HUDLADCP(h,1).(fldname)(1,8)-0.05,['depthmax ',num2str(round(HUDLADCP(h,1).(fldname)(1,12)))], 'color','c')
        text (HUDLADCP(h,1).lon,HUDLADCP(h,1).(fldname)(1,8),['heading ',num2str(round(HUDLADCP(h,1).(fldname)(1,7)))], 'color','k')
        title([['HUD', a.cruise_id], ' cast',num2str(HUDLADCP(h,1).cast)])
            sb=sb+1;
        end
        m=h;
    ttt = ['Heading of current aligned with Bathemetry for ', num2str(radius),' km radius'];
    P = mtit(ttt,'fontsize',10); %place a title at top centered over subplots
   set(P.ah, 'position',[P.pos(1) P.pos(2)*1.05 P.pos(3) P.pos(4)])% change position of title
    orient tall
    print('-djpeg', '-r300', [['HUD', a.cruise_id],'Heading',num2str(g)]);% save figures
    hgsave( [['HUD', a.cruise_id],'Heading',num2str(g)])
    end
    
    figure(numsubplots) %last subplot
        sb=1;
        for h=m+1:plotnum
             subplot(5,2,sb)
         plot(HUDLADCP(h,1).lon, HUDLADCP(h,1).lat,'ko')
        hold on;
        plot([HUDLADCP(h,1).lon HUDLADCP(h,1).(fldname)(1,3)], [HUDLADCP(h,1).lat HUDLADCP(h,1).(fldname)(1,1)],'r-')
        plot([HUDLADCP(h,1).lon HUDLADCP(h,1).(fldname)(1,10)], [HUDLADCP(h,1).lat HUDLADCP(h,1).(fldname)(1,8)],'c-')
        plot([HUDLADCP(h,1).(fldname)(1,3)  HUDLADCP(h,1).(fldname)(1,10)], [HUDLADCP(h,1).(fldname)(1,1)  HUDLADCP(h,1).(fldname)(1,8)],'k')
        plot(HUDLADCP(h,1).(fldname)(1,4),  HUDLADCP(h,1).(fldname)(1,2),'yo')
        plot(HUDLADCP(h,1).(fldname)(1,11),  HUDLADCP(h,1).(fldname)(1,9),'yo')
        text (HUDLADCP(h,1).(fldname)(1,3),HUDLADCP(h,1).(fldname)(1,1),['depthmin ',num2str(round(HUDLADCP(h,1).(fldname)(1,5)))], 'color','r')
        text (HUDLADCP(h,1).(fldname)(1,10),HUDLADCP(h,1).(fldname)(1,8)-0.05,['depthmax ',num2str(round(HUDLADCP(h,1).(fldname)(1,12)))], 'color','c')
        text (HUDLADCP(h,1).lon,HUDLADCP(h,1).(fldname)(1,8),['heading ',num2str(round(HUDLADCP(h,1).(fldname)(1,7)))], 'color','k')
        title([['HUD', a.cruise_id], ' cast',num2str(HUDLADCP(h,1).cast)])
            sb=sb+1;
        end
    ttt = ['Heading of current aligned with Bathemetry for ', num2str(radius),' km radius'];
    P = mtit(ttt,'fontsize',10); %place a title at top centered over subplots
       set(P.ah, 'position',[P.pos(1) P.pos(2)*1.05 P.pos(3) P.pos(4)])% change position of title
    orient tall
    print('-djpeg', '-r300', [['HUD', a.cruise_id],'Heading',num2str(numsubplots)]);% save figures
    hgsave([['HUD', a.cruise_id],'Heading',num2str(numsubplots)])
end
           
