function D_UVprofiles_check(a) 
%===================================================
% Script Name: D_UVprofiles_check.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to plot of U and V profiles for Comparison of current 
%               velocity aligned using different methods
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				Input currents were aligned along the bathymetry 
%				for a 1, 5, 10 and 100 m radius around each station and
%				aligned along the transect line using 3 points and polynomial curve fitting method

% subroutines:   mtit             

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
% STEP 2: plot U rotated and original
%----------------------------------------------------------------------
% 
  %determine how many Subplot of 10 plots each is required
plotnum=length(HUDLADCP);  nrc=plotnum/10;
numsubplots=ceil(nrc);

if numsubplots==1
figure (1)
    for h=1:plotnum
        subplot(plotnum,2,h)
        plot(HUDLADCP(h,1).ualign, HUDLADCP(h,1).z,'k')
        hold on;
        plot(HUDLADCP(h,1).ualign1km, HUDLADCP(h,1).z,'y')
        plot(HUDLADCP(h,1).ualign5km, HUDLADCP(h,1).z,'b')
        plot(HUDLADCP(h,1).ualign10km, HUDLADCP(h,1).z,'g')
        plot(HUDLADCP(h,1).ualign100km, HUDLADCP(h,1).z,'c')
        plot(HUDLADCP(h,1).u, HUDLADCP(h,1).z,'m')
        set(gca,'YDir','reverse')
        title([['HUD', a.cruise_id], ' U, cast',num2str(HUDLADCP(h,1).cast)])
    end
    ttt = 'U aligned with Bathy using different radius and transect using 3 points with poly fit'; %title for plot
    P = mtit(ttt,'fontsize',10); %place a title at top centered over subplots
    set(P.ah, 'position',[P.pos(1) P.pos(2)*1.08 P.pos(3) P.pos(4)]) % change position of title
    hp=legend('3 point method','1 km radius', '5 km radius','10 km radius','100 km radius', 'original', 'fontsize',6 );
    hp=legend('orientation','horizontal','Location','SouthOutside') ; 
    hpos=get(hp,'position'); 
    set(hp,'fontsize',8,'position',[-.15 hpos(2) hpos(3) hpos(4)]);% change position of legend
    orient tall
    filename=[['HUD', a.cruise_id], 'comparison_U']; % save figures
    print('-djpeg', '-r300', filename);
    hgsave(filename);

else
    m=0;
    for g=1:numsubplots-1;  
        figure(g)
        m=m+1; sb=1;
        for h=m:m+9;
            subplot(5,2,sb)
            plot(HUDLADCP(h,1).ualign, HUDLADCP(h,1).z,'k')
            hold on;
            plot(HUDLADCP(h,1).ualign1km, HUDLADCP(h,1).z,'y')
            plot(HUDLADCP(h,1).ualign5km, HUDLADCP(h,1).z,'b')
            plot(HUDLADCP(h,1).ualign10km, HUDLADCP(h,1).z,'g')
            plot(HUDLADCP(h,1).ualign100km, HUDLADCP(h,1).z,'c')
            plot(HUDLADCP(h,1).u, HUDLADCP(h,1).z,'m')
            set(gca,'YDir','reverse')
            title([['HUD', a.cruise_id], ' U, cast',num2str(HUDLADCP(h,1).cast)])
            sb=sb+1;
        end
        m=h;
    ttt = 'U aligned with Bathy using different radius and transect using 3 points with poly fit';%title for plot
    P = mtit(ttt,'fontsize',10);%place a title at top centered over subplots
    set(P.ah, 'position',[P.pos(1) P.pos(2)*1.08 P.pos(3) P.pos(4)]) % change position of title
    hp=legend('3 point method','1 km radius', '5 km radius','10 km radius','100 km radius', 'original', 'fontsize',6 );
    hp=legend('orientation','horizontal','Location','SouthOutside') ; 
    hpos=get(hp,'position'); 
    set(hp,'fontsize',8,'position',[-.15 hpos(2) hpos(3) hpos(4)]);% change position of legend
    orient tall
    filename=[['HUD', a.cruise_id], ['comparison_U',num2str(g)]]; % save figures
    print('-djpeg', '-r300', filename);
    hgsave(filename);
    end
    figure(numsubplots)
    sb=1;
        for h=m+1:plotnum
            subplot(5,2,sb)
            plot(HUDLADCP(h,1).ualign, HUDLADCP(h,1).z,'k')
            hold on;
    	    plot(HUDLADCP(h,1).ualign1km, HUDLADCP(h,1).z,'y')
            plot(HUDLADCP(h,1).ualign5km, HUDLADCP(h,1).z,'b')
            plot(HUDLADCP(h,1).ualign10km, HUDLADCP(h,1).z,'g')
            plot(HUDLADCP(h,1).ualign100km, HUDLADCP(h,1).z,'c')
            plot(HUDLADCP(h,1).u, HUDLADCP(h,1).z,'m')
            set(gca,'YDir','reverse')
            title([['HUD', a.cruise_id], ' U, cast',num2str(HUDLADCP(h,1).cast)])
            sb=sb+1;
       end
    ttt = 'U aligned with Bathy using different radius and transect using 3 points with poly fit';%title for plot
    P = mtit(ttt,'fontsize',10);%place a title at top centered over subplots
    set(P.ah, 'position',[P.pos(1) P.pos(2)*1.08 P.pos(3) P.pos(4)]) % change position of title
    hp=legend('3 point method','1 km radius', '5 km radius','10 km radius','100 km radius', 'original', 'fontsize',6 );
    hp=legend('orientation','horizontal','Location','SouthOutside') ; 
    hpos=get(hp,'position'); 
    set(hp,'fontsize',8,'position',[-.15 hpos(2) hpos(3) hpos(4)]);% change position of legend
    orient tall
    filename=[['HUD', a.cruise_id], ['comparison_U',num2str(numsubplots)]]; % save figures
    print('-djpeg', '-r300', filename);
    hgsave(filename);
end

%----------------------------------------------------------------------
% STEP 3: plot V rotated and original
%----------------------------------------------------------------------
if numsubplots==1
figure(numsubplots+1) 
    for h=1:plotnum
        subplot(plotnum,2,h)
        plot(HUDLADCP(h,1).valign, HUDLADCP(h,1).z,'k')
        hold on;
        plot(HUDLADCP(h,1).ualign1km, HUDLADCP(h,1).z,'y')
        plot(HUDLADCP(h,1).valign5km, HUDLADCP(h,1).z,'b')
        plot(HUDLADCP(h,1).valign10km, HUDLADCP(h,1).z,'g')
        plot(HUDLADCP(h,1).valign100km, HUDLADCP(h,1).z,'c')
        plot(HUDLADCP(h,1).v, HUDLADCP(h,1).z,'m')
        set(gca,'YDir','reverse')
        title([['HUD', a.cruise_id], ' V, cast',num2str(HUDLADCP(h,1).cast)])
    end
    ttt = 'V aligned with Bathy using different radius and transect using 3 points with poly fit';%title for plot
    P = mtit(ttt,'fontsize',10);%place a title at top centered over subplots
    set(P.ah, 'position',[P.pos(1) P.pos(2)*1.08 P.pos(3) P.pos(4)])% change position of title
    hp=legend('3 point method','1 km radius', '5 km radius','10 km radius','100 km radius', 'original', 'fontsize',6 );
    hp=legend('orientation','horizontal','Location','SouthOutside') ; 
    hpos=get(hp,'position'); 
    set(hp,'fontsize',8,'position',[-.15 hpos(2) hpos(3) hpos(4)]);% change position of legend
    orient tall
    filename=[['HUD', a.cruise_id], 'comparison_V']; % save figures
    print('-djpeg', '-r300', filename);
    hgsave(filename);
else
    m=0;
    for g=1:numsubplots-1;  
        figure(numsubplots+g)
        m=m+1; sb=1;
        for h=m:m+9
            subplot(5,2,sb)
            plot(HUDLADCP(h,1).valign, HUDLADCP(h,1).z,'k')
            hold on;
            plot(HUDLADCP(h,1).ualign1km, HUDLADCP(h,1).z,'y')
            plot(HUDLADCP(h,1).valign5km, HUDLADCP(h,1).z,'b')
            plot(HUDLADCP(h,1).valign10km, HUDLADCP(h,1).z,'g')
            plot(HUDLADCP(h,1).valign100km, HUDLADCP(h,1).z,'c')
            plot(HUDLADCP(h,1).v, HUDLADCP(h,1).z,'m')
            set(gca,'YDir','reverse')
            title([['HUD', a.cruise_id], ' V, cast',num2str(HUDLADCP(h,1).cast)])
            sb=sb+1;
        end
        m=h;
    ttt = 'V aligned with Bathy using different radius and transect using 3 points with poly fit';%title for plot
    P = mtit(ttt,'fontsize',10);%place a title at top centered over subplots
    set(P.ah, 'position',[P.pos(1) P.pos(2)*1.08 P.pos(3) P.pos(4)])% change position of title
    hp=legend('3 point method','1 km radius', '5 km radius','10 km radius','100 km radius', 'original', 'fontsize',6 );
    hp=legend('orientation','horizontal','Location','SouthOutside') ; 
    hpos=get(hp,'position'); 
    set(hp,'fontsize',8,'position',[-.15 hpos(2) hpos(3) hpos(4)]);% change position of legend
    orient tall
    filename=[['HUD', a.cruise_id], ['comparison_V',num2str(g)]]; % save figures
    print('-djpeg', '-r300', filename);
    hgsave(filename);

    end
    figure(numsubplots*2)
     sb=1;
       for h=m+1:plotnum
            subplot(5,2,sb)
            plot(HUDLADCP(h,1).valign, HUDLADCP(h,1).z,'k')
            hold on;
            plot(HUDLADCP(h,1).ualign1km, HUDLADCP(h,1).z,'y')
            plot(HUDLADCP(h,1).valign5km, HUDLADCP(h,1).z,'b')
            plot(HUDLADCP(h,1).valign10km, HUDLADCP(h,1).z,'g')
            plot(HUDLADCP(h,1).valign100km, HUDLADCP(h,1).z,'c')
            plot(HUDLADCP(h,1).v, HUDLADCP(h,1).z,'m')
            set(gca,'YDir','reverse')
            title([['HUD', a.cruise_id], ' V, cast',num2str(HUDLADCP(h,1).cast)])
            sb=sb+1;
        end
    ttt = 'V aligned with Bathy using different radius and transect using 3 points with poly fit';%title for plot
    P = mtit(ttt,'fontsize',10);%place a title at top centered over subplots
    set(P.ah, 'position',[P.pos(1) P.pos(2)*1.08 P.pos(3) P.pos(4)])% change position of title
    hp=legend('3 point method','1 km radius', '5 km radius','10 km radius','100 km radius', 'original', 'fontsize',6 );
    hp=legend('orientation','horizontal','Location','SouthOutside') ; 
    hpos=get(hp,'position'); 
    set(hp,'fontsize',8,'position',[-.15 hpos(2) hpos(3) hpos(4)]);% change position of legend
    orient tall
    filename=[['HUD', a.cruise_id], ['comparison_V',num2str(numsubplots)]]; % save figures
    print('-djpeg', '-r300', filename);
    hgsave(filename);
end