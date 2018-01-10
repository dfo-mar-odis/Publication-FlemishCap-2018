%===================================================

% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Main function to call functions that analyse and plot LADCP data 
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi

% Modification Log:
% May 30, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================

%----------------------------------------------------------------------
% STEP 1: set location of raw and processed LADCP data
%----------------------------------------------------------------------

a.raw= 'U:\LADCP\HUD2014017_Flem\raw'; % dirctory with LADCP data
a.processed='U:\LADCP\HUD2014017_Flem\processed\FlemishCaponly'; % dirctory with processed LADCP data

%----------------------------------------------------------------------
% STEP 2: set cruise number and coordinates that will be used for mapping
% station locations and currents
%----------------------------------------------------------------------

a.cruise_id = '2014017';    
a.latmax=49.7;a.latmin=46;
a.lonmax=-43.5;a.lonmin=-48;

%----------------------------------------------------------------------
% STEP 3: make array of station numbers
%----------------------------------------------------------------------

cd(a.raw) % dirctory with LADCP data
ctddir=dir;
cd ../
cd(a.processed)
for k=3:length(ctddir)
    stnnames(k-2)=str2num(ctddir(k,1).name(1:3));
end
stnnumber=unique(stnnames); % station numbers
stnnumber(1:10)=[]; %remove cast 2 Bedford Basin test
stnnumber(23)=[]; %remove cast 2 Bedford Basin test

%----------------------------------------------------------------------
% Function to extract and save LADCP data in a mat structure 
%----------------------------------------------------------------------
A_extractLADCPprocesseddata(a,stnnumber) ;

%----------------------------------------------------------------------
% Function to plot a map of station locations with LADCP data
%----------------------------------------------------------------------
a=B_MAPcastlocation(a);
close all

%----------------------------------------------------------------------
% Function to align currents of  LADCP data along the transect line using 
% 3 points and polynomial curve fitting method
%----------------------------------------------------------------------
% Run this for each transect
% input each transect station numbers and transect name 
% do not repeat the same station in more than 1 transect
% if there are certain stations that do not fall within a transect this
% function must still be run, just create another vector of these values.
stnnumtransect1=stnnumber(1:7); 
transect1=[stnnumtransect1]; name1='transect1';
stnnumtransect2=stnnumber(8:21);
transect2=[stnnumtransect2]; name2='transect2';
stnnumtransect3=stnnumber([22]);   %([20:22 19 23 27 24:26  28:31]);
transect3=[stnnumtransect3]; name3='transect3';
% nontransect=stnnumber([7 32 33]);
% transect4=[nontransect]; name4='transect4';
% nontransect2=stnnumber([33]);
% transect5=[nontransect2]; name5='transect5';
% nontransect3=stnnumber([7]);
% transect6=[nontransect3]; name6='transect6';

% stationnumbers=[033 035 036 037 034 038 042 039 040 041 044 045 046 047];

C_aligncurrents_withtransect3point(a,transect3);

%----------------------------------------------------------------------
% Function to align currents of  LADCP data along the bathymetry for 
% a 1, 5, 10 and 100 km radius around each station
%----------------------------------------------------------------------
C_aligncurrents_withbath2(a);

%----------------------------------------------------------------------
% Function to plot of U and V profiles for Comparison of current 
% velocity aligned using different methods
%----------------------------------------------------------------------
D_UVprofiles_check(a);
close all

% look at teh plot and choose which radius to use:
radius=100;

%----------------------------------------------------------------------
% Function to plot headings for Comparison of current velocity aligned 
% with bathy using the given radius size around original point
%----------------------------------------------------------------------
E_Plotheadings_check(a,radius);
close all

%----------------------------------------------------------------------
% Function to plot current headings on a map for currents aligned along 
% the bathymetry and transect
%----------------------------------------------------------------------
F_MAPheading(a,stnnumber,radius)
close all

%----------------------------------------------------------------------
% Function to bin currents data
%----------------------------------------------------------------------
% enter bins depths and bin size
bins=   [0 50 100 200 300 400 500 750 1000 1500 2000 2500 3000]; %bin depths
binsize=[10 10 25 25  25  25  25  75  100  100  100  100  100 ]; %size of each bin must be same length as bin vector

% G_Creatbins(a,bins,binsize,radius)
G_Creatbins(a,bins,binsize,radius)%% can not run unless all stations were run C_aligncurrents_withbath3point_new2 you need HUDLADCP to have values for the ualign and valign

%----------------------------------------------------------------------
% Function to plot current on a map for currents aligned along 
% the bathymetry and transect and not aligned
%----------------------------------------------------------------------
H_map_currents(a, bins,binsize, stnnumber,radius)
close all

%----------------------------------------------------------------------
% Function to plot stick current figure forn original non aligned currents
%----------------------------------------------------------------------
H_StickPlot(a,bins,binsize, stnnumber)
close all

%----------------------------------------------------------------------
% Function to plot contour plots of currents aligned along 
% the bathymetry and transect and not aligned
%----------------------------------------------------------------------
% Run this for each transect 

I_SectionPlot_rotated(a,transect3,name3) ;
close all

I_SectionPlot_notrotated_nomap(a,transect1,name1) ;
close all

I_SectionPlot_aligned(a,transect1,name1,radius) ;
close all

I_SectionPlot_notrotated(a,transect2,name2) ;

%----------------------------------------------------------------------
% Function to create a table of uncertainty estimates of LADCP velocity profile
%----------------------------------------------------------------------
J_ErrorStats(a)

%----------------------------------------------------------------------
% Function to plot currents and ship drift 
%----------------------------------------------------------------------
K_map_shipdrift(a,bins,binsize)
close all

