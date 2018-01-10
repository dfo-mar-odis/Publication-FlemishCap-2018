function G_Creatbins(a,bins,binsize,radius) 
%===================================================
% Script Name: G_Creatbins2.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to bin currents data
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%				Input currents were aligned along the bathymetry and
%				transect
%				
% subroutines:     meannan  
%                  uv2polar

% Modification Log:
% August 26, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================
%----------------------------------------------------------------------
% STEP 1: load Data
%----------------------------------------------------------------------

load(['HUD', a.cruise_id]) 
load elevations 
load lat 
load lon
fldnameu=['ualign',num2str(radius),'km'];
fldnamev=['valign',num2str(radius),'km'];

%----------------------------------------------------------------------
% STEP 2: Bin Data
%----------------------------------------------------------------------

for i=1:length(HUDLADCP)
    
  latcast=  HUDLADCP(i,1).lat;
  loncast=  HUDLADCP(i,1).lon;
%   y=find(abs(lon(:)-loncast) <=min(abs(lon(:)-loncast)));
%   x=find(abs(lat(:)-latcast) <=min(abs(lat(:)-latcast)));
  depth(i)=HUDLADCP(i,1).z(end)+10; % depth of bottom determined from Bathy data in B
  z=  HUDLADCP(i,1).z;

  for k=1:length(bins)
  binz= find(z >= bins(k)-binsize(k) & z <= bins(k)+binsize(k));  
      if isempty(binz)
          HUDLADCP(i,1).binu(k)= NaN;
          HUDLADCP(i,1).binv(k)= NaN;
          HUDLADCP(i,1).bindrc(k) = NaN;
          HUDLADCP(i,1).binspd(k) = NaN;
          HUDLADCP(i,1).binubathy(k)= NaN;
          HUDLADCP(i,1).binvbathy(k)= NaN;
          HUDLADCP(i,1).bindrcbathy(k) = NaN;
          HUDLADCP(i,1).binspdbathy(k) = NaN;
          HUDLADCP(i,1).binutran(k)= NaN;
          HUDLADCP(i,1).binvtran(k)= NaN;
          HUDLADCP(i,1).bindrctran(k) = NaN;
          HUDLADCP(i,1).binspdtran(k) = NaN;
          HUDLADCP(i,1).depthbins(k) = bins(k);
      else
           binu(k)=  meannan(HUDLADCP(i,1).u(binz));
           binv(k)=  meannan(HUDLADCP(i,1).v(binz));
           [dir,spd] = uv2polar(binu(k),binv(k));
           binubathy(k)=  meannan(HUDLADCP(i,1).(fldnameu)(binz));
           binvbathy(k)=  meannan(HUDLADCP(i,1).(fldnamev)(binz));
           [dirbathy,spdbathy] = uv2polar(binubathy(k),binvbathy(k));
           binutran(k)=  meannan(HUDLADCP(i,1).ualign(binz));
           binvtran(k)=  meannan(HUDLADCP(i,1).valign(binz));
           [dirtran,spdtran] = uv2polar(binutran(k),binvtran(k));
          
          HUDLADCP(i,1).binu(k)= binu(k);
          HUDLADCP(i,1).binv(k)= binv(k);
          HUDLADCP(i,1).bindrc(k) = dir;
          HUDLADCP(i,1).binspd(k) = spd;
          HUDLADCP(i,1).binubathy(k)= binubathy(k);
          HUDLADCP(i,1).binvbathy(k)= binvbathy(k);
          HUDLADCP(i,1).bindrcbathy(k) = dirbathy;
          HUDLADCP(i,1).binspdbathy(k) = spdbathy;
          HUDLADCP(i,1).binutran(k)= binutran(k);
          HUDLADCP(i,1).binvtran(k)= binvtran(k);
          HUDLADCP(i,1).bindrctran(k) = dirtran;
          HUDLADCP(i,1).binspdtran(k) = spdtran;
          HUDLADCP(i,1).depthbins(k) = bins(k);   
        drc(k) = atan2(binu(k),binv(k))*180/pi + 360*(sign(binu(k))<0);
        spd(k) = sqrt(binu(k).^2 + binv(k).^2);
      end
      HUDLADCP(i,1).depthbottom = depth(i);
      HUDLADCP(i,1).depthbottomcast = max(HUDLADCP(i,1).z);        
      
  end
clear binu  binv binz
end

%----------------------------------------------------------------------
% STEP 3: Save Structure 
%----------------------------------------------------------------------

nnn='HUDLADCP';
save(['HUD', a.cruise_id],nnn)
