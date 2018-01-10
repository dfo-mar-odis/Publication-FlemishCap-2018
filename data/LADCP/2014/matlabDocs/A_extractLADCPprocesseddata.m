function A_extractLADCPprocesseddata(a,stnnumber) 

%===================================================
% Script Name: A_extractLADCPprocesseddata.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to extract and save in a mat structure LADCP data 
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
% subroutines: 
%               uv2polar.m
%               julian.m

% Modification Log:
% May 30, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================

%----------------------------------------------------------------------
% STEP 1: Get data for each station and insert in Structure 
%----------------------------------------------------------------------
r=1;
cd(a.processed)  % directory with processed LADCP data
for n=1:length(stnnumber)
    stn= stnnumber(n);
    data = sprintf('%03d/%03d.mat',stn,stn); %a.processed '/

    data=load(data);  %load data from each file folder created by LADCP processing software
    lat=data.dr.lat;
    lon=data.dr.lon;

    if isfield(data.dr, 'ubot')
        ubot=data.dr.ubot;
        vbot=data.dr.vbot;
        zbot=data.dr.zbot;
        [dirbot spbot]=uv2polar(ubot, vbot);

    else 
        ubot=NaN;
        vbot=NaN;
        zbot=NaN;
        dirbot = NaN;
        spbot=NaN;
    end
    u=data.dr.u;
    v=data.dr.v;
    z=data.dr.z;
    [dir sp]=uv2polar(u, v);
    u_do=data.dr.u_do;
    v_do=data.dr.v_do;
    [dir_do sp_do]=uv2polar(u_do, v_do);
    u_up=data.dr.u_up;
    v_up=data.dr.v_up;
    [dir_up sp_up]=uv2polar(u_up, v_up);
    ualign=0;
    valign=0;
    uerr=data.dr.uerr;
    tim=data.dr.tim;
    if isfield(data.dr, 'shiplon')
        shiplon=data.dr.shiplon;
        shiplat=data.dr.shiplat;
        xship=data.dr.xship;
        yship=data.dr.yship;
        uship=data.dr.uship;
        vship=data.dr.vship;

    else
        shiplon=NaN;
        shiplat=NaN;
        xship=NaN;
        yship=NaN;
        uship=NaN;
        vship=NaN;
    end
    daysdif  = julian([1900,1,1,0,0,0])-datenum(1900,1,1,0,0,0); 
    tim=tim-daysdif;

      HUDLADCP(r,1)=struct('cast',stn,'lat', lat, 'lon', lon, 'ubot', ubot,'vbot', vbot,...
    'zbot', zbot,'dirbot', dirbot,'spbot', spbot,'u', u,'v', v,'z', z,'dir', ...
    dir,'sp', sp,'u_do', u_do,'v_do', v_do,'dir_do', dir_do, 'sp_do', sp_do,...
    'u_up', u_up,'v_up', v_up, 'dir_up', dir_up, 'sp_up', sp_up, ...
    'uerr',uerr, 'shiplon', shiplon,'shiplat',shiplat,'xship',xship,...
    'yship',yship,'uship',uship,'vship',vship,'tim', tim,'ualign',ualign, 'valign',valign);

    r=r+1;
    clear data 
end

%----------------------------------------------------------------------
% STEP 2: Save Structure 
%----------------------------------------------------------------------

nnn=['HUD', a.cruise_id];
save(nnn,'HUDLADCP')  %save structure
