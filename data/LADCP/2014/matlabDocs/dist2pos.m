function [lat_end,long_end] = dist2pos(lag1,lon1,dist,heading)
%Returns a lat and long for a position, when given a distance and heading from another position. 
%
% Source: Dave Kellow, 902-426-2245, kellowd@mar.dfo-mpo.gc.ca
% 
% Description:
% Returns a lat and long for a position, when given a distance and heading
% from another position. 
%
% August 11, 2008
% Error fix by Inna Yashayaeva:
%    In the original line <km_per_deg_lo = (sin((pi/180)*(lag1))*40041.47)/360>
%          <sin> was replaced with <cos>
% 
% Syntax: 
% [lat_end,long_end] = dist2pos(lag1,lon1,dist,heading)
%
% Documentation Date: Oct.01,2007 14:02:26
% 
% Tags:
% {TAG} {TAG} {TAG}
% 
% 


if nargin < 4
    disp('Number of input arguments error! ');
    return;
end
if abs(lag1)>90 ||abs(lon1)>360
    disp('Degree(s) illegal! ');
    return;
end
if lon1 < 0
    lon1 = lon1 + 360;
end

km_per_deg_la = 111.3237;
km_la = dist*cos((pi/180)*heading);
km_lo = dist*sin((pi/180)*heading);

km_per_deg_lo = (cos((pi/180)*(lag1))*40041.47)/360; % changed <sin> to <cos> by Inna Yashayaeva

lat_end = lag1+(km_la/km_per_deg_la);
long_end = lon1+(km_lo/km_per_deg_lo);
