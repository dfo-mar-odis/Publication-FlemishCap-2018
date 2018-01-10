function [u,v] = polar2uv(dir,spd)
%  polar2uv:  (east, north) currents of direction and speed
%
% Source:  Fran Hotchkiss, February 20, 1998.
%
% Description: None
%
% Syntax: See below
%
% Documentation Date: Oct.16,2006 10:40:02
%
% Tags:
% {ADCPTOOLKIT} {TAG}
%
% [u,v] = polar2uv(dir,spd)
%
%       u is eastward current component
%       v is northward current component
%       dir is compass direction of current
%             (90 for eastward flow, 270 for westward)
%       spd is magnitude of current
%
%       Fran Hotchkiss, February 20, 1998.
dir = dir .* pi ./180.;
[v,u] = pol2cart(dir,spd);
