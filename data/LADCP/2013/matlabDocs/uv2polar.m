function [dir,spd] = uv2polar(u,v)                         
% {|DOCUMENTED|}                                           
% 03-APR-2006 10:45:49.00                                  
%  uv2polar:  direction and speed of (east, north) currents
%                                                          
%       u is eastward current component                    
%       v is northward current component                   
%       dir is compass direction of current                
%             (90 for eastward flow, 270 for westward)     
%       spd is magnitude of current                        
%                                                          
%       Fran Hotchkiss, February 20, 1998.                 
%Class: Third Party Toolbox: ADCPTOOL                      
[dir,spd] = cart2pol(v,u);                                 
i = find(dir < 0);                                         
dir(i) =dir(i) + 2*pi;                                     
dir = dir .* 180 ./ pi;                                    
