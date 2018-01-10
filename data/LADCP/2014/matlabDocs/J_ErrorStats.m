function J_ErrorStats(a) 
%===================================================
% Script Name: J_ErrorStats.m
%
% Created By:   Diana Cardoso, Bedford Institute of Oceangraphy
% Description:  Function to create a table of uncertainty estimates of LADCP
                % velocity profile
%               Processed by LDEO software version IX.8,
%               maintained by A.M. Thurnherr and downloaded from:
          %http://www.ldeo.columbia.edu/cgi-bin/ladcp-cgi-bin/hgwebdir.cgi
%								
% subroutines:  

% Modification Log:
% August 26, 2013: - Original file created.
%               - Implemented by Diana Cardoso, Bedford Institute of
%                 Oceanography, Fisheries and Oceans Canada.
%===================================================
%----------------------------------------------------------------------
% STEP 1: load data
%----------------------------------------------------------------------
load(['HUD', a.cruise_id]) 

%----------------------------------------------------------------------
% STEP 2: Set up data arrays and calculate stats
%----------------------------------------------------------------------

data=ones(1,3);
stats=ones(1,4);

for i=1:length(HUDLADCP)
    
  uerr=  HUDLADCP(i,1).uerr;% uerr is calculated by the LDEO software
  avg_uerr=mean(uerr);
  min_uerr=min(uerr);
  max_uerr=max(uerr);
  data=[data;  uerr ones(length(uerr),1)*i  ones(length(uerr),1)*HUDLADCP(i,1).cast]; 
  stats=[stats; avg_uerr   min_uerr   max_uerr  HUDLADCP(i,1).cast];
end

data(1,:)=[];
stats(1,:)=[];

%----------------------------------------------------------------------
% STEP 5: Save table in a text file
%----------------------------------------------------------------------


fid = fopen([['HUD', a.cruise_id],'errorstats.txt'], 'wt');
     fprintf(fid, '\n  Uncertainty estimates of LADCP velocity profile \n\n');
     fprintf(fid, '\n Mean\t  Min\t  Max\t  Cast# \n\n');
    for h=1:length(stats)
    fprintf(fid, '%2.3f\t  %2.3f\t  %2.3f\t  %2.0f\t \n', stats(h,:));
    end
   fclose(fid)
   
