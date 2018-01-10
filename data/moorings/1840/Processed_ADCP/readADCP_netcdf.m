%----------------------------------------------------------------------
% STEP 1: read netcdf
%----------------------------------------------------------------------

dircdf=dir('*.nc');

for i=1:size(dircdf,1)
    file=strfind(dircdf(i,:).name,'interpolated');
    if file>0
       filename=dircdf(i,:).name;
    end
end


finfo=ncinfo(filename); %Return information about NetCDF data source
var1name=finfo.Variables(1,1).Name ;
var1= ncread(filename,finfo.Variables(1,1).Name); %Read data from variable in NetCDF data source
var3name=finfo.Variables(1,3).Name ;
var3= ncread(filename,finfo.Variables(1,3).Name); %Read data from variable in NetCDF data source
var4name=finfo.Variables(1,4).Name ;
var4= ncread(filename,finfo.Variables(1,4).Name); %Read data from variable in NetCDF data source
var5name=finfo.Variables(1,5).Name ;
var5= ncread(filename,finfo.Variables(1,5).Name); %Read data from variable in NetCDF data source
var6name=finfo.Variables(1,6).Name ;
var6= ncread(filename,finfo.Variables(1,6).Name); %Read data from variable in NetCDF data source
var7name=finfo.Variables(1,7).Name ;
var7= ncread(filename,finfo.Variables(1,7).Name); %Read data from variable in NetCDF data source

svar6=size(var6); %Returns the size of each array dimension.
dimvar6=ndims(var6); % Returns the number of dimensions in the array.
u=squeeze(var6); %Provides information on the format and storage of the array.
v=squeeze(var7); %Provides information on the format and storage of the array.
time=julian2datenum(var1);
depth1=var3;
%----------------------------------------------------------------------
% STEP 3: Create plot
%----------------------------------------------------------------------
figure
zmin=-50; zmax=50; %limits of current velocity for colour scale cm/s %dz=0.04;
   ttlEW = ['HUD2013021', ', E W Current Speed, NOT Aligned']; %titles
   ttlNS = ['HUD2013021', ', N S Current Speed, NOT Aligned']; 

   clrlim = [zmin,zmax]; % limits for colour bar
   pcolor(time,depth1(1:22),u(1:22,:))
   hold on
    set(gca,'ydir','reverse')
    set(gca,'ylim',[0 200])
     shading interp
    caxis(clrlim(1,:))
    colormap(jet(256))
    colorbar
    ylabel('Depth (m)')
    xlabel('Time (km)')
    
  %[C,h] = contour(time,depth1(1:22),u(1:22,:),[10 20 30 40 50 ],'Color',[0.7,0.7,0.7]); clabel(C,h,'FontSize',7,'fontweight','bold','Rotation',0,'LabelSpacing',500);
%----------------------------------------------------------------------
% STEP 5: Save figures
%----------------------------------------------------------------------
%  print('-djpeg', '-r300', filename);
%  print('-dpng', '-r300', filename);
%  hgsave(filename);
%  
%  clear u v z data2 latloncasti xi yi prmi transect_index bdat dist depth par x0 
%  close
