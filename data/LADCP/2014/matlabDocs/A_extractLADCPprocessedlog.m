% a.raw= 'C:\Hud2014017_diana\LADCP\HUD2014017_Flem\raw'; % dirctory with LADCP data
% a.processed='C:\Hud2014017_diana\LADCP\HUD2014017_Flem\processed'; % dirctory with processed LADCP data
% 
% a.cruise_id = '2014017';    
% a.latmax=54;a.latmin=48;
% a.lonmax=-50;a.lonmin=-44; 
% 
% cd(a.raw) % dirctory with LADCP data
% ctddir=dir;
% cd ../
% cd(a.processed)
% for k=3:length(ctddir)
%     stnnames(k-2)=str2num(ctddir(k,1).name(1:3));
% end
% stnnumber=unique(stnnames); % station numbers

r=1;HUDLADCPlog=zeros(1,5);
for n=1:length(stnnumber)
    stn= stnnumber(n);
    data = sprintf(['%03d/%03d.log'],stn,stn);
    log=textread(data,'%s','delimiter', '\n');
    nofixpingrate = strmatch('warning instruments have no fixed ping rate ', log);
    maxvel = strmatch('** WARNING  check maximum velocity setting on CMD-file', log);
    sp25index=[];sp30index=[];
    for lo=1:length(log)
         sp25 = strfind(log{lo,1},'because of horizontal speed > 2.5 m/s');
         sp30 = strfind(log{lo,1},'horizontal velocities > 2.5 m/s'); 
         if isempty(sp25)~=1
             sp25index=lo;
         end
         if isempty(sp30)~=1
             sp30index=lo;
         end        
    end    
    if isempty(nofixpingrate)~=1
        x=strfind(log{nofixpingrate(1)+1,1},':');
        x1=log{nofixpingrate(1)+1,1}(x(1)+1:x(1)+6);
        x2=log{nofixpingrate(1)+1,1}(x(2)+1:x(2)+6);
        HUDLADCPlog(r,1)=str2num(x1);    HUDLADCPlog(r,2)=str2num(x2);
        %     HUDLADCPlog(r,1)=struct('cast',stn,'nofixpingrateDOWN', x1, 'nofixpingrateUP', x2,'maxvel3', NaN, 'maxvel2_5', NaN);
    else
        HUDLADCPlog(r,1)=0;HUDLADCPlog(r,2)=0;
    %     HUDLADCPlog(r,1)=struct('cast',stn,'nofixpingrateDOWN', NaN, 'nofixpingrateUP', NaN,'maxvel3', NaN, 'maxvel2_5', NaN);
    end

    if isempty(sp25index)~=1
          x2=log{sp25index,1}(9:11);
          x3=log{sp25index,1}(21:25);
%         x=strfind(log{maxvel(1)-1,1},'found');
%         x1=log{maxvel(1)-1,1}(x(1)+7:x(1)+9);
%         x2=log{maxvel(1)-2,1}(9:11);
%         HUDLADCPlog(r,3)=str2num(x1);
          HUDLADCPlog(r,5)=str2num(x2);
          HUDLADCPlog(r,6)=str2num(x3);
    %     HUDLADCPlog(r,1).maxvel3= x1;
    %     HUDLADCPlog(r,1).maxvel2_5=x2;
    else
%         HUDLADCPlog(r,3)=0;
        HUDLADCPlog(r,6)=0;HUDLADCPlog(r,5)=0;
    %     HUDLADCPlog(r,1).maxvel3= NaN;
    %     HUDLADCPlog(r,1).maxvel2_5=NaN;
    end
    
    if isempty(sp30index)~=1
          x1=log{sp30index,1}(11:13);
         HUDLADCPlog(r,3)=str2num(x1);
        x4=log{sp30index,1}(16:20);
         HUDLADCPlog(r,4)=str2num(x4);
    else
         HUDLADCPlog(r,3)=0;
         HUDLADCPlog(r,4)=0;
    end
    
    
HUDLADCPlog(r,7)=stn;
r=r+1;
end

%HUDLADCPlog:
% Column 1:  down dt for common ping number
% Column 2:  up dt for common ping number
% Column 3:  number of values found  with horizontal velocities > 2.5 m/s
% Column 4:  percent found  with horizontal velocities > 2.5 m/s
% Column 5:  number of values removed  with horizontal velocities > 2.5m/s
% Column 6:  percent removed  with horizontal velocities > 2.5m/s
% Column 7:  cast number

break
plot(HUDLADCPlog(:,5), HUDLADCPlog(:,4))
hold on
plot(HUDLADCPlog(:,5), HUDLADCPlog(:,3),'r')

figure (2)
plot(HUDLADCPlog(:,5),HUDLADCPlog(:,2)- HUDLADCPlog(:,1),'ko')
xlabel('Cast Number')
ylabel('Difference up and down dt for common ping number')

figure (3)
plot(HUDLADCPlog(:,7), HUDLADCPlog(:,3),'o-b','linewidth',1.5)
hold on
plot(HUDLADCPlog(:,7), mean(HUDLADCPlog(:,3))*ones(length(HUDLADCPlog(:,3)),1),'-.b','linewidth',1.5)
plot(HUDLADCPlog(:,7), HUDLADCPlog(:,5),'o-r','linewidth',1.5)
plot(HUDLADCPlog(:,7), mean(HUDLADCPlog(:,5))*ones(length(HUDLADCPlog(:,5)),1),'-.r','linewidth',1.5)
plot(HUDLADCPlog(:,7), mean(HUDLADCPlog([1:2 4:end],5))*ones(length(HUDLADCPlog(:,5)),1),'-.m','linewidth',1.5)
xlabel('Cast Number')
legend('horizontal velocities in middle hour of cast > 2.5m/s', 'Mean','horizontal velocities > 2.5m/s', 'Mean', 'Mean without 533 value')

figure (4)
plot(HUDLADCPlog(:,7), HUDLADCPlog(:,4),'o-b','linewidth',1.5)
hold on
plot(HUDLADCPlog(:,7), mean(HUDLADCPlog(:,4))*ones(length(HUDLADCPlog(:,4)),1),'-.b','linewidth',1.5)
plot(HUDLADCPlog(:,7), HUDLADCPlog(:,6),'o-r','linewidth',1.5)
plot(HUDLADCPlog(:,7), mean(HUDLADCPlog(:,6))*ones(length(HUDLADCPlog(:,6)),1),'-.r','linewidth',1.5)
plot(HUDLADCPlog(:,7), mean(HUDLADCPlog([1:2 4:end],6))*ones(length(HUDLADCPlog(:,6)),1),'-.m','linewidth',1.5)
xlabel('Cast Number')
legend('percent of horizontal velocities in middle hour of cast> 2.5m/s', 'Mean','percent of horizontal velocities > 2.5m/s', 'Mean', 'Mean without 533 value')
