            
function linecolours = getcolour(linecolours)

linecolours=[1 1 0; 1 0 1; 0 1 1; 1 0 0; 0 1 0; 0 0 1; 0 0 0; 1 1 1];
            linecolours=[linecolours; linecolours; linecolours; linecolours; linecolours;linecolours;linecolours];
             linecolours(8,:)=[ 0.9  0.9 0.9];
%             linecolours(9:8:56,3)=[ 0.3  0.5  0.7  0.9  0.4 0.15];
            linecolours(10:8:56,2)=[0.15  0.3  0.5  0.7  0.9  0.4];
            linecolours(11:8:56,2)=[ 0.6  0.9  0.8  1 0.15 0.4 ];
            linecolours(12:8:56,2)=[0.3  0.5  0.7  0.8  1  1];
            linecolours(12:8:56,1)=[0.9  0.88  0.88  0.85  0.8  0.8];
            linecolours(13:8:56,1)=[0.15  0.3  0.5  0.7  0.9  0.4];
            linecolours(14:8:56,2)=[0.15  0.3  0.5  0.7  0.9  0.4];
            linecolours(15:8:56,2)=[0.3  0.5  0.6  0.7  0.8  0.9];
            linecolours(16:8:56,1)=[0.65  0.5  0.4  0.2  0  0.1];
            linecolours(16:8:56,2)=[0.65  0.5  0.4  0.2  0  0.1];
            linecolours(16:8:56,3)=[0.65 0.5  0.4  0.2  0  0.1];
            linecolours(10,1)=0.7; linecolours(18,1)=0.4;linecolours(26,1)=0.6;
            linecolours(13:8:56,2)=[0.6  0.75  0.8 0.85  0.92  0.95];
%             linecolours(19:8:56,3)=[0.5  0.7  0.8 0.87  0.92  ];
            linecolours(14:8:56,3)=[0.6  0.7  0.8  0.85  0.92  0.95];
            linecolours(9:8:56,2)=[0.6  0.9  0.8  0.85  0.92  0.95];
        linecolours(15:8:56,1)=[0.4  0.6  0.7  0.8  0.9  1];
        
        linecolours(22,:)=[ 0.3   0.3 0.6];
        linecolours(24,:)=[ 0.3  0 0.3];
        linecolours(27,:)=[ 0.2  0.8   0.6];
        linecolours(28,:)=[ 0.4  0.5  .2];
        linecolours(49,:)=[ 1  0  .2];
        linecolours(43,:)=[ 0  0  .5];
        linecolours(48,:)=[ .8  1  1]; 
         linecolours(47,:)=[ .8  1  .8]; 
          linecolours(45,:)=[ 1  1  .2]; 
          linecolours(44,:)=[ .3  1  .3]; 
          linecolours(46,:)=[ 1  .7  0];
          
  linecolours=[linecolours; linecolours; linecolours; linecolours; linecolours; linecolours; linecolours; linecolours; linecolours; linecolours];