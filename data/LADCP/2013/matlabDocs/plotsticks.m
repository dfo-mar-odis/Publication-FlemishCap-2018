%----------------------------------------------------------------
function h = plotsticks(x,y,x0,y0,axeshandle)
hold on;
h = line(x',y','Color','b','LineStyle','-');
xaxescolor = get(axeshandle,'XColor');
line(x0,y0,'Color',xaxescolor,'LineStyle','-');

