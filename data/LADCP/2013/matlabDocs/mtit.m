function	par=mtit(varargin)

defunit='normalized';
	if	nargout
		par=[];
	end

% check input
	if	nargin < 1
		help(mfilename);
		return;
	end
	if	isempty(get(0,'currentfigure'))
		disp('MTIT> no figure');
		return;
	end

		vl=true(size(varargin));
	if	ischar(varargin{1})
		vl(1)=false;
		figh=gcf;
		txt=varargin{1};
	elseif	any(ishandle(varargin{1}(:)))		&&...
		ischar(varargin{2})
		vl(1:2)=false;
		figh=varargin{1};
		txt=varargin{2};
	else
		error('MTIT> invalid input');
	end
		vin=varargin(vl);
		[off,vout]=get_off(vin{:});

% find surrounding box
		ah=findall(figh,'type','axes');
	if	isempty(ah)
		disp('MTIT> no axis');
		return;
	end
		oah=ah(1);

		ou=get(ah,'units');
		set(ah,'units',defunit);
		ap=get(ah,'position');
	if	iscell(ap)
		ap=cell2mat(get(ah,'position'));
	end
		ap=[	min(ap(:,1)),max(ap(:,1)+ap(:,3)),...
			min(ap(:,2)),max(ap(:,2)+ap(:,4))];
		ap=[	ap(1),ap(3),...
			ap(2)-ap(1),ap(4)-ap(3)];

% create axis...
		xh=axes('position',ap);
% ...and title
		th=title(txt,vout{:});
		tp=get(th,'position');
		set(th,'position',tp+off);
		set(xh,'visible','off','hittest','on');
		set(th,'visible','on');

% reset original units
		ix=find(~strcmpi(ou,defunit));
	if	~isempty(ix)
	for	i=ix(:).'
		set(ah(i),'units',ou{i});
	end
	end

% ...and axis' order
		uistack(xh,'bottom');
		axes(oah);				%#ok

	if	nargout
		par.pos=ap;
		par.oh=oah;
		par.ah=xh;
		par.th=th;
	end
end
%-------------------------------------------------------------------------------
function	[off,vout]=get_off(varargin)

% search for pairs <.off>/<value>

		off=zeros(1,3);
		io=0;
	for	mode={'xoff','yoff','zoff'};
		ix=strcmpi(varargin,mode);
	if	any(ix)
		io=io+1;
		yx=find(ix);
		ix(yx+1)=1;
		off(1,io)=varargin{yx(end)+1};
		varargin=varargin(xor(ix,1));
	end
	end
		vout=varargin;
end