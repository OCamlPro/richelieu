function RegiontoSuperBlock_()
//** 29 June 2006
//** This is a quite f..ing function : handle with care    
  
  disablemenus() ;

  %win = curwin // only in main window
 
  if Select==[] then
    
    // %pt = []
    // pause
    // disp (".... do_region2block()... ");
    
      [%pt, scs_m] = do_region2block(%pt,scs_m); //** see file: 
    
    disp ("dd_6 "); // pause
    
    //** evenually put "%pt = [] " here 
                                  
  else
    
    if Select(1,2)<>curwin then
         return ; //** --> Exit point 
    end
    
    [%pt,scs_m] = do_select2block(%pt,scs_m);  //** ---> see below in this file :)    
  end
  
  enablemenus()
  
  Cmenu='Replot'; %pt=[];
  
  // Cmenu=[];%pt=[];
    
  // drawnow();show_pixmap(); 
  
  // pause
 
  
endfunction
//**---------------------------------------------------------------------


//**---------------------------------------------------------------------
function [%pt,scs_m] = do_select2block(%pt,scs_m)

  scs_m_save = scs_m,nc_save = needcompile ; 
  
  keep = [] ; del = [] ;
  
  sel = Select(:,1)'; nsel = setdiff(1:size(scs_m.objs),sel)
  
  //**-----------------------------------------------------------------
  for bl=sel
    
    if typeof(scs_m.objs(bl))=='Block' then
      
      if or(scs_m.objs(bl).gui==['IN_f' 
		    'OUT_f'
		    'CLKINV_f'
		    'CLKIN_f'
		    'CLKOUTV_f'
		    'CLKOUT_f'
                    'INIMPL_f'
		    'OUTIMPL_f']) then
	message('Input/Output ports are not allowed in the region.')
	return ; //** Exit point !
      end //** check block type 
      
      keep=[keep bl]
    
    end //** end block filter 
  
  end
  //**------------------------------------------------------------------
  
  
  for bl=nsel
    if typeof(scs_m.objs(bl))=='Block' then
      del=[del bl]
    end
  end
  
  
  prt=splitted_links(scs_m,keep,del)
  [reg,DEL]=do_delete2(scs_m,del,%f)
  rect=dig_bound(reg)
  nin=0
  nout=0
  ncin=0
  ncout=0
 
  //add input and output ports
  for k=1:size(prt,1)
    nreg=size(reg.objs)
    k1=prt(k,1); typ=prt(k,5);tp=prt(k,3)
    o1=reg.objs(k1) //block inside the region
    orient=o1.graphics.flip

    if tp==1 then //input port
      // build the link between block and port
      if typ>1 then //implicit regular link
	[x,y,vtyp]=getinputports(o1)
	from=[nreg+1,1,0] //added port
	to=prt(k,1:3)
      else	    //explicit regular link
	[x,y,vtyp]=getinputs(o1),
	from=[nreg+1,1,0] //added port
	to=prt(k,1:3)
      end

      if typ>0 then //input regular port
	x=x(prt(k,2))
	y=y(prt(k,2))
	nin=nin+1
	if typ==1 then sp=IN_f('define'),else sp=INIMPL_f('define'),end
	sz=20*sp.graphics.sz
	sp.graphics.sz=sz; //sz
	sp.graphics.flip=orient; //flip
	sp.graphics.exprs=string(nin); //expr
	sp.graphics.pout=nreg+2;//pout
	sp.model.ipar=nin; //port number

	o1.graphics.pin(prt(k,2))=nreg+2
	if orient then  //not flipped
	  sp.graphics.orig=[x-2*sz(1),y-sz(2)/2]; //orig
	  xl=[x-sz(1);x]
	  yl=[y;y]
	else // flipped
	  sp.graphics.orig=[x+sz(1),y-sz(2)/2]; //orig
	  xl=[x+sz(1);x]
	  yl=[y;y]
	end
	prt(k,2)=nin
      else //input event port
	p=prt(k,2)+size(find(vtyp==1),'*')
	x=x(p)
	y=y(p)
	ncin=ncin+1
	sp=CLKINV_f('define')
	sz=20*sp.graphics.sz
	sp.graphics.orig=[x-sz(1)/2,y+sz(2)]; //orig
	sp.graphics.sz=sz; //sz
	sp.graphics.exprs=string(ncin); //expr
	sp.graphics.peout=nreg+2;//peout
	sp.model.ipar=ncin; //port number
	
	o1.graphics.pein(prt(k,2))=nreg+2
	xl=[x;x]
	yl=[y+sz(2);y]
	prt(k,2)=ncin
      end
    else //  output port
      if typ>1 then //implicit regular link
	[x,y,vtyp]=getoutputports(o1)
	to=[nreg+1,1,1]
	from=prt(k,1:3)
      else //explicit regular link
	[x,y,vtyp]=getoutputs(o1)
	to=[nreg+1,1,1]
	from=prt(k,1:3)
      end
      if typ>0 then //output regular port
	x=x(prt(k,2))
	y=y(prt(k,2))
	nout=nout+1
	if typ==1 then sp=OUT_f('define'),else sp=OUTIMPL_f('define'),end
	sz=20*sp.graphics.sz
	sp.graphics.sz=sz; //sz
	sp.graphics.flip=orient; //flip
	sp.graphics.exprs=string(nout); //expr
	sp.graphics.pin=nreg+2;//pin
	sp.model.ipar=nout; //port number
	o1.graphics.pout(prt(k,2))=nreg+2
	sz=sp.graphics.sz
	if orient then  //not flipped 
	  sp.graphics.orig=[x+sz(1),y-sz(2)/2]; //orig
	  xl=[x;x+sz(1)]
	  yl=[y;y]
	else //flipped
	  sp.graphics.orig=[x-2*sz(1),y-sz(2)/2]; //orig
	  xl=[x;x-sz(1)]
	  yl=[y;y]
	end
	prt(k,2)=nout
      else //output event port
	p=prt(k,2)+size(find(vtyp==1),'*')
	x=x(p)
	y=y(p)
	ncout=ncout+1
	sp=CLKOUTV_f('define')
	sz=20*sp.graphics.sz
	sp.graphics.orig=[x-sz(1)/2,y-2*sz(2)]; //orig
	sp.graphics.sz=sz; //sz
	sp.graphics.exprs=string(ncout); //expr
	sp.graphics.pein=nreg+2;//pein
	sp.model.ipar=ncout; //port number
	o1.graphics.peout(prt(k,2))=nreg+2
	xl=[x;x]
	yl=[y;y-sz(2)]
	prt(k,2)=ncout
      end
    end
    lk=scicos_link(xx=xl,yy=yl,ct=[prt(k,4),typ],from=from,to=to)
    reg.objs(nreg+1)=sp
    reg.objs(nreg+2)=lk
    reg.objs(k1)=o1
  end
  reg=do_purge(reg)
  
  if lstsize(reg.objs)==0 then return, end
  //superblock should not inherit the context nor the name
  reg.props.context=' ' 
  reg.props.title(1)='Untitled'

  sup = SUPER_f('define')
  sup.graphics.orig   = [(rect(1)+rect(3))/2-20,(rect(2)+rect(4))/2-20]
  sup.graphics.sz     = [40 40]
  
  sup.model.in        = 1
  sup.model.out       = 1
  sup.model.rpar      = reg
  sup.model.blocktype = 'h'
  sup.model.dep_ut    = [%f %f]
  // open the superblock in editor
  [ok,sup]=adjust_s_ports(sup)

  [scs_m,DEL]=do_delete2(scs_m,keep,%t)
  drawobj(sup)
  
  scs_m.objs($+1)=sup
  // connect it
  nn=lstsize(scs_m.objs)  //superblock number
  nnk=nn
  for k=1:size(prt,1)
    k1=prt(k,6);tp1=prt(k,8);
    ksup=prt(k,1);tpsup=prt(k,3)
    typ=prt(k,5)
    o1=scs_m.objs(k1) // block origin of the link outside the superblock
    
    if typ>0 then //regular link
      if tp1==0 then  //link connected to an output port of o1
	if typ>1 then //implicit regular link
	  [x,y,vtyp]=getoutputports(o1)
	else //explicit regular link
	  [x,y,vtyp]=getoutputs(o1)
	end
	if tpsup==1 then //link connected to an input port of the superblock 
	  if typ>1 then //implicit regular link
	    [xn,yn,vtypn]=getinputports(sup)
	  else	    //explicit regular link
	    [xn,yn,vtypn]=getinputs(sup),
	  end
	  Psup='pin'
	else //link connected to an output port of the superblock 
	  if typ>1 then //implicit regular link
	    [xn,yn,vtypn]=getoutputports(sup)
	  else //explicit regular link
	    [xn,yn,vtypn]=getoutputs(sup),
	  end
	  Psup='pout'
	end
	p=prt(k,7)
	pn=prt(k,2)
	xl=[x(p);xn(pn)]
	yl=[y(p);yn(pn)]
	
	from=prt(k,6:8)
	to=[nn,prt(k,2:3)]
	o1.graphics.pout(prt(k,7))=nnk+1
	scs_m.objs(nn).graphics(Psup)(prt(k,2))=nnk+1
      else //link connected to an input port of o1
	if typ>1 then //implicit regular link
	  [x,y,vtyp]=getinputports(o1)
	else
	  [x,y,vtyp]=getinputs(o1)
	end
	if tpsup==1 then //link connected to an input port of the superblock 
	  if typ>1 then //implicit regular link
	    [xn,yn,vtypn]=getinputports(sup)
	  else //explicit regular link
	    [xn,yn,vtypn]=getinputs(sup),
	  end
	  Psup='pin'
	else  //link connected to an output port of the superblock 
	  if typ>1 then //implicit regular link
	    [xn,yn,vtypn]=getoutputports(sup)
	  else //explicit regular link
	    [xn,yn,vtypn]=getoutputs(sup),
	  end
	  Psup='pout'
	end
	p=prt(k,7)
	pn=prt(k,2)
	
	xl=[xn(pn);x(p)]
	yl=[yn(pn);y(p)]
	from=[nn,prt(k,2:3)]
	to=prt(k,6:8)
	o1.graphics.pin(prt(k,7))=nnk+1
	scs_m.objs(nn).graphics(Psup)(prt(k,2))=nnk+1
      end
    else //event link
      if tpsup==1 then //link connected to an event input port of the superblock 
	[x,y,vtyp]=getoutputs(o1)
	[xn,yn,vtypn]=getinputs(sup),
	p=prt(k,7)+size(find(vtyp==1),'*')+size(find(vtyp==2),'*')
	pn=prt(k,2)+size(find(vtypn==1),'*')+size(find(vtypn==2),'*')
	xl=[x(p);xn(pn)]
	yl=[y(p);yn(pn)]
	from=prt(k,6:8)
	to=[nn,prt(k,2:3)]
	o1.graphics.peout(prt(k,7))=nnk+1
	scs_m.objs(nn).graphics.pein(prt(k,2))=nnk+1
      else //link connected to an event output port of the superblock 
	[x,y,vtyp]=getinputs(o1)
	[xn,yn,vtypn]=getoutputs(sup),
	p=prt(k,7)+size(find(vtyp==1),'*')+size(find(vtyp==2),'*')
	pn=prt(k,2)+size(find(vtypn==1),'*')+size(find(vtypn==2),'*')
	xl=[xn(pn);x(p)]
	yl=[yn(pn);y(p)]
	from=[nn,prt(k,2:3)]
	to=prt(k,6:8)
	o1.graphics.pein(prt(k,7))=nnk+1
	scs_m.objs(nn).graphics.peout(prt(k,2))=nnk+1
      end
    end
    
    
    if xl(1)<>xl(2)&yl(1)<>yl(2) then //oblique link
      if prt(k,4)>0 then //regular port
	xl=[xl(1);xl(1)+(xl(2)-xl(1))/2;xl(1)+(xl(2)-xl(1))/2;xl(2)]
	yl=[yl(1);yl(1);yl(2);yl(2)]
      else
	xl=[xl(1);xl(1);xl(2);xl(2)]
	yl=[yl(1);yl(1)+(yl(2)-yl(1))/2;yl(1)+(yl(2)-yl(1))/2;yl(2)]
      end
    end

    lk=scicos_link(xx=xl,yy=yl,ct=prt(k,4:5),from=from,to=to)
    drawobj(lk)

    scs_m.objs($+1)=lk
    scs_m.objs(k1)=o1
    nnk=nnk+1
  end
  [scs_m_save,nc_save,enable_undo,edited,needcompile,..
   needreplay]=resume(scs_m_save,nc_save,%t,%t,4,needreplay)
endfunction
