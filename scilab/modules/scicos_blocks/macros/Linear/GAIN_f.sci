//  Scicos
//
//  Copyright (C) INRIA - METALAU Project <scicos@inria.fr>
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
//
// See the file ../license.txt
//

function [x,y,typ]=GAIN_f(job,arg1,arg2)
x=[];y=[];typ=[];
select job
 case 'plot' then
  C=arg1.graphics.exprs;
  standard_draw(arg1)
case 'getinputs' then
  [x,y,typ]=standard_inputs(arg1)
case 'getoutputs' then
  [x,y,typ]=standard_outputs(arg1)
case 'getorigin' then
  [x,y]=standard_origin(arg1)
case 'set' then
  x=arg1;
  graphics=arg1.graphics;exprs=graphics.exprs
  model=arg1.model;
  while %t do
    [ok,gain,exprs]=scicos_getvalue('Set gain block parameters',..
	['Gain'],list('mat',[-1,-1]),exprs(1))
    if ~ok then break,end
    if gain==[] then
      message('Gain must have at least one element')
    else
      [out,in]=size(gain)
      [model,graphics,ok]=check_io(model,graphics,in,out,[],[])
      if ok then
	graphics.exprs=exprs
	model.rpar=gain(:);
	x.graphics=graphics;x.model=model
	break
      end
    end
  end
case 'define' then

  gain=1;in=1;out=1;
  model=scicos_model()
  model.sim='gain'
  model.in=1
  model.out=1
  model.rpar=gain
  model.blocktype='c'
  model.dep_ut=[%t %f]
  
  exprs=[strcat(sci2exp(gain));strcat(sci2exp(in));strcat(sci2exp(out))]
  gr_i=['gain=C,'
      'dx=sz(1)/5;';
      'dy=sz(2)/10;';
      'xx=orig(1)+      [1 4 1 1]*dx;';
      'yy=orig(2)+sz(2)-[1 5 9 1]*dy;';
      'xpoly(xx,yy,''lines'');';
      'w=sz(1)-2*dx;h=sz(2)-2*dy;';
      'xstringb(orig(1)+dx,orig(2)+dy,gain,w,h,''fill'');']
  x=standard_define([2 2],model,exprs,gr_i)
end
endfunction
