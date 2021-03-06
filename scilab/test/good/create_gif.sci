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
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
// See the file ./license.txt
//

function create_gif(TXT,path)
// creates gif files of icon size from scicos blocks. The list of
// block names can be provided in TXT. path contains the path to the
// directory where the gif files are to be stored. Background color
// is set to transparent using "convert" is available on the system.
// If the function is called with no argument, gif files of standard
// scicos library blocks are generated and placed in
// SCI+'/macros/scicos/scicos_doc/man/gif_icons'
  if argn(2)==0 then
    [j,TXT]=create_palette(%f);
    path=%scicos_gif(1)
  end
  if exists('scicos_scicoslib')==0 then
    load("SCI/modules/scicos/macros/scicos_scicos/lib") ;
  end

  if exists('scicos_autolib')==0 then
    load("SCI/modules/scicos/macros/scicos_auto/lib") ;
  end

  if exists('scicos_utilslib')==0 then
    load("SCI/modules/scicos/macros/scicos_utils/lib") ;
  end

  exec(loadpallibs,-1)
  a=gcf()
  rgb=[.3,.4,.5] ; // used for transparent
  options=default_options();
  a.color_map=[a.color_map;options(6)]  ;
  a.color_map=[a.color_map;rgb] ; // used for transparent
  last_col=size(a.color_map,1);
  R=60
  for i=1:size(TXT,1)
    [xpath,name,ext]=splitfilepath(TXT(i))
    execstr('o='+name+'(''define'')');
    sz=20*o.graphics.sz;
    o.graphics.sz=sz;
    //R=(max(sz)+15)  // add margin for ports
    orig=(R*[1,1]-sz)/2+[5 0];
    o.graphics.orig=orig;
    o.graphics.id="";
    clf()
    a.children.data_bounds = R*[0,0;1.5,1];
    a.children.margins=[0,0,0,0];
    a.children.axes_bounds = [0,0,1,1];
    a.immediate_drawing='on';
    a.auto_resize='off';
    a.background=last_col;
    execstr(name+'(''plot'',o)');
    a.axes_size=50*[2 1];
    xs2gif(a.figure_id,path+'/'+name+'.gif')
    r=string(100*rgb(1));g=string(100*rgb(2));b=string(100*rgb(3));
    unix('convert -transparent '"rgb(76,102,127)'" '+path+'/'+name+'.gif '+ ...
	 +path+'/'+name+'.gif');
  end
  delete(a)
endfunction

