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
// See the file ../license.txt
//

function  [%cpr,ok] = xcos_compile(scs_m)

//-- BJ : Alias Warning Function
  prot = funcprot();
  funcprot(0);
  hilite_obj = xcosShowBlockWarning;
  unhilite_obj = xcosClearBlockWarning;
  funcprot(prot);
  //-- end

////////////////////////////////////////////////////////////////
// Add global environment variable so that scicos is not lost //
////////////////////////////////////////////////////////////////
%state0     = list();
needcompile = 4;
curwin      = 1000;
%cpr        = struct();
%tcur       = 0;
%cpr.state  = %state0;
tf          = scs_m.props.tf;
alreadyran  = %f;

show_trace = %f; //** tracing and profiling (probably by Alan L. )

if show_trace then 
  disp("c_pass0:"+string(timer())); 
end

if ~exists("%scicos_solver") then 
  %scicos_solver = 0 ; 
end

if ~exists("%scicos_debug_gr") then
  %scicos_debug_gr = %f; //** debug mode : default is "%f"
end

// modelica_libs needed to compile Modelica blocks
if ~exists("modelica_libs") then
  modelica_libs = getModelicaPath();
end

par = scs_m.props;

if alreadyran then 
  // terminate current simulation
  do_terminate();
end

timer() ; //** profiling timer 

IN=[];OUT=[];
for i=1:lstsize(scs_m.objs)
  if typeof(scs_m.objs(i))=="Block" then 
     if scs_m.objs(i).gui=="IN_f" then
        scs_m.objs(i).gui="INPUTPORT";
        scs_m.objs(i).model.sim="bidon"
        IN=[IN scs_m.objs(i).model.ipar]
     elseif scs_m.objs(i).gui=="OUT_f" then
        scs_m.objs(i).gui="OUTPUTPORT";
        scs_m.objs(i).model.sim="bidon"
        OUT=[OUT  scs_m.objs(i).model.ipar]
    end
  end
end

IN=-gsort(-IN);
if or(IN<>[1:size(IN,"*")]) then 
  ok=%f;%cpr=list()
  message("Input ports are not numbered properly.")
  return
end

OUT=-gsort(-OUT);
if or(OUT<>[1:size(OUT,"*")]) then 
  ok=%f;%cpr=list()
  message("Output ports are not numbered properly.")
  return
end

//** First PASS
[bllst,connectmat,clkconnect,cor,corinv,ok] = c_pass1(scs_m);

if show_trace then
  disp("c_pass1:"+string(timer()))
end

if ~ok then %cpr=list()
   return ; //** incase of any error EXIT 
end

if size(connectmat,2)==6 then 
  connectmat = connectmat(:,[1 2 4 5])
end

scs_m = null() ;

if ~ok then %cpr=list()
  return
end

// newc_pass2 destroys the corinv component associated
// to the modelica blocks preserve it
// clast=corinv($)
// if type(clast)==15 then corinv($)=clast(1),klast=size(corinv),end
// %cpr=newc_pass2(bllst,connectmat,clkconnect,cor,corinv);
// newc_pass2 destroys the corinv component associated
// to the modelica blocks
// if type(clast)==15 then %cpr.corinv(klast)=clast,end

//** Second PASS
%cpr = c_pass2(bllst,connectmat,clkconnect,cor,corinv);

if %cpr==list() then
  ok = %f ; 
end 
endfunction
