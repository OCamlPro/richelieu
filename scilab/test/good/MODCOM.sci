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

function [ok,tt]=MODCOM(funam,tt,vinp,vout,vparam,vparamv,vpprop)
//
 [dirF,nameF,extF]=fileparts(funam);

 //the new head
 class_txt_new=build_classhead(funam,vinp,vout,vparam,vparamv,vpprop)

 if (tt==[]) then
   tete4= ['';' //     Real x(start=1), y(start=2);']
   tete5='equation';

   tete6=['      // exemple'];
   tete7='      //der(x)=x-x*y;';
   tete8='      //der(y)+2*y=x*y;';
   tete9='end '+nameF+';';
   textmp=[class_txt_new;tete4;tete5;tete6;tete7;tete8;tete9];
 else
   modif=%f;
   for i=1:size(tt,'*')
     if strindex(stripblanks(tt(i)),...
                 '////do not modif above this line ////')<>[] then
        //Alan, 07/10/07
        //tt(1:i-1) : the current head
        textmp=[class_txt_new;tt(i+1:$)]
        modif=%t
        break
     end
   end
   if ~modif then textmp=tt, end;
 end

 editblk=%f
 //## set param of scstxtedit
 ptxtedit = scicos_txtedit(clos = 0,...
            typ  = "ModelicaClass",...
            head = ['Function definition in Modelica';
                    'Here is a skeleton of the functions'+...
                     ' which you should edit'])

 while %t

   if (extF=='' | (extF=='.mo' & fileinfo(funam)==[])) then
     editblk=%t;
     [txt,Quit] = scstxtedit(textmp,ptxtedit);
   elseif (extF=='.mo' & fileinfo(funam)<>[]) then
     txt=tt;
   end
   
   
   if ptxtedit.clos==1 then
     break;
   end

   if txt<>[] then
     //## TODO : compilation
     //## printf("Compil !!");
     ok=%t;

     //** saving in the filename
     if ok then
       tarpath=pathconvert(TMPDIR+'/Modelica/',%f,%t);

       if (extF=='')  then
         funam=tarpath+nameF+'.mo';
         mputl(txt,funam);
       elseif fileinfo(funam)==[] then
         mputl(txt,funam);
       end
       ptxtedit.clos = 1;
       tt   = txt;
     end
     textmp    = txt;
   end

   if editblk then
     if Quit==1 then
       ok=%f;
       break;
     end
   elseif txt==[] then
     ok=%f; // cancel bouton
     break
   end
 end
endfunction

//build_classhead : build the head of the modelica function
function class_txt=build_classhead(funam,vinp,vout,vparam,vparamv,vpprop)

  [dirF,nameF,extF]=fileparts(funam);

  ni=size(vinp,'r');   //** number of inputs
  no=size(vout,'r');   //** number of outputs
  np=size(vparam,'r'); //** number of params

  tete1=['class '+nameF]

  //** parameters head
  if np<>0 then
    tete1b= '      //parameters';
    for i=1:np
      //** param
      if vpprop(i)==0 then
         head='      parameter Real '
         if size(vparamv(i),'*')==1 then
           if int(vparamv(i))==vparamv(i) then
             head=head+vparam(i)+'='+string(vparamv(i))+'.0;'
           else
             head=head+vparam(i)+'='+string(vparamv(i))+';'
           end
         else
           head=head+vparam(i)+'['+string(size(vparamv(i),'*'))+']={';
           for j=1:size(vparamv(i),'*')
             if int(vparamv(i)(j))==vparamv(i)(j) then
               head=head+string(vparamv(i)(j))+'.0'
             else
               head=head+string(vparamv(i)(j))
             end
             if j<>size(vparamv(i),'*') then
               head=head+','
             end
           end
           head=head+'};'
         end
      //** state
      elseif vpprop(i)==1 then
         head='      Real           '
         if size(vparamv(i),'*')==1 then
           if int(vparamv(i))==vparamv(i) then
             head=head+vparam(i)+'(start='+string(vparamv(i))+'.0);'
           else
             head=head+vparam(i)+'(start='+string(vparamv(i))+');'
           end
         else
           head=head+vparam(i)+'['+string(size(vparamv(i),'*'))+'](start={';
           for j=1:size(vparamv(i),'*')
             if int(vparamv(i)(j))==vparamv(i)(j) then
               head=head+string(vparamv(i)(j))+'.0'
             else
               head=head+string(vparamv(i)(j))
             end
             if j<>size(vparamv(i),'*') then
               head=head+','
             end
           end
           head=head+'});'
         end
      //** fixed state
      elseif vpprop(i)==2 then
         head='      Real           '
         if size(vparamv(i),'*')==1 then
           if int(vparamv(i))==vparamv(i) then
             head=head+vparam(i)+'(fixed=true,start='+string(vparamv(i))+'.0);'
           else
             head=head+vparam(i)+'(fixed=true,start='+string(vparamv(i))+');'
           end
         else
           head=head+vparam(i)+'['+string(size(vparamv(i),'*'))+'](start={';
           P_fix='fixed={'
           for j=1:size(vparamv(i),'*')
             if int(vparamv(i)(j))==vparamv(i)(j) then
               head=head+string(vparamv(i)(j))+'.0'
             else
               head=head+string(vparamv(i)(j))
             end
             P_fix=P_fix+'true'
             if j<>size(vparamv(i),'*') then
               head=head+','
               P_fix=P_fix+','
             end
           end
           head=head+'},'+P_fix+'});'
         end
      end
      tete1b=[tete1b
              head]
    end
  else
    tete1b=[];
  end

  //** inputs head
  if ni<>0 then
    tete2= '      Real ';
    for i=1:ni
      tete2=tete2+vinp(i);
      if (i==ni) then  tete2=tete2+';';else  tete2=tete2+',';end
    end
    tete2=['      //input variables';
           tete2];
  else
    tete2=[];
  end

  //** outputs head
  if no<>0 then
    tete3= '      Real '
    for i=1:no
      tete3=tete3+vout(i);
      if (i==no) then  tete3=tete3+';';else  tete3=tete3+',';end
    end
    tete3=['      //output variables';
           tete3];
  else
   tete3=[];
  end

  tete4='  ////do not modif above this line ////'
   //-----------------------------------------

  class_txt=[tete1;
             '  ////automatically generated ////';
             tete1b;tete2;tete3;tete4]
endfunction
