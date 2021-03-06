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
//## adjust scs_m.objs.model with bllst
function scs_m_temporary=adjust_scs_m_temp(scs_m_temporary,corinvtemp,ind)

  //## test if we are in a sblock
  corinvtemp1=corinvtemp(1);
  corinvtemp(1)=[];
  if size(corinvtemp,'*')>0 then
   scs_m_temporary1=adjust_scs_m_temp(scs_m_temporary.objs(corinvtemp1).model.rpar,corinvtemp,ind);

   //## adjust models of blocks in sblock
   scs_m_temporary.objs(corinvtemp1).model.rpar=scs_m_temporary1;

  //## test if it's a block
  else
    scs_m_temporary.objs(corinvtemp1).model=bllst(ind);
  end
endfunction
