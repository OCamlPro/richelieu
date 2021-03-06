//  Scicos
//
//  Copyright (C) INRIA - Author : EADS-CCR
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

function [ok]=CFORTREDP(funam,tt)
// cette fonction est pour la complation et 
// le link dynamique du bloc EDP avec Scilab
// voir CFORTR2.sci pour le bloc Scicos c_block2 
 if tt<>[] then
   [ok]=scicos_block_link(funam,tt,'c');
 else
   ok=%f;break;
 end  
endfunction
