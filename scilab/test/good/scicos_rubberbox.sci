//  Scicos
//
//  Copyright (C) INRIA - Serge Steer <serge.steer@inria.fr>
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

function [rect,btn] = scicos_rubberbox(rect, edit_mode)

//** 18 Mar 2008 : this function has been renomed "scicos_rubberbox" in order to 
//**               avoid confusion with the Scilab 5 internal "C" primitive
//**               "rubberbox" 

//** 24 Nov 2008 : Scilab 5 "graphics" update with the new "rubberbox" primitive

edition_mode = %t ; 
[final_rect, final_btn] = rubberbox(rect, edition_mode); 

rect = final_rect ;
btn  = final_btn  ; 

endfunction
