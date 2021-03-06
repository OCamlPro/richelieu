// Non-regression test file for bug 1491
// Copyright INRIA
// Scilab Project - Jean-Baptiste Silvy
// Copyright INRIA 2006
// Date : 10/02/2006

mode(-1);
clear ;

clipBox = [0.1,0.9,0.8,0.8] ;

a = gca() ;
a.box        = 'on' ;
// draw the clipping boundaries
xrect( clipBox(1), clipBox(2), clipBox(3), clipBox(4) ) ;
r1 = gce() ;
a.clip_box   = clipBox ;
a.clip_state = 'on' ;

xfrect( 0.0, 0.5, 0.5, 0.5 ) ;
r2 = gce() ;

xpoly([0,1],[0,1]) ;
p1 = gce() ;

checkR1 = ( r1.clip_state == 'off' ) ;
checkR2 = ( r2.clip_state == 'on'  ) & ( r2.clip_box == clipBox ) ;
checkP1 = ( p1.clip_state == 'on'  ) & ( p1.clip_box == clipBox ) ;

// check if the clip_box is set for objects wich are
// created after the axes have been clipped
if ( checkR1 & checkR2 & checkP1  ) then
  affich_result(%T,1491);
else
  affich_result(%F,1491);
end

clear ;
