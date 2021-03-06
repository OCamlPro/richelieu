// ============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2012 - DIGITEO - Antoine ELIAS
//
//  This file is distributed under the same license as the Scilab package.
// ============================================================================

// <-- JVM NOT MANDATORY -->

r = 2;
R = [1,2;3,4];
R3(:,:,1) = R;
R3(:,:,2) = R';
c = 1 - 2*%i;
C = [1+2*%i,2+4*%i;3+6*%i,4+8*%i];
C3(:,:,1) = C;
C3(:,:,2) = C';


// double - double

//r - r
assert_checkequal(r - r, 0)
//r - c
assert_checkequal(r - c, 1+%i*2)
//c - r
assert_checkequal(c - r, -1-%i*2)
//c - c
assert_checkequal(c - c, 0*%i)


//double - DOUBLE

//r - R
assert_checkequal(r - R, [1,0;-1,-2])
rR3ref(:,:,1) = [1,0;-1,-2];
rR3ref(:,:,2) = [1,-1;0,-2] ;
assert_checkequal(r - R3, rR3ref)
//r - C
assert_checkequal(r - C, [ 1-%i*2,-%i*4;-1-%i*6,-2-%i*8])
rC3ref(:,:,1) = [ 1-%i*2,-%i*4;-1-%i*6,-2-%i*8];
rC3ref(:,:,2) = [ 1+%i*2,-1+%i*6;%i*4,-2+%i*8];
assert_checkequal(r - C3, rC3ref)
//c - R
assert_checkequal(c - R, [-%i*2,-1-%i*2;-2-%i*2,-3-%i*2])
cR3ref(:,:,1) = [-%i*2,-1-%i*2;-2-%i*2,-3-%i*2];
cR3ref(:,:,2) = [-%i*2,-2-%i*2;-1-%i*2,-3-%i*2];
assert_checkequal(c - R3, cR3ref)
//c - C
assert_checkequal(c - C, [-%i*4,-1-%i*6;-2-%i*8,-3-%i*10])
cC3ref(:,:,1) = [-%i*4,-1-%i*6;-2-%i*8,-3-%i*10];
cC3ref(:,:,2) = [0,-2+%i*4;-1+%i*2,-3+%i*6];
assert_checkequal(c - C3, cC3ref)


//DOUBLE - double

//R - r
assert_checkequal(R - r, [-1,0;1,2])
R3rref(:,:,1) = [-1,0;1,2];
R3rref(:,:,2) = [-1,1;0,2];
assert_checkequal(R3 - r, R3rref)
//R - c
assert_checkequal(R - c, [%i*2, 1+%i*2; 2+%i*2, 3+%i*2])
R3cref(:,:,1) = [%i*2, 1+%i*2; 2+%i*2, 3+%i*2];
R3cref(:,:,2) = [%i*2, 2+%i*2; 1+%i*2, 3+%i*2];
assert_checkequal(R3 - c, R3cref)
//C - r
assert_checkequal(C - r, [-1+%i*2,%i*4; 1+%i*6, 2+%i*8])
C3rref(:,:,1) = [-1+%i*2,%i*4; 1+%i*6, 2+%i*8];
C3rref(:,:,2) = [-1-%i*2, 1-%i*6;-%i*4, 2-%i*8];
assert_checkequal(C3 - r, C3rref)
//c - c
assert_checkequal(C - c, [%i*4, 1+%i*6; 2+%i*8, 3+%i*10])
C3cref(:,:,1) = [%i*4, 1+%i*6; 2+%i*8, 3+%i*10];
C3cref(:,:,2) = [0, 2-%i*4; 1-%i*2, 3-%i*6];
assert_checkequal(C3 - c, C3cref)

//DOUBLE - DOUBLE
//R - R
assert_checkequal(R - R, [0,0;0,0])
R3R3ref(:,:,1) = [0,0;0,0];
R3R3ref(:,:,2) = [0,0;0,0];
assert_checkequal(R3 - R3, R3R3ref)
//R - C
assert_checkequal(R - C, [-%i*2,-%i*4;-%i*6,-%i*8])
R3C3ref(:,:,1) = [-%i*2,-%i*4;-%i*6,-%i*8];
R3C3ref(:,:,2) = [%i*2,%i*6;%i*4,%i*8];
assert_checkequal(R3 - C3, R3C3ref)
//C - R
assert_checkequal(C - R, [%i*2,%i*4;%i*6,%i*8])
C3R3ref(:,:,1) = [%i*2,%i*4;%i*6,%i*8];
C3R3ref(:,:,2) = [-%i*2,-%i*6;-%i*4,-%i*8];
assert_checkequal(C3 - R3, C3R3ref)
//C - C
assert_checkequal(C - C, [0,0;0,0]*%i)
C3C3ref(:,:,1) = [0,0;0,0];
C3C3ref(:,:,2) = [0,0;0,0];
assert_checkequal(C3 - C3, C3C3ref*%i)
