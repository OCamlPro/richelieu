// ============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2011-2011 - Gsoc 2011 - Iuri SILVIO
//
//  This file is distributed under the same license as the Scilab package.
// ============================================================================

// <-- JVM NOT MANDATORY -->
// <-- ENGLISH IMPOSED -->
// ============================================================================
// Unitary tests for mxGetNaN mex function
// ============================================================================

cd(TMPDIR);
ilib_verbose(0);
mputl(['#include ""mex.h""';
       'void mexFunction(int nlhs, mxArray *plhs[], int nrhs, mxArray *prhs[])';
       '{';
       '    double dblNaN = mxGetNaN();';
       '    mxArray* pOut = mxCreateDoubleScalar(dblNaN);';
       '    plhs[0] = pOut;';
       '}'],'mexgetNaN.c');
ilib_mex_build('libmextest',['getNaN','mexgetNaN','cmex'], 'mexgetNaN.c',[],'Makelib','','','');
exec('loader.sce');

a = getNaN();
if isnan(a) <> %t then pause end
