// ============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2011-2011 - Gsoc 2011 - Iuri SILVIO
//
//  This file is distributed under the same license as the Scilab package.
// ============================================================================

// <-- JVM NOT MANDATORY -->
// <-- ENGLISH IMPOSED -->
// ============================================================================
// Unitary tests for mxGetFieldByNumber mex function
// ============================================================================

cd(TMPDIR);
ilib_verbose(0);
mputl(['#include ""mex.h""';
       'void mexFunction(int nlhs, mxArray *plhs[], int nrhs, mxArray *prhs[])';
       '{';
       '    mxArray *value = mxGetFieldByNumber(prhs[0], 0, 1);';
       '    if (value == NULL) value = mxCreateDoubleScalar(-1);';
       '    plhs[0] = value;';
       '}'],'mexgetFieldByNumber.c');
ilib_mex_build('libmextest',['getFieldByNumber','mexgetFieldByNumber','cmex'], 'mexgetFieldByNumber.c',[],'Makelib','','','');
exec('loader.sce');

c = struct("a", 5, "b", 2);
r = getFieldByNumber(c);
if r <> 2 then pause end

c = struct("b", 32);
r = getFieldByNumber(c);
if r <> -1 then pause end

r = getFieldByNumber(1);
if r <> -1 then pause end
