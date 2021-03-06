// ============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2011-2011 - Gsoc 2011 - Iuri SILVIO
//
//  This file is distributed under the same license as the Scilab package.
// ============================================================================

// <-- JVM NOT MANDATORY -->
// <-- ENGLISH IMPOSED -->
// ============================================================================
// Unitary tests for mexGetArray mex function
// ============================================================================

cd(TMPDIR);
ilib_verbose(0);
mputl([ '#include ""mex.h""';
        'void mexFunction(int nlhs, mxArray *plhs[], int nrhs, mxArray *prhs[])';
        '{';
        '    char *workspace = mxArrayToString(prhs[0]);';
        '    char *name = mxArrayToString(prhs[1]);';
        '    mxArray *pOut = mexGetArray(name, workspace);';
        '    plhs[0] = pOut != NULL ? pOut : mxCreateString(""NULL"");';
        '}'],'mexgetArray.c');
ilib_mex_build('libmextest', ['getArray', 'mexgetArray', 'cmex'], 'mexgetArray.c', [], 'Makelib', '', '', '');
exec('loader.sce');

global globalvar;
globalvar = "my global var";
callervar = "my caller var";
if getArray("global", "globalvar") <> "my global var" then pause end
if getArray("caller", "globalvar") <> "NULL" then pause end
if getArray("base", "globalvar") <> "my global var" then pause end
if getArray("global", "callervar") <> "NULL" then pause end
if getArray("caller", "callervar") <> "my caller var" then pause end
if getArray("base", "callervar") <> "my caller var" then pause end
