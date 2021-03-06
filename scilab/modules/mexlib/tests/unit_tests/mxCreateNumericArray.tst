// ============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2009 - DIGITEO - Allan CORNET
//
//  This file is distributed under the same license as the Scilab package.
// ============================================================================

// <-- JVM NOT MANDATORY -->
// <-- ENGLISH IMPOSED -->
// ============================================================================
// Unitary tests for mxCreateNumericArray mex function
// ============================================================================

cd(TMPDIR);
ilib_verbose(0);
mputl([ '#include ""mex.h""';
        '';
        'void mexFunction(int nlhs, mxArray *plhs[], int nrhs, mxArray *prhs[])';
        '{';
        '   int piDims[3] = {4,3,2};';
        '   int iDims = 3;';
        '   mxClassID CLASS = 0;';
        '   double* pdblType = NULL;';
        '   mxArray* pOut = NULL;';
        '';
        '   if(nrhs != 1 /*|| !mxIsDouble(prhs[0])*/)';
        '   {';
        '       mexErrMsgTxt(""Wrong number or type of input argument"");';
        '   }';
        '';
        '   pdblType = mxGetPr(prhs[0]);';
        '   pOut = mxCreateNumericArray(iDims, piDims, (mxClassID)pdblType[0], mxREAL);';
        '   plhs[0] = pOut;';
        '}'],'mexCreateNumericArray.c');
ilib_mex_build('libmextest',['createNumericArray','mexCreateNumericArray','cmex'], 'mexCreateNumericArray.c',[],'Makelib','','','');
exec('loader.sce');

ref_int8 = int8(1);
ref_int8(4,3,2) = int8(1);
ref_int8(:) = int8(0);

ref_uint8 = uint8(1);
ref_uint8(4,3,2) = uint8(1);
ref_uint8(:) = uint8(0);

ref_int16 = int16(1);
ref_int16(4,3,2) = int16(1);
ref_int16(:) = int16(0);

ref_uint16 = uint16(1);
ref_uint16(4,3,2) = uint16(1);
ref_uint16(:) = uint16(0);

ref_int32 = int32(1);
ref_int32(4,3,2) = int32(1);
ref_int32(:) = int32(0);

ref_uint32 = uint32(1);
ref_uint32(4,3,2) = uint32(1);
ref_uint32(:) = uint32(0);

ref_int64 = int64(1);
ref_int64(4,3,2) = int64(1);
ref_int64(:) = int64(0);

ref_uint64 = uint64(1);
ref_uint64(4,3,2) = uint64(1);
ref_uint64(:) = uint64(0);

ret8 = createNumericArray(8);
retU8 = createNumericArray(9);
ret16 = createNumericArray(10);
retU16 = createNumericArray(11);
ret32 = createNumericArray(12);
retU32 = createNumericArray(13);
ret64 = createNumericArray(14);
retU64 = createNumericArray(15);
