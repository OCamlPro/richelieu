// =============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2011 - DIGITEO - Bruno JOFRET
// Copyright (C) 2011 - DIGITEO - Allan CORNET
//
//  This file is distributed under the same license as the Scilab package.
// =============================================================================

// <-- CLI SHELL MODE -->
ierr = execstr("getrelativefilename();","errcatch");
if ierr <> 77 then pause,end

ierr = execstr("getrelativefilename([], []);","errcatch");
if ierr <> 999 then pause,end

__dir1  = TMPDIR+"/dir1";
__dir11 = TMPDIR+"/dir1/dir1.1";
__dir12 = TMPDIR+"/dir1/dir1.2";

mkdir(__dir1);
mkdir(__dir11);
mkdir(__dir12);

__file1  = __dir1+"/file1.txt";
__file11 = __dir11+"/file11.txt";
__file12 =  __dir12+"/file12.txt";

mputl("", __file1);
mputl("", __file11);
mputl("", __file12);

if getrelativefilename(__dir1, __file1) <> "file1.txt" then pause, end
if getrelativefilename(__dir1, __file11) <> pathconvert("dir1.1/file11.txt", %f) then pause, end
if getrelativefilename(__dir1, __file12) <> pathconvert("dir1.2/file12.txt", %f) then pause, end

if getrelativefilename(__dir11, __file1) <> pathconvert("../file1.txt", %f) then pause, end
if getrelativefilename(__dir11, __file11) <> "file11.txt" then pause, end
if getrelativefilename(__dir11, __file12) <> pathconvert("../dir1.2/file12.txt", %f) then pause, end

if getrelativefilename(__dir12, __file1) <> pathconvert("../file1.txt", %f) then pause, end;
if getrelativefilename(__dir12, __file11) <> pathconvert("../dir1.1/file11.txt", %f) then pause, end;
if getrelativefilename(__dir12, __file12) <> "file12.txt" then pause, end;
