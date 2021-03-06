// =============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2010 - DIGITEO - Allan CORNET
//
//  This file is distributed under the same license as the Scilab package.
// =============================================================================
//
// <-- CLI SHELL MODE -->
//
// <-- Non-regression test for bug 4572 -->
//
// <-- Bugzilla URL -->
// http://bugzilla.scilab.org/show_bug.cgi?id=4572
//
// <-- Short Description -->
// scilab did a SIGSEGV after stacksize('max') + stacksize()

ierr = execstr("stacksize(''max'');r = stacksize();","errcatch");
if ierr <> 0 then pause, end

 stacksize('max');stacksize();