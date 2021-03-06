// =============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2008-2008 - DIGITEO - Simon LIPP <simon.lipp@scilab.org>
//
//  This file is distributed under the same license as the Scilab package.
// =============================================================================

// <-- CLI SHELL MODE -->

// <-- Non-regression test for bug 3500 -->
//
// <-- Bugzilla URL -->
// http://bugzilla.scilab.org/show_bug.cgi?id=3500
//
// Short description:
// No character matched shoud be a "no match" even if we matched a position

if regexp('foo', '/^/') <> [] then pause, end;
if regexp('foo', '/$/') <> [] then pause, end;
if regexp('foo', '/(?=o)/') <> [] then pause, end;
if regexp('foo', '/(?=f)/') <> [] then pause, end;
if regexp('foo', '/(?=oo)/') <> [] then pause, end;
