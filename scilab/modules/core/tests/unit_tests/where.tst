// =============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2008 - INRIA
//
//  This file is distributed under the same license as the Scilab package.
// =============================================================================

//where
deff("[a,b]=foo()","[a,b]=where()")
[a,b]=foo();
if a(1)<>2|b(1)<>"foo" then pause,end
