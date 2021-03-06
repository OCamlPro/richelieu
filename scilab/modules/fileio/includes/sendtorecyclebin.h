/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2011 - DIGITEO - Allan CORNET
 * 
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at    
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */
/*--------------------------------------------------------------------------*/
#ifdef _MSC_VER
#include "BOOL.h"
#include "charEncoding.h"

/* send a file to recycle bin on Windows */
BOOL sendToRecycleBin(const char* filename);
BOOL sendToRecycleBinW(const wchar_t* wfilename);

#endif
/*--------------------------------------------------------------------------*/