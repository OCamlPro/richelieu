/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2011 - DIGITEO - Cedric DELAMARRE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#ifndef __SCILAB_SCANF_HXX__
#define __SCILAB_SCANF_HXX__

#include "dynlib_fileio.h"
#include "internal.hxx"

int scilab_sscanf(wchar_t* _wcsFormat, wchar_t* _wcsData, int _iIterrator, int iNiter, std::vector<types::InternalType*>* _pITOut);

#endif /* !__SCILAB_SCANF_HXX__ */
