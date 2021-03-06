/*
*  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
*  Copyright (C) 2012 - DIGITEO - cedric delamarre
*
*  This file must be used under the terms of the CeCILL.
*  This source file is licensed as described in the file COPYING, which
*  you should have received as part of this distribution.  The terms
*  are also available at
*  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
*
*/

#ifndef __PROD_H__
#define __PROD_H__

#include "double.hxx"
#include "polynom.hxx"

extern "C"
{
#include "dynlib_elementary_functions.h"
}

ELEMENTARY_FUNCTIONS_IMPEXP types::Double*  prod(types::Double*  pIn, int iOrientation);
ELEMENTARY_FUNCTIONS_IMPEXP types::Polynom* prod(types::Polynom* pIn, int iOrientation);

#endif /* __PROD_H__ */
