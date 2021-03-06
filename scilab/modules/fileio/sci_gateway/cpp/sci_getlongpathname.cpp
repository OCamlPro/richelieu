/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2011 - DIGITEO - Bruno JOFRET
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */
/*--------------------------------------------------------------------------*/
#include "fileio_gw.hxx"
#include "function.hxx"
#include "string.hxx"
#include "bool.hxx"

extern "C"
{
#include "getlongpathname.h"
#include "Scierror.h"
#include "localization.h"
}
/*--------------------------------------------------------------------------*/

using namespace types;

Function::ReturnValue sci_getlongpathname(typed_list &in, int _iRetCount, typed_list &out)
{
    if(in.size() != 1)
    {
        Scierror(999, _("%s: Wrong number of input arguments: %d expected.\n"), "getlongpathname" , 1);
        return Function::Error;
    }

    if(_iRetCount != 1 && _iRetCount != 2)
    {
        Scierror(78, _("%s: Wrong number of output argument(s): %d to %d expected.\n"), "getlongpathname", 1, 2);
        return Function::Error;
    }

    if(in[0]->isString() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d: Matrix of strings expected.\n"), "getlongpathname", 1);
        return Function::Error;
    }

    String* pS  = in[0]->getAs<types::String>();

    String* pOut1 = new String(pS->getRows() , pS->getCols());
    Bool* pOut2 = new Bool(pS->getRows() , pS->getCols());
    int* pBool = pOut2->get();
    for(int i = 0 ; i < pS->getSize(); i++)
    {
        pOut1->set(i, getlongpathnameW(pS->get(i), (BOOL*) &pBool[i]));
    }

    out.push_back(pOut1);
    if(_iRetCount == 2)
    {
        out.push_back(pOut2);
    }
    else
    {
        delete pOut2;
    }

    return Function::OK;
}
/*--------------------------------------------------------------------------*/
