/*
*  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
*  Copyright (C) 2011 - DIGITEO - Antoine ELIAS
*
*  This file must be used under the terms of the CeCILL.
*  This source file is licensed as described in the file COPYING, which
*  you should have received as part of this distribution.  The terms
*  are also available at
*  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
*
*/

#include "types_gw.hxx"
#include "function.hxx"
#include "double.hxx"
#include "inspector.hxx"

extern "C"
{
#include "Scierror.h"
#include "localization.h"
}

using namespace types;

Function::ReturnValue sci_inspectorGetItem(typed_list &in, int _iRetCount, typed_list &out)
{
    if(in.size() != 1)
    {
        Scierror(999, _("%s: Wrong number of input arguments: %d expected.\n"), "inspectorGetItem", 1);
        return Function::Error;
    }

    if(in[0]->isDouble() == false)
    {

        Scierror(999, _("%s: Wrong type for input argument #%d: A scalar expected.\n"), "inspectorGetItem", 1);
        return Function::Error;
    }

    Double *pD = in[0]->getAs<Double>();
    if(pD->isScalar() == false)
    {
        Scierror(999, _("%s: Wrong size for input argument #%d: A scalar expected.\n"), "inspectorGetItem", 1);
        Function::Error;
    }

    int iPos = (int)pD->get(0) - 1;

    out.push_back(Inspector::getItem(iPos));
    return Function::OK;
}
