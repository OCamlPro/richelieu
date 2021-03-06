/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2012 - DIGITEO - Cedric DELAMARRE
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */
/*--------------------------------------------------------------------------*/
#include "elem_func_gw.hxx"
#include "function.hxx"
#include "double.hxx"
#include "overload.hxx"
#include "execvisitor.hxx"


extern "C"
{
#include "Scierror.h"
#include "localization.h"
#include "basic_functions.h"
}

/*--------------------------------------------------------------------------*/
types::Function::ReturnValue sci_frexp(types::typed_list &in, int _iRetCount, types::typed_list &out)
{
    if (in.size() != 1)
    {
        Scierror(77, _("%s: Wrong number of input argument(s): %d expected.\n"), "frexp", 1);
        return types::Function::Error;
    }

    if (_iRetCount != 2)
    {
        Scierror(78, _("%s: Wrong number of output argument(s): %d expected.\n"), "frexp", 2);
        return types::Function::Error;
    }

    if (in[0]->isDouble() == false)
    {
        std::wstring wstFuncName = L"%"  + in[0]->getShortTypeStr() + L"_frexp";
        return Overload::call(wstFuncName, in, _iRetCount, out, new ExecVisitor());
    }

    types::Double* pDblIn = in[0]->getAs<types::Double>();

    if (pDblIn->getDims() > 2)
    {
        std::wstring wstFuncName = L"%hm_frexp";
        return Overload::call(wstFuncName, in, _iRetCount, out, new ExecVisitor());
    }

    if (pDblIn->isComplex())
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A real matrix expected.\n"), "frexp", 1);
        return types::Function::Error;
    }

    types::Double* pDblCoef = new types::Double(pDblIn->getDims(), pDblIn->getDimsArray());
    types::Double* pDblExp  = new types::Double(pDblIn->getDims(), pDblIn->getDimsArray());

    for (int i = 0 ; i < pDblIn->getSize() ; i++)
    {
        pDblCoef->set(i, dfrexps(pDblIn->get(i), pDblExp->get() + i));
    }

    out.push_back(pDblCoef);
    out.push_back(pDblExp);

    return types::Function::OK;
}
/*--------------------------------------------------------------------------*/
