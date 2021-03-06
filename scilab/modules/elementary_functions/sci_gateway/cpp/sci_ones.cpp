/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2006 - INRIA - Allan CORNET
 * Copyright (C) 2011 - DIGITEO - Antoine ELIAS
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

extern "C"
{
#include "Scierror.h"
#include "localization.h"
#include "charEncoding.h"
}

using namespace types;

/*--------------------------------------------------------------------------*/
Function::ReturnValue sci_ones(types::typed_list &in, int _iRetCount, types::typed_list &out)
{
    Double* pOut = NULL;
    if(in.size() == 0)
    {
        out.push_back(new Double(1));
        return Function::OK;
    }
    else if(in.size() == 1)
    {
        types::GenericType* pIn = in[0]->getAs<types::GenericType>();
        int iDims = pIn->getDims();
        int* piDims = pIn->getDimsArray();

        pOut = new Double(iDims, piDims);
    }
    else //size > 1
    {
        int iDims = static_cast<int>(in.size());
        int* piDims = new int[iDims];
        for(int i = 0 ; i < iDims ; i++)
        {
            Double* pIn = in[i]->getAs<Double>();
            if(pIn->isDouble() == false || pIn->isScalar() == false || pIn->isComplex())
            {
                Scierror(999, _("%s: Wrong type for input argument #%d: Real scalar expected.\n"), "ones", i + 1);
                return Function::Error;
            }
            piDims[i] = static_cast<int>(pIn->getReal()[0]);
        }
        pOut = new Double(iDims, piDims);
        delete[] piDims;
    }

    pOut->setOnes();
    out.push_back(pOut);
    return Function::OK;
}
/*--------------------------------------------------------------------------*/
