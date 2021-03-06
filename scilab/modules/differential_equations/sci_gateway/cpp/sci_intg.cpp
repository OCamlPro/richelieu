/*
* Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
* Copyright (C) 2011 - DIGITEO - Cedric DELAMARRE
*
* This file must be used under the terms of the CeCILL.
* This source file is licensed as described in the file COPYING, which
* you should have received as part of this distribution.  The terms
* are also available at
* http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
*
*/
/*--------------------------------------------------------------------------*/

#include "differential_equations_gw.hxx"
#include "function.hxx"
#include "double.hxx"
#include "string.hxx"
#include "list.hxx"
#include "callable.hxx"
#include "differentialequationfunctions.hxx"

extern "C"
{
#include "MALLOC.h"
#include "localization.h"
#include "Scierror.h"
#include "scifunctions.h"
#include "sci_warning.h"
#include "sciprint.h"
}

/*--------------------------------------------------------------------------*/
types::Function::ReturnValue sci_intg(types::typed_list &in, int _iRetCount, types::typed_list &out)
{
    double pdA    = 0;
    double pdB    = 0;
    double pdEpsR = 1.0e-8;
    double pdEpsA = 1.0e-14;

    double result = 0;
    double abserr = 0;

// *** check the minimal number of input args. ***
    if(in.size() < 3 || in.size() > 5)
    {
        Scierror(77, _("%s: Wrong number of input argument(s): %d expected.\n"), "intg", 3);
        return types::Function::Error;
    }

// *** check number of output args ***
    if(_iRetCount > 2)
    {
        Scierror(78, _("%s: Wrong number of output argument(s): %d expected.\n"), "intg", 2);
        return types::Function::Error;
    }

// *** check type of input args and get it. ***
    // A
    if(in[0]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "intg", 1);
        return types::Function::Error;
    }

    types::Double* pDblA = in[0]->getAs<types::Double>();

    if(pDblA->isScalar() == false)
    {
        Scierror(999, _("%s: Wrong size for input argument #%d : A scalar expected.\n"), "intg", 1);
        return types::Function::Error;
    }

    pdA = pDblA->get(0);

    // B
    if(in[1]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "intg", 2);
        return types::Function::Error;
    }

    types::Double* pDblB = in[1]->getAs<types::Double>();

    if(pDblB->isScalar() == false)
    {
        Scierror(999, _("%s: Wrong size for input argument #%d : A scalar expected.\n"), "intg", 2);
        return types::Function::Error;
    }

    pdB = pDblB->get(0);

    // function
    DifferentialEquationFunctions* deFunctionsManager = new DifferentialEquationFunctions(L"intg");
    DifferentialEquation::addDifferentialEquationFunctions(deFunctionsManager);

    if(in[2]->isCallable())
    {
        types::Callable* pCall = in[2]->getAs<types::Callable>();
        deFunctionsManager->setFFunction(pCall);

        // check function
        double t = 1;
        double ret = intg_f(&t);
        if(ret == 0)
        {
            Scierror(50, _("%s: Argument #%d : Variable returned by scilab argument function is incorrect.\n"), "intg", 3);
            DifferentialEquation::removeDifferentialEquationFunctions();
            return types::Function::Error;
        }
    }
    else if(in[2]->isString())
    {
        bool bOK = false;
        types::String* pStr = in[2]->getAs<types::String>();
        bOK = deFunctionsManager->setFFunction(pStr);

        if(bOK == false)
        {
            char* pst = wide_string_to_UTF8(pStr->get(0));
            Scierror(50, _("%s: Subroutine not found: %s\n"), "intg", pst);
            FREE(pst);
            DifferentialEquation::removeDifferentialEquationFunctions();
            return types::Function::Error;
        }
    }
    else if(in[2]->isList())
    {
        types::List* pList = in[2]->getAs<types::List>();

        if(pList->getSize() == 0)
        {
            Scierror(50, _("%s: Argument #%d : Subroutine not found in list: %s\n"), "intg", 3, "(string empty)");
            DifferentialEquation::removeDifferentialEquationFunctions();
            return types::Function::Error;
        }

        if(pList->get(0)->isCallable())
        {
            deFunctionsManager->setFFunction(pList->get(0)->getAs<types::Callable>());
            for(int iter = 1; iter < pList->getSize(); iter++)
            {
                deFunctionsManager->setFArgs(pList->get(iter)->getAs<types::InternalType>());
            }
        }
        else
        {
            Scierror(999, _("%s: Wrong type for input argument #%d : The first argument in the list must be a Scilab function.\n"), "intg", 3);
            DifferentialEquation::removeDifferentialEquationFunctions();
            return types::Function::Error;
        }
    }
    else
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A function expected.\n"), "intg", 3);
        DifferentialEquation::removeDifferentialEquationFunctions();
        return types::Function::Error;
    }

    if(in.size() > 3)
    {
        if(in[3]->isDouble() == false)
        {
            Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "intg", 4);
            DifferentialEquation::removeDifferentialEquationFunctions();
            return types::Function::Error;
        }

        types::Double* pDblEpsR = in[3]->getAs<types::Double>();

        if(pDblEpsR->isScalar() == false)
        {
            Scierror(999, _("%s: Wrong size for input argument #%d : A scalar expected.\n"), "intg", 4);
            DifferentialEquation::removeDifferentialEquationFunctions();
            return types::Function::Error;
        }

        pdEpsR = pDblEpsR->get(0);
    }

    if(in.size() == 5)
    {
        if(in[4]->isDouble() == false)
        {
            Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "intg", 5);
            DifferentialEquation::removeDifferentialEquationFunctions();
            return types::Function::Error;
        }

        types::Double* pDblEpsA = in[4]->getAs<types::Double>();

        if(pDblEpsA->isScalar() == false)
        {
            Scierror(999, _("%s: Wrong size for input argument #%d : A scalar expected.\n"), "intg", 5);
            DifferentialEquation::removeDifferentialEquationFunctions();
            return types::Function::Error;
        }
        pdEpsA = pDblEpsA->get(0);
    }

// *** Create working table. ***
    int limit = 750;

    // rwork
    double* alist   = (double*)malloc(limit * sizeof(double));
    double* blist   = (double*)malloc(limit * sizeof(double));
    double* elist   = (double*)malloc(limit * sizeof(double));
    double* rlist   = (double*)malloc(limit * sizeof(double));

    int* iwork      = (int*)malloc(limit * sizeof(int));

    double epsabs   = fabs(pdEpsA);
    double epsrel   = fabs(pdEpsR);

// *** Perform operation. ***
    int ier = 0;
    C2F(dqags)(intg_f, &pdA, &pdB, &epsabs, &epsrel, alist, blist, elist, rlist, &limit, iwork, &limit, &result, &abserr, &ier);

    free(alist);
    free(blist);
    free(elist);
    free(rlist);
    free(iwork);
    DifferentialEquation::removeDifferentialEquationFunctions();

    if(ier)
    {
        switch(ier)
        {
            case 1 :
            {
                Scierror(999, _("%s: Maximum number of subdivisions allowed has been achieved.\n"), "intg");
                return types::Function::Error;
            }
            case 2 :
            {
                if(getWarningMode())
                {
                    sciprint(_("%ls: Warning : The occurrence of roundoff error is detected, which prevents the requested tolerance from being achieved. The error may be under-estimated.\n"),L"intg");
                }
                break;
            }
            case 3 :
            {
                Scierror(999, _("%s: Extremely bad integrand behaviour occurs at some points of the integration interval.\n"), "intg");
                return types::Function::Error;
            }
            case 4 :
            {
                if(getWarningMode())
                {
                    sciprint(_("%ls: Warning : The algorithm does not converge. Roundoff error is detected in the extrapolation table. It is presumed that the requested tolerance cannot be achieved, and that the returned result is the best which can be obtained.\n"),L"intg");
                }
                break;
            }
            case 5 :
            {
                Scierror(999, _("%s: The integral is probably divergent, or slowly convergent.\n"), "intg");
                return types::Function::Error;
            }
        }
    }

// *** Return result in Scilab. ***
    types::Double* pDblOut = new types::Double(result);
    out.push_back(pDblOut);

    if(_iRetCount == 2)
    {
        types::Double* pDblErrOut = new types::Double(abserr);
        out.push_back(pDblErrOut);
    }

    return types::Function::OK;
}
/*--------------------------------------------------------------------------*/

