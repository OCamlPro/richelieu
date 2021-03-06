/*
* Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
* Copyright (//) 2011 - DIGITEO - Cedric DELAMARRE
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
}

/*--------------------------------------------------------------------------*/
types::Function::ReturnValue sci_bvode(types::typed_list &in, int _iRetCount, types::typed_list &out)
{
    int iPos = 0;

    // input
    types::Double* pDblXpts     = NULL;
    types::Double* pDblM        = NULL;
    types::Double* pDblZeta     = NULL;
    types::Double* pDblIpar     = NULL;
    types::Double* pDblLtol     = NULL;
    types::Double* pDblTol      = NULL;
    types::Double* pDblFixpnt   = NULL;

    double aleft    = 0;
    double aright   = 0;
    int ncomp       = 0;
    int sumM        = 0;
    int maxM        = 0;
    int iflag       = 0;
    int ipar[11];

// *** check the minimal number of input args. ***
    if(in.size() != 15)
    {
        Scierror(77, _("%s: Wrong number of input argument(s): %d expected.\n"), "bvode", 15);
        return types::Function::Error;
    }

// *** check number of output args ***
    if(_iRetCount > 1)
    {
        Scierror(78, _("%s: Wrong number of output argument(s): %d expected.\n"), "bvode", 1);
        return types::Function::Error;
    }

// *** check type of input args and get it. ***

    // xpoints
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "bvode", iPos+1);
        return types::Function::Error;
    }

    pDblXpts = in[iPos]->getAs<types::Double>();
    iPos++;

    // ncomp
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A scalar expected.\n"), "bvode", iPos+1);
        return types::Function::Error;
    }

    types::Double* pDblN = in[iPos]->getAs<types::Double>();

    if(pDblN->isScalar() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A scalar expected.\n"), "bvode", iPos+1);
        return types::Function::Error;
    }

    ncomp = (int)pDblN->get(0);

    if(ncomp > 20)
    {
        Scierror(999, _("%s: Wrong value for input argument #%d : Value at most 20 expected.\n"), "bvode", iPos+1);
        return types::Function::Error;
    }
    iPos++;

    // m
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "bvode", iPos+1);
        return types::Function::Error;
    }

    pDblM = in[iPos]->getAs<types::Double>();

    if(pDblM->getSize() != ncomp)
    {
        Scierror(999, _("%s: Wrong size for input argument #%d : A vector of size %d (N) expected.\n"), "bvode", iPos+1, ncomp);
        return types::Function::Error;
    }

    int* M = (int*)malloc(pDblM->getSize() * sizeof(int));
    for(int i = 0; i < pDblM->getSize(); i++)
    {
        M[i] = (int)pDblM->get(i);
        sumM += (int)pDblM->get(i);
        maxM = Max(maxM, (int)pDblM->get(i));
    }

    if(sumM > 40)
    {
        Scierror(999, _("%s: Wrong value for input argument #%d : Sum of m must be less than 40.\n"), "bvode", iPos+1, sumM);
        free(M);
        return types::Function::Error;
    }

    iPos++;

    // aleft
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A scalar expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    types::Double* pDblXLow = in[iPos]->getAs<types::Double>();

    if(pDblXLow->isScalar() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A scalar expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    aleft = pDblXLow->get(0);
    iPos++;

    // aright
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A scalar expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    types::Double* pDblXUp = in[iPos]->getAs<types::Double>();

    if(pDblXUp->isScalar() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A scalar expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    aright = pDblXUp->get(0);
    iPos++;

    // zeta
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    pDblZeta = in[iPos]->getAs<types::Double>();

    for(int i = 0; i < pDblZeta->getSize() - 1; i++)
    {
        if(pDblZeta->get(i) > pDblZeta->get(i+1))
        {
            Scierror(999, _("%s: Wrong value for input argument #%d : zeta(j) lower or equal to zeta(j+1) expected.\n"), "bvode", iPos+1);
            free(M);
            return types::Function::Error;
        }
    }

    iPos++;

    // ipar
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    pDblIpar = in[iPos]->getAs<types::Double>();

    if(pDblIpar->getSize() != 11)
    {
        Scierror(999, _("%s: Wrong size for input argument #%d : A vector of size 11 expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    for(int i = 0; i < 11; i++)
    {
       ipar[i] = (int)pDblIpar->get(i);
    }

    if(ipar[1] < maxM || ipar[1] > 7)
    {
        ipar[1] = Max(maxM + 1, 5 - maxM);
    }

    if(ipar[2] == 0)
    {
        ipar[2] = 5;
    }

    iPos++;

    // ltol
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    pDblLtol = in[iPos]->getAs<types::Double>();

    if(pDblLtol->getSize() != ipar[3])
    {
        Scierror(999, _("%s: Wrong size for input argument #%d : An array of size %d (ipar(4)) expected.\n"), "bvode", iPos+1, ipar[3]);
        free(M);
        return types::Function::Error;
    }

    iPos++;

    // tol
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    pDblTol = in[iPos]->getAs<types::Double>();

    if(pDblTol->getSize() != ipar[3])
    {
        Scierror(999, _("%s: Wrong size for input argument #%d : An array of size %d (ipar(4)) expected.\n"), "bvode", iPos+1, ipar[3]);
        free(M);
        return types::Function::Error;
    }

    iPos++;

    // fixpnt
    if(in[iPos]->isDouble() == false)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d : A matrix expected.\n"), "bvode", iPos+1);
        free(M);
        return types::Function::Error;
    }

    pDblFixpnt = in[iPos]->getAs<types::Double>();

    if(pDblFixpnt->getSize() != ipar[10] && ipar[10] != 0)
    {
        Scierror(999, _("%s: Wrong size for input argument #%d : An array of size %d (ipar(11)) expected.\n"), "bvode", iPos+1, ipar[10]);
        free(M);
        return types::Function::Error;
    }

    iPos++;

    // functions : fsub,dfsub,gsub,dgsub,guess
    DifferentialEquationFunctions* deFunctionsManager = new DifferentialEquationFunctions(L"bvode");
    DifferentialEquation::addDifferentialEquationFunctions(deFunctionsManager);
    deFunctionsManager->setBvodeM(sumM);
    deFunctionsManager->setBvodeN(ncomp);

    for(int i = iPos; i < 15; i++)
    {
        if(in[i]->isCallable())
        {
            types::Callable* pCall = in[i]->getAs<types::Callable>();
            if(i == 10) // fsub
            {
                deFunctionsManager->setFsubFunction(pCall);
            }
            else if(i == 11) // dfsub
            {
                deFunctionsManager->setDfsubFunction(pCall);
            }
            else if(i == 12) // gsub
            {
                deFunctionsManager->setGsubFunction(pCall);
            }
            else if(i == 13) // dgsub
            {
                deFunctionsManager->setDgsubFunction(pCall);
            }
            else if(i == 14 && ipar[8] == 1) // guess is needed only if ipar(9) == 1
            {
                deFunctionsManager->setGuessFunction(pCall);
            }
        }
        else if(in[i]->isString())
        {
            bool bOK = false;
            types::String* pStr = in[i]->getAs<types::String>();
            if(i == 10) // fsub
            {
                bOK = deFunctionsManager->setFsubFunction(pStr);
            }
            else if(i == 11) // dfsub
            {
                bOK = deFunctionsManager->setDfsubFunction(pStr);
            }
            else if(i == 12) // gsub
            {
                bOK = deFunctionsManager->setGsubFunction(pStr);
            }
            else if(i == 13) // dgsub
            {
                bOK = deFunctionsManager->setDgsubFunction(pStr);
            }
            else if(i == 14 && ipar[8] == 1) // guess is needed only if ipar(9) == 1
            {
                bOK = deFunctionsManager->setGuessFunction(pStr);
            }

            if(bOK == false)
            {
                char* pst = wide_string_to_UTF8(pStr->get(0));
                Scierror(50, _("%s: Subroutine not found: %s\n"), "bvode", pst);
                FREE(pst);
                DifferentialEquation::removeDifferentialEquationFunctions();
                free(M);
                return types::Function::Error;
            }
        }
        else if(in[i]->isList())
        {
            types::List* pList = in[i]->getAs<types::List>();

            if(pList->getSize() == 0)
            {
                Scierror(50, _("%s: Argument #%d : Subroutine not found in list: %s\n"), "bvode", i+1, "(string empty)");
                DifferentialEquation::removeDifferentialEquationFunctions();
                free(M);
                return types::Function::Error;
            }

            if(pList->get(0)->isCallable())
            {
                if(i == 10) // fsub
                {
                    deFunctionsManager->setFsubFunction(pList->get(0)->getAs<types::Callable>());
                    for(int iter = 1; iter < pList->getSize(); iter++)
                    {
                        deFunctionsManager->setFsubArgs(pList->get(iter)->getAs<types::InternalType>());
                    }
                }
                else if(i == 11) // dfsub
                {
                    deFunctionsManager->setDfsubFunction(pList->get(0)->getAs<types::Callable>());
                    for(int iter = 1; iter < pList->getSize(); iter++)
                    {
                        deFunctionsManager->setDfsubArgs(pList->get(iter)->getAs<types::InternalType>());
                    }
                }
                else if(i == 12) // gsub
                {
                    deFunctionsManager->setGsubFunction(pList->get(0)->getAs<types::Callable>());
                    for(int iter = 1; iter < pList->getSize(); iter++)
                    {
                        deFunctionsManager->setGsubArgs(pList->get(iter)->getAs<types::InternalType>());
                    }
                }
                else if(i == 13) // dgsub
                {
                    deFunctionsManager->setDgsubFunction(pList->get(0)->getAs<types::Callable>());
                    for(int iter = 1; iter < pList->getSize(); iter++)
                    {
                        deFunctionsManager->setDgsubArgs(pList->get(iter)->getAs<types::InternalType>());
                    }
                }
                else if(i == 14 && ipar[8] == 1) // guess is needed only if ipar(9) == 1
                {
                    deFunctionsManager->setGuessFunction(pList->get(0)->getAs<types::Callable>());
                    for(int iter = 1; iter < pList->getSize(); iter++)
                    {
                        deFunctionsManager->setGuessArgs(pList->get(iter)->getAs<types::InternalType>());
                    }
                }
            }
            else
            {
                Scierror(999, _("%s: Wrong type for input argument #%d : The first argument in the list must be a Scilab function.\n"), "bvode", i+1);
                DifferentialEquation::removeDifferentialEquationFunctions();
                free(M);
                return types::Function::Error;
            }
        }
        else
        {
            Scierror(999, _("%s: Wrong type for input argument #%d : A function expected.\n"), "bvode", i+1);
            DifferentialEquation::removeDifferentialEquationFunctions();
            free(M);
            return types::Function::Error;
        }
    }

// *** check functions. ***

    int ret     = 0;
    int i       = 1;
    double x    = 1;
    double* z   = (double*)malloc(sumM * sizeof(double));
    double* d   = (double*)malloc(ncomp * sumM * sizeof(double)); // max size

    memset(z, 0x01, sumM);

    // fsub
    ret = bvode_fsub(&x, z, d);
    if(ret == 0)
    {
        Scierror(50, _("%s: Argument #%d : Execution of %s function failed.\n"), "bvode", 11, "fsub");
        free(d);
        free(z);
        DifferentialEquation::removeDifferentialEquationFunctions();
        return types::Function::Error;
    }

    // dfsub
    ret = bvode_dfsub(&x, z, d);
    if(ret == 0)
    {
        Scierror(50, _("%s: Argument #%d : Execution of %s function failed.\n"), "bvode", 12, "dfsub");
        free(d);
        free(z);
        DifferentialEquation::removeDifferentialEquationFunctions();
        return types::Function::Error;
    }

    // gsub
    ret = bvode_gsub(&i, z, d);
    if(ret == 0)
    {
        Scierror(50, _("%s: Argument #%d : Execution of %s function failed.\n"), "bvode", 13, "gsub");
        free(d);
        free(z);
        DifferentialEquation::removeDifferentialEquationFunctions();
        return types::Function::Error;
    }

    // dgsub
    ret = bvode_dgsub(&i, z, d);
    if(ret == 0)
    {
        Scierror(50, _("%s: Argument #%d : Execution of ls function failed.\n"), "bvode", 14, "dgsub");
        free(d);
        free(z);
        DifferentialEquation::removeDifferentialEquationFunctions();
        return types::Function::Error;
    }

    // guess
    ret = bvode_guess(&x, z, d);
    if(ret == 0)
    {
        Scierror(50, _("%s: Argument #%d : Execution of %s function failed.\n"), "bvode", 15, "guess");
        free(d);
        free(z);
        DifferentialEquation::removeDifferentialEquationFunctions();
        return types::Function::Error;
    }

    free(d);
    free(z);

// *** Perform operation. ***
    double* rwork   = (double*)malloc(ipar[4] * sizeof(double));
    int* iwork      = (int*)malloc(ipar[5] * sizeof(int));
    int* ltol       = (int*)malloc(pDblLtol->getSize() * sizeof(int));

    for(int i = 0; i < pDblLtol->getSize(); i++)
    {
        ltol[i] = (int)pDblLtol->get(i);
    }

    C2F(colnew)(&ncomp, M, &aleft, &aright, pDblZeta->get(), ipar, ltol, pDblTol->get(), pDblFixpnt->get(), iwork, rwork, &iflag, bvode_fsub, bvode_dfsub, bvode_gsub, bvode_dgsub, bvode_guess);

    if(iflag != 1)
    {
        if(iflag == 0)
        {
            Scierror(999, _("%s: The collocation matrix is singular.\n"), "bvode");
        }
        else if(iflag == -1)
        {
            Scierror(999, _("%s: The expected no. of subintervals exceeds storage specifications.\n"), "bvode");
        }
        else if(iflag == -2)
        {
            Scierror(999, _("%s: The nonlinear iteration has not converged.\n"), "bvode");
        }
        else if(iflag == -3)
        {
            Scierror(999, _("%s: There is an input data error.\n"), "bvode");
        }

        free(iwork);
        free(rwork);
        free(M);
        free(ltol);
        DifferentialEquation::removeDifferentialEquationFunctions();

        return types::Function::Error;
    }

    types::Double* pDblRes = new types::Double(sumM, pDblXpts->getSize());
    double* res = (double*)malloc(pDblXpts->getSize() * sumM * sizeof(double));
    for(int i = 0; i < pDblXpts->getSize(); i++)
    {
        double val = pDblXpts->get(i);
        C2F(appsln)(&val, &res[i * sumM], rwork, iwork);
    }

    pDblRes->set(res);

    free(iwork);
    free(rwork);
    free(M);
    free(ltol);
    free(res);
    DifferentialEquation::removeDifferentialEquationFunctions();

// *** Return result in Scilab. ***
    out.push_back(pDblRes);

    return types::Function::OK;
}
/*--------------------------------------------------------------------------*/

