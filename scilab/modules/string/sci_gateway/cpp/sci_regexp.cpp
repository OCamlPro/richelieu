/*
* Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
* Copyright (C) 2008 - INRIA - Cong WU
* Copyright (C) 2008 - 2009 - DIGITEO - Allan CORNET
*
* This file must be used under the terms of the CeCILL.
* This source file is licensed as described in the file COPYING, which
* you should have received as part of this distribution.  The terms
* are also available at
* http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
*
*/

/* desc : search position of a character string in an other string
using regular expression .                                      */
/*------------------------------------------------------------------------*/
#include "function.hxx"
#include "context.hxx"
#include "string.hxx"
#include "string_gw.hxx"

extern "C"
{
#include "localization.h"
#include "pcre.h"
#include "pcre_private.h"
#include "pcre_error.h"
#include "Scierror.h"
#include "charEncoding.h"
#include "os_strdup.h"
#include "freeArrayOfString.h"
}
/*------------------------------------------------------------------------*/
#define WCHAR_S L's'
#define WCHAR_R L'r'
#define WSTR_ONCE L'o'
/*------------------------------------------------------------------------*/

using namespace types;

Function::ReturnValue sci_regexp(typed_list &in, int _iRetCount, typed_list &out)
{
    wchar_t wcType          = WCHAR_S;
    wchar_t* pwstInput      = NULL;
    wchar_t* pwstPattern    = NULL;

    int iPcreStatus         = 0;
    int iStart              = 0;
    int iStep               = 0;
    int iEnd                = 0;
    int* piStart            = NULL;
    int* piEnd              = NULL;
    int iOccurs             = 0;

    /*for captured sub strings*/
    wchar_t*** pwstCapturedString = NULL;
    int* piCapturedStringCount = NULL;

    if (in.size() < 2 || in.size() > 3)
    {
        Scierror(999, _("%s: Wrong number of input arguments: %d or %d expected.\n"), "regexp", 2, 3);
        return Function::Error;
    }

    // check output parameters
    if (_iRetCount < 1 || _iRetCount > 4)
    {
        Scierror(999, _("%s: Wrong number of output arguments: %d to %d expected.\n"), "regexp", 1, 4);
        return Function::Error;
    }

    if (in[0]->isString() == false || in[0]->getAs<types::String>()->getSize() != 1)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d: Single string expected.\n"), "regexp", 1);
        return Function::Error;
    }
    pwstInput = in[0]->getAs<types::String>()->get(0);

    if (in[1]->isString() == false || in[1]->getAs<types::String>()->getSize() != 1)
    {
        Scierror(999, _("%s: Wrong type for input argument #%d: Single string expected.\n"), "regexp", 2);
        return Function::Error;
    }
    pwstPattern = in[1]->getAs<types::String>()->get(0);

    if (in.size() == 3)
    {
        if (in[2]->isString() == false || in[2]->getAs<types::String>()->getSize() != 1)
        {
            Scierror(999, _("%s: Wrong type for input argument #%d: Single string expected.\n"), "regexp", 3);
            return Function::Error;
        }

        if (in[2]->getAs<types::String>()->get(0)[0] != WSTR_ONCE)
        {
            Scierror(999, _("%s: Wrong type for input argument #%d: '%s' expected.\n"), "regexp", 3, "o");
            return Function::Error;
        }
        wcType = WSTR_ONCE;
    }

    //input is empty
    if (wcslen(pwstInput) == 0)
    {
        Double* pStart = new Double(0, 0);
        out.push_back(pStart);
        if (_iRetCount > 1)
        {
            Double* pEnd = new Double(0, 0);
            out.push_back(pEnd);

            if (_iRetCount > 2)
            {
                String* pS = new String(0, 0);
                out.push_back(pS);
            }
        }
        return Function::OK;
    }

    piStart     = new int[wcslen(pwstInput)];
    piEnd       = new int[wcslen(pwstInput)];

    pwstCapturedString = (wchar_t***)MALLOC(sizeof(wchar_t**) * wcslen(pwstInput));
    piCapturedStringCount = (int*)MALLOC(sizeof(int) * wcslen(pwstInput));

    do
    {
        iPcreStatus = wide_pcre_private(pwstInput + iStep, pwstPattern, &iStart, &iEnd, &pwstCapturedString[iOccurs], &piCapturedStringCount[iOccurs]);
        if (iPcreStatus == PCRE_FINISHED_OK && iStart != iEnd)
        {
            piStart[iOccurs]    = iStart + iStep;
            piEnd[iOccurs++]    = iEnd + iStep;
            iStep               += iEnd;
        }
        else if (iPcreStatus != NO_MATCH)
        {
            pcre_error("regexp", iPcreStatus);
            delete[] piStart;
            delete[] piEnd;
            return Function::Error;
        }
    }
    while (iPcreStatus == PCRE_FINISHED_OK && iStart != iEnd && wcType != WSTR_ONCE);

    if (iOccurs == 0)
    {
        out.push_back(Double::Empty());
        if (_iRetCount > 1)
        {
            out.push_back(Double::Empty());
        }

        if (_iRetCount > 2)
        {
            out.push_back(new String(L""));
        }

        if (_iRetCount > 3)
        {
            out.push_back(new String(L""));
        }
        return Function::OK;
    }

    Double* pStart = new Double(1, iOccurs);
    double* pdblStart = pStart->getReal();

    for (int i = 0 ; i < iOccurs ; i++)
    {
        pdblStart[i] = piStart[i] + 1; //one indexed
    }

    out.push_back(pStart);

    if (_iRetCount > 1)
    {
        Double* pEnd = new Double(1, iOccurs);
        double* pdblEnd = pEnd->getReal();
        for (int i = 0 ; i < iOccurs ; i++)
        {
            pdblEnd[i]   = piEnd[i];
        }
        out.push_back(pEnd);
    }

    if (_iRetCount > 2)
    {
        String *pS = NULL;
        if (iOccurs == 0)
        {
            pS = new String(1, 1);
            pS->set(0, L"");
        }
        else
        {
            pS = new String(iOccurs, 1);
            for (int i = 0 ; i < iOccurs ; i++)
            {
                wchar_t* pwstTemp = new wchar_t[piEnd[i] - piStart[i] + 1];
                wcsncpy(pwstTemp, pwstInput + piStart[i], piEnd[i] - piStart[i]);
                pwstTemp[piEnd[i] - piStart[i]] = 0;
                pS->set(i, 0, pwstTemp);
                delete[] pwstTemp;
            }
        }
        out.push_back(pS);
    }

    if(_iRetCount > 3)
    {
        //find max occurrences
        int iMax = 0;
        for(int i = 0 ; i < iOccurs ; i++)
        {
            iMax = Max(iMax, piCapturedStringCount[i]);
        }

        String* pS = NULL;
        if(iOccurs == 0 || iMax == 0)
        {
            pS = new String(L"");
        }
        else
        {
            int index = 0;
            pS = new String(iOccurs, iMax);
            for(int i = 0 ; i < iMax ; i++)
            {
                for(int j = 0 ; j < iOccurs ; j++)
                {
                    if(i < piCapturedStringCount[j])
                    {
                        pS->set(index, pwstCapturedString[j][i]);
                    }
                    else
                    {
                        pS->set(index, L"");
                    }

                    index++;
                }
            }

        }
        out.push_back(pS);

        for(int i = 0 ; i < iOccurs ; i++)
        {
            freeArrayOfWideString(pwstCapturedString[i], piCapturedStringCount[i]);
        }

        FREE(pwstCapturedString);
        FREE(piCapturedStringCount);
    }

    delete[] piStart;
    delete[] piEnd;
    return Function::OK;
}
/*-----------------------------------------------------------------------------------*/
