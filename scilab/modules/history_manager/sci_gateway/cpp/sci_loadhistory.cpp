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

#include "history_manager_gw.hxx"
#include "HistoryManager.hxx"
#include "function.hxx"
#include "string.hxx"

extern "C"
{
#include "MALLOC.h"
#include "localization.h"
#include "Scierror.h"
#include "charEncoding.h"
#include "expandPathVariable.h"
}
/*--------------------------------------------------------------------------*/

types::Function::ReturnValue sci_loadhistory(types::typed_list &in, int _iRetCount, types::typed_list &out)
{
    if(in.size() == 0)
    {
		char* pstFilename = HistoryManager::getInstance()->getFilename();
        if(pstFilename)
        {
            BOOL bOK = HistoryManager::getInstance()->loadFromFile(pstFilename);
            if(!bOK)
            {
                Scierror(999, _("%s: loadScilabHistoryFromFile failed.\n"), "loadhistory");
                return types::Function::Error;
            }
            FREE(pstFilename);
        }
        else
        {
            Scierror(999, _("%s: getFilenameScilabHistory failed.\n"), "loadhistory");
            return types::Function::Error;
        }
    }
    else if(in.size() == 1)
    {
        wchar_t* pwstFilename = NULL;

        if((in[0]->isString() == false) || (in[0]->getAs<types::String>()->isScalar() == false))
        {
            Scierror(999, _("%s: Wrong type for input argument #%d: A string expected.\n"), "loadhistory", 1);
            return types::Function::Error;
        }

        pwstFilename = expandPathVariableW(in[0]->getAs<types::String>()->get(0));
        if(pwstFilename)
        {
            char* pstFilename = wide_string_to_UTF8(pwstFilename);
            if(pstFilename)
            {
                BOOL bOK = HistoryManager::getInstance()->loadFromFile(pstFilename);
                if(!bOK)
                {
                    Scierror(999, _("%s: Load Scilab history from file failed.\n"), "loadhistory");
                    return types::Function::Error;
                }
                FREE(pstFilename);
            }
            FREE(pwstFilename);
        }
        else
        {
            Scierror(999, _("%s: expandPathVariableW failed.\n"), "loadhistory");
            return types::Function::Error;
        }
    }
    else
    {
        Scierror(77, _("%s: Wrong number of input argument(s): %d to %d expected.\n"), "loadhistory", 0, 1);
        return types::Function::Error;
    }
    return types::Function::OK;
}
/*--------------------------------------------------------------------------*/

