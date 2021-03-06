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
#include "inspector.hxx"

extern "C"
{
#include "Scierror.h"
#include "localization.h"
}

using namespace types;

Function::ReturnValue sci_inspectorDeleteUnreferencedItems(typed_list &in, int _iRetCount, typed_list &out)
{
    if(in.size() != 0)
    {
        Scierror(999, _("%s: Wrong number of input arguments: %d expected.\n"), "inspectorDeleteUnreferencedItems", 0);    
        return Function::Error;
    }

    while(Inspector::getUnreferencedItemCount() != 0)
    {
        delete Inspector::getUnreferencedItem(0);
    }

    return Function::OK;
}
