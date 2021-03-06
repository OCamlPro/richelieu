/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2006 - INRIA - Jean-Baptiste Silvy
 * Copyright (C) 2012 - scilab-Enterprises - Cedric DELAMARRE
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

/*------------------------------------------------------------------------*/
/* file: sci_relocate_handle.h                                            */
/* desc : interface for relocate_handle routine                           */
/*------------------------------------------------------------------------*/

#include "gw_graphics.h"
#include "setGraphicObjectProperty.h"
#include "api_scilab.h"
#include "MALLOC.h"
#include "HandleManagement.h"
#include "localization.h"
#include "Scierror.h"
/*--------------------------------------------------------------------------*/

int sci_relocate_handle(char * fname, void *pvApiCtx)
{
    SciErr sciErr;

    int* piAddrhandleStkIndex = NULL;
    long long* handleStkIndex = NULL;
    int* piAddrparentStkIndex = NULL;
    long long* parentStkIndex = NULL;
    long long* outIndex = NULL;

    int handleCol  = 0;
    int handleRow = 0;
    int nbHandle = 0;
    int parentCol = 0;
    int parentRow = 0;
    int i = 0;

    long children = 0;
    char* childrenID = NULL;
    long parent = 0;
    char* parentID = NULL;

    /* the function should be called with relocate_handle(handle, parent_handle) */
    CheckInputArgument(pvApiCtx, 2, 2);
    CheckOutputArgument(pvApiCtx, 0, 1);


    sciErr = getVarAddressFromPosition(pvApiCtx,  1, &piAddrhandleStkIndex);
    if (sciErr.iErr)
    {
        printError(&sciErr, 0);
        return 1;
    }

    // Retrieve a matrix of handle at position  1.
    sciErr = getMatrixOfHandle(pvApiCtx, piAddrhandleStkIndex, &handleRow, &handleCol, &handleStkIndex);
    if (sciErr.iErr)
    {
        printError(&sciErr, 0);
        Scierror(202, _("%s: Wrong type for argument %d: Handle matrix expected.\n"), fname,  1);
        return 1;
    }

    nbHandle = handleRow * handleCol;
    sciErr = getVarAddressFromPosition(pvApiCtx,  2, &piAddrparentStkIndex);
    if (sciErr.iErr)
    {
        printError(&sciErr, 0);
        return 1;
    }

    // Retrieve a matrix of handle at position  2.
    sciErr = getMatrixOfHandle(pvApiCtx, piAddrparentStkIndex, &parentRow, &parentCol, &parentStkIndex);
    if (sciErr.iErr)
    {
        printError(&sciErr, 0);
        Scierror(202, _("%s: Wrong type for argument %d: Handle matrix expected.\n"), fname,  2);
        return 1;
    }


    if (parentCol * parentRow != 1)
    {
        Scierror(999, _("%s: Handles must be relocated under a single parent.\n"), fname);
        return 0;
    }

    parent = (long) * (parentStkIndex);
    parentID = (char*)getObjectFromHandle(parent);

    for (i = 0; i < nbHandle; i++)
    {
        children = (long)handleStkIndex[i];
        childrenID = (char*)getObjectFromHandle(children);
        setGraphicObjectRelationship(parentID, childrenID);
    }

    sciErr = allocMatrixOfHandle(pvApiCtx,  nbInputArgument(pvApiCtx) + 1, handleCol, handleRow, &outIndex);
    if (sciErr.iErr)
    {
        printError(&sciErr, 0);
        Scierror(999, _("%s: Memory allocation error.\n"), fname);
        return 1;
    }

    *(outIndex) = *(handleStkIndex);
    AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
    ReturnArguments(pvApiCtx);
    return 0;

}
/*--------------------------------------------------------------------------*/
