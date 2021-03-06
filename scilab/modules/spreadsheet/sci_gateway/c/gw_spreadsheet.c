
/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2007-2008 - INRIA - Allan CORNET <allan.cornet@inria.fr>
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#include <string.h>
#include "gw_spreadsheet.h"
#include "api_scilab.h"
#include "MALLOC.h"
#include "callFunctionFromGateway.h"
/*--------------------------------------------------------------------------*/
static gw_generic_table Tab[] =
{
    {NULL, ""}, // "xls_open"
    {NULL, ""}, // "xls_read"
    {NULL, ""}, // "csvDefault"
    {NULL, ""}, // "csvIsnum"
    {NULL, ""}, // "csvRead"
    {NULL, ""}, // "csvStringToDouble"
    {NULL, ""}, // "csvTextScan"
    {NULL, ""}, // "csvWrite"
    {NULL, ""}, // "write_csv"
    {NULL, ""} // "read_csv"
};
/*--------------------------------------------------------------------------*/
int gw_spreadsheet(void)
{
	return 0;
}
/*--------------------------------------------------------------------------*/
