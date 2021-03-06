/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) INRIA - Allan CORNET
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

/*--------------------------------------------------------------------------*/
#include <string.h>
#include "gw_dynamic_link.h"
#include "api_scilab.h"
#include "MALLOC.h"
#include "callFunctionFromGateway.h"
/*--------------------------------------------------------------------------*/
static gw_generic_table Tab[] =
{
	{NULL, ""}, //getdynlibext
	{NULL, ""}, //addinter
	{sci_fort,"fort"},
	{sci_call,"call"},
	{NULL ,""}, //link
	{NULL, ""}, //ulink
	{NULL, ""}, //c_link
	{NULL, ""} // ilib_verbose
};
/*--------------------------------------------------------------------------*/
int gw_dynamic_link(void)
{
	return 0;
}
/*--------------------------------------------------------------------------*/
