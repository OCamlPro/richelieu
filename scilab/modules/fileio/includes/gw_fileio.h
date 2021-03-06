/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2006 - INRIA - Allan CORNET
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */
/*--------------------------------------------------------------------------*/
#ifndef __GW_FILEIO_H__
#define __GW_FILEIO_H__
#include "dynlib_fileio.h"
#include "c_gateway_prototype.h"
/*--------------------------------------------------------------------------*/
FILEIO_IMPEXP int gw_fileio(void);
/*--------------------------------------------------------------------------*/
C_GATEWAY_PROTOTYPE(sci_pwd);
C_GATEWAY_PROTOTYPE(sci_chdir);
C_GATEWAY_PROTOTYPE(sci_copyfile);
C_GATEWAY_PROTOTYPE(sci_movefile);
C_GATEWAY_PROTOTYPE(sci_fscanfMat);
C_GATEWAY_PROTOTYPE(sci_fprintfMat);

/*--------------------------------------------------------------------------*/
#endif /* __GW_FILEIO_H__ */
/*--------------------------------------------------------------------------*/
