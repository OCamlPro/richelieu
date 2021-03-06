/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2010-2011 - DIGITEO - Bruno JOFRET
 *  Copyright (C) 2011 - DIGITEO - Cedric DELAMARRE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#ifndef __ACTION_BINDING_GW_HXX__
#define __ACTION_BINDING_GW_HXX__

#include "cpp_gateway_prototype.hxx"

extern "C"
{
    #include "dynlib_action_binding_gw.h"
}

CPP_GATEWAY_PROTOTYPE_EXPORT(sci_notify, ACTION_BINDING_GW_IMPEXP);

#endif /* !__ACTION_BINDING_GW_HXX__ */
