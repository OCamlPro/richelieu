/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2010-2010 - DIGITEO - Bruno JOFRET
 * 
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 * 
 */

#ifndef __CONTAINER_HXX__
#define __CONTAINER_HXX__

#include "types.hxx"
#include "dynlib_types.h"

namespace types
{
    class TYPES_IMPEXP Container : public GenericType
    {

    public :
                                Container() : GenericType() {}
        virtual                 ~Container() {}

        virtual void            whoAmI(void) { std::cout << "types::Container"; }

        virtual int             getSize() = 0 ;

        virtual bool            isAssignable(void) { return true; }

        virtual RealType        getType(void) { return RealContainer; }
        virtual bool            isContainer(void) { return true; }

        /* return type as string ( double, int, cell, list, ... )*/
        virtual std::wstring    getTypeStr() = 0;
        /* return type as short string ( s, i, ce, l, ... )*/
        virtual std::wstring    getShortTypeStr() = 0;
    };
}

#endif /* !__CONTAINER_HXX__ */
