/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2009-2010 - DIGITEO - Antoine ELIAS
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#ifndef __LISTUNDEFINED_HXX__
#define __LISTUNDEFINED_HXX__

#include "types.hxx"
#include "listoperation.hxx"
#include "dynlib_types.h"

namespace types
{
    class TYPES_IMPEXP ListUndefined : public ListOperation
    {
    public :
                                ListUndefined(): ListOperation(){};
        virtual                 ~ListUndefined();

        // FIXME : Should not return NULL;
        InternalType*           clone();

        RealType                getType(void);
        bool                    isListUndefined() { return true; }

        void                    whoAmI();

        bool                    toString(std::wostringstream& ostr);

        /* return type as string ( double, int, cell, list, ... )*/
        virtual std::wstring    getTypeStr() {return L"listundefined";}
        /* return type as short string ( s, i, ce, l, ... )*/
        virtual std::wstring    getShortTypeStr() {return L"";}
    };
}


#endif /* !__LISTUNDEFINED_HXX__ */
