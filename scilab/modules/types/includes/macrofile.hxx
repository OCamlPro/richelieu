/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2009-2010 - DIGITEO - Bruno JOFRET
 *  Copyright (C) 2009-2009 - DIGITEO - Antoine ELIAS
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#ifndef __MACROFILE_HXX__
#define __MACROFILE_HXX__

#include "callable.hxx"
#include "macro.hxx"

namespace types
{
    class MacroFile : public Callable
    {
    public :
                                MacroFile(): Callable(){};
                                MacroFile(wstring _stName, wstring _stPath, wstring _stModule);
        virtual                 ~MacroFile();

        //FIXME : Should not return NULL
        InternalType*           clone();

        RealType                getType(void);
        bool                    isMacroFile() { return true; }

        void                    whoAmI();

        bool                    toString(std::wostringstream& ostr);

        Callable::ReturnValue   call(typed_list &in, optional_list &opt, int _iRetCount, typed_list &out, ast::ConstVisitor* execFunc);
        bool                    parse(void);

        Macro*                  getMacro(void){return m_pMacro;}

        /* return type as string ( double, int, cell, list, ... )*/
        virtual wstring         getTypeStr() {return L"macrofile";}
        /* return type as short string ( s, i, ce, l, ... )*/
        virtual wstring         getShortTypeStr() {return L"function";}

    private :
        Macro*                  m_pMacro;
        wstring                 m_stPath;
    };
}


#endif /* !__MACRO_HXX__ */
