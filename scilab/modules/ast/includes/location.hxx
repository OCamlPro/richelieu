/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2007-2008 - DIGITEO - Bruno JOFRET
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

/**
** \file location.hxx
** Define the Location class.
*/

#ifndef __LOCATION_HXX__
#define __LOCATION_HXX__

#include <iostream>
#include <string>
#include <sstream>

#include "dynlib_ast_tools.hxx"

/** \brief Abstract a Location. */
class EXTERN_AST_TOOLS Location
{
    /** \name Ctor & dtor.
    ** \{ */
public:
    /** \brief Construct a Location. */
    Location (void)
    {
        first_line = last_line = 1;
        first_column = last_column = 1;
    }
    /** \} */

    Location* clone()
    {
        Location* ret = new Location();
        ret->first_line = first_line;
        ret->first_column = first_column;
        ret->last_line = last_line;
        ret->last_column = last_column;
        return ret;
    }

    /** \name Line and Column related manipulators
    ** \{ */
public:
    /** \brief Reset initial location to final location. */
    inline void step (void)
    {
        first_line = last_line;
        first_column = last_column;
    }

    /** \brief Extend the current location to the COUNT next columns. */
    inline void columns (unsigned int count = 1)
    {
        last_column += count;
    }

    /** \brief Extend the current location to the COUNT next lines. */
    inline void lines (unsigned int count = 1)
    {
        last_line += count;
    }
    /** \} */

    std::wstring location_getString() const
    {
        std::wostringstream os;
        os << L" (" << first_line << L"," << first_column << L")";
        return os.str();
    }

public:
    /** \brief Beginning of the located region. */
    int first_line;
    int first_column;
    /** \brief End of the located region. */
    int last_line;
    int last_column;
};

#endif //! __LOCATION_HXX__
