/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2012-2013 - OCAMLPRO INRIA - Fabrice LE FESSANT
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#ifndef __JIT_OCAML_HH__
#define __JIT_OCAML_HH__

#include "ast.hxx"
#include "exp.hxx"

extern char* scicaml_ast2string(ast::Exp*);
extern ast::Exp* scicaml_string2ast(char*);

#endif /* __JIT_OCAML_HH__ */
