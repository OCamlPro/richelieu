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

#ifndef __SCICAML_HH__
#define __SCICAML_HH__

#include "ast.hxx"
#include "exp.hxx"

extern char* scicaml_ast2string(const ast::Exp* e);
extern ast::Exp* scicaml_string2ast(char*);

// extern char* scicaml_analyze(char*);

#include "runvisitor.hxx"

// scicaml_visit() is supposed to exec a node of the AST
// and modify the runvisitor to set the result.

// action is either 
// 0 for exec
// 1 for print and exec
// 2 for time exec

extern int scicaml_visit(const ast::Exp &e, 
			  ast::RunVisitor *visitor, int action);

#endif /* __SCICAML_HH__ */
