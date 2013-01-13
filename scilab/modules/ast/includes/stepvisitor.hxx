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

#ifndef __AST_STEPVISITOR_HXX__
#define __AST_STEPVISITOR_HXX__

#include <iostream>

#include "runvisitor.hxx"
#include "execvisitor.hxx"
#include "printvisitor.hxx"


// print steps of seqexp and exec
#define SCICAML_EXEC_ACTION 1

namespace ast
{
	class StepVisitor : public RunVisitorT<StepVisitor>
	{
        void visit(const SeqExp &e)
        {
            std::list<Exp*>::const_iterator it;
            for (it = e.exps_get().begin() ; it != e.exps_get().end() ; ++it)
            {
                PrintVisitor *pv = new PrintVisitor(std::wcerr);
                (*it)->accept(*pv);
                std::wcerr << std::endl;
                //ExecVisitor *pe = new ExecVisitor();
                //(*it)->accept(*pe);
            }
            exec_visitprivate(e);
        }

        void visit (const MatrixExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const MatrixLineExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const CellExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const StringExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const CommentExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const IntExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const FloatExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const DoubleExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const BoolExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const NilExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const SimpleVar &e)
        {
            exec_visitprivate(e);
        }

        void visit (const ColonVar &e)
        {
            exec_visitprivate(e);
        }

        void visit (const DollarVar &e)
        {
            exec_visitprivate(e);
        }

        void visit (const ArrayListVar &e)
        {
            exec_visitprivate(e);
        }

        void visit (const FieldExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const OpExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const LogicalOpExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const AssignExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const CellCallExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const CallExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const IfExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const TryCatchExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const WhileExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const ForExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const BreakExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const ContinueExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const ReturnExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const SelectExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const CaseExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const ArrayListExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const AssignListExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const NotExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const TransposeExp &e)
        {
            exec_visitprivate(e);
        }

        void visit (const VarDec &e)
        {
            exec_visitprivate(e);
        }

        void visit (const FunctionDec &e)
        {
            exec_visitprivate(e);
        }

        void visit(const ListExp &e)
        {
            exec_visitprivate(e);
        }
    };
}

#endif /* !__AST_STEPVISITOR_HXX__ */
