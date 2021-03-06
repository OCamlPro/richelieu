/*
*  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
*  Copyright (C) 2008-2008 - DIGITEO - Antoine ELIAS
*  Copyright (C) 2010-2010 - DIGITEO - Bruno JOFRET
*
*  This file must be used under the terms of the CeCILL.
*  This source file is licensed as described in the file COPYING, which
*  you should have received as part of this distribution.  The terms
*  are also available at
*  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
*
*/

#ifndef __TYPES_SUB_H__
#define __TYPES_SUB_H__

#include "generic_operations.hxx"
#include "double.hxx"
#include "polynom.hxx"
#include "sparse.hxx"

// FIXME : remove using namespace
using namespace types;

int SubstractDoubleToDouble(Double* _pDouble1, Double* _pDouble2, Double **_pDoubleOut);
int SubstractPolyToDouble(Double *_pDouble, Polynom *_pPoly, Polynom **_pPolyOut);
int SubstractDoubleToPoly(Polynom *_pPoly, Double *_pDouble, Polynom **_pPolyOut);
int SubstractPolyToPoly(Polynom *_pPoly1, Polynom *_pPoly2, Polynom **_pPolyOut);

//Sparse
int SubstractSparseToSparse(Sparse* _pSparse1, Sparse* _pSparse2, GenericType **_pSparseOut);
int SubstractSparseToDouble(Sparse* _pSparse, Double* _pDouble, GenericType **_pDoubleOut);
int SubstractDoubleToSparse(Double* _pDouble, Sparse* _pSparse, GenericType **_pDoubleOut);

#endif /* __TYPES_SUB_H__ */
