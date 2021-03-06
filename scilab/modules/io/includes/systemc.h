/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2007 - INRIA - Sylvestre LEDRU
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */
#ifndef __SYSTEMC_H__
#define __SYSTEMC_H__

#include "dynlib_io.h"
/**
 * Execute a command by the system
 * @param[in] command the command
 * @param[out] the status of the operation
 */
IO_IMPEXP int systemc(char *command, int *stat);

/**
 * Execute a command by the system
 * @param[in] command the command
 * @param[out] the status of the operation
 */
IO_IMPEXP int systemcW(wchar_t* _pstCommand, int *stat);

#endif
