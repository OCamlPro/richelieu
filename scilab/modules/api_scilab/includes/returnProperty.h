/*
 * Scilab (http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2006 - INRIA - Jean-Baptiste SILVY
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */
/*------------------------------------------------------------------------*/
/* file: returnProperty.h                                                 */
/* desc : a set of functions used to return values in Scilab              */
/*------------------------------------------------------------------------*/

#ifndef _RETURN_PROPERTY_H_
#define _RETURN_PROPERTY_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "dynlib_api_scilab.h"


/**
 * create an empty matrix in the scilab stack in order to see it in the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnEmptyMatrix();

/**
 * copy the string value in the scilab stack in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnString(const char * value ) ;

/**
 * copy a char value into a scilab string in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnChar(char value ) ;

/**
 * copy a single double in the scilab stack in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnDouble(double value ) ;

/**
 * copy a single int in the scilab stack in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnInt(int value ) ;

/**
 * copy a double array into a row vector in the scilab stack in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnRowVector(const double values[], int nbValues ) ;

/**
* copy a int array into a row vector (of double) in the scilab stack
* in order to see it the console.
* @return 0 if the function was executed correctly, -1 if an error occurred.
*/
void* sciReturnRowVectorFromInt(const int values[], int nbValues ) ;

/**
 * copy an int array into an int row vector in the scilab stack
 * in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnRowIntVector(const int values[], int nbValues ) ;

/**
 * copy a char * array into an string row vector in the scilab stack
 * in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnRowStringVector(char * values[], int nbValues ) ;

/**
 * copy a handle in the scilab stack in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnHandle(long handle ) ;

/**
 * copy an handle array into a row vector in the scilab stack in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnRowHandleVector(const long handles[], int nbValues ) ;

/**
* copy an handle array into a col vector in the scilab stack in order to see it the console.
* @return 0 if the function was executed correctly, -1 if an error occurred.
*/
void* sciReturnColHandleVector(const long handles[], int nbValues ) ;

/**
 * copy a matrix stored has a double array
 * into a Scilab matrix in the stack in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnMatrix(double values[], int nbRow, int nbCol ) ;

/**
 * copy a matrix stored has a char * array
 * into a Scilab string matrix in the stack in order to see it the console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnStringMatrix(char * values[], int nbRow, int nbCol ) ;

/**
 * return user data in the scilab console.
 * @return 0 if the function was executed correctly, -1 if an error occurred.
 */
void* sciReturnUserData(const int * userData, int userDataSize ) ;

#ifdef __cplusplus
}
#endif
#endif /* _RETURN_PROPERTY_H_ */
