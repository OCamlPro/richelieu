/*
* Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
* Copyright (C) 2009 - DIGITEO - Antoine ELIAS
*
* This file must be used under the terms of the CeCILL.
* This source file is licensed as described in the file COPYING, which
* you should have received as part of this distribution.  The terms
* are also available at
* http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
*
* Please note that piece of code will be rewrited for the Scilab 6 family
* However, the API (profile of the functions in the header files) will be
* still available and supported in Scilab 6.
*/

#include <vector>
#include <map>
#include <string>
#include "list.hxx"

extern "C"
{
#include <stdio.h>
#include <string.h>
#include "call_scilab.h"
#include "api_scilab.h"
#include "api_internal_common.h"
#include "api_internal_double.h"
#include "api_internal_poly.h"
#include "api_internal_int.h"
#include "api_internal_sparse.h"
#include "api_internal_boolean_sparse.h"
#include "api_internal_pointer.h"
#include "localization.h"
}

//internal functions
static SciErr createCommonList(void* _pvCtx, int _iVar, int _iListType, int _iNbItem, int** _piAddress);
static SciErr createCommonListInList(void* _pvCtx, int _iVar, const char* _pstName, int* _piParent, int _iItemPos, int _iListType, int _iNbItem, int** _piAddress, int iNamed);
static SciErr createCommonNamedList(void* _pvCtx, const char* _pstName, int _iListType, int _iNbItem, int** _piAddress);
static SciErr createCommonListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iListType, int _iNbItem, int** _piAddress);
static SciErr getCommonListInList(void* _pvCtx, int* _piParent, int _iItemPos, int _iListType, int** _piAddress);
static SciErr getCommomListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iListType, int** _piAddress);
static SciErr readCommonNamedList(void* _pvCtx, const char* _pstName, int _iListType, int* _piNbItem, int** _piAddress);
static SciErr fillCommonList(void* _pvCtx, int* _piAddress, int _iListType, int _iNbItem);
static int isKindOfList(int* _piNode);
static int getParentList(void* _pvCtx, int* _piStart, int* _piToFind, int* _piDepth, int** _piParent);
static void closeList(int _iVar, int *_piEnd);
static void updateListOffset(void* _pvCtx, int _iVar, int *_piCurrentNode, int _iItemPos, int *_piEnd);
static void updateNamedListOffset(void* _pvCtx, int _iVar, const char* _pstName, int *_piCurrentNode, int _iItemPos, int *_piEnd);
static void updateCommunListOffset(void* _pvCtx, int _iVar, const char* _pstName, int *_piCurrentNode, int _iItemPos, int *_piEnd);

static SciErr allocCommonMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iComplex, int _iRows, int _iCols, double **_pdblReal, double **_pdblImg);
static SciErr getCommonMatrixOfDoubleInList(void* _pvCtx, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, double** _pdblReal, double** _pdblImg);
static SciErr createCommonMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iComplex, int _iRows, int _iCols, const double* _pdblReal, const double* _pdblImg);
static SciErr fillCommonMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iComplex, int _iRows, int _iCols, double** _pdblReal, double** _pdblImg);
static SciErr createCommomMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int _iRows, int _iCols, const double* _pdblReal, const double* _pdblImg);
static SciErr readCommonMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, double* _pdblReal, double* _pdblImg);
static SciErr allocCommonItemInList(void* _pvCtx, int* _piParent, int _iItemPos, int** _piChildAddr);
static SciErr fillMatrixOfBoolInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, int** _piBool);
static SciErr getCommonMatrixOfPolyInList(void* _pvCtx, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, int* _piNbCoef, double** _pdblReal, double** _pdblImg);
static SciErr createCommonMatrixOfPolyInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, char* _pstVarName, int _iComplex, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal, const double* const* _pdblImg);
static SciErr createCommonMatrixOfPolyInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, char* _pstVarName, int _iComplex, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal, const double* const* _pdblImg);
static SciErr readCommonMatrixOfPolyInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, int* _piNbCoef, double** _pdblReal, double** _pdblImg);
static SciErr fillCommonMatrixOfPolyInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, char* _pstVarName, int _iComplex, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal, const double* const* _pdblImg, int* _piTotalLen);
static SciErr fillCommonMatrixOfStringInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const char* const* _pstStrings, int* _piTotalLen);
static SciErr readCommonSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, int* _piNbItem, int* _piNbItemRow, int* _piColPos, double* _pdblReal, double* _pdblImg);
static SciErr createCommonSparseMatrixInList(void* _pvCtx, int _iVar, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal, const double* _pdblImg);
static SciErr createCommonSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal, const double* _pdblImg);

struct ListInfo
{
    ListInfo(int* _piAddr, int _iLast) : m_piAddr(_piAddr), m_iLast(_iLast) {};
    int* m_piAddr;
    int m_iLast;
};

typedef std::vector<ListInfo*> VectListInfo;
typedef std::map<int, VectListInfo*> StackList;
typedef std::map<std::string, VectListInfo*> StackNamedList;
static StackList stackListPosition;
static StackNamedList stackNamedListPosition;


// Push a list address on the stackListPosition
static void pushListAddress(int _iRhsPos, int* _piAddr)
{
    if (stackListPosition.find(_iRhsPos) == stackListPosition.end())
    {
        VectListInfo* pVect = new VectListInfo();
        pVect->push_back(new ListInfo(_piAddr, 0));
        stackListPosition[_iRhsPos] = pVect;
    }
    else
    {
        stackListPosition[_iRhsPos]->push_back(new ListInfo(_piAddr, 0));
    }
}

static void pushNamedListAddress(std::string _stNamedList, int* _piAddr)
{
    if (stackNamedListPosition.find(_stNamedList) == stackNamedListPosition.end())
    {
        VectListInfo* pVect = new VectListInfo();
        pVect->push_back(new ListInfo(_piAddr, 0));
        stackNamedListPosition[_stNamedList] = pVect;
    }
    else
    {
        stackNamedListPosition[_stNamedList]->push_back(new ListInfo(_piAddr, 0));
    }
}

// Pop a list address from the stackListPosition
static void popListAddress(int _iRhsPos)
{
    StackList::iterator it = stackListPosition.find(_iRhsPos);
    if (it != stackListPosition.end())
    {
        delete it->second->back();
        stackListPosition[_iRhsPos]->pop_back();
        if(it->second->size() > 0 && it->second->back()->m_iLast == 1)
        {//close cascade
            popListAddress(_iRhsPos);
        }

        if(stackListPosition.find(_iRhsPos) != stackListPosition.end() && stackListPosition[_iRhsPos]->empty())
        {
            stackListPosition.erase(stackListPosition.find(_iRhsPos));
            //TODO : check to close list
        }
    }
}

static void popNamedListAddress(std::string _stNamedList)
{
    StackNamedList::iterator it = stackNamedListPosition.find(_stNamedList);
    // FIXME
}

/*get last store address*/
int* getLastListAddress(int _iRhsPos, int _iItemPos)
{
    StackList::iterator it = stackListPosition.find(_iRhsPos);
    if (it == stackListPosition.end() || it->second->empty())
    {
        return NULL;
    }

    if(_iItemPos == it->second->back()->m_piAddr[1])
    {
        it->second->back()->m_iLast = 1;
    }

    return it->second->back()->m_piAddr;
}

static int* getLastNamedListAddress(std::string _stNamedList, int _iItemPos)
{
    StackNamedList::iterator it = stackNamedListPosition.find(_stNamedList);
    if (it == stackNamedListPosition.end() || it->second->empty())
    {
        return NULL;
    }

    if (_iItemPos == it->second->back()->m_piAddr[1])
    {
        it->second->back()->m_iLast = 1;
    }

    return it->second->back()->m_piAddr;
}


//get address list
static void getListAdressses(int _iRhsPos, int** _piAddresses)
{
    StackList::iterator it = stackListPosition.find(_iRhsPos);
    if (it == stackListPosition.end() || it->second->empty() || _piAddresses == NULL)
    {
        return;
    }

    VectListInfo::iterator vit;
    int i = 0;
    for (vit = it->second->begin() ; vit != it->second->end() ; vit++, i++)
    {
        _piAddresses[i] = (*vit)->m_piAddr;
    }
}

static void getNamedListAdressses(std::string _stName, int** _piAddresses)
{
    StackNamedList::iterator it = stackNamedListPosition.find(_stName);
    if (it == stackNamedListPosition.end() || it->second->empty() || _piAddresses == NULL)
    {
        return;
    }

    VectListInfo::iterator vit;
    int i = 0;
    for (vit = it->second->begin() ; vit != it->second->end() ; vit++, i++)
    {
        _piAddresses[i] = (*vit)->m_piAddr;
    }
}

//get Depth of list
static int getDepthList(int _iRhsPos)
{
    StackList::iterator it = stackListPosition.find(_iRhsPos);
    if (it == stackListPosition.end() || it->second->empty())
    {
        return 0;
    }
    return (int)it->second->size();
}

static int getDepthNamedList(std::string _stNamedList)
{
    StackNamedList::iterator it = stackNamedListPosition.find(_stNamedList);
    if (it == stackNamedListPosition.end() || it->second->empty())
    {
        return 0;
    }
    return (int)it->second->size();
}

char* getListTypeName(int _iType)
{
    switch (_iType)
    {
        case sci_list :
            return "list";
            break;
        case sci_tlist :
            return "tlist";
            break;
        case sci_mlist :
            return "mlist";
            break;
        default:
            break;
    }
    return "";
}

SciErr getListItemNumber(void* _pvCtx, int* _piAddress, int* _piNbItem)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iType = 0;

    sciErr = getVarType(_pvCtx, _piAddress, &iType);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_LIST_ITEM_NUMBER, _("%s: Unable to get item number of list"), "getListItemNumber");
        return sciErr;
    }

    types::List* pL = (types::List*)_piAddress;
    switch (iType)
    {
        case sci_list :
        case sci_mlist :
        case sci_tlist :
            *_piNbItem = pL->getSize();
            break;
        default :
            addErrorMessage(&sciErr, API_ERROR_INVALID_LIST_TYPE, _("%s: Invalid argument type, %s excepted"), "getListItemNumber", _("list"));
            return sciErr;
    }
    return sciErr;
}

SciErr getListItemAddress(void* _pvCtx, int* _piAddress, int _iItemNum, int** _piItemAddress)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iItem      = 0;
    int* piOffset    = NULL;
    int* piItemAddress = NULL;

    //get item count
    sciErr = getListItemNumber(_pvCtx, _piAddress, &iItem);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_ITEM_ADDRESS, _("%s: Unable to get address of item #%d in argument #%d"), "getListItemAddress", _iItemNum + 1, getRhsFromAddress(_pvCtx, _piAddress));
        return sciErr;
    }

    if (_iItemNum > iItem)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_ITEM_ADDRESS, _("%s: Unable to get address of item #%d in argument #%d"), "getListItemAddress", _iItemNum + 1, getRhsFromAddress(_pvCtx, _piAddress));
        return sciErr;
    }

    types::List* pL = (types::List*)_piAddress;
    //get offset of item array
    types::InternalType* pIT = pL->get(_iItemNum - 1);
    if (pIT->isListUndefined())
    {
        *_piItemAddress = NULL;
    }
    else
    {
        *_piItemAddress = (int*)pIT;
    }
    return sciErr;
}

SciErr createList(void* _pvCtx, int _iVar, int _iNbItem, int** _piAddress)
{
    return createCommonList(_pvCtx, _iVar, sci_list, _iNbItem, _piAddress);
}

SciErr createMList(void* _pvCtx, int _iVar, int _iNbItem, int** _piAddress)
{
    return createCommonList(_pvCtx, _iVar, sci_mlist, _iNbItem, _piAddress);
}

SciErr createTList(void* _pvCtx, int _iVar, int _iNbItem, int** _piAddress)
{
    return createCommonList(_pvCtx, _iVar, sci_tlist, _iNbItem, _piAddress);
}

SciErr createNamedList(void* _pvCtx, const char* _pstName, int _iNbItem, int** _piAddress)
{
    return createCommonNamedList(_pvCtx, _pstName, sci_list, _iNbItem, _piAddress);
}

SciErr createNamedTList(void* _pvCtx, const char* _pstName, int _iNbItem, int** _piAddress)
{
    return createCommonNamedList(_pvCtx, _pstName, sci_tlist, _iNbItem, _piAddress);
}

SciErr createNamedMList(void* _pvCtx, const char* _pstName, int _iNbItem, int** _piAddress)
{
    return createCommonNamedList(_pvCtx, _pstName, sci_mlist, _iNbItem, _piAddress);
}

SciErr createNamedList(void* _pvCtx, const char* _pstName, int _iNbItem, int** _piAddress, bool useStack)
{

    return createCommonNamedList(_pvCtx, _pstName, sci_list, _iNbItem, _piAddress);
}

SciErr createNamedTList(void* _pvCtx, const char* _pstName, int _iNbItem, int** _piAddress, bool useStack)
{
    return createCommonNamedList(_pvCtx, _pstName, sci_tlist, _iNbItem, _piAddress);
}

SciErr createNamedMList(void* _pvCtx, const char* _pstName, int _iNbItem, int** _piAddress, bool useStack)
{
    return createCommonNamedList(_pvCtx, _pstName, sci_mlist, _iNbItem, _piAddress);
}

static SciErr createCommonNamedList(void* _pvCtx, const char* _pstName, int _iListType, int _iNbItem, int** _piAddress)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

static SciErr createCommonList(void* _pvCtx, int _iVar, int _iListType, int _iNbItem, int** _piAddress)
{
	SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr fillCommonList(void* _pvCtx, int* _piAddress, int _iListType, int _iNbItem)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piOffset = NULL;

    _piAddress[0] = _iListType;
    _piAddress[1] = _iNbItem;

    piOffset  = _piAddress + 2;
    piOffset[0] = 1; //always

    for (int i = 0 ; i < _iNbItem; i++)
    {
        //initialize item offset
        piOffset[i + 1] = -1;
    }
    return sciErr;
}

SciErr readNamedList(void* _pvCtx, const char* _pstName, int* _piNbItem, int** _piAddress)
{
    return readCommonNamedList(_pvCtx, _pstName, sci_list, _piNbItem, _piAddress);
}

SciErr readNamedTList(void* _pvCtx, const char* _pstName, int* _piNbItem, int** _piAddress)
{
    return readCommonNamedList(_pvCtx, _pstName, sci_tlist, _piNbItem, _piAddress);
}

SciErr readNamedMList(void* _pvCtx, const char* _pstName, int* _piNbItem, int** _piAddress)
{
    return readCommonNamedList(_pvCtx, _pstName, sci_mlist, _piNbItem, _piAddress);
}

static SciErr readCommonNamedList(void* _pvCtx, const char* _pstName, int _iListType, int* _piNbItem, int** _piAddress)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;
    int iNbItem  = 0;

    sciErr = getVarAddressFromName(_pvCtx, _pstName, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_NAMED_LIST, _("%s: Unable to get variable \"%s\""), "readNamedList", _pstName);
        return sciErr;
    }

    if (piAddr[0] != _iListType)
    {
        addErrorMessage(&sciErr, API_ERROR_INVALID_LIST_TYPE, _("%s: Invalid argument type, %s excepted"), "readNamedList", getListTypeName(_iListType));
        return sciErr;
    }

    sciErr = getListItemNumber(_pvCtx, piAddr, &iNbItem);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_NAMED_LIST, _("%s: Unable to get variable \"%s\""), "readNamedList", _pstName);
        return sciErr;
    }

    *_piNbItem = iNbItem;
    *_piAddress = piAddr;

    return sciErr;
}

SciErr getListInList(void* _pvCtx, int* _piParent, int _iItemPos, int** _piAddress)
{
    return getCommonListInList(_pvCtx, _piParent, _iItemPos, sci_list, _piAddress);
}

SciErr getTListInList(void* _pvCtx, int* _piParent, int _iItemPos, int** _piAddress)
{
    return getCommonListInList(_pvCtx, _piParent, _iItemPos, sci_tlist, _piAddress);
}

SciErr getMListInList(void* _pvCtx, int* _piParent, int _iItemPos, int** _piAddress)
{
    return getCommonListInList(_pvCtx, _piParent, _iItemPos, sci_mlist, _piAddress);
}

SciErr getCommonListInList(void* _pvCtx, int* _piParent, int _iItemPos, int _iListType, int** _piAddress)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, _piAddress);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_LIST_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getListInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    if ((*_piAddress)[0] != _iListType)
    {
        addErrorMessage(&sciErr, API_ERROR_INVALID_LIST_TYPE, _("%s: Invalid argument type, %s excepted"), "getListInList", getListTypeName(_iListType));
        return sciErr;
    }
    return sciErr;
}

SciErr getListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int** _piAddress)
{
    return getCommomListInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, sci_list, _piAddress);
}

SciErr getTListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int** _piAddress)
{
    return getCommomListInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, sci_tlist, _piAddress);
}

SciErr getMListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int** _piAddress)
{
    return getCommomListInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, sci_mlist, _piAddress);
}

SciErr getCommomListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iListType, int** _piAddress)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_LIST_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "getListInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    if (piAddr[0] != _iListType)
    {
        addErrorMessage(&sciErr, API_ERROR_INVALID_LIST_TYPE, _("%s: Invalid argument type, %s excepted"), "getListInNamedList", getListTypeName(_iListType));
        return sciErr;
    }

    *_piAddress = piAddr;
    return sciErr;
}

SciErr createListInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iNbItem, int** _piAddress)
{
    return createCommonListInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, sci_list, _iNbItem, _piAddress, 0);
}

SciErr createTListInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iNbItem, int** _piAddress)
{
    return createCommonListInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, sci_tlist, _iNbItem, _piAddress, 0);
}

SciErr createMListInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iNbItem, int** _piAddress)
{
    return createCommonListInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, sci_mlist, _iNbItem, _piAddress, 0);
}

static SciErr createCommonListInList(void* _pvCtx, int _iVar, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iListType, int _iNbItem, int** _piAddress, int iNamed)
{

    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
	// FIXME

    return sciErr;
}

SciErr createListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iNbItem, int** _piAddress)
{
    return createCommonListInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, sci_list, _iNbItem, _piAddress);
}

SciErr createTListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iNbItem, int** _piAddress)
{
    return createCommonListInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, sci_tlist, _iNbItem, _piAddress);
}

SciErr createMListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iNbItem, int** _piAddress)
{
    return createCommonListInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, sci_mlist, _iNbItem, _piAddress);
}

SciErr createCommonListInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iListType, int _iNbItem, int** _piAddress)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr allocCommonItemInList(void* _pvCtx, int* _piParent, int _iItemPos, int** _piChildAddr)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piOffset = NULL;

    //Does previous items was already inserted
    piOffset  = _piParent + 2;
    if (piOffset[_iItemPos - 1] == -1)
    {
        //Previous items wasn't inserted
        addErrorMessage(&sciErr, API_ERROR_NON_ORDERED_INSERTION, _("%s: Items must be inserted in order"), "allocItemInList");
        return sciErr;
    }

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, _piChildAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_ALLOC_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "allocItemInList", _iItemPos + 1);
        return sciErr;
    }

    return sciErr;
}

/******************************
* Void and defined functions *
******************************/
SciErr createVoidInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr createUndefinedInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

/*********************
* Double functions *
*********************/

SciErr getMatrixOfDoubleInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, double** _pdblReal)
{
    return getCommonMatrixOfDoubleInList(_pvCtx, _piParent, _iItemPos, 0, _piRows, _piCols, _pdblReal, NULL);
}

SciErr getComplexMatrixOfDoubleInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, double** _pdblReal, double** _pdblImg)
{
    return getCommonMatrixOfDoubleInList(_pvCtx, _piParent, _iItemPos, 1, _piRows, _piCols, _pdblReal, _pdblImg);
}

static SciErr getCommonMatrixOfDoubleInList(void* _pvCtx, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, double** _pdblReal, double** _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_DOUBLE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), _iComplex ? "getComplexMatrixOfDoubleInList" : "getMatrixOfDoubleInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    sciErr = getCommonMatrixOfDouble(_pvCtx, piAddr, '$', _iComplex, _piRows, _piCols, _pdblReal, _pdblImg);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_DOUBLE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), _iComplex ? "getComplexMatrixOfDoubleInList" : "getMatrixOfDoubleInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }
    return sciErr;
}

SciErr allocMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, double** _pdblReal)
{
    return allocCommonMatrixOfDoubleInList(_pvCtx, _iVar, _piParent, _iItemPos, 0, _iRows, _iCols, _pdblReal, NULL);
}

SciErr allocComplexMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, double** _pdblReal, double** _pdblImg)
{
    return allocCommonMatrixOfDoubleInList(_pvCtx, _iVar, _piParent, _iItemPos, 1, _iRows, _iCols, _pdblReal, _pdblImg);
}

static SciErr allocCommonMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* /*_piParent*/, int _iItemPos, int _iComplex, int _iRows, int _iCols, double** _pdblReal, double** _pdblImg)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME
	return sciErr;
}

static SciErr fillCommonMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iComplex, int _iRows, int _iCols, double** _pdblReal, double** _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int* piOffset   = NULL;
    int* piChildAddr = NULL;

    //Does item can be added in the list
    sciErr = getListItemNumber(_pvCtx, _piParent, &iNbItem);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_ALLOC_DOUBLE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), _iComplex ? "allocComplexMatrixOfDoubleInList" : "allocMatrixOfDoubleInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    if (iNbItem < _iItemPos)
    {
        addErrorMessage(&sciErr, API_ERROR_ITEM_LIST_NUMBER, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexMatrixOfDoubleInList" : "createMatrixOfDoubleInList", _iItemPos + 1);
        return sciErr;
    }


    sciErr = allocCommonItemInList(_pvCtx, _piParent, _iItemPos, &piChildAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_ALLOC_DOUBLE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), _iComplex ? "allocComplexMatrixOfDoubleInList" : "allocMatrixOfDoubleInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    //sciErr = fillCommonMatrixOfDouble(_pvCtx, piChildAddr, _iComplex, _iRows, _iCols, _pdblReal, _pdblImg);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_ALLOC_DOUBLE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), _iComplex ? "allocComplexMatrixOfDoubleInList" : "allocMatrixOfDoubleInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    piOffset      = _piParent + 2;
    piOffset[_iItemPos] = piOffset[_iItemPos - 1] + _iRows * _iCols * (_iComplex + 1) + 2;

    return sciErr;
}

SciErr createMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const double* _pdblReal)
{
    return createCommonMatrixOfDoubleInList(_pvCtx, _iVar, _piParent, _iItemPos, 0, _iRows, _iCols, _pdblReal, NULL);
}

SciErr createComplexMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const double* _pdblReal, const double* _pdblImg)
{
    return createCommonMatrixOfDoubleInList(_pvCtx, _iVar, _piParent, _iItemPos, 1, _iRows, _iCols, _pdblReal, _pdblImg);
}

SciErr createComplexZMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const doublecomplex* _pdblData)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    double *pdblReal = NULL;
    double *pdblImg  = NULL;

    sciErr = allocCommonMatrixOfDoubleInList(_pvCtx, _iVar, _piParent, _iItemPos, 1, _iRows, _iCols, &pdblReal, &pdblImg);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_DOUBLE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createComplexZMatrixOfDoubleInList", _iItemPos + 1);
        return sciErr;
    }

    vGetPointerFromDoubleComplex(_pdblData, _iRows * _iCols, pdblReal, pdblImg);

    return sciErr;
}

SciErr createCommonMatrixOfDoubleInList(void* _pvCtx, int _iVar, int* /*_piParent*/, int _iItemPos, int _iComplex, int _iRows, int _iCols, const double* _pdblReal, const double* _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    double *pdblReal = NULL;
    double *pdblImg  = NULL;

    sciErr = allocCommonMatrixOfDoubleInList(_pvCtx, _iVar, NULL/*_piParent*/, _iItemPos, _iComplex, _iRows, _iCols, &pdblReal, &pdblImg);
    if(sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_DOUBLE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexMatrixOfDoubleInList" : "createMatrixOfDoubleInList", _iItemPos + 1);
        return sciErr;
    }

    if (_pdblReal != NULL)
    {
        memcpy(pdblReal, _pdblReal, _iRows * _iCols * sizeof(double));
    }

    if (_iComplex && _pdblImg != NULL)
    {
        memcpy(pdblImg, _pdblImg, _iRows * _iCols * sizeof(double));
    }
    return sciErr;
}

SciErr createMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const double* _pdblReal)
{
    return createCommomMatrixOfDoubleInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 0, _iRows, _iCols, _pdblReal, NULL);
}

SciErr createComplexMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const double* _pdblReal, const double* _pdblImg)
{
    return createCommomMatrixOfDoubleInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 1, _iRows, _iCols, _pdblReal, _pdblImg);
}

SciErr createComplexZMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, const doublecomplex* _pdblData)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr createCommomMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iComplex, int _iRows, int _iCols, const double* _pdblReal, const double* _pdblImg)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr readMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, double* _pdblReal)
{
    return readCommonMatrixOfDoubleInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 0, _piRows, _piCols, _pdblReal, NULL);
}

SciErr readComplexMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, double* _pdblReal, double* _pdblImg)
{
    return readCommonMatrixOfDoubleInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 1, _piRows, _piCols, _pdblReal, _pdblImg);
}

static SciErr readCommonMatrixOfDoubleInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, double* _pdblReal, double* _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int* piAddr    = NULL;
    int* piRoot    = NULL;
    double* pdblReal = NULL;
    double* pdblImg  = NULL;

    if (_piParent == NULL)
    {
        sciErr = readNamedList(_pvCtx, _pstName, &iNbItem, &piRoot);
        if (sciErr.iErr)
        {
            addErrorMessage(&sciErr, API_ERROR_READ_DOUBLE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexMatrixOfDoubleInNamedList" : "readMatrixOfDoubleInNamedList", _iItemPos + 1, _pstName);
            return sciErr;
        }

        sciErr = getListItemAddress(_pvCtx, piRoot, _iItemPos, &piAddr);
    }
    else
    {
        sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    }

    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_DOUBLE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexMatrixOfDoubleInNamedList" : "readMatrixOfDoubleInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    sciErr = getCommonMatrixOfDouble(_pvCtx, piAddr, '$', _iComplex, _piRows, _piCols, &pdblReal, &pdblImg);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_DOUBLE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexMatrixOfDoubleInNamedList" : "readMatrixOfDoubleInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    if (_pdblReal == NULL || (_iComplex && _pdblImg == NULL))
    {
        return sciErr;
    }

    memcpy(_pdblReal, pdblReal, sizeof(double) * *_piRows * *_piCols);
    if (_iComplex)
    {
        memcpy(_pdblImg, pdblImg, sizeof(double) * *_piRows * *_piCols);
    }
    return sciErr;
}


/*********************
* Strings functions *
*********************/

SciErr getMatrixOfStringInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piLength, char** _pstStrings)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_STRING_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getMatrixOfStringInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    sciErr = getMatrixOfString(_pvCtx, piAddr, _piRows, _piCols, _piLength, _pstStrings);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_STRING_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getMatrixOfStringInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    return sciErr;
}


SciErr createMatrixOfStringInList(void* _pvCtx, int _iVar, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, const char* const* _pstStrings)
{
    SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    // FIXME
    return sciErr;
}

SciErr fillCommonMatrixOfStringInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const char* const* _pstStrings, int* _piTotalLen)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem   = 0;

    int* piAddr   = NULL;
    int* piOffset  = NULL;

    //Does item can be added in the list
    getListItemNumber(_pvCtx, _piParent, &iNbItem);
    if (iNbItem < _iItemPos)
    {
        addErrorMessage(&sciErr, API_ERROR_ITEM_LIST_NUMBER, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfStringInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = allocCommonItemInList(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_STRING_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "fillMatrixOfStringInList", _iItemPos + 1);
        return sciErr;
    }

    //sciErr = fillMatrixOfString(_pvCtx, piAddr, _iRows, _iCols, _pstStrings, _piTotalLen);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_STRING_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "fillMatrixOfStringInList", _iItemPos + 1);
        return sciErr;
    }

    piOffset      = _piParent + 2;
    piOffset[_iItemPos] = piOffset[_iItemPos - 1] + (*_piTotalLen + 5 + _iRows * _iCols + !((*_piTotalLen + _iRows * _iCols) % 2)) / 2;

    return sciErr;
}

SciErr createMatrixOfStringInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, const char* const* _pstStrings)
{

    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr readMatrixOfStringInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piLength, char** _pstStrings)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr    = NULL;
    int* piRoot    = NULL;
    int iNbItem    = 0;

    if (_piParent == NULL)
    {
        sciErr = readNamedList(_pvCtx, _pstName, &iNbItem, &piRoot);
        if (sciErr.iErr)
        {
            addErrorMessage(&sciErr, API_ERROR_READ_STRING_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfStringInNamedList", _iItemPos + 1, _pstName);
            return sciErr;
        }

        sciErr = getListItemAddress(_pvCtx, piRoot, _iItemPos, &piAddr);
    }
    else
    {
        sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    }

    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_STRING_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfStringInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    sciErr = getMatrixOfString(_pvCtx, piAddr, _piRows, _piCols, _piLength, _pstStrings);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_STRING_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfStringInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    return sciErr;
}

/*********************
* boolean functions *
*********************/

SciErr getMatrixOfBooleanInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int** _piBool)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_BOOLEAN_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getMatrixOfBooleanInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    sciErr = getMatrixOfBoolean(_pvCtx, piAddr, _piRows, _piCols, _piBool);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_BOOLEAN_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getMatrixOfBooleanInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    return sciErr;
}

SciErr createMatrixOfBooleanInList(void* _pvCtx, int _iVar, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, const int* _piBool)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int *piBool   = NULL;

    sciErr = allocMatrixOfBooleanInList(_pvCtx, _iVar, NULL/*_piParent*/, _iItemPos, _iRows, _iCols, &piBool);
    if(sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_BOOLEAN_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfBooleanInList", _iItemPos + 1);
        return sciErr;
    }

    if (_piBool != NULL)
    {
        memcpy(piBool, _piBool, _iRows * _iCols * sizeof(int));
    }
    return sciErr;
}

SciErr allocMatrixOfBooleanInList(void* _pvCtx, int _iVar, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, int** _piBool)
{
    SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    // FIXME
    return sciErr;
}

static SciErr fillMatrixOfBoolInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, int** _piBool)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
/*
    int iNbItem    = 0;
    int* piOffset   = NULL;
    int* piChildAddr = NULL;

    //Does item can be added in the list
    sciErr = getListItemNumber(_pvCtx, _piParent, &iNbItem);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_BOOLEAN_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfBoolInList", _iItemPos + 1);
        return sciErr;
    }

    if (iNbItem < _iItemPos)
    {
        addErrorMessage(&sciErr, API_ERROR_ITEM_LIST_NUMBER, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfBooleanInList", _iItemPos + 1);
        return sciErr;
    }


    sciErr = allocCommonItemInList(_pvCtx, _piParent, _iItemPos, &piChildAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_BOOLEAN_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfBoolInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = fillMatrixOfBoolean(_pvCtx, piChildAddr, _iRows, _iCols, _piBool);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_BOOLEAN_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfBoolInList", _iItemPos + 1);
        return sciErr;
    }

    piOffset      = _piParent + 2;
    piOffset[_iItemPos] = piOffset[_iItemPos - 1] + ((3 + _iRows * _iCols + !((_iRows * _iCols) % 2)) / 2);

*/    return sciErr;
}

SciErr createMatrixOfBooleanInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, const int* _piBool)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr readMatrixOfBooleanInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piBool)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr    = NULL;
    int* piRoot    = NULL;
    int iNbItem    = 0;
    int* piBool    = NULL;


    if (_piParent == NULL)
    {
        sciErr = readNamedList(_pvCtx, _pstName, &iNbItem, &piRoot);
        if (sciErr.iErr)
        {
            addErrorMessage(&sciErr, API_ERROR_READ_BOOLEAN_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfBooleanInNamedList", _iItemPos + 1, _pstName);
            return sciErr;
        }

        sciErr = getListItemAddress(_pvCtx, piRoot, _iItemPos, &piAddr);
    }
    else
    {
        sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    }

    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_BOOLEAN_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfBooleanInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    sciErr = getMatrixOfBoolean(_pvCtx, piAddr, _piRows, _piCols, &piBool);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_BOOLEAN_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfBooleanInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    if (_piBool == NULL)
    {
        return sciErr;
    }

    memcpy(_piBool, piBool, *_piRows * *_piCols * sizeof(int));
    return sciErr;
}


/*************************
* polynomials functions *
*************************/

SciErr getMatrixOfPolyInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbCoef, double** _pdblReal)
{
    return getCommonMatrixOfPolyInList(_pvCtx, _piParent, _iItemPos, 0, _piRows, _piCols, _piNbCoef, _pdblReal, NULL);
}

SciErr getComplexMatrixOfPolyInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbCoef, double** _pdblReal, double** _pdblImg)
{
    return getCommonMatrixOfPolyInList(_pvCtx, _piParent, _iItemPos, 1, _piRows, _piCols, _piNbCoef, _pdblReal, _pdblImg);
}

SciErr getCommonMatrixOfPolyInList(void* _pvCtx, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, int* _piNbCoef, double** _pdblReal, double** _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_POLY_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), _iComplex ? "getComplexMatrixOfPolyInList" : "getMatrixOfPolyInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    sciErr = getCommonMatrixOfPoly(_pvCtx, piAddr, _iComplex, _piRows, _piCols, _piNbCoef, _pdblReal, _pdblImg);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_POLY_IN_LIST, _("API_ERROR_GET_POLY_IN_LIST"));
        return sciErr;
    }

    return sciErr;
}

SciErr createMatrixOfPolyInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, char* _pstVarName, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal)
{
    return createCommonMatrixOfPolyInList(_pvCtx, _iVar, _piParent, _iItemPos, _pstVarName, 0, _iRows, _iCols, _piNbCoef, _pdblReal, NULL);
}

SciErr createComplexMatrixOfPolyInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, char* _pstVarName, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal, const double* const* _pdblImg)
{
    return createCommonMatrixOfPolyInList(_pvCtx, _iVar, _piParent, _iItemPos, _pstVarName, 1, _iRows, _iCols, _piNbCoef, _pdblReal, _pdblImg);
}

SciErr createCommonMatrixOfPolyInList(void* _pvCtx, int _iVar, int* /*_piParent*/, int _iItemPos, char* _pstVarName, int _iComplex, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal, const double* const* _pdblImg)
{
    SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piItemAddr = NULL;
    int *piEnd      = NULL;
    int iItemLen    = 0;
    int iTotalLen   = 0;
    int* piParent   = getLastListAddress(_iVar, _iItemPos);

    sciErr = getListItemAddress(_pvCtx, piParent, _iItemPos, &piItemAddr);
    if(sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_POLY_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexMatrixOfPolyInList" : "createMatrixOfPolyInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = fillCommonMatrixOfPolyInList(_pvCtx, _iVar, piParent, _iItemPos, _pstVarName, _iComplex, _iRows, _iCols, _piNbCoef, _pdblReal, _pdblImg, &iTotalLen);
    if(sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_POLY_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexMatrixOfPolyInList" : "createMatrixOfPolyInList", _iItemPos + 1);
        return sciErr;
    }

    iItemLen      = 9 + _iRows * _iCols + (9 + _iRows * _iCols) % 2;
    iItemLen      += iTotalLen;
    piEnd        = piItemAddr + iItemLen;
    if(_iItemPos == piParent[1])
    {
        updateListOffset(_pvCtx, _iVar, piParent, _iItemPos, piEnd);
        popListAddress(_iVar);
    }

    closeList(_iVar, piEnd);

    return sciErr;
}

static SciErr fillCommonMatrixOfPolyInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, char* _pstVarName, int _iComplex, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal, const double* const* _pdblImg, int* _piTotalLen)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int iTotalLen   = 0;
    int* piOffset   = NULL;
    int* piChildAddr = NULL;

    int iItemLen   = 0;

    //Does item can be added in the list
    sciErr = getListItemNumber(_pvCtx, _piParent, &iNbItem);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_POLY_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexMatrixOfPolyInList" : "createMatrixOfPolyInList", _iItemPos + 1);
        return sciErr;
    }

    if (iNbItem < _iItemPos)
    {
        addErrorMessage(&sciErr, API_ERROR_ITEM_LIST_NUMBER, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexMatrixOfPolyInList" : "createMatrixOfPolyInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = allocCommonItemInList(_pvCtx, _piParent, _iItemPos, &piChildAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_POLY_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexMatrixOfPolyInList" : "createMatrixOfPolyInList", _iItemPos + 1);
        return sciErr;
    }

    //sciErr = fillCommonMatrixOfPoly(_pvCtx, piChildAddr, _pstVarName, _iComplex, _iRows, _iCols, _piNbCoef, _pdblReal, _pdblImg, &iTotalLen);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_POLY_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexMatrixOfPolyInList" : "createMatrixOfPolyInList", _iItemPos + 1);
        return sciErr;
    }

    piOffset      = _piParent + 2;

    iItemLen      = 9 + _iRows * _iCols + (9 + _iRows * _iCols) % 2;
    iItemLen      += iTotalLen;
    piOffset[_iItemPos] = piOffset[_iItemPos - 1] + ((iItemLen + 1) / 2);

    *_piTotalLen = iTotalLen;
    return sciErr;
}


SciErr createMatrixOfPolyInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, char* _pstVarName, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal)
{
    return createCommonMatrixOfPolyInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, _pstVarName, 0, _iRows, _iCols, _piNbCoef, _pdblReal, NULL);
}

SciErr createComplexMatrixOfPolyInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, char* _pstVarName, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal, const double* const* _pdblImg)
{
    return createCommonMatrixOfPolyInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, _pstVarName, 1, _iRows, _iCols, _piNbCoef, _pdblReal, _pdblImg);
}

SciErr createCommonMatrixOfPolyInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, char* _pstVarName, int _iComplex, int _iRows, int _iCols, const int* _piNbCoef, const double* const* _pdblReal, const double* const* _pdblImg)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr readMatrixOfPolyInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbCoef, double** _pdblReal)
{
    return readCommonMatrixOfPolyInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 0, _piRows, _piCols, _piNbCoef, _pdblReal, NULL);
}

SciErr readComplexMatrixOfPolyInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbCoef, double** _pdblReal, double** _pdblImg)
{
    return readCommonMatrixOfPolyInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 1, _piRows, _piCols, _piNbCoef, _pdblReal, _pdblImg);
}

SciErr readCommonMatrixOfPolyInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, int* _piNbCoef, double** _pdblReal, double** _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr    = NULL;
    int* piRoot    = NULL;
    int iNbItem    = 0;

    if (_piParent == NULL)
    {
        sciErr = readNamedList(_pvCtx, _pstName, &iNbItem, &piRoot);
        if (sciErr.iErr)
        {
            addErrorMessage(&sciErr, API_ERROR_READ_POLY_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexMatrixOfPolyInNamedList" : "readMatrixOfPolyInNamedList", _iItemPos + 1, _pstName);
            return sciErr;
        }

        sciErr = getListItemAddress(_pvCtx, piRoot, _iItemPos, &piAddr);
    }
    else
    {
        sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    }

    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_POLY_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexMatrixOfPolyInNamedList" : "readMatrixOfPolyInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    sciErr = getCommonMatrixOfPoly(_pvCtx, piAddr, _iComplex, _piRows, _piCols, _piNbCoef, _pdblReal, _pdblImg);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_POLY_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexMatrixOfPolyInNamedList" : "readMatrixOfPolyInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    return sciErr;
}

/**********************
* integers functions *
**********************/

static SciErr fillCommonMatrixOfIntegerInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iPrecision, int _iRows, int _iCols, void** _pvData)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int* piOffset   = NULL;
    int* piChildAddr = NULL;

    //Does item can be added in the list
    sciErr = getListItemNumber(_pvCtx, _piParent, &iNbItem);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_INT_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfIntegerInList", _iItemPos + 1);
        return sciErr;
    }

    if (iNbItem < _iItemPos)
    {
        addErrorMessage(&sciErr, API_ERROR_ITEM_LIST_NUMBER, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfIntegerInList", _iItemPos + 1);
        return sciErr;
    }


    sciErr = allocCommonItemInList(_pvCtx, _piParent, _iItemPos, &piChildAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_INT_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfIntegerInList", _iItemPos + 1);
        return sciErr;
    }

    //sciErr = fillCommonMatrixOfInteger(_pvCtx, piChildAddr, _iPrecision, _iRows, _iCols, _pvData);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_INT_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfIntegerInList", _iItemPos + 1);
        return sciErr;
    }

    piOffset      = _piParent + 2;

    //integer : size in double
    //1st case, 5 * 1 int8  -> (10,5) (1,1) (1,2,3,4,5,x,x,x)           -> 3 : 2 + 5/8 + !!(5%8) -> 2 + 0 + 1 -> 3
    //2nd case, 5 * 1 int16 -> (10,5) (1,2)   (1,2,3,4)    (5,x,x,x)     -> 4 : 2 + 5/4 + !!(5%4) -> 2 + 1 + 1 -> 4
    //3th case, 5 * 1 int32 -> (10,5) (1,3)    (1,2)      (3,4)   (5,x) -> 5 : 2 + 5/2 + !!(5%2) -> 2 + 2 + 1 -> 5

    //with 5*5 int matrix
    //1st case, 5 * 5 int8  -> (10,5) (5,1) (1:25) -> 3 : 2 + 25/8 + !!(25%8) -> 2 + 3  + 1 -> 6
    //2nd case, 5 * 1 int16 -> (10,5) (5,2) (1:25) -> 4 : 2 + 25/4 + !!(25%4) -> 2 + 6  + 1 -> 9
    //3th case, 5 * 5 int32 -> (10,5) (5,3) (1:25) -> 5 : 2 + 25/2 + !!(25%2) -> 2 + 12 + 1 -> 15
    piOffset[_iItemPos] = piOffset[_iItemPos - 1] + 2 + _iRows * _iCols / (sizeof(double) / (_iPrecision % 10 )) + (int)(!!(_iRows * _iCols)) % (sizeof(double) / (_iPrecision % 10 ));

    return sciErr;
}

static SciErr allocCommonMatrixOfIntegerInList(void* _pvCtx, int _iVar, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iPrecision, int _iRows, int _iCols, void** _pvData)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr allocMatrixOfUnsignedInteger8InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, unsigned char** _pucData)
{
    return allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_UINT8, _iRows, _iCols, (void **)_pucData);
}

SciErr allocMatrixOfUnsignedInteger16InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, unsigned short** _pusData)
{
    return allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_UINT16, _iRows, _iCols, (void**)_pusData);
}

SciErr allocMatrixOfUnsignedInteger32InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, unsigned int** _puiData)
{
    return allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_UINT32, _iRows, _iCols, (void**)_puiData);
}

#ifdef __SCILAB_INT64__
SciErr allocMatrixOfUnsignedInteger64InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, unsigned long long** _pullData)
{
	return allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_UINT64, _iRows, _iCols, (void**)_pullData);
}
#endif

SciErr allocMatrixOfInteger8InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, char** _pcData)
{
    return allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_INT8, _iRows, _iCols, (void**)_pcData);
}

SciErr allocMatrixOfInteger16InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, short** _psData)
{
    return allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_INT16, _iRows, _iCols, (void**)_psData);
}

SciErr allocMatrixOfInteger32InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, int** _piData)
{
    return allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_INT32, _iRows, _iCols, (void**)_piData);
}

#ifdef __SCILAB_INT64__
SciErr allocMatrixOfInteger64InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, long long** _pllData)
{
	return allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_INT64, _iRows, _iCols, (void**)_pllData);
}
#endif

static SciErr createCommomMatrixOfIntegerInList(void* _pvCtx, int _iVar, const char* _pstName, int* _piParent, int _iItemPos, int _iPrecision, int _iRows, int _iCols, const void* _pvData)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    void *pvData = NULL;

    sciErr = allocCommonMatrixOfIntegerInList(_pvCtx, _iVar, _pstName, _piParent, _iItemPos, _iPrecision, _iRows, _iCols, &pvData);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_INT_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createMatrixOfIntegerInList", _iItemPos + 1);
        return sciErr;
    }

    if (pvData != NULL)
    {
        memcpy(pvData, _pvData, _iRows * _iCols * (_iPrecision % 10));
    }
    return sciErr;
}

SciErr createMatrixOfUnsignedInteger8InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const unsigned char* _pucData)
{
    return createCommomMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_UINT8, _iRows, _iCols, _pucData);
}

SciErr createMatrixOfUnsignedInteger16InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const unsigned short* _pusData)
{
    return createCommomMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_UINT16, _iRows, _iCols, _pusData);
}

SciErr createMatrixOfUnsignedInteger32InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const unsigned int* _puiData)
{
    return createCommomMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_UINT32, _iRows, _iCols, _puiData);
}

#ifdef __SCILAB_INT64__
SciErr createMatrixOfUnsignedInteger64InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const unsigned long long* _pullData)
{
	return createCommomMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_UINT64, _iRows, _iCols, _pullData);
}
#endif

SciErr createMatrixOfInteger8InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const char* _pcData)
{
    return createCommomMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_INT8, _iRows, _iCols, _pcData);
}

SciErr createMatrixOfInteger16InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const short* _psData)
{
    return createCommomMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_INT16, _iRows, _iCols, _psData);
}

SciErr createMatrixOfInteger32InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const int* _piData)
{
    return createCommomMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_INT32, _iRows, _iCols, _piData);
}

#ifdef __SCILAB_INT64__
SciErr createMatrixOfInteger64InList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, const long long* _pllData)
{
	return createCommomMatrixOfIntegerInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, SCI_INT64, _iRows, _iCols, _pllData);
}
#endif

static SciErr getCommonMatrixOfIntegerInList(void* _pvCtx, int* _piParent, int _iItemPos, int _iPrecision, int* _piRows, int* _piCols, void** _pvData)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_INT_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getMatrixOfIntegerInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    sciErr = getCommonMatrixOfInteger(_pvCtx, piAddr, _iPrecision, _piRows, _piCols, _pvData);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_INT_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getMatrixOfIntegerInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    return sciErr;
}

SciErr getMatrixOfUnsignedInteger8InList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, unsigned char** _pucData)
{
    return getCommonMatrixOfIntegerInList(_pvCtx, _piParent, _iItemPos, SCI_UINT8, _piRows, _piCols, (void**)_pucData);
}

SciErr getMatrixOfUnsignedInteger16InList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, unsigned short** _pusData)
{
    return getCommonMatrixOfIntegerInList(_pvCtx, _piParent, _iItemPos, SCI_UINT16, _piRows, _piCols, (void**)_pusData);
}

SciErr getMatrixOfUnsignedInteger32InList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, unsigned int** _puiData)
{
    return getCommonMatrixOfIntegerInList(_pvCtx, _piParent, _iItemPos, SCI_UINT32, _piRows, _piCols, (void**)_puiData);
}

#ifdef __SCILAB_INT64__
SciErr getMatrixOfUnsignedInteger64InList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, unsigned long long** _pullData)
{
	return getCommonMatrixOfIntegerInList(_pvCtx, _piParent, _iItemPos, SCI_UINT64, _piRows, _piCols, (void**)_pullData);
}
#endif

SciErr getMatrixOfInteger8InList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, char** _pcData)
{
    return getCommonMatrixOfIntegerInList(_pvCtx, _piParent, _iItemPos, SCI_INT8, _piRows, _piCols, (void**)_pcData);
}

SciErr getMatrixOfInteger16InList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, short** _psData)
{
    return getCommonMatrixOfIntegerInList(_pvCtx, _piParent, _iItemPos, SCI_INT16, _piRows, _piCols, (void**)_psData);
}

SciErr getMatrixOfInteger32InList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int** _piData)
{
    return getCommonMatrixOfIntegerInList(_pvCtx, _piParent, _iItemPos, SCI_INT32, _piRows, _piCols, (void**)_piData);
}

#ifdef __SCILAB_INT64__
SciErr getMatrixOfInteger64InList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, long long** _pllData)
{
	return getCommonMatrixOfIntegerInList(_pvCtx, _piParent, _iItemPos, SCI_INT64, _piRows, _piCols, (void**)_pllData);
}
#endif

static SciErr createCommonMatrixOfIntegerInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iPrecision, int _iRows, int _iCols, const void* _pvData)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr createMatrixOfUnsignedInteger8InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const unsigned char* _pucData)
{
    return createCommonMatrixOfIntegerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_UINT8, _iRows, _iCols, _pucData);
}

SciErr createMatrixOfUnsignedInteger16InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const unsigned short* _pusData)
{
    return createCommonMatrixOfIntegerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_UINT16, _iRows, _iCols, _pusData);
}

SciErr createMatrixOfUnsignedInteger32InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const unsigned int* _puiData)
{
    return createCommonMatrixOfIntegerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_UINT32, _iRows, _iCols, _puiData);
}

#ifdef __SCILAB_INT64__
SciErr createMatrixOfUnsignedInteger64InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const unsigned long long* _pullData)
{
	return createCommonMatrixOfIntegerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_UINT64, _iRows, _iCols, _pullData);
}
#endif

SciErr createMatrixOfInteger8InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const char* _pcData)
{
    return createCommonMatrixOfIntegerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_INT8, _iRows, _iCols, _pcData);
}

SciErr createMatrixOfInteger16InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const short* _psData)
{
    return createCommonMatrixOfIntegerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_INT16, _iRows, _iCols, _psData);
}

SciErr createMatrixOfInteger32InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const int* _piData)
{
    return createCommonMatrixOfIntegerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_INT32, _iRows, _iCols, _piData);
}

#ifdef __SCILAB_INT64__
SciErr createMatrixOfInteger64InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, const long long* _pllData)
{
	return createCommonMatrixOfIntegerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_INT64, _iRows, _iCols, _pllData);
}
#endif

static SciErr readCommonMatrixOfIntgerInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iPrecision, int* _piRows, int* _piCols, void* _pvData)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int* piAddr    = NULL;
    int* piRoot    = NULL;
    void* pvData   = NULL;

    if (_piParent == NULL)
    {
        sciErr = readNamedList(_pvCtx, _pstName, &iNbItem, &piRoot);
        if (sciErr.iErr)
        {
            addErrorMessage(&sciErr, API_ERROR_READ_INT_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfIntgerInNamedList", _iItemPos + 1, _pstName);
            return sciErr;
        }

        sciErr = getListItemAddress(_pvCtx, piRoot, _iItemPos, &piAddr);
    }
    else
    {
        sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    }

    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_INT_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfIntgerInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    sciErr = getCommonMatrixOfInteger(_pvCtx, piAddr, _iPrecision, _piRows, _piCols, &pvData);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_INT_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readMatrixOfIntgerInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    if (_pvData == NULL)
    {
        return sciErr;
    }

    memcpy(_pvData, pvData, (_iPrecision % 10 ) * *_piRows * *_piCols);
    return sciErr;
}

SciErr readMatrixOfUnsignedInteger8InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, unsigned char* _pucData)
{
    return readCommonMatrixOfIntgerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_UINT8, _piRows, _piCols, _pucData);
}

SciErr readMatrixOfUnsignedInteger16InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, unsigned short* _pusData)
{
    return readCommonMatrixOfIntgerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_UINT16, _piRows, _piCols, _pusData);
}

SciErr readMatrixOfUnsignedInteger32InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, unsigned int* _puiData)
{
    return readCommonMatrixOfIntgerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_UINT32, _piRows, _piCols, _puiData);
}

#ifdef __SCILAB_INT64__
SciErr readMatrixOfUnsignedInteger64InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, unsigned long long* _pullData)
{
	return readCommonMatrixOfIntgerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_UINT64, _piRows, _piCols, _pullData);
}
#endif

SciErr readMatrixOfIntger8InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, char* _pcData)
{
    return readCommonMatrixOfIntgerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_INT8, _piRows, _piCols, _pcData);
}

SciErr readMatrixOfIntger16InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, short* _psData)
{
    return readCommonMatrixOfIntgerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_INT16, _piRows, _piCols, _psData);
}

SciErr readMatrixOfIntger32InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piData)
{
    return readCommonMatrixOfIntgerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_INT32, _piRows, _piCols, _piData);
}

#ifdef __SCILAB_INT64__
SciErr readMatrixOfIntger64InNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, long long* _pllData)
{
	return readCommonMatrixOfIntgerInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, SCI_INT64, _piRows, _piCols, _pllData);
}
#endif

/*********************
* sparses functions *
*********************/

static SciErr fillCommonSparseMatrixInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iComplex, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal, const double* _pdblImg, int* _piTotalSize)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int iTotalLen   = 0;
    int* piOffset   = NULL;
    int* piNbItemRow = NULL;
    int* piColPos   = NULL;
    int* piChildAddr = NULL;
    double* pdblReal = NULL;
    double* pdblImg  = NULL;

    int iItemLen   = 0;

    //Does item can be added in the list
    sciErr = getListItemNumber(_pvCtx, _piParent, &iNbItem);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexSparseMatrixInList" : "createComplexSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    if (iNbItem < _iItemPos)
    {
        addErrorMessage(&sciErr, API_ERROR_ITEM_LIST_NUMBER, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexSparseMatrixInList" : "createSparseMatrixInNamedList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = allocCommonItemInList(_pvCtx, _piParent, _iItemPos, &piChildAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexSparseMatrixInList" : "createComplexSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = fillCommonSparseMatrix(_pvCtx, piChildAddr, _iComplex, _iRows, _iCols, _iNbItem, &piNbItemRow, &piColPos, &pdblReal, &pdblImg, &iTotalLen);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexSparseMatrixInList" : "createComplexSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    piOffset      = _piParent + 2;

    iItemLen      = 5 + _iRows + _iNbItem + !((_iRows + _iNbItem) % 2);
    iItemLen      += iTotalLen * 2;
    piOffset[_iItemPos] = piOffset[_iItemPos - 1] + ((iItemLen + 1) / 2);

    memcpy(piNbItemRow, _piNbItemRow, _iRows * sizeof(int));
    memcpy(piColPos, _piColPos, _iNbItem * sizeof(int));

    memcpy(pdblReal, _pdblReal, _iNbItem * sizeof(double));
    if (_iComplex)
    {
        memcpy(pdblImg, _pdblImg, _iNbItem * sizeof(double));
    }

    *_piTotalSize = iTotalLen;
    return sciErr;
}

static SciErr createCommonSparseMatrixInList(void* _pvCtx, int _iVar, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iComplex, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal, const double* _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr     = NULL;
    int *piEnd      = NULL;
    int iItemLen    = 0;
    int iTotalLen   = 0;
    int* piParent   = NULL;

    if(_pstName)
    {
        piParent = getLastNamedListAddress(_pstName, _iItemPos);
    }
    else
    {
        piParent = getLastListAddress(_iVar, _iItemPos);
    }

    sciErr = getListItemAddress(_pvCtx, piParent, _iItemPos, &piAddr);
    if(sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexSparseMatrixInList" : "createComplexSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = fillCommonSparseMatrixInList(_pvCtx, _iVar, piParent, _iItemPos, _iComplex, _iRows, _iCols, _iNbItem, _piNbItemRow, _piColPos, _pdblReal, _pdblImg, &iTotalLen);
    if(sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), _iComplex ? "createComplexSparseMatrixInList" : "createComplexSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    iItemLen      = 5 + _iRows + _iNbItem + !((_iRows + _iNbItem) % 2);
    iItemLen      += iTotalLen * 2;
    piEnd        = piAddr + iItemLen;
    if(_iItemPos == piParent[1])
    {
        updateListOffset(_pvCtx, _iVar, piParent, _iItemPos, piEnd);
        popListAddress(_iVar);
    }

    closeList(_iVar, piEnd);

    return sciErr;
}

SciErr createSparseMatrixInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal)
{
    return createCommonSparseMatrixInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, 0, _iRows, _iCols, _iNbItem, _piNbItemRow, _piColPos, _pdblReal, NULL);
}

SciErr createComplexSparseMatrixInList(void* _pvCtx, int _iVar, int* _piParent, int _iItemPos, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal, const double* _pdblImg)
{
    return createCommonSparseMatrixInList(_pvCtx, _iVar, NULL, _piParent, _iItemPos, 1, _iRows, _iCols, _iNbItem, _piNbItemRow, _piColPos, _pdblReal, _pdblImg);
}

SciErr createCommonSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iComplex, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal, const double* _pdblImg)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr createSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal)
{
    return createCommonSparseMatrixInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 0, _iRows, _iCols, _iNbItem, _piNbItemRow, _piColPos, _pdblReal, NULL);
}

SciErr createComplexSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos, const double* _pdblReal, const double* _pdblImg)
{
    return createCommonSparseMatrixInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 1, _iRows, _iCols, _iNbItem, _piNbItemRow, _piColPos, _pdblReal, _pdblImg);
}

static SciErr getCommonSparseMatrixInList(void* _pvCtx, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, int* _piNbItem, int** _piNbItemRow, int** _piColPos, double** _pdblReal, double** _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_SPARSE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), _iComplex ? "getComplexSparseMatrixInList" : "getSparseMatrixInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    sciErr = getCommonSparseMatrix(_pvCtx, piAddr, _iComplex, _piRows, _piCols, _piNbItem, _piNbItemRow, _piColPos, _pdblReal, _pdblImg);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_SPARSE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), _iComplex ? "getComplexSparseMatrixInList" : "getSparseMatrixInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    return sciErr;
}

SciErr getSparseMatrixInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbItem, int** _piNbItemRow, int** _piColPos, double** _pdblReal)
{
    return getCommonSparseMatrixInList(_pvCtx, _piParent, _iItemPos, 0, _piRows, _piCols, _piNbItem, _piNbItemRow, _piColPos, _pdblReal, NULL);
}

SciErr getComplexSparseMatrixInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbItem, int** _piNbItemRow, int** _piColPos, double** _pdblReal, double** _pdblImg)
{
    return getCommonSparseMatrixInList(_pvCtx, _piParent, _iItemPos, 1, _piRows, _piCols, _piNbItem, _piNbItemRow, _piColPos, _pdblReal, _pdblImg);
}

static SciErr readCommonSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int _iComplex, int* _piRows, int* _piCols, int* _piNbItem, int* _piNbItemRow, int* _piColPos, double* _pdblReal, double* _pdblImg)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int* piAddr    = NULL;
    int* piRoot    = NULL;
    int* piNbItemRow = NULL;
    int* piColPos   = NULL;
    double* pdblReal = NULL;
    double* pdblImg  = NULL;

    if (_piParent == NULL)
    {
        sciErr = readNamedList(_pvCtx, _pstName, &iNbItem, &piRoot);
        if (sciErr.iErr)
        {
            addErrorMessage(&sciErr, API_ERROR_READ_SPARSE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexSparseMatrixInNamedList" : "readSparseMatrixInNamedList", _iItemPos + 1, _pstName);
            return sciErr;
        }

        sciErr = getListItemAddress(_pvCtx, piRoot, _iItemPos, &piAddr);
    }
    else
    {
        sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    }

    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_SPARSE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexSparseMatrixInNamedList" : "readSparseMatrixInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    sciErr = getCommonSparseMatrix(_pvCtx, piAddr, _iComplex, _piRows, _piCols, _piNbItem, &piNbItemRow, &piColPos, &pdblReal, &pdblImg);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_SPARSE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), _iComplex ? "readComplexSparseMatrixInNamedList" : "readSparseMatrixInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    if (_piNbItemRow == NULL)
    {
        return sciErr;
    }
    memcpy(_piNbItemRow, piNbItemRow, *_piRows * sizeof(int));

    if (_piColPos == NULL)
    {
        return sciErr;
    }
    memcpy(_piColPos, piColPos, *_piNbItem * sizeof(int));

    if (_pdblReal == NULL || (_iComplex && _pdblImg == NULL))
    {
        return sciErr;
    }

    memcpy(_pdblReal, pdblReal, sizeof(double) * *_piNbItem);
    if (_iComplex)
    {
        memcpy(_pdblImg, pdblImg, sizeof(double) * *_piNbItem);
    }

    return sciErr;
}

SciErr readSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbItem, int* _piNbItemRow, int* _piColPos, double* _pdblReal)
{
    return readCommonSparseMatrixInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 0, _piRows, _piCols, _piNbItem, _piNbItemRow, _piColPos, _pdblReal, NULL);
}

SciErr readComplexSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbItem, int* _piNbItemRow, int* _piColPos, double* _pdblReal, double* _pdblImg)
{
    return readCommonSparseMatrixInNamedList(_pvCtx, _pstName, _piParent, _iItemPos, 1, _piRows, _piCols, _piNbItem, _piNbItemRow, _piColPos, _pdblReal, _pdblImg);
}


/*****************************
* boolean sparses functions *
*****************************/
static SciErr fillBooleanSparseMatrixInList(void* _pvCtx, int _iVar, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int* piOffset   = NULL;
    int* piNbItemRow = NULL;
    int* piColPos   = NULL;
    int* piChildAddr = NULL;
    int iItemLen   = 0;

    int* piParent = NULL;

    if(_pstName)
    {
        piParent = getLastNamedListAddress(_pstName, _iItemPos);
    }
    else
    {
        piParent = getLastListAddress(_iVar, _iItemPos);
    }

    //Does item can be added in the list
    sciErr = getListItemNumber(_pvCtx, piParent, &iNbItem);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_BOOLEAN_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createBooleanSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    if (iNbItem < _iItemPos)
    {
        addErrorMessage(&sciErr, API_ERROR_ITEM_LIST_NUMBER, _("%s: Unable to create list item #%d in Scilab memory"), "createBooleanSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = allocCommonItemInList(_pvCtx, piParent, _iItemPos, &piChildAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_BOOLEAN_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createBooleanSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = fillBooleanSparseMatrix(_pvCtx, piChildAddr, _iRows, _iCols, _iNbItem, &piNbItemRow, &piColPos);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_FILL_BOOLEAN_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createBooleanSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    piOffset      = piParent + 2;

    iItemLen      = 5 + _iRows + _iNbItem + !((_iRows + _iNbItem) % 2);
    piOffset[_iItemPos] = piOffset[_iItemPos - 1] + ((iItemLen + 1) / 2);

    memcpy(piNbItemRow, _piNbItemRow, _iRows * sizeof(int));
    memcpy(piColPos, _piColPos, _iNbItem * sizeof(int));

    return sciErr;
}

SciErr createBooleanSparseMatrixInList(void* _pvCtx, int _iVar, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr     = NULL;
    int *piEnd      = NULL;
    int iItemLen    = 0;
    int* piParent   = getLastListAddress(_iVar, _iItemPos);

    sciErr = getListItemAddress(_pvCtx, piParent, _iItemPos, &piAddr);
    if(sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_BOOLEAN_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createBooleanSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    sciErr = fillBooleanSparseMatrixInList(_pvCtx, _iVar, NULL, piParent, _iItemPos, _iRows, _iCols, _iNbItem, _piNbItemRow, _piColPos);
    if(sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_CREATE_BOOLEAN_SPARSE_IN_LIST, _("%s: Unable to create list item #%d in Scilab memory"), "createBooleanSparseMatrixInList", _iItemPos + 1);
        return sciErr;
    }

    iItemLen      = 5 + _iRows + _iNbItem + !((_iRows + _iNbItem) % 2);
    piEnd        = piAddr + iItemLen;
    if(_iItemPos == piParent[1])
    {
        updateListOffset(_pvCtx, _iVar, piParent, _iItemPos, piEnd);
        popListAddress(_iVar);
    }

    closeList(_iVar, piEnd);

    return sciErr;
}

SciErr createBooleanSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, int _iRows, int _iCols, int _iNbItem, const int* _piNbItemRow, const int* _piColPos)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}

SciErr getBooleanSparseMatrixInList(void* _pvCtx, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbItem, int** _piNbItemRow, int** _piColPos)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_BOOLEAN_SPARSE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getBooleanSparseMatrixInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    sciErr = getBooleanSparseMatrix(_pvCtx, piAddr, _piRows, _piCols, _piNbItem, _piNbItemRow, _piColPos);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_BOOLEAN_SPARSE_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getBooleanSparseMatrixInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    return sciErr;
}

SciErr readBooleanSparseMatrixInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, int* _piRows, int* _piCols, int* _piNbItem, int* _piNbItemRow, int* _piColPos)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int* piAddr    = NULL;
    int* piRoot    = NULL;
    int* piNbItemRow = NULL;
    int* piColPos   = NULL;

    if (_piParent == NULL)
    {
        sciErr = readNamedList(_pvCtx, _pstName, &iNbItem, &piRoot);
        if (sciErr.iErr)
        {
            addErrorMessage(&sciErr, API_ERROR_READ_BOOLEAN_SPARSE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readBooleanSparseMatrixInNamedList", _iItemPos + 1, _pstName);
            return sciErr;
        }

        sciErr = getListItemAddress(_pvCtx, piRoot, _iItemPos, &piAddr);
    }
    else
    {
        sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    }

    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_BOOLEAN_SPARSE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readBooleanSparseMatrixInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    sciErr = getBooleanSparseMatrix(_pvCtx, piAddr, _piRows, _piCols, _piNbItem, &piNbItemRow, &piColPos);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_BOOLEAN_SPARSE_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readBooleanSparseMatrixInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    if (_piNbItemRow == NULL)
    {
        return sciErr;
    }
    memcpy(_piNbItemRow, piNbItemRow, *_piRows * sizeof(int));

    if (_piColPos == NULL)
    {
        return sciErr;
    }
    memcpy(_piColPos, piColPos, *_piNbItem * sizeof(int));

    return sciErr;
}

/*********************
* Pointer functions *
*********************/
SciErr getPointerInList(void* _pvCtx, int* _piParent, int _iItemPos, void** _pvPtr)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int* piAddr  = NULL;

    sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_POINTER_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getPointerInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    sciErr = getPointer(_pvCtx, piAddr, _pvPtr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_GET_POINTER_IN_LIST, _("%s: Unable to get address of item #%d in argument #%d"), "getPointerInList", _iItemPos + 1, getRhsFromAddress(_pvCtx, _piParent));
        return sciErr;
    }

    return sciErr;
}

SciErr createPointerInList(void* _pvCtx, int _iVar, int* /*_piParent*/, int _iItemPos, void* _pvPtr)
{
    SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    // FIXME
    return sciErr;
}

SciErr readPointerInNamedList(void* _pvCtx, const char* _pstName, int* _piParent, int _iItemPos, void** _pvPtr)
{
	SciErr sciErr; sciErr.iErr = 0; sciErr.iMsgCount = 0;
    int iNbItem    = 0;
    int* piAddr    = NULL;
    int* piRoot    = NULL;

    if (_piParent == NULL)
    {
        sciErr = readNamedList(_pvCtx, _pstName, &iNbItem, &piRoot);
        if (sciErr.iErr)
        {
            addErrorMessage(&sciErr, API_ERROR_READ_POINTER_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readPointerInNamedList", _iItemPos + 1, _pstName);
            return sciErr;
        }

        sciErr = getListItemAddress(_pvCtx, piRoot, _iItemPos, &piAddr);
    }
    else
    {
        sciErr = getListItemAddress(_pvCtx, _piParent, _iItemPos, &piAddr);
    }

    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_POINTER_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readPointerInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    sciErr = getPointer(_pvCtx, piAddr, _pvPtr);
    if (sciErr.iErr)
    {
        addErrorMessage(&sciErr, API_ERROR_READ_POINTER_IN_NAMED_LIST, _("%s: Unable to get address of item #%d in variable \"%s\""), "readPointerInNamedList", _iItemPos + 1, _pstName);
        return sciErr;
    }

    return sciErr;
}

SciErr createPointerInNamedList(void* _pvCtx, const char* _pstName, int* /*_piParent*/, int _iItemPos, void* _pvPtr)
{
    SciErr sciErr;
    sciErr.iErr = 0;
    sciErr.iMsgCount = 0;
    // FIXME

    return sciErr;
}



/********************
* tools  functions *
********************/

static void updateNamedListOffset(void* _pvCtx, int _iVar, const char* _pstName, int *_piCurrentNode, int _iItemPos, int *_piEnd)
{
    updateCommunListOffset(_pvCtx, _iVar, _pstName, _piCurrentNode, _iItemPos, _piEnd);
}

static void updateListOffset(void* _pvCtx, int _iVar, int *_piCurrentNode, int _iItemPos, int *_piEnd)
{
    // FIXME
}

//internal tool functions
static void updateCommunListOffset(void* _pvCtx, int _iVar, const char* _pstName, int *_piCurrentNode, int _iItemPos, int *_piEnd)
{
    // FIXME

}

static void closeList(int _iVar, int *_piEnd)
{
    // FIXME
}
/*--------------------------------------------------------------------------*/

int isListType(void* _pvCtx, int* _piAddress)
{
    return checkVarType(_pvCtx, _piAddress, sci_list);
}

/*--------------------------------------------------------------------------*/
int isNamedListType(void* _pvCtx, const char* _pstName)
{
    return checkNamedVarType(_pvCtx, _pstName, sci_list);
}

/*--------------------------------------------------------------------------*/
int isTListType(void* _pvCtx, int* _piAddress)
{
    return checkVarType(_pvCtx, _piAddress, sci_tlist);
}

/*--------------------------------------------------------------------------*/
int isNamedTListType(void* _pvCtx, const char* _pstName)
{
    return checkNamedVarType(_pvCtx, _pstName, sci_tlist);
}

/*--------------------------------------------------------------------------*/
int isMListType(void* _pvCtx, int* _piAddress)
{
    return checkVarType(_pvCtx, _piAddress, sci_mlist);
}

/*--------------------------------------------------------------------------*/
int isNamedMListType(void* _pvCtx, const char* _pstName)
{
    return checkNamedVarType(_pvCtx, _pstName, sci_mlist);
}

/*--------------------------------------------------------------------------*/
