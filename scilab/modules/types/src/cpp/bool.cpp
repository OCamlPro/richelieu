/*
*  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
*  Copyright (C) 2008-2008 - DIGITEO - Antoine ELIAS
*
*  This file must be used under the terms of the CeCILL.
*  This source file is licensed as described in the file COPYING, which
*  you should have received as part of this distribution.  The terms
*  are also available at
*  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
*
*/

#include <sstream>
#include "bool.hxx"
#include "tostring_common.hxx"
#include "formatmode.h"

extern "C"
{
#include "core_math.h"
}

namespace types
{
    Bool::~Bool()
    {
        if(isDeletable() == true)
        {
            deleteAll();
        }
#ifndef NDEBUG
        Inspector::removeItem(this);
#endif
    }

    Bool::Bool(int _iRows, int _iCols)
    {
        int piDims[2]   = {_iRows, _iCols};
        int *piBool = NULL;
        create(piDims, 2, &piBool, NULL);
#ifndef NDEBUG
        Inspector::addItem(this);
#endif
    }

    Bool::Bool(int _iReal)
    {
        int piDims[2]   = {1, 1};
        int *piBool = NULL;
        create(piDims, 2, &piBool, NULL);
        piBool[0] = _iReal == 0 ? 0 : 1;
#ifndef NDEBUG
        Inspector::addItem(this);
#endif
    }

    Bool::Bool(int _iRows, int _iCols, int **_piData)
    {
        int piDims[2] = {_iRows, _iCols};
        create(piDims, 2, _piData, NULL);
#ifndef NDEBUG
        Inspector::addItem(this);
#endif
    }

    Bool::Bool(int _iDims, int* _piDims)
    {
        int* piData = NULL;
        create(_piDims, _iDims, &piData, NULL);
#ifndef NDEBUG
        Inspector::addItem(this);
#endif
    }

    InternalType* Bool::clone()
    {
        Bool *pbClone =  new Bool(getDims(), getDimsArray());
        pbClone->set(get());
        return pbClone;
    }

    void Bool::whoAmI()
    {
        std::cout << "types::Bool";
    }

    GenericType::RealType Bool::getType(void)
    {
        return RealBool;
    }

    bool Bool::setFalse()
    {
        for(int i = 0 ; i < getSize() ; i++)
        {
            set(i, 0);
        }
        return true;
    }

    bool Bool::setTrue()
    {
        for(int i = 0 ; i < getSize() ; i++)
        {
            set(i, 1);
        }
        return true;
    }

    bool Bool::subMatrixToString(std::wostringstream& ostr, int* _piDims, int _iDims)
    {
        int iCurrentLine = 0;
        int iLineLen = getConsoleWidth();
        int iMaxLines = getConsoleLines();

        if(isScalar())
        {//scalar
            _piDims[0] = 0;
            _piDims[1] = 0;
            int iPos = getIndex(_piDims);
            ostr << (get(iPos) == 1 ? L"  T" : L"  F");
            ostr << std::endl;
        }
        else if(getCols() == 1)
        {//column vector
            for(int i = m_iRows1PrintState ; i < getRows() ; i++)
            {
                iCurrentLine++;
                if((iMaxLines == 0 && iCurrentLine >= MAX_LINES) || (iMaxLines != 0 && iCurrentLine >= iMaxLines))
                {
                    m_iRows1PrintState = i;
                    return false;
                }

                _piDims[1] = 0;
                _piDims[0] = i;
                int iPos = getIndex(_piDims);
                ostr << (get(iPos) == 1 ? L"  T" : L"  F");
                ostr << std::endl;
            }
        }
        else if(getRows() == 1)
        {//row vector
            std::wostringstream ostemp;
            int iLastVal = m_iCols1PrintState;
            int iLen = 0;

            for(int i = m_iCols1PrintState ; i < getCols() ; i++)
            {
                _piDims[0] = 0;
                _piDims[1] = i;
                int iPos = getIndex(_piDims);

                if(iLen + 2 >= iLineLen)
                {
                    iCurrentLine += 4; //"column x to Y" + empty line + value + empty line
                    if((iMaxLines == 0 && iCurrentLine >= MAX_LINES) || (iMaxLines != 0 && iCurrentLine >= iMaxLines))
                    {
                        m_iCols1PrintState = iLastVal;
                        return false;
                    }

                    ostr << std::endl << L"       column " << iLastVal + 1 << L" to " << i << std::endl << std::endl;
                    ostr << L" " << ostemp.str() << std::endl;
                    ostemp.str(L"");
                    iLastVal = i;
                    iLen = 0;
                }

                ostemp << (get(iPos) ? L" T" : L" F");
                iLen += 2;
            }

            if(iLastVal != 0)
            {
                ostr << std::endl << L"       column " << iLastVal + 1 << L" to " << getCols() << std::endl << std::endl;
            }
            ostr << L" " << ostemp.str() << std::endl;
        }
        else
        {
            std::wostringstream ostemp;
            int iLen = 0;
            int iLastCol = m_iCols1PrintState;

            //compute the row size for padding for each printed bloc.
            for(int iCols1 = m_iCols1PrintState ; iCols1 < getCols() ; iCols1++)
            {
                if(iLen + 2 > iLineLen)
                {//find the limit, print this part
                    for(int iRows2 = m_iRows2PrintState ; iRows2 < getRows() ; iRows2++)
                    {
                        iCurrentLine++;
                        if((iMaxLines == 0 && iCurrentLine >= MAX_LINES) ||
                            ( (iMaxLines != 0 && iCurrentLine + 3 >= iMaxLines && iRows2 == m_iRows2PrintState) || 
                            (iMaxLines != 0 && iCurrentLine + 1 >= iMaxLines && iRows2 != m_iRows2PrintState)))
                        {
                            if(m_iRows2PrintState == 0 && iRows2 != 0)
                            {//add header
                                ostr << std::endl << L"       column " << iLastCol + 1 << L" to " << iCols1 << std::endl << std::endl;
                            }
                            ostr << L" " << ostemp.str();
                            m_iRows2PrintState = iRows2;
                            m_iCols1PrintState = iLastCol;
                            return false;
                        }

                        for(int iCols2 = iLastCol ; iCols2 < iCols1 ; iCols2++)
                        {
                            _piDims[0] = iRows2;
                            _piDims[1] = iCols2;
                            int iPos = getIndex(_piDims);
                            ostemp << (get(iPos) == 0 ? L" F" : L" T");
                        }
                        ostemp << std::endl << L" ";
                    }
                    iLen = 0;
                    iCurrentLine++;
                    if(m_iRows2PrintState == 0)
                    {
                        iCurrentLine += 3;
                        ostr << std::endl << L"       column " << iLastCol + 1 << L" to " << iCols1 << std::endl << std::endl;
                    }

                    ostr << L" " << ostemp.str();
                    ostemp.str(L"");
                    iLastCol = iCols1;
                    m_iRows2PrintState = 0;
                    m_iCols1PrintState = 0;
                }
                iLen += 2;
            }

            for(int iRows2 = m_iRows2PrintState ; iRows2 < getRows() ; iRows2++)
            {
                iCurrentLine++;
                if((iMaxLines == 0 && iCurrentLine >= MAX_LINES) || (iMaxLines != 0 && iCurrentLine >= iMaxLines))
                {
                    if(m_iRows2PrintState == 0 && iLastCol != 0)
                    {//add header
                        ostr << std::endl << L"       column " << iLastCol + 1 << L" to " << getCols() << std::endl << std::endl;
                    }

                    ostr << ostemp.str();
                    m_iRows2PrintState = iRows2;
                    m_iCols1PrintState = iLastCol;
                    return false;
                }

                for(int iCols2 = iLastCol ; iCols2 < getCols() ; iCols2++)
                {
                    _piDims[0] = iRows2;
                    _piDims[1] = iCols2;
                    int iPos = getIndex(_piDims);

                    ostemp << (get(iPos) == 0 ? L" F" : L" T");
                }
                ostemp << std::endl << L" ";
            }
            if(m_iRows2PrintState == 0 && iLastCol != 0)
            {
                ostr << std::endl << L"       column " << iLastCol + 1 << L" to " << getCols() << std::endl << std::endl;
            }
            ostr << ostemp.str();
        }

        return true;
    }

    bool Bool::operator==(const InternalType& it)
    {
        if(const_cast<InternalType &>(it).isBool() == false)
        {
            return false;
        }

        Bool* pb = const_cast<InternalType &>(it).getAs<types::Bool>();

        if(pb->getDims() != getDims())
        {
            return false;
        }

        for(int i = 0 ; i < getDims() ; i++)
        {
            if(pb->getDimsArray()[i] != getDimsArray()[i])
            {
                return false;
            }
        }

        if(memcmp(get(), pb->get(), getSize() * sizeof(int)) != 0)
        {
            return false;
        }
        return true;
    }

    bool Bool::operator!=(const InternalType& it)
    {
        return !(*this == it);
    }

    int Bool::getNullValue()
    {
        return 0;
    }

    Bool* Bool::createEmpty(int _iDims, int* _piDims, bool _bComplex)
    {
        return new Bool(_iDims, _piDims);
    }

    int Bool::copyValue(int _iData)
    {
        return _iData == 0 ? 0 : 1;
    }

    void Bool::deleteAll()
    {
        delete[] m_pRealData;
        m_pRealData = NULL;
        deleteImg();
    }

    void Bool::deleteImg()
    {
    }

    int* Bool::allocData(int _iSize)
    {
        return new int[_iSize];
    }

    //std::wstring Bool::toStringInLine(int _iPrecision, int iLineLen)
    //{
    //    std::wostringstream ostr;

    //    if(isScalar() || (isVector() && getRows() == 1))
    //    {
    //        for(int i = 0 ; i < getSize() ; i++)
    //        {
    //            ostr << L"  ";
    //            ostr << (get(i) == 0 ? L"F" : L"T");

    //            if(ostr.str().length() > iLineLen)
    //            {
    //                break;
    //            }
    //        }

    //        return ostr.str();
    //    }

    //    //all other cases
    //    ostr << L"[";
    //    for(int i = 0 ; i < m_iDims ; i++)
    //    {
    //        if(i > 0)
    //        {
    //            ostr << L"x";
    //        }
    //        ostr << m_piDims[i];
    //    }

    //    ostr << L" " << getTypeStr() << L"]";
    //    return ostr.str();
    //}
}
