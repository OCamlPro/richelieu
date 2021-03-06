/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2011 - DIGITEO - Cedric DELAMARRE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#ifndef __DIFFERENTIALEQUATIONS_HXX__
#define __DIFFERENTIALEQUATIONS_HXX__

#include <map>
#include "dynlib_differential_equations.h"
#include "string.hxx"
#include "callable.hxx"

extern "C"
{
#include "Thread_Wrapper.h"
}

extern "C"
{
    // jacobian
    void C2F(jex)   (int*, double*, double*, int*, int*, double*, int*);

    // ode
    void C2F(arnol) (int*, double*, double*, double*);
    void C2F(fex)   (int*, double*, double*, double*);
    void C2F(fex2)  (int*, double*, double*, double*);
    void C2F(fex3)  (int*, double*, double*, double*);
    void C2F(fexab) (int*, double*, double*, double*);
    void C2F(loren) (int*, double*, double*, double*);
    void C2F(bcomp) (int*, double*, double*, double*);
    void C2F(lcomp) (int*, double*, double*, double*);

    // odedc
    typedef int(*odedc_f_t)(int*, int*, int*, double *, double*, double*);
    void C2F(fcd)   (int*, int*, int*, double *, double*, double*);
    void C2F(fcd1)  (int*, int*, int*, double *, double*, double*);
    void C2F(fexcd) (int*, int*, int*, double *, double*, double*);
    void C2F(phis)  (int*, int*, int*, double *, double*, double*);
    void C2F(phit)  (int*, int*, int*, double *, double*, double*);

    // intg
    double C2F(intgex)(double*);

    // int2d
    double C2F(int2dex)(double*, double*);

    // int3d
    void C2F(int3dex)(double*, int*, double*);

    // feval
    int C2F(parab)  (int*, double*, double*, double*, int*);
    int C2F(parabc) (int*, double*, double*, double*, int*);

    // bvode
    void C2F(cndg)  (int*, double*, double*);
    void C2F(cng)   (int*, double*, double*);
    void C2F(cnf)   (double*, double*, double*);
    void C2F(cndf)  (double*, double*, double*);
    void C2F(cngu)  (double*, double*, double*);

    // impl
    void C2F(resid) (int*, double*, double*, double*, double*, int*);
    void C2F(aplusp)(int*, double*, double*, double*, double*, double*, int*);
    void C2F(dgbydy)(int*, double*, double*, double*, double*, double*, double*, int*);

    // dassl dasrt
    void C2F(res1)  (double*, double*, double*, double*, int*, double*, int*);
    void C2F(res2)  (double*, double*, double*, double*, int*, double*, int*);
    void C2F(dres1) (double*, double*, double*, double*, int*, double*, int*);
    void C2F(dres2) (double*, double*, double*, double*, int*, double*, int*);
    void C2F(jac2)  (double*, double*, double*, double*, double*, double*, int*);
    void C2F(djac2) (double*, double*, double*, double*, double*, double*, int*);
    void C2F(djac1) (double*, double*, double*, double*, double*, double*, int*);

    // dasrt
    void C2F(gr1)(int*, double*, double*, int*, double*, double*, int*);
    void C2F(gr2)(int*, double*, double*, int*, double*, double*, int*);
}

class DIFFERENTIAL_EQUATIONS_IMPEXP DifferentialEquationFunctions
{

public :

    DifferentialEquationFunctions(std::wstring callerName);
    ~DifferentialEquationFunctions();

    void setOdeYRows(int);
    void setOdeYCols(int);

    void setOdedcYDSize(int);
    void setOdedcFlag();
    void resetOdedcFlag();

    void setFFunction(types::Callable*);
    void setJacFunction(types::Callable*);
    void setGFunction(types::Callable*);

    bool setFFunction(types::String*);
    bool setJacFunction(types::String*);
    bool setGFunction(types::String*);

    void setFArgs(types::InternalType*);
    void setJacArgs(types::InternalType*);
    void setGArgs(types::InternalType*);

    int getOdeYRows();
    int getOdeYCols();

    int getOdedcYDSize();
    int getOdedcFlag();

    int execOdeF(int* n, double* t, double* y, double* ydot);
    int execFunctionJac(int *n,double *t,double *y,int *ml,int *mu,double *J,int *nrpd);
    int execFunctionG(int* n, double* t, double* y, int* ng, double* gout);

    double execIntgF(double* x);
    double execInt2dF(double* x, double* y);
    double execInt3dF(double* x, int* numfun, double* funvls);

    int execFevalF(int *nn, double *x1, double *x2, double *xres, int *itype);

    // bvode
    void setFsubFunction(types::Callable*);
    void setDfsubFunction(types::Callable*);
    void setGsubFunction(types::Callable*);
    void setDgsubFunction(types::Callable*);
    void setGuessFunction(types::Callable*);

    bool setFsubFunction(types::String*);
    bool setDfsubFunction(types::String*);
    bool setGsubFunction(types::String*);
    bool setDgsubFunction(types::String*);
    bool setGuessFunction(types::String*);

    void setFsubArgs(types::InternalType*);
    void setDfsubArgs(types::InternalType*);
    void setGsubArgs(types::InternalType*);
    void setDgsubArgs(types::InternalType*);
    void setGuessArgs(types::InternalType*);

    void setBvodeM(int);
    void setBvodeN(int);

    int execBvodeGsub(int*, double*, double*);
    int execBvodeDgsub(int*, double*, double*);
    int execBvodeFsub(double*, double*, double*);
    int execBvodeDfsub(double*, double*, double*);
    int execBvodeGuess(double*, double*, double*);

    // impl
    int execImplF(int*, double*, double*, double*, double*, int*);
    int execImplG(int*, double*, double*, double*, double*, double*, int*);
    int execImplJac(int*, double*, double*, double*, double*, double*, double*, int*);

    //dassl dasrt
    int execDasslF(double*, double*, double*, double*, int*, double*, int*);
    int execDasslJac(double*, double*, double*, double*, double*, double*, int*);
    void setMu(int);
    void setMl(int);

    // dasrt
    int execDasrtG(int*, double*, double*, int*, double*, double*, int*);

private :

    std::map<std::wstring, void*> m_staticFunctionMap;
    std::wstring m_wstrCaller;

    int m_odeYRows;
    int m_odeYCols;

    // odedc
    int m_odedcYDSize;
    int m_odedcFlag;

    // bvode
    int m_bvodeM;
    int m_bvodeN;

    // dassl dasrt
    int m_mu;
    int m_ml;
    bool m_bandedJac;

    types::Callable* m_pCallFFunction;
    types::Callable* m_pCallJacFunction;
    types::Callable* m_pCallGFunction;

    types::String* m_pStringFFunctionDyn;
    types::String* m_pStringJacFunctionDyn;
    types::String* m_pStringGFunctionDyn;

    types::String* m_pStringFFunctionStatic;
    types::String* m_pStringJacFunctionStatic;
    types::String* m_pStringGFunctionStatic;

    std::vector<types::InternalType*> m_FArgs;
    std::vector<types::InternalType*> m_JacArgs;
    std::vector<types::InternalType*> m_odeGArgs;

    // ode / odedc
    int callOdeMacroF(int* n, double* t, double* y, double* ydot);
    int callMacroJac(int *n,double *t,double *y,int *ml,int *mu,double *J,int *nrpd);
    int callMacroG(int* n, double* t, double* y, int* ng, double* gout);

    // intg
    double callIntgMacroF(double* x);

    // int2d
    double callInt2dMacroF(double* x, double* y);

    // int3d
    double callInt3dMacroF(double* x, int* numfun, double* funvls);

    // feval
    int callFevalMacroF(int *nn, double *x1, double *x2, double *xres, int *itype);

    // bvode
    types::Callable* m_pCallFsubFunction;
    types::Callable* m_pCallDfsubFunction;
    types::Callable* m_pCallGsubFunction;
    types::Callable* m_pCallDgsubFunction;
    types::Callable* m_pCallGuessFunction;

    types::String* m_pStringFsubFunctionDyn;
    types::String* m_pStringDfsubFunctionDyn;
    types::String* m_pStringGsubFunctionDyn;
    types::String* m_pStringDgsubFunctionDyn;
    types::String* m_pStringGuessFunctionDyn;

    types::String* m_pStringFsubFunctionStatic;
    types::String* m_pStringDfsubFunctionStatic;
    types::String* m_pStringGsubFunctionStatic;
    types::String* m_pStringDgsubFunctionStatic;
    types::String* m_pStringGuessFunctionStatic;

    std::vector<types::InternalType*> m_FsubArgs;
    std::vector<types::InternalType*> m_DfsubArgs;
    std::vector<types::InternalType*> m_GsubArgs;
    std::vector<types::InternalType*> m_DgsubArgs;
    std::vector<types::InternalType*> m_GuessArgs;

    int callBvodeMacroGsub  (int* i, double* z, double* g);
    int callBvodeMacroDgsub (int* i, double* z, double* g);
    int callBvodeMacroFsub  (double* x, double* z, double* d);
    int callBvodeMacroDfsub (double* x, double* z, double* d);
    int callBvodeMacroGuess (double* x, double* z, double* d);

    // impl
    int callImplMacroF(int*, double*, double*, double*, double*, int*);
    int callImplMacroG(int*, double*, double*, double*, double*, double*, int*);
    int callImplMacroJac(int*, double*, double*, double*, double*, double*, double*, int*);

    // dassl dasrt
    int callDasslMacroF(double*, double*, double*, double*, int*, double*, int*);
    int callDasslMacroJac(double*, double*, double*, double*, double*, double*, int*);

    // dasrt
    int callDasrtMacroG(int*, double*, double*, int*, double*, double*, int*);
};

class DIFFERENTIAL_EQUATIONS_IMPEXP DifferentialEquation 
{
    // differential equation functions
private :
    static std::map<__threadId, DifferentialEquationFunctions*> m_mapDifferentialEquationFunctions;

public :
    static void addDifferentialEquationFunctions(DifferentialEquationFunctions* _deFunction);
    static void removeDifferentialEquationFunctions();
    static DifferentialEquationFunctions* getDifferentialEquationFunctions();
};
#endif /* !__DIFFERENTIALEQUATIONS_HXX__ */

