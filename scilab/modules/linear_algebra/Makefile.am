# Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
# Copyright (C) 2006 - INRIA - Sylvestre LEDRU
#
# This file is distributed under the same license as the Scilab package.

LINEAR_ALGEBRA_CPP_SOURCES = \
    src/cpp/scischur.cpp \
    src/cpp/schurSelect.cpp

LINEAR_ALGEBRA_C_SOURCES = src/c/schurtable.c \
src/c/issymmetric.c \
src/c/invert_matrix.c \
src/c/lu.c \
src/c/qr.c \
src/c/hess.c \
src/c/eigen.c \
src/c/balanc.c \
src/c/chol.c \
src/c/svd.c \
src/c/rcond.c \
src/c/det.c \
src/c/lsq.c

LINEAR_ALGEBRA_FORTRAN_SOURCES = \
    src/fortran/intdggbal.f \
    src/fortran/intzgeqpf4.f \
    src/fortran/intzgehrd.f \
    src/fortran/intzgesvd1.f \
    src/fortran/intzgesvd2.f \
    src/fortran/intdgesv3.f \
    src/fortran/intdgesv4.f \
    src/fortran/DGELSY1.f \
    src/fortran/intdpotrf.f \
    src/fortran/intdoldsvd.f \
    src/fortran/intdgecon.f \
    src/fortran/zoldqr.f \
    src/fortran/intzgetrf.f \
    src/fortran/complexify.f \
    src/fortran/intzgebal.f \
    src/fortran/intzgetri.f \
    src/fortran/intzggbal.f \
    src/fortran/intddet.f \
    src/fortran/intzgesv3.f \
    src/fortran/intzgesv4.f \
    src/fortran/intzpotrf.f \
    src/fortran/intzgecon.f \
    src/fortran/intzoldsvd.f \
    src/fortran/intdgelsy.f \
    src/fortran/intdgeqpf3.f \
    src/fortran/intdgeqpf4.f \
    src/fortran/intdgesvd1.f \
    src/fortran/intdgehrd.f \
    src/fortran/intdgesvd2.f \
    src/fortran/intzdet.f \
    src/fortran/doldqr.f \
    src/fortran/intdgetrf.f \
    src/fortran/intzgelsy.f \
    src/fortran/intdgetri.f \
    src/fortran/intdgebal.f \
    src/fortran/intzgeqpf3.f \
    src/fortran/writebuf.f \
    src/fortran/intzschur.f

GATEWAY_C_SOURCES = \
    sci_gateway/c/sci_backslash.c \
    sci_gateway/c/sci_slash.c \
    sci_gateway/c/gw_linear_algebra2.c \
    sci_gateway/c/gw_linear_algebra.c \
    sci_gateway/c/assembleEigenvectors.c \
    sci_gateway/c/sci_dgeev.c \
    sci_gateway/c/sci_dggev.c \
    sci_gateway/c/sci_dsyev.c \
    sci_gateway/c/sci_zgeev.c \
    sci_gateway/c/sci_zggev.c \
    sci_gateway/c/sci_zheev.c

GATEWAY_CPP_SOURCES = \
    sci_gateway/cpp/linear_algebra_gw.cpp \
    sci_gateway/cpp/sci_chol.cpp \
    sci_gateway/cpp/sci_bdiag.cpp \
    sci_gateway/cpp/sci_inv.cpp \
    sci_gateway/cpp/sci_svd.cpp \
    sci_gateway/cpp/sci_rcond.cpp \
    sci_gateway/cpp/sci_qr.cpp \
    sci_gateway/cpp/sci_lu.cpp \
    sci_gateway/cpp/sci_lsq.cpp \
    sci_gateway/cpp/sci_hess.cpp \
    sci_gateway/cpp/sci_spec.cpp \
    sci_gateway/cpp/sci_det.cpp \
    sci_gateway/cpp/sci_balanc.cpp \
    sci_gateway/cpp/sci_schur.cpp

libscilinear_algebra_la_CPPFLAGS = -I$(srcdir)/includes/ \
-I$(srcdir)/src/c/ \
-I$(srcdir)/src/cpp/ \
-I$(top_srcdir)/libs/MALLOC/includes/ \
-I$(top_srcdir)/modules/symbol/includes \
-I$(top_srcdir)/modules/types/includes \
-I$(top_srcdir)/modules/ast/includes \
-I$(top_srcdir)/modules/output_stream/includes \
-I$(top_srcdir)/modules/api_scilab/includes \
-I$(top_srcdir)/modules/console/includes \
-I$(top_srcdir)/modules/threads/includes \
-I$(top_srcdir)/modules/system_env/includes \
-I$(top_srcdir)/modules/string/includes \
-I$(top_srcdir)/modules/elementary_functions/includes \
-I$(top_srcdir)/modules/dynamic_link/includes \
-I$(top_srcdir)/modules/operations/includes \
-I$(top_srcdir)/libs/dynamiclibrary/includes \
-I$(top_srcdir)/modules/localization/includes/ \
$(EIGEN_CPPFLAGS) \
$(AM_CPPFLAGS)

if MAINTAINER_MODE
pkglib_LTLIBRARIES = libscilinear_algebra.la
noinst_LTLIBRARIES = libscilinear_algebra-algo.la
else
noinst_LTLIBRARIES = libscilinear_algebra-algo.la libscilinear_algebra.la
endif



libscilinear_algebra_algo_la_SOURCES = $(LINEAR_ALGEBRA_FORTRAN_SOURCES) $(LINEAR_ALGEBRA_C_SOURCES) $(LINEAR_ALGEBRA_CPP_SOURCES)
libscilinear_algebra_la_SOURCES = $(GATEWAY_C_SOURCES) $(GATEWAY_CPP_SOURCES)
libscilinear_algebra_algo_la_CPPFLAGS = $(libscilinear_algebra_la_CPPFLAGS)

# For the code check (splint)
CHECK_SRC= $(LINEAR_ALGEBRA_C_SOURCES) $(GATEWAY_C_SOURCES)
INCLUDE_FLAGS = $(libscilinear_algebra_la_CPPFLAGS)

libscilinear_algebra_la_LIBADD = libscilinear_algebra-algo.la

#### Target ######
modulename=linear_algebra


#### linear_algebra : Conf files ####
libscilinear_algebra_la_rootdir = $(mydatadir)
libscilinear_algebra_la_root_DATA =  license.txt


#### linear_algebra : init scripts ####
libscilinear_algebra_la_etcdir = $(mydatadir)/etc
libscilinear_algebra_la_etc_DATA = etc/linear_algebra.quit etc/linear_algebra.start
#### linear_algebra : gateway declaration ####
libscilinear_algebra_la_sci_gatewaydir = $(mydatadir)/sci_gateway
libscilinear_algebra_la_sci_gateway_DATA = sci_gateway/linear_algebra_gateway.xml


include $(top_srcdir)/Makefile.incl.am
