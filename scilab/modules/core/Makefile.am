# Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
# Copyright (C) 2006-2008 - INRIA - Sylvestre LEDRU
#
# This file must be used under the terms of the CeCILL.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution.  The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt


CORE_C_SOURCES = \
src/c/inffic.c \
src/c/cs2st.c \
src/c/intmacr2tree.c \
src/c/InitScilab.c \
src/c/getval.c \
src/c/md5.c \
src/c/inisci-c.c \
src/c/IsAScalar.c \
src/c/texmacs.c \
src/c/sciquit.c \
src/c/stack1.c \
src/c/stack3.c \
src/c/str2sci.c \
src/c/getmodules.c \
src/c/getmemory.c \
src/c/Funtab.c \
src/c/csignal.c \
src/c/callinterf.c \
src/c/scimem.c \
src/c/isanan.c \
src/c/timer.c \
src/c/stack2.c \
src/c/xscion.c \
src/c/returnanan.c \
src/c/LoadFunctionsTab.c \
src/c/loadversion.c \
src/c/version.c \
src/c/stackinfo.c \
src/c/InitializeCore.c \
src/c/TerminateCore.c \
src/c/coretable.c \
src/c/GetXmlFileEncoding.c \
src/c/islittleendian.c \
src/c/terme.c \
src/c/ifexpr.c \
src/c/getcommandlineargs.c \
src/c/callFunctionFromGateway.c \
src/c/getvariablesname.c \
src/c/commandwords.c \
src/c/freeArrayOfString.c \
src/c/getstaticdebuginfo.c \
src/c/getdynamicdebuginfo.c \
src/c/gw_dynamic_generic.c \
src/c/dynamic_gateways.c \
src/c/readGateway.c \
src/c/comparehandles.c \
src/c/setPrecisionFPU.c \
src/c/LaunchScilabSignal.c \
src/c/getos.c \
src/c/mode_exec.c \
src/c/predef.c \
src/c/transposeMatrix.c \
src/c/recursionFunction.c \
src/c/typename.c \
src/c/inittypenames.c \
src/c/funcprot.c \
src/c/eqid.c \
src/c/exitCodeValue.c \
src/c/getversion.c \
src/c/dynamic_tclsci.c \
src/c/dynamic_parallel.c \
src/c/backtrace.c \
src/c/signal_mgmt.c \
src/c/suspendProcess.c \
src/c/windowsChangeManagement.c \
src/c/setlines.c \
src/c/setformat.c \
src/c/InitializePreferences.c

CORE_CPP_SOURCES = \
src/cpp/core_gw.cpp \
src/cpp/hashtable_core.cpp \
src/cpp/namstr.cpp \
src/cpp/search_functions.cpp \
src/cpp/demangle.cpp \
src/cpp/backtrace_print.cpp \
src/cpp/storeCommand.cpp \
src/cpp/banner.cpp \
src/cpp/with_module.cpp \
src/cpp/runner.cpp

if USE_DYNAMIC_STACK
CORE_C_SOURCES += src/c/scimem64.c
endif

CORE_FORTRAN_SOURCES = src/fortran/clunit.f \
src/fortran/getlin.f \
src/fortran/allowptr.f \
src/fortran/isany.f \
src/fortran/hmcreate.f \
src/fortran/nextj.f \
src/fortran/setgetmode.f \
src/fortran/showstack.f \
src/fortran/iseye.f \
src/fortran/setippty.f \
src/fortran/compil.f \
src/fortran/funnam.f \
src/fortran/isnum.f \
src/fortran/cmdstr.f \
src/fortran/hndlops.f \
src/fortran/cmplxt.f \
src/fortran/skpins.f \
src/fortran/folhp.f \
src/fortran/cvname.f \
src/fortran/typ2cod.f \
src/fortran/prompt.f \
src/fortran/intstr.f \
src/fortran/createref.f \
src/fortran/btof.f \
src/fortran/getnum.f \
src/fortran/btofm.f \
src/fortran/getsym.f \
src/fortran/findequal.f \
src/fortran/stackg.f \
src/fortran/find.f \
src/fortran/setlnb.f \
src/fortran/mrknmd.f \
src/fortran/ref2val.f \
src/fortran/stack.f \
src/fortran/isbrk.f \
src/fortran/xerbla.f \
src/fortran/dtosci.f \
src/fortran/bexec.f \
src/fortran/varfunptr.f \
src/fortran/getfun.f \
src/fortran/savlod.f \
src/fortran/error.f \
src/fortran/cvdm.f \
src/fortran/tradsl.f \
src/fortran/mkindx.f \
src/fortran/whatln.f \
src/fortran/errmgr.f \
src/fortran/defmat.f \
src/fortran/mklist.f \
src/fortran/sigbas.f \
src/fortran/indxg.f \
src/fortran/istrue.f \
src/fortran/inisci.f \
src/fortran/getfunction.f \
src/fortran/extlarg.f \
src/fortran/getstr.f \
src/fortran/cvwm.f \
src/fortran/storeglobal.f \
src/fortran/lst2vars.f \
src/fortran/basnms.f \
src/fortran/ptrback.f \
src/fortran/getch.f \
src/fortran/ftob.f \
src/fortran/seteol.f \
src/fortran/compcl.f \
src/fortran/termf.f \
src/fortran/expsum.f \
src/fortran/copyvar.f \
src/fortran/putid.f \
src/fortran/itosci.f \
src/fortran/stackr2d.f \
src/fortran/stacki2d.f \
src/fortran/stackc2i.f \
src/fortran/isinstring.f

if USE_DYNAMIC_STACK
CORE_FORTRAN_SOURCES += src/fortran/relocstack.f
endif

GATEWAY_C_SOURCES = \
	sci_gateway/c/sci_mtlb_mode.c \
	sci_gateway/c/sci_where.c \
	sci_gateway/c/sci_errcatch.c \
	sci_gateway/c/sci_getmemory.c \
	sci_gateway/c/sci_havewindow.c \
	sci_gateway/c/sci_delbpt.c \
	sci_gateway/c/sci_macr2lst.c \
	sci_gateway/c/sci_errclear.c \
	sci_gateway/c/sci_funcprot.c \
	sci_gateway/c/sci_newfun.c \
	sci_gateway/c/sci_dispbpt.c \
	sci_gateway/c/sci_intppty.c \
	sci_gateway/c/sci_gstacksize.c \
	sci_gateway/c/sci_funptr.c \
	sci_gateway/c/sci_return.c \
	sci_gateway/c/sci_clearfun.c \
	sci_gateway/c/sci_setbpt.c \
	sci_gateway/c/sci_what.c \
	sci_gateway/c/sci_predef.c \
	sci_gateway/c/sci_type.c \
	sci_gateway/c/sci_typename.c \
	sci_gateway/c/sci_mode.c \
	sci_gateway/c/sci_macr2tree.c \
	sci_gateway/c/sci_iserror.c \
	sci_gateway/c/sci_getdebuginfo.c \
	sci_gateway/c/sci_debug.c \
	sci_gateway/c/gw_core.c \
	sci_gateway/c/gw_user.c \
	sci_gateway/c/gw_user2.c \
	sci_gateway/c/sci_getvariablesonstack.c \
	sci_gateway/c/sci_readgateway.c \
	sci_gateway/c/sci_comp.c

GATEWAY_FORTRAN_SOURCES =  \
sci_gateway/fortran/sci_f_mtlb_mode.f \
sci_gateway/fortran/sci_f_dispbpt.f \
sci_gateway/fortran/sci_f_intppty.f \
sci_gateway/fortran/sci_f_macrovar.f \
sci_gateway/fortran/sci_f_errcatch.f \
sci_gateway/fortran/sci_f_setbpt.f \
sci_gateway/fortran/sci_f_delbpt.f \
sci_gateway/fortran/where.f \
sci_gateway/fortran/sci_f_iserror.f

GATEWAY_CPP_SOURCES = 	\
	sci_gateway/cpp/sci_getmodules.cpp \
	sci_gateway/cpp/sci_clear.cpp \
	sci_gateway/cpp/sci_banner.cpp \
	sci_gateway/cpp/sci_quit.cpp \
	sci_gateway/cpp/sci_exit.cpp \
	sci_gateway/cpp/sci_argn.cpp \
	sci_gateway/cpp/sci_clearglobal.cpp \
	sci_gateway/cpp/sci_global.cpp \
	sci_gateway/cpp/sci_isglobal.cpp \
	sci_gateway/cpp/sci_getscilabmode.cpp \
	sci_gateway/cpp/sci_getos.cpp \
	sci_gateway/cpp/sci_exists.cpp \
	sci_gateway/cpp/sci_with_module.cpp \
	sci_gateway/cpp/sci_lasterror.cpp \
	sci_gateway/cpp/sci_getversion.cpp \
	sci_gateway/cpp/sci_warning.cpp \
	sci_gateway/cpp/sci_getmd5.cpp \
	sci_gateway/cpp/sci_error.cpp \
	sci_gateway/cpp/sci_pause.cpp \
	sci_gateway/cpp/sci_resume.cpp \
	sci_gateway/cpp/sci_abort.cpp \
	sci_gateway/cpp/sci_who.cpp \
	sci_gateway/cpp/sci_stacksize.cpp \
	sci_gateway/cpp/sci_format.cpp \
	sci_gateway/cpp/sci_sciargs.cpp \
	sci_gateway/cpp/sci_ieee.cpp


libscicore_la_CPPFLAGS = -I$(srcdir)/includes/ \
	-I$(srcdir)/src/c/ \
	-I$(top_srcdir)/libs/MALLOC/includes/ \
	-I$(top_srcdir)/libs/dynamiclibrary/includes/ \
	-I$(top_srcdir)/libs/doublylinkedlist/includes/ \
	-I$(top_srcdir)/modules/intersci/includes/ \
	-I$(top_srcdir)/modules/call_scilab/includes/ \
	-I$(top_srcdir)/modules/api_scilab/includes/ \
	-I$(top_srcdir)/modules/action_binding/includes/ \
	-I$(top_srcdir)/modules/output_stream/includes/ \
	-I$(top_srcdir)/modules/localization/includes/ \
	-I$(top_srcdir)/modules/functions_manager/includes/ \
	-I$(top_srcdir)/modules/elementary_functions/includes/ \
	-I$(top_srcdir)/modules/ast/includes/ \
	-I$(top_srcdir)/modules/parse/includes/ \
	-I$(top_srcdir)/modules/symbol/includes/ \
	-I$(top_srcdir)/modules/system_env/includes/ \
	-I$(top_srcdir)/modules/types/includes/ \
	-I$(top_srcdir)/modules/io/includes/ \
	-I$(top_srcdir)/modules/tclsci/includes/ \
	-I$(top_srcdir)/modules/dynamic_link/includes/ \
	-I$(top_srcdir)/modules/elementary_functions/includes/ \
	-I$(top_srcdir)/modules/string/includes/ \
	-I$(top_srcdir)/modules/fileio/includes/ \
	-I$(top_srcdir)/modules/operations/includes/ \
	-I$(top_srcdir)/modules/console/includes/ \
	-I$(top_srcdir)/modules/threads/includes/ \
	-I$(top_srcdir)/modules/history_manager/includes/ \
	-I$(top_srcdir)/modules/preferences/includes/ \
	-I$(top_srcdir)/modules/external_objects/includes/ \
	-I$(top_srcdir)/modules/jit_ocaml/includes/ \
	$(EIGEN_CPPFLAGS) \
	$(XML_FLAGS) \
	$(AM_CPPFLAGS)

# Used by sci_getdebuginfo:
if TCLTK
libscicore_la_CPPFLAGS += $(TCL_INC_PATH) \
		                  $(TK_INC_PATH)
endif

if MAINTAINER_MODE
pkglib_LTLIBRARIES = libscicore.la
noinst_LTLIBRARIES = libscicore-algo.la
else
noinst_LTLIBRARIES = libscicore-algo.la libscicore.la
endif


libscicore_la_LDFLAGS = $(XML_LIBS) $(AM_LDFLAGS)
#-Wl,--version-script,LibScilab_Import.def

# For the code check (splint)
CHECK_SRC= $(CORE_C_SOURCES) $(GATEWAY_C_SOURCES)
INCLUDE_FLAGS = $(libscicore_la_CPPFLAGS)

libscicore_algo_la_SOURCES = $(CORE_C_SOURCES) $(CORE_CPP_SOURCES) $(CORE_FORTRAN_SOURCES)
libscicore_la_SOURCES = $(GATEWAY_C_SOURCES) $(GATEWAY_FORTRAN_SOURCES) $(GATEWAY_CPP_SOURCES)
libscicore_algo_la_CPPFLAGS = $(libscicore_la_CPPFLAGS)

# Commented because it is easier to comment first the core module
libscicore_la_LIBADD = libscicore-algo.la

#### Target ######
modulename=core

#### core : Conf files ####
libscicore_la_rootdir = $(mydatadir)
libscicore_la_root_DATA =  license.txt


#### core : init scripts ####
libscicore_la_etcdir = $(mydatadir)/etc
libscicore_la_etc_DATA = etc/core.quit etc/core.start

#### core : gateway declaration ####
libscicore_la_sci_gatewaydir = $(mydatadir)/sci_gateway
libscicore_la_sci_gateway_DATA = sci_gateway/core_gateway.xml

#### core :  dtd files ####
libscicore_la_xmldir = $(mydatadir)/xml
libscicore_la_xml_DATA = xml/modules.dtd xml/version.dtd

#### core : include files ####
libscicore_la_includedir=$(pkgincludedir)
libscicore_la_include_HEADERS = \
includes/BOOL.h \
includes/core_math.h \
includes/ExceptionMessage.h \
includes/existfunction.h \
includes/freeArrayOfString.h \
includes/getcommandlineargs.h \
includes/getmemory.h \
includes/getos.h \
includes/machine.h \
includes/mode_exec.h \
includes/PATH_MAX.h \
includes/scilabDefaults.h \
includes/scisparse.h \
includes/sciquit.h \
includes/stack-c.h \
includes/sci_types.h \
includes/stack-def.h \
includes/stack.h \
includes/stack1.h \
includes/stack2.h \
includes/stack3.h \
includes/stackinfo.h \
includes/stackTypeVariable.h \
includes/version.h \
includes/getversion.h \
includes/backtrace_print.h

if GUI
USEANT=1
endif


include $(top_srcdir)/Makefile.incl.am
