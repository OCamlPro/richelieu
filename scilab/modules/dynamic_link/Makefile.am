# Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
# Copyright (C) 2006 - INRIA - Sylvestre LEDRU
#
# This file is distributed under the same license as the Scilab package.


DYNAMIC_LINK_C_SOURCES = \
    src/c/dl_genErrorMessage.c \
    src/c/GetFunctionByName.c \
    src/c/ilib_verbose.c

DYNAMIC_LINK_CPP_SOURCES = \
    src/cpp/dynamic_link.cpp \
    src/cpp/addinter.cpp

GATEWAY_C_SOURCES = \
    sci_gateway/c/gw_dynamic_link.c \
    sci_gateway/c/sci_call.c

GATEWAY_CPP_SOURCES = \
    sci_gateway/cpp/dynamic_link_gw.cpp \
    sci_gateway/cpp/sci_link.cpp \
    sci_gateway/cpp/sci_c_link.cpp \
    sci_gateway/cpp/sci_ulink.cpp \
    sci_gateway/cpp/sci_ilib_verbose.cpp \
    sci_gateway/cpp/sci_getdynlibext.cpp \
    sci_gateway/cpp/sci_addinter.cpp


GATEWAY_FORTRAN_SOURCES = sci_gateway/fortran/sci_fort.f

libscidynamic_link_la_CPPFLAGS = \
	-I$(srcdir)/includes/ \
	-I$(srcdir)/src/c/ \
    -I$(top_srcdir)/libs/MALLOC/includes/ \
    -I$(top_srcdir)/libs/dynamiclibrary/includes/ \
    -I$(top_srcdir)/modules/io/includes/ \
    -I$(top_srcdir)/modules/api_scilab/includes/ \
	-I$(top_srcdir)/modules/operations/includes/ \
	-I$(top_srcdir)/modules/symbol/includes/ \
	-I$(top_srcdir)/modules/types/includes/ \
	-I$(top_srcdir)/modules/output_stream/includes/ \
	-I$(top_srcdir)/modules/functions_manager/includes/ \
	-I$(top_srcdir)/modules/ast/includes/ \
    -I$(top_srcdir)/modules/string/includes/ \
    -I$(top_srcdir)/modules/fileio/includes/ \
    -I$(top_srcdir)/modules/system_env/includes/ \
    -I$(top_srcdir)/modules/threads/includes/ \
    -I$(top_srcdir)/modules/localization/includes/ \
    $(AM_CPPFLAGS)

if MAINTAINER_MODE
pkglib_LTLIBRARIES = libscidynamic_link.la
noinst_LTLIBRARIES = libscidynamic_link-algo.la
else
noinst_LTLIBRARIES = libscidynamic_link-algo.la libscidynamic_link.la
endif



libscidynamic_link_algo_la_SOURCES = $(DYNAMIC_LINK_C_SOURCES) $(DYNAMIC_LINK_CPP_SOURCES)
libscidynamic_link_la_SOURCES = $(GATEWAY_C_SOURCES) $(GATEWAY_CPP_SOURCES) $(GATEWAY_FORTRAN_SOURCES)
libscidynamic_link_algo_la_CPPFLAGS = $(libscidynamic_link_la_CPPFLAGS)


# For the code check (splint)
CHECK_SRC=  $(DYNAMIC_LINK_C_SOURCES) $(GATEWAY_C_SOURCES)
INCLUDE_FLAGS = $(libscidynamic_link_la_CPPFLAGS)

# cyclic  
libscidynamic_link_la_LIBADD = libscidynamic_link-algo.la

#### Target ######
modulename=dynamic_link


#### dynamic_link : Conf files ####
libscidynamic_link_la_rootdir = $(mydatadir)
libscidynamic_link_la_root_DATA =  license.txt 

#### dynamic_link : init scripts #### 
libscidynamic_link_la_etcdir = $(mydatadir)/etc
libscidynamic_link_la_etc_DATA = etc/dynamic_link.quit etc/dynamic_link.start

#### dynamic_link : gateway declaration ####
libscidynamic_link_la_sci_gatewaydir = $(mydatadir)/sci_gateway
libscidynamic_link_la_sci_gateway_DATA = sci_gateway/dynamic_link_gateway.xml

#### dynamic_link : compilation scripts #### 
libscidynamic_link_la_scriptdir = $(mydatadir)/src/scripts/
libscidynamic_link_la_script_DATA = src/scripts/Makefile.in \
src/scripts/configure.ac \
src/scripts/ltmain.sh \
src/scripts/Makefile.am \
src/scripts/aclocal.m4

libscidynamic_link_la_script_SCRIPTS = src/scripts/compilerDetection.sh \
src/scripts/config.guess \
src/scripts/config.sub \
src/scripts/configure \
src/scripts/depcomp \
src/scripts/install-sh \
src/scripts/missing \
src/scripts/scicompile.sh

# This is a cache file for the dynamic link (on use)
CLEANFILES = src/scripts/Makefile.orig src/scripts/config.log src/scripts/config.status src/scripts/libtool

include $(top_srcdir)/Makefile.incl.am
