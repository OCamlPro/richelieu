# Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
# Copyright (C) 2006-2007 - INRIA - Sylvestre LEDRU
#
# This file is distributed under the same license as the Scilab package.

STRING_CPP_SOURCES = src/cpp/stripblanks.cpp

STRING_C_SOURCES = \
src/c/convstr.c \
src/c/men_Sutils.c \
src/c/tokens.c \
src/c/strcatfunction.c \
src/c/strsplitfunction.c \
src/c/partfunction.c \
src/c/InitializeString.c \
src/c/setScilabCharactersCodes.c \
src/c/cvstr.c \
src/c/getfastcode.c \
src/c/pcre_private.c \
src/c/pcre_error.c \
src/c/isletter.c \
src/c/isdigit.c \
src/c/strsubst.c \
src/c/stringsstrrev.c \
src/c/stringsCompare.c \
src/c/isalphanum.c \
src/c/StringConvert.c \
src/c/os_wcsdup.c \
src/c/os_wcstok.c \
src/c/scistrtostr.c \
src/c/stringToDouble.c

STRING_FORTRAN_SOURCES =

GATEWAY_C_SOURCES = \
    sci_gateway/c/gw_string.c \
    sci_gateway/c/sci_strsplit.c \
    sci_gateway/c/sci_isdigit.c \
    sci_gateway/c/sci_isalphanum.c \
    sci_gateway/c/sci_isascii.c

GATEWAY_CPP_SOURCES = \
    sci_gateway/cpp/sci_stripblanks.cpp \
    sci_gateway/cpp/sci_grep.cpp \
    sci_gateway/cpp/string_gw.cpp \
    sci_gateway/cpp/sci_regexp.cpp \
    sci_gateway/cpp/sci_part.cpp \
    sci_gateway/cpp/sci_length.cpp \
    sci_gateway/cpp/sci_strindex.cpp \
    sci_gateway/cpp/sci_strsubst.cpp \
    sci_gateway/cpp/sci_ascii.cpp \
    sci_gateway/cpp/sci_strcat.cpp \
    sci_gateway/cpp/sci_string.cpp \
    sci_gateway/cpp/sci_convstr.cpp \
    sci_gateway/cpp/sci_strncpy.cpp \
    sci_gateway/cpp/sci_strrchr.cpp \
    sci_gateway/cpp/sci_strchr.cpp \
    sci_gateway/cpp/sci_strstr.cpp \
    sci_gateway/cpp/sci_strrev.cpp \
    sci_gateway/cpp/sci_strtod.cpp \
    sci_gateway/cpp/sci_tokens.cpp \
    sci_gateway/cpp/sci_strcmp.cpp \
    sci_gateway/cpp/sci_isletter.cpp \
    sci_gateway/cpp/sci_strcspn.cpp \
    sci_gateway/cpp/sci_strtok.cpp \
    sci_gateway/cpp/sci_strspn.cpp

GATEWAY_FORTRAN_SOURCES =  sci_gateway/fortran/sci_f_string.f \
sci_gateway/fortran/strops.f

libscistring_la_CFLAGS= $(PCRE_CFLAGS) \
	-I$(srcdir)/src/c \
	-I$(srcdir)/includes/ \
	-I$(top_srcdir)/libs/MALLOC/includes/ \
	-I$(top_srcdir)/modules/operations/includes \
	-I$(top_srcdir)/modules/functions/includes \
	-I$(top_srcdir)/modules/api_scilab/includes \
	-I$(top_srcdir)/modules/localization/includes \
	-I$(top_srcdir)/modules/output_stream/includes \
	-I$(top_srcdir)/modules/system_env/includes

libscistring_la_CPPFLAGS= $(PCRE_CFLAGS) \
	-I$(srcdir)/src/c \
	-I$(srcdir)/src/cpp \
	-I$(srcdir)/includes/ \
	-I$(top_srcdir)/libs/MALLOC/includes/ \
	-I$(top_srcdir)/modules/operations/includes \
	-I$(top_srcdir)/modules/api_scilab/includes \
	-I$(top_srcdir)/modules/localization/includes \
	-I$(top_srcdir)/modules/output_stream/includes \
	-I$(top_srcdir)/modules/functions_manager/includes \
	-I$(top_srcdir)/modules/symbol/includes \
	-I$(top_srcdir)/modules/ast/includes \
	-I$(top_srcdir)/modules/elementary_funcions/includes \
	-I$(top_srcdir)/modules//includes \
	-I$(top_srcdir)/modules/core/includes \
	-I$(top_srcdir)/modules/types/includes \
    -I$(top_srcdir)/libs/dynamiclibrary/includes

libscistring_algo_la_CPPFLAGS = $(libscistring_la_CPPFLAGS)

if MAINTAINER_MODE
pkglib_LTLIBRARIES = libscistring.la
noinst_LTLIBRARIES = libscistring-algo.la
else
noinst_LTLIBRARIES = libscistring-algo.la libscistring.la
endif


libscistring_la_LDFLAGS = -version-number $(SCILAB_LIBRARY_VERSION) $(PCRE_LIBS)

libscistring_algo_la_SOURCES = $(STRING_C_SOURCES) $(STRING_CPP_SOURCES) $(STRING_FORTRAN_SOURCES)
libscistring_la_SOURCES = $(GATEWAY_C_SOURCES) $(GATEWAY_FORTRAN_SOURCES) $(GATEWAY_CPP_SOURCES)
libscistring_algo_la_CFLAGS = $(libscistring_la_CFLAGS)

# For the code check (splint)
CHECK_SRC= $(STRING_C_SOURCES) $(GATEWAY_C_SOURCES)
INCLUDE_FLAGS = $(libscistring_la_CFLAGS)

# commented since it is the first module to be processed
libscistring_la_LIBADD = libscistring-algo.la

#### Target ######
modulename=string


#### string : Conf files ####
libscistring_la_rootdir = $(mydatadir)
libscistring_la_root_DATA =  license.txt

#### string : init scripts ####
libscistring_la_etcdir = $(mydatadir)/etc
libscistring_la_etc_DATA = etc/string.quit etc/string.start

#### string : gateway declaration ####
libscistring_la_sci_gatewaydir = $(mydatadir)/sci_gateway
libscistring_la_sci_gateway_DATA = sci_gateway/string_gateway.xml

include $(top_srcdir)/Makefile.incl.am

