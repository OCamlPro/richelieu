##
##  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
##  Copyright (C) 2008-2009 - DIGITEO - Sylvestre LEDRU
##  Copyright (C) 2008-2008 - DIGITEO - Bruno JOFRET
##
##  This file must be used under the terms of the CeCILL.
##  This source file is licensed as described in the file COPYING, which
##  you should have received as part of this distribution.  The terms
##  are also available at
##  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
##
##
FLEX_SCANSCILAB_CXX	= src/cpp/scanscilab.cpp
BISON_PARSESCILAB_CXX = src/cpp/parsescilab.cpp

if BUILD_PARSER

BUILT_SOURCES = $(FLEX_SCANSCILAB_CXX) $(BISON_PARSESCILAB_CXX)

LFLAGS = --noline --nounistd -o $(FLEX_SCANSCILAB_CXX)
FLEX_SCANSCILAB_LL	= src/flex/scanscilab.ll

BISON_FLAGS = --no-lines -dvy -ra
BISON_PARSESCILAB_YY	= src/bison/parsescilab.yy
BISON_PARSESCILAB_OUTPUT= src/bison/parsescilab.output
BISON_PARSESCILAB_HXX	= includes/parsescilab.hxx

$(FLEX_SCANSCILAB_CXX): $(FLEX_SCANSCILAB_LL)
	$(LEX) $(AM_FLAGS) $(LFLAGS) $(FLEX_SCANSCILAB_LL)

$(BISON_PARSESCILAB_CXX): $(BISON_PARSESCILAB_YY)
	$(YACC) $(AM_FLAGS) $(BISON_FLAGS) $(YFLAGS) $(BISON_PARSESCILAB_YY) \
	&& mv y.tab.c $(BISON_PARSESCILAB_CXX) \
	&& mv y.tab.h $(BISON_PARSESCILAB_HXX) \
	&& mv y.output $(BISON_PARSESCILAB_OUTPUT)

# Do not clean these files since we are storing them in the repository
#CLEANFILES = $(BISON_PARSESCILAB_CPP) \
#		$(BISON_PARSESCILAB_HXX) \
#		$(BISON_PARSESCILAB_OUTPUT) \
#		$(FLEX_SCANSCILAB_CPP)

endif

PARSE_CXX_SOURCES 	= $(BISON_PARSESCILAB_CXX) \
			$(FLEX_SCANSCILAB_CXX) \
			src/cpp/parser.cpp \
			src/cpp/printerror.cpp

pkglib_LTLIBRARIES = libsciparse.la

libsciparse_la_SOURCES 	= $(PARSE_CXX_SOURCES)

libsciparse_la_CPPFLAGS = \
	-Iincludes \
	-Isrc/cpp \
    -I$(top_srcdir)/libs/dynamiclibrary/includes \
    -I$(top_srcdir)/libs/MALLOC/includes \
	-I$(top_builddir)/modules/core/includes \
	-I$(top_builddir)/modules/ast/includes \
	-I$(top_builddir)/modules/localization/includes \
	-I$(top_builddir)/modules/symbol/includes \
	-I$(top_builddir)/modules/types/includes \
	-I$(top_builddir)/modules/output_stream/includes \
	-I$(top_builddir)/modules/string/includes \
	-I$(top_builddir)/modules/system_env/includes \
	$(AM_CPPFLAGS)

include $(top_srcdir)/Makefile.incl.am
