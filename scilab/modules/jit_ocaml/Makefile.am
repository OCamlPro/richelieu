#
# Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
# Copyright (C) DIGITEO - 2010 - Allan CORNET
# Copyright (C) DIGITEO - 2010 - Bruno JOFRET
#
# This file must be used under the terms of the CeCILL.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution.  The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
#


# We generate:
# - libscijit_ocaml.la : a bridge between the C++ AST and a serialized
#      representation
# - libocpsci.la : some C++ code to be linked statically, to manipulate C++
#      objects from OCaml
# - ocamlcode.o : a module containing all the OCaml code, to be linked
#      statically with libocpsci.la

#### Target ######
modulename=jit_ocaml

#### jit_ocaml : Conf files ####
libscijit_ocaml_la_rootdir = $(mydatadir)
libscijit_ocaml_la_root_DATA =  license.txt

OCAMLLIB=$(shell ocamlc -where)


#### jit_ocaml : init scripts ####
libscijit_ocaml_la_etcdir = $(mydatadir)/etc
libscijit_ocaml_la_etc_DATA = etc/jit_ocaml.quit \
			  etc/jit_ocaml.start

include $(top_srcdir)/Makefile.incl.am

JIT_OCAML_CPP_SOURCES = \
			src/cpp/scicaml_ast2string.cpp \
			src/cpp/scicaml_string2ast.cpp \
			src/cpp/scicaml_stubs.cpp

libscijit_ocaml_la_CFLAGS = \
   -I$(srcdir)/includes \
   -I $(OCAMLLIB)

CLASSIC_CPPFLAGS= \
	-I$(srcdir)/includes \
        -I$(top_srcdir)/libs/MALLOC/includes \
	-I$(top_srcdir)/modules/elementary_functions/includes \
	-I$(top_srcdir)/modules/localization/includes \
	-I$(top_srcdir)/modules/operations/includes \
	-I$(top_srcdir)/modules/symbol/includes \
	-I$(top_srcdir)/modules/ast/includes \
	-I$(top_srcdir)/modules/types/includes \
	-I$(top_srcdir)/modules/parse/includes \
	-I$(top_srcdir)/modules/system_env/includes \
	-I$(top_srcdir)/modules/functions_manager/includes \
	-I$(top_srcdir)/modules/string/includes \
	-I$(top_srcdir)/modules/threads/includes \
        -I$(top_srcdir)/modules/api_scilab/includes \
        -I$(top_srcdir)/modules/console/includes \
        -I$(top_srcdir)/modules/output_stream/includes \
        -I$(top_srcdir)/libs/dynamiclibrary/includes \
	-I src/cpp \
	$(EIGEN_CPPFLAGS) \
	$(AM_CPPFLAGS)

libscijit_ocaml_la_CPPFLAGS = $(CLASSIC_CPPFLAGS)

pkglib_LTLIBRARIES = libscijit_ocaml.la
#BUILT_SOURCES = ocamlcode.o
#
libscijit_ocaml_la_LIBADD= ocamlcode.o
libscijit_ocaml_la_SOURCES = $(JIT_OCAML_CPP_SOURCES)

noinst_LTLIBRARIES= libocpsci.la
libocpsci_la_SOURCES= \
    src/cpp/scicaml_stubs.cpp \
    src/cpp/ocpsci_gw.cpp \
    src/cpp/ocpsci_stubs.cpp

EXTRA_libocpsci_la_DEPENDENCIES=ocamlcode.o

libocpsci_la_CFLAGS = \
   -I$(srcdir)/includes \
   -I $(OCAMLLIB)
libocpsci_la_CPPFLAGS = $(CLASSIC_CPPFLAGS) -I $(OCAMLLIB)

OCAMLOPT=ocamlopt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

OCAML_COMMON_MLS = \
	src/common/scilabSymbol.ml \
	src/common/scilabAst.ml \
	src/common/scilabString2Ast.ml \
	src/common/scilabAstPrinter.ml \
	src/common/scilabPrettyPrinter.ml

OCAML_COMMON_MLIS = \
	src/interp/scilabAstPrinter.mli

OCAML_INTERP_MLS = \
	src/interp/scilabMisc.ml \
	src/interp/scilabValue.ml \
	src/interp/scilabContext.ml \
	src/interp/scilabTypes.ml \
	src/interp/scilabAst2String.ml \
	src/interp/scilabInternalType.ml \
	src/interp/scilabInterp.ml \
	src/interp/scilabCallbacks.ml

OCAML_INTERP_MLIS = \
	src/interp/scilabValue.mli \
	src/interp/scilabContext.mli \
	src/interp/scilabAst2String.mli \
	src/interp/scilabInternalType.mli \
	src/interp/scilabInterp.mli \
	src/interp/scilabCallbacks.mli

OCAML_PARSER_MLS = \
	src/parser/scilabPreParser.ml \
	src/parser/scilabParser.ml \
	src/parser/uutf.ml \
	src/parser/scilabLexer.ml

OCAML_PARSER_MLIS= \
	src/parser/uutf.mli \
	src/parser/scilabParser.mli

OCAML_SCILINT_MLS = \
	src/scilint/scilabFunctionAnalyze.ml \
	src/scilint/scilabUtils.ml \
	src/scilint/scilabDeffRefactoring.ml \
	src/scilint/scilabAstStats.ml \
	src/scilint/scilintMain.ml

OCAML_SCILINT_MLIS = 


######### OCAML_JIT

OCAML_JIT_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_INTERP_MLS)

OCAML_JIT_MLIS = \
	$(OCAML_COMMON_MLIS) \
	$(OCAML_INTERP_MLIS)

OCAML_JIT_CMIS = $(OCAML_JIT_MLS:.ml=.cmi) $(OCAML_JIT_MLIS:.mli=.cmi)
OCAML_JIT_CMXS = $(OCAML_JIT_MLS:.ml=.cmx)
OCAML_JIT_OBJS = $(OCAML_JIT_MLS:.ml=.o)

########## SCILINT

SCILINT_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_PARSER_MLS) \
	$(OCAML_SCILINT_MLS)

SCILINT_MLIS = \
	$(OCAML_COMMON_MLIS) \
	$(OCAML_PARSER_MLIS) \
	$(OCAML_SCILINT_MLIS)

SCILINT_CMIS = $(SCILINT_MLS:.ml=.cmi) $(SCILINT_MLIS:.mli=.cmi)
SCILINT_CMXS = $(SCILINT_MLS:.ml=.cmx)
SCILINT_OBJS = $(SCILINT_MLS:.ml=.o)

OCAML_INCL= -I src/common -I src/parser -I src/scilint -I src/interp
OPTFLAGS = -g -c -fPIC $(OCAML_INCL)

CLEANFILES= \
	$(OCAML_JIT_CMIS)  \
	$(OCAML_JIT_CMXS)  \
	$(OCAML_JIT_OBJS)  \
	$(SCILINT_CMIS)  \
	$(SCILINT_CMXS)  \
	$(SCILINT_OBJS)  \
	scicaml_stubs.o \
	ocaml_parser \
	src/lex/*.cm* \
	src/lex/*.o \
	src/yacc/*.cm* \
	src/yacc/*.o \
	src/lex/scilabLexer.ml \
	src/yacc/scilabParser.ml \
	src/yacc/scilabParser.mli \
	scilab_parser \
	macros/lib


scilint : $(SCILINT_CMXS)
	$(OCAMLOPT) bigarray.cmxa str.cmxa \
	  -o scilint $(SCILINT_CMXS)


ocamlcode.o: $(OCAML_JIT_CMXS)
	$(OCAMLOPT) -o ocamlcode.o -output-obj unix.cmxa $(OCAML_JIT_CMXS)

libasmrun.a:
	cp $(OCAMLLIB)/libasmrun.a .
libunix.a:
	cp $(OCAMLLIB)/libunix.a .


depend_ocaml: $(OCAML_PARSER_MLS)
	$(OCAMLDEP) -native $(OCAML_INCL) \
		$(OCAML_JIT_MLS) $(OCAML_JIT_MLIS) \
		$(SCILINT_MLS) $(SCILINT_MLIS) > .depend_ocaml
include .depend_ocaml

########## COMMON
SUFFIXES += .ml .mli .mll .mly .cmi .cmx

.ml.cmx:
	$(OCAMLOPT) $(OPTFLAGS) -c $<

.mll.cmx:
	$(OCAMLLEX) $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.ml

.mli.cmi:
	$(OCAMLOPT) $(OPTFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.cmx:
	$(OCAMLYACC) $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.mli
	$(OCAMLOPT) $(OPTFLAGS) -c $*.ml

.mly.cmi:
	$(OCAMLYACC) -v $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.mli

