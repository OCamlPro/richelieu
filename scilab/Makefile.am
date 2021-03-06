#
# Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
# Copyright (C) INRIA - 2006-2008 - Sylvestre Ledru
# Copyright (C) DIGITEO - 2009 - Sylvestre Ledru
#
# This file must be used under the terms of the CeCILL.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution.  The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
#

## Process this file with automake to produce Makefile.in (which will be changed by the ./configure)

AUTOMAKE_OPTIONS = 1.11 dist-zip dist-bzip2

SUBDIRS= libs \
	modules \
	contrib \
	desktop

ACLOCAL_AMFLAGS = -I m4/

# Set AM_* SCI detected settings
AM_CPPFLAGS = $(SCI_CPPFLAGS)
AM_CFLAGS   = $(SCI_CFLAGS)
AM_CXXFLAGS = $(SCI_CXXFLAGS)
AM_FFLAGS   = $(SCI_FFLAGS)
AM_LDFLAGS  = $(SCI_LDFLAGS)

changelogs = CHANGES CHANGES_5.1.X CHANGES_5.2.X CHANGES_5.3.X CHANGES_5.4.X CHANGES_5.5.X CHANGES_6.0.X

bin_PROGRAMS		= scilab-bin scilab-cli-bin

bin_SCRIPTS			= bin/scilab bin/scilab-adv-cli bin/scilab-cli

if IS_MACOSX
bin_SCRIPTS 		+= bin/checkmacosx.applescript
endif

scilab_bin_LDFLAGS 	    = $(AM_LDFLAGS)
scilab_cli_bin_LDFLAGS  = $(AM_LDFLAGS)

if IS_MACOSX
scilab_bin_LDFLAGS 		+= -framework Cocoa
endif

scilab_bin_CPPFLAGS = \
    -I$(top_srcdir)/modules/console/includes/ \
	-I$(top_srcdir)/modules/output_stream/includes/ \
    -I$(top_srcdir)/modules/parse/includes/ \
    -I$(top_srcdir)/modules/ast/includes/ \
    -I$(top_srcdir)/modules/symbol/includes/ \
    -I$(top_srcdir)/modules/functions_manager/includes/ \
    -I$(top_srcdir)/modules/types/includes/ \
    -I$(top_srcdir)/modules/system_env/includes/ \
    -I$(top_srcdir)/modules/operations/includes/ \
    -I$(top_srcdir)/modules/elementary_functions/includes/ \
    -I$(top_srcdir)/modules/fileio/includes/ \
    -I$(top_srcdir)/modules/string/includes/ \
    -I$(top_srcdir)/modules/history_manager/includes/ \
    -I$(top_srcdir)/modules/threads/includes/ \
    -I$(top_srcdir)/modules/api_scilab/includes/ \
    -I$(top_srcdir)/modules/localization/includes/ \
    -I$(top_srcdir)/modules/jit_ocaml/includes/ \
    -I$(top_srcdir)/libs/dynamiclibrary/includes/ \
    -I$(top_srcdir)/libs/MALLOC/includes/ \
    $(EIGEN_CPPFLAGS) \
	$(AM_CPPFLAGS)
# --no-undefined --relax

# Localization dep already exists because of the `exportLocaleToSystem' symbol
# (causing issues on some platform)
COMMON_LIBS = $(LTLIBINTL)

scilab_bin_SOURCES = \
    $(top_srcdir)/modules/core/src/cpp/scilab.cpp \
    $(top_srcdir)/modules/core/src/cpp/tasks.cpp

if IS_MACOSX
scilab_bin_SOURCES		+=  $(top_srcdir)/modules/core/src/c/initMacOSXEnv.c
endif

OCAMLLIB=$(shell ocamlc -where)

JIT_LIBS = \
  modules/jit_ocaml/ocamlcode.o \
  modules/jit_ocaml/.libs/libocpsci.a \
  $(OCAMLLIB)/libunix.a \
  $(OCAMLLIB)/libasmrun.a 

modules/jit_ocaml/.libs/libocpsci.a:
	$(MAKE) -C modules/jit_ocaml libocpsci.la

modules/jit_ocaml/ocamlcode.o:
	$(MAKE) -C modules/jit_ocaml ocamlcode.o

scilab_bin_LDADD 		= $(COMMON_LIBS)
if GUI
# Force the libscijvm to be linked first. Under Debian, this avoid some reordering issues during the link
scilab_bin_LDADD += $(top_builddir)/modules/jvm/libscijvm.la
endif
scilab_bin_LDADD += $(top_builddir)/modules/libscilab.la $(JIT_LIBS) $(FLIBS)

# Match scilab -nwni (ie command line without graphics)
scilab_cli_bin_SOURCES	=	$(top_srcdir)/modules/core/src/cpp/scilab.cpp \
							$(top_srcdir)/modules/core/src/cpp/tasks.cpp

scilab_cli_bin_CPPFLAGS = -DWITHOUT_GUI $(scilab_bin_CPPFLAGS)
scilab_cli_bin_LDADD 	= $(COMMON_LIBS) $(top_builddir)/modules/libscilab-cli.la $(JIT_LIBS) $(FLIBS)


if NEED_JAVA
# Don't do it for scilab_cli_bin. We want to be able to call it without
# the java libs
        scilab_bin_LDADD += $(JAVA_JNI_LIBS)
endif

if IS_SOLARIS
# The binary itself needs this dep
        scilab_bin_LDADD += $(BLAS_LIBS)
endif

if ENABLE_STATIC
scilab_static_bin_LDFLAGS = -static -static-libgcc --start-group $(AM_LDFLAGS)
scilab_static_bin_CFLAGS  = -Wl,--start-group `$(CXX) -print-file-name=libstdc++.a` $(AM_CFLAGS)

#-static
bin_PROGRAMS	   		+= scilab-static-bin
scilab_static_bin_LDADD = $(top_builddir)/modules/libscilab.la

scilab_static_bin_SOURCES = $(scilab_bin_SOURCES)
endif

LANG_DOMAIN=scilab

TARGETS_ALL=macros

#  The user wants to compile the localization files
if GENERATE_LOCALIZATION_FILES
TARGETS_ALL+=localization
endif

if COMPILATION_TESTS
TARGETS_ALL+=check-java
endif

# check is performed with scilab
TESTS_ENVIRONMENT = LANG=C $(top_builddir)/bin/scilab-cli -nb -noatomsautoload -f
TESTS = $(top_srcdir)/modules/development_tools/tests/unit_tests/test_run_level_1.tst

all-local: $(TARGETS_ALL)

rootdir=$(pkgdatadir)/etc/
root_DATA = $(top_srcdir)/etc/scilab.start \
$(top_srcdir)/etc/scilab.quit \
$(top_srcdir)/etc/toolboxes.xml \
$(top_builddir)/etc/jvm_options.xml \
$(top_builddir)/etc/modules.xml \
$(top_builddir)/etc/classpath.xml \
$(top_builddir)/etc/logging.properties \
$(top_builddir)/etc/librarypath.xml

infofilesdir=$(pkgdatadir)/
infofiles_DATA = $(top_srcdir)/CHANGES_2.X \
$(top_srcdir)/CHANGES_3.X \
$(top_srcdir)/CHANGES_4.X \
$(top_srcdir)/CHANGES_5.0.X \
$(top_srcdir)/CHANGES_5.1.X \
$(top_srcdir)/CHANGES_5.2.X \
$(top_srcdir)/CHANGES_5.3.X \
$(top_srcdir)/CHANGES_5.4.X \
$(top_srcdir)/CHANGES_6.0.X \
$(top_srcdir)/ACKNOWLEDGEMENTS \
$(top_srcdir)/COPYING \
$(top_srcdir)/COPYING-BSD \
$(top_srcdir)/COPYING-FR \
$(top_srcdir)/README_Unix \
$(top_srcdir)/RELEASE_NOTES_5.0.X \
$(top_srcdir)/RELEASE_NOTES_5.1.X \
$(top_srcdir)/RELEASE_NOTES_5.2.X \
$(top_srcdir)/RELEASE_NOTES_5.3.X \
$(top_srcdir)/Readme_Visual.txt \
$(top_srcdir)/README_Windows.txt \
$(top_builddir)/Version.incl

valgrinddir=$(pkgdatadir)/tools/profiling
valgrind_DATA = $(top_srcdir)/tools/profiling/valgrind.supp

macros: $(top_builddir)/scilab-cli-bin $(top_builddir)/bin/scilab-cli check-jvm-dep
	-@( cd $(top_builddir) ; \
	echo "$(top_builddir)/bin/scilab-cli -ns -noatomsautoload -f modules/functions/scripts/buildmacros/buildmacros.sce" ; \
	$(top_builddir)/bin/scilab-cli -ns -noatomsautoload -f modules/functions/scripts/buildmacros/buildmacros.sce) || exit 1


check-jvm-dep:
# Check that scilab-cli does not have a dependency on libjvm
if !IS_MACOSX
	@ldd .libs/scilab-cli-bin|grep libjvm > /dev/null; \
	if test $$? == 0; then \
		echo "Found dependency from scilab-cli-bin to libjvm found"; exit 2;\
	fi
endif

# Build the public Javadoc (javasci v2 and Java/Scilab types)
# This is not done in the module itself because its needs also the "types"
# module
javadoc:
	$(JAVADOC) -noindex -nohelp -nonavbar -notree \
	-d $(top_builddir)/modules/javasci/javadoc \
	-sourcepath $(top_srcdir)/modules/javasci/src/java:$(top_srcdir)/modules/types/src/java::$(top_srcdir)/modules/localization/src/java \
	org.scilab.modules.javasci org.scilab.modules.types

javadoc-all:
	MASK="$(top_srcdir)/modules/*/src/java/"; \
	MODULES=`ls -d $$MASK`; \
	for M in $$MODULES; do \
		LISTMODULES="$$M:$$LISTMODULES"; \
		MODULE=$$(echo $$M|cut -d/ -f3); \
		LISTPKG="$$LISTPKG org.scilab.modules.$$MODULE"; \
	done; \
	$(JAVADOC) \
	-d $(top_builddir)/javadocall \
	-sourcepath $$LISTMODULES \
	$$LISTPKG

doc-fr_FR:
	@$(MAKE) $(AM_MAKEFLAGS) doc ALL_LINGUAS_DOC=fr_FR

doc-en_US:
	@$(MAKE) $(AM_MAKEFLAGS) doc ALL_LINGUAS_DOC=en_US

doc-pt_BR:
	@$(MAKE) $(AM_MAKEFLAGS) doc ALL_LINGUAS_DOC=pt_BR

doc-ru_RU:
	@$(MAKE) $(AM_MAKEFLAGS) doc ALL_LINGUAS_DOC=ru_RU

doc-ja_JP:
	@$(MAKE) $(AM_MAKEFLAGS) doc ALL_LINGUAS_DOC=ja_JP


doc: javadoc $(top_builddir)/scilab-bin $(top_builddir)/bin/scilab-adv-cli
if BUILD_HELP
	@if test -x $(top_builddir)/scilab-bin; then \
		cd $(top_builddir);\
		for l in $(ALL_LINGUAS_DOC); do \
			echo "-- Building documentation ($$l) --"; \
			COMMAND="try xmltojar([],[],'$$l');catch disp(lasterror()); exit(-1);end;exit(0);"; \
			echo "LANG=C SCI_DISABLE_TK=1 SCI_JAVA_ENABLE_HEADLESS=1 $(top_builddir)/bin/scilab-adv-cli -noatomsautoload -nb -nouserstartup -e \"$$COMMAND\"";\
			LANG=C SCI_DISABLE_TK=1 SCI_JAVA_ENABLE_HEADLESS=1 $(top_builddir)/bin/scilab-adv-cli -noatomsautoload -nb -l $$l -nouserstartup -e "$$COMMAND" || exit 1;\
		done; \
	else \
		echo "Cannot find Scilab-adv-cli binary"; \
	fi
else
	@echo "Cannot buid help. Add --enable-build-help to the ./configure if you want to build it."
endif

doc-web: javadoc $(top_builddir)/scilab-bin $(top_builddir)/bin/scilab-adv-cli
if BUILD_HELP
	@if test -x $(top_builddir)/scilab-bin; then \
		cd $(top_builddir);\
		for l in $(ALL_LINGUAS_DOC); do \
			echo "-- Building documentation ($$l) --"; \
			COMMAND="try xmltoweb([],[],'$$l');catch exit(-1);end;exit(0);";\
			echo "LANG=C SCI_DISABLE_TK=1 SCI_JAVA_ENABLE_HEADLESS=1 $(top_builddir)/bin/scilab-adv-cli -nb -l $$l -noatomsautoload -nouserstartup -e \"$$COMMAND\"";\
			LANG=C SCI_DISABLE_TK=1 SCI_JAVA_ENABLE_HEADLESS=1 $(top_builddir)/bin/scilab-adv-cli -nb -l $$l -noatomsautoload -nouserstartup -e "$$COMMAND" || exit 1;\
			JAVADOC_TARGET=$(top_builddir)/modules/helptools/web/$$l/javasci; \
			if test ! -d $$JAVADOC_TARGET; then \
				mkdir $$JAVADOC_TARGET; \
				ln -s ../../../../javasci/javadoc/ $$JAVADOC_TARGET/; \
			fi; \
		done; \
	else \
		echo "Cannot find Scilab-adv-cli binary"; \
	fi
else
	@echo "Cannot buid help. Add --enable-build-help to the ./configure if you want to build it."
endif

doxygen: modules/development_tools/etc/doxyfile
if DOXYGEN
	mkdir $(top_builddir)/reports/
	$(DOXYGEN_BIN) modules/development_tools/etc/doxyfile
endif

check-help:$(top_builddir)/scilab-cli-bin $(top_builddir)/bin/scilab-cli
	@if test -x $(top_builddir)/scilab-cli-bin; then \
		for l in $(ALL_LINGUAS_DOC); do \
			echo "-- Checking documentation ($$l) --"; \
			COMMAND="try check_help();catch exit(-1);end;exit(0);";\
			echo "$(top_builddir)/bin/scilab-cli -nb -l $$l -noatomsautoload -e \"$$COMMAND\"";\
			$(top_builddir)/bin/scilab-cli -nb -l $$l -noatomsautoload -e "$$COMMAND" || exit 1;\
		done; \
	else \
		echo "Cannot find Scilab binary"; \
	fi

code-coverage:
if CODE_COVERAGE
	@if test ! -d $(top_builddir)/coverage/; then \
		mkdir $(top_builddir)/coverage/; \
	fi; \
	touch $(top_srcdir)/modules/differential_equations/callinter.h $(top_srcdir)/modules/linear_algebra/callinter.h $(top_srcdir)/modules/scicos/callinter.h $(top_srcdir)/modules/scicos_blocks/callinter.h; \
	$(LCOV) --directory $(top_builddir) --capture --ignore-errors source --output-file $(top_builddir)/coverage/scilab-code-coverage.info; \
	$(GENHTML) -o $(top_builddir)/coverage/results/ --show-details --highlight --legend $(top_builddir)/coverage/scilab-code-coverage.info; \
	echo "To open the result: firefox  $(top_builddir)/coverage/results/index.html"; \
	rm -f $(top_srcdir)/modules/differential_equations/callinter.h $(top_srcdir)/modules/linear_algebra/callinter.h $(top_srcdir)/modules/scicos/callinter.h $(top_srcdir)/modules/scicos_blocks/callinter.h
#--frames is disable because of : http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=607206
else
	@echo "Cannot start the code coverage. Add --enable-code-coverage to the ./configure and 'make clean all'."
endif

distclean-recursive: distclean-generated clean-recursive

clean-recursive: clean-help clean-localization clean-macros clean-generated

clean-macros:
# Find is necessary because of subdirs...
	-@echo "-- Cleaning macros (*.bin) --"; \
	find $(top_builddir) -name "*.bin" | xargs --no-run-if-empty rm 2> /dev/null; \
	echo "-- Cleaning names files (names) --"; \
	find $(top_builddir)/modules/*/macros -name "names" | xargs --no-run-if-empty rm 2> /dev/null; \
	echo "-- Cleaning lib files (lib) --"; \
	find $(top_builddir)/modules/*/macros -name "lib" | xargs --no-run-if-empty rm 2> /dev/null

# To ensure previous compatibility
clean-doc: clean-help

clean-help:
	-@if test -x $(top_builddir)/scilab-cli-bin; then \
		COMMAND="try clean_help();catch exit(-1);end;exit(0)"; \
		for l in $(ALL_LINGUAS_DOC); do \
			echo "$(top_builddir)/bin/scilab-cli -nb -l $$l -noatomsautoload -e '$$COMMAND'"; \
			$(top_builddir)/bin/scilab-cli -nb -l $$l -noatomsautoload -e "$$COMMAND" || exit 1; \
		done; \
	else \
		echo "Cannot find Scilab binary"; \
	fi; \
	echo "rmdir $(top_builddir)/modules/helptools/javaHelp/"; \
	rm -rf $(top_builddir)/modules/helptools/javaHelp/; \
	echo "rm -rf $(top_builddir)/modules/scicos/help/*/scilab_*_help"; \
	rm -rf $(top_builddir)/modules/scicos/help/*/scilab_*_help; \
	echo "rm -f $(top_builddir)/modules/scicos/help/en_US/master_help.xml"; \
	rm -f $(top_builddir)/modules/scicos/help/en_US/master_help.xml; \
	echo "rm -f $(top_builddir)/modules/scicos/jar/"; \
	rm -rf $(top_builddir)/modules/scicos/jar/; \
	echo "rm -rf $(top_builddir)/modules/javasci/javadoc"; \
	rm -rf $(top_builddir)/modules/javasci/javadoc; \
	echo "rm -rf $(top_builddir)/html"; \
	rm -rf $(top_builddir)/html;

clean-tests:
	@if test -x $(top_builddir)/scilab-cli-bin; then \
		$(top_builddir)/bin/scilab-cli -nb -noatomsautoload -l fr -e 'try test_clean();catch exit(-1);end;exit(0);' || exit 1; \
	else \
		echo "Cannot find Scilab binary"; \
	fi

clean-localization:
	rm -rf $(top_srcdir)/locale/*

clean-generated:
# Deleted junit + code coverage
	rm -rf $(top_builddir)/reports/
	rm -rf $(top_builddir)/modules/scicos_blocks/build/ 2> /dev/null

distclean-generated:
	rm -f $(top_builddir)/modules/core/includes/stack.h 2> /dev/null


if IS_MACOSX
# Used for mac os x because it needs to remove the charset=UTF-8 because it is breaking the accents.
MACOSX=1
endif

clean-reports:
	rm -f reports/xml/TEST-org.scilab.*.xml

# For each module, build and call Java unitary tests
# If fails, stops the call
check-java: clean-reports
	@echo "-- Launch all Junit (Java) unitary tests"; \
	MASK="$(top_srcdir)/modules/*/tests/java/"; \
	MODULES=`ls -d $$MASK`; \
	for M in $$MODULES; do \
		BASE=`echo $$M|sed -e "s|tests/java/$$||g"`; \
		$(ANT) -buildfile $$BASE/build.xml test || exit $$?; \
	done
	rm -f reports/xml/TEST-org.scilab.*.xml

codecoverage-java: clean-reports
	@echo "Check if --enable-debug-java has been set"; \
	grep "build.debug=on" $(top_srcdir)/scilab.properties > /dev/null || exit 10; \
	echo "-- Launch all Junit (Java) unitary tests + code coverage"; \
	MASK="$(top_srcdir)/modules/*/tests/java/"; \
	MODULES=`ls -d $$MASK`; \
	for M in $$MODULES; do \
		BASE=`echo $$M|sed -e "s|tests/java/$$||g"`; \
		$(ANT) -buildfile $$BASE/build.xml codecoverage || exit $$?; \
	done
	rm -f reports/xml/TEST-org.scilab.*.xml

# The user wants to compile the localization files
# Please note that ALL_LINGUAS is defined in configure.ac
if GENERATE_LOCALIZATION_FILES
localization:
	@echo "-- Build localization (.mo from .po) --"; \
	MASK="modules/*/locales/"; \
	LC="LC_MESSAGES"; \
	MSGFMT_OPTS="$MSGFMT_OPTS --check"; \
	EXTENSION="*.po"; \
	rm modules/*/locales/*~ 2> /dev/null; \
	for LOCALE in $(ALL_LINGUAS); do \
		echo "        -- Building for $$LOCALE --"; \
		if test "$$LOCALE" == "en_US"; then \
			LOCFILE=$$MASK/*.pot; \
		else \
			LOCFILE=$$MASK/$$LOCALE$$EXTENSION; \
		fi; \
		FILES=`ls $$LOCFILE`; \
		PATHTO=$(top_builddir)/locale/$$LOCALE/$$LC/; \
		if test ! -d $$PATHTO; then \
			echo "        Creating $$PATHTO"; \
			mkdir -p $$PATHTO; \
		fi; \
		POFILE=$$PATHTO/$(LANG_DOMAIN).po; \
		$(MSGCAT) --use-first -o $$POFILE $$FILES || exit 2; \
		if test "$(MACOSX)" = "1"; then sed -i -e '/charset=UTF-8/d' $$POFILE; fi; \
		$(MSGFMT) $(MSGFMT_OPTS) --statistics -o $$PATHTO/$(LANG_DOMAIN).mo $$POFILE || exit 2; \
	done;


localization-status:
	@for LOCALE in $(ALL_LINGUAS_DOC); do \
		if test "$$LOCALE" != "en_US"; then \
			PATHTO=$(top_builddir)/locale-status/$$LOCALE/; \
			if test ! -d $$PATHTO; then \
				echo "        Creating $$PATHTO"; \
				mkdir -p $$PATHTO; \
			fi; \
			echo "Building status for $$LOCALE"; \
			echo "SCI=$(top_srcdir) php tools/localization/revcheck.php $$LOCALE > $$PATHTO/index.html"; \
			SCI=$(top_srcdir) php tools/localization/revcheck.php $$LOCALE > $$PATHTO/index.html; \
		fi; \
	done;

endif

# Misc variable for the documentation installation

# How help files look like.
DOCMASK=scilab_*_help.jar

# Path to builded files
PATHTOHELPFILES = modules/helptools/jar/

# Path javasci to builded files
PATHTOJAVASCIDOC = modules/javasci/javadoc/
JAVASCI_DIRS = . resources \
	org/scilab/modules/javasci/ \
	org/scilab/modules/types/

if GENERATE_LOCALIZATION_FILES
install-data-local:
	@$(mkinstalldirs) $(DESTDIR)$(localedir); \
	echo $(mkinstalldirs) $(DESTDIR)$(localedir); \
	for lang in $(ALL_LINGUAS); do \
		LANG_PATH_DIST=$(DESTDIR)$(localedir)/$$lang/LC_MESSAGES/; \
		echo $(mkinstalldirs) $$LANG_PATH_DIST; \
		$(mkinstalldirs) $$LANG_PATH_DIST; \
		echo $(INSTALL_DATA) $(top_builddir)/locale/$$lang/LC_MESSAGES/$(LANG_DOMAIN).mo $$LANG_PATH_DIST/; \
		$(INSTALL_DATA) $(top_builddir)/locale/$$lang/LC_MESSAGES/$(LANG_DOMAIN).mo $$LANG_PATH_DIST/; \
	done
endif

################ INSTALL DOCUMENTATION #################
# Starting from Scilab 5, documentation file are provided by Docbook
# In the Scilab application we are generating a JavaHelp file.
if BUILD_HELP
	@$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/$(PATHTOHELPFILES); \
	echo $(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/$(PATHTOHELPFILES); \
	if ls -lLd $(top_builddir)/$(PATHTOHELPFILES)/$(DOCMASK) >/dev/null 2>&1; then \
		for file in $(top_builddir)/$(PATHTOHELPFILES)/$(DOCMASK); do \
		    echo "$(INSTALL_DATA) $$file $(DESTDIR)$(pkgdatadir)/$(PATHTOHELPFILES)" ; \
	    	$(INSTALL_DATA) "$$file" $(DESTDIR)$(pkgdatadir)/$(PATHTOHELPFILES) ; \
		done; \
	fi
## Install Javasci documentation (javadoc)
## See target javadoc in this file to see how it is build
	@if test -d $(PATHTOJAVASCIDOC); then \
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/$(PATHTOJAVASCIDOC); \
	echo $(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/$(PATHTOJAVASCIDOC); \
	for dir in $(JAVASCI_DIRS) ; do \
		$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/$(PATHTOJAVASCIDOC)/$$dir; \
		echo $(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/$(PATHTOJAVASCIDOC)/$$dir; \
			for file in $(top_builddir)/$(PATHTOJAVASCIDOC)/$(dir)/*; do \
			    echo "$(INSTALL_DATA) $$file $(DESTDIR)$(pkgdatadir)/$(PATHTOJAVASCIDOC)/$$dir" ; \
				$(INSTALL_DATA) "$$file" $(DESTDIR)$(pkgdatadir)/$(PATHTOJAVASCIDOC)/$$dir ; \
			done; \
	done; \
	fi
endif

# Pkgconfig directory
pkgconfigdir = $(libdir)/pkgconfig

# Files to install in Pkgconfig directory
pkgconfig_DATA = scilab.pc

.PHONY: macros localization doc

.NOTPARALLEL: macros doc

include $(top_srcdir)/Makefile.call_scilab.am
