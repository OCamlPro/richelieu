# Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
# Copyright (C) 2006 - INRIA - Sylvestre LEDRU
#
# This file is distributed under the same license as the Scilab package.
 

MALLOC_C_SOURCES = src/getmaxMALLOC.c \
src/sci_mem_alloc.c

# Commented since it is causing more problems than it solves...
# If you are trying to run Scilab on a 64 bits CPU and a seg fault
# is triggered on startup, please uncomment the following line:
#MALLOC_C_SOURCES += src/malloc.c
#if IS_64_BITS_CPU
#MALLOC_C_SOURCES += src/malloc.c
#endif

libscimalloc_la_includedir=$(pkgincludedir)

libscimalloc_la_include_HEADERS = includes/sci_mem_alloc.h \
includes/MALLOC.h \
includes/getmaxMALLOC.h \
includes/ExportImport.h

pkglib_LTLIBRARIES = libscimalloc.la

libscimalloc_la_SOURCES = $(MALLOC_C_SOURCES)

# For the code check (splint)
CHECK_SRC= $(MALLOC_C_SOURCES)
INCLUDE_FLAGS = $(libscimalloc_la_CFLAGS)

libscimalloc_la_LIBADD = 

#### Target ######
modulename=malloc

include $(top_srcdir)/Makefile.incl.am
