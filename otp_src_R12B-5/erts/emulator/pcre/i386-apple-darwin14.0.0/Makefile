# Makefile for zlib
# Copyright (C) 1995-1996 Jean-loup Gailly.
# For conditions of distribution and use, see copyright notice in zlib.h 

# To compile and test, type:
#   ./configure; make test
# The call of configure is optional if you don't have special requirements

# To install /usr/local/lib/libz.* and /usr/local/include/zlib.h, type:
#    make install
# To install in $HOME instead of /usr/local, use:
#    make install prefix=$HOME

ARFLAGS = rc

O = \
pcre_latin_1_table.o \
pcre_compile.o \
pcre_config.o \
pcre_dfa_exec.o \
pcre_exec.o \
pcre_fullinfo.o \
pcre_get.o \
pcre_globals.o \
pcre_info.o \
pcre_maketables.o \
pcre_newline.o \
pcre_ord2utf8.o \
pcre_refcount.o \
pcre_study.o \
pcre_tables.o \
pcre_try_flipped.o \
pcre_ucp_searchfuncs.o \
pcre_valid_utf8.o \
pcre_version.o \
pcre_xclass.o

OBJS = $(O:%=$(OBJDIR)/%)

GENINC = pcre_exec_loop_break_cases.inc

#### Begin OTP targets

include $(ERL_TOP)/make/target.mk

# On windows we need a separate zlib during debug build
ifeq ($(TARGET),win32)

ifeq ($(TYPE),debug)
CFLAGS = $(subst -O2, -g, -g -O2 -I/Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/i386-apple-darwin14.0.0  -no-cpp-precomp   -DHAVE_CONFIG_H -g  -DUSE_THREADS -D_THREAD_SAFE -D_REENTRANT -DERLANG_INTEGRATION)
else  # debug
CFLAGS = -g -O2 -I/Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/i386-apple-darwin14.0.0  -no-cpp-precomp   -DHAVE_CONFIG_H  -DUSE_THREADS -D_THREAD_SAFE -D_REENTRANT -DERLANG_INTEGRATION
endif # debug

else  # win32

ifeq ($(TYPE),debug)
TYPE_FLAGS = -g  -I/Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/i386-apple-darwin14.0.0   
else  # debug
ifeq ($(TYPE),gcov)
TYPE_FLAGS = -O0 -fprofile-arcs -ftest-coverage
else  # gcov 
TYPE_FLAGS = -O3
endif # gcov
endif # debug

CFLAGS =  $(TYPE_FLAGS) $(subst -O2,, -g -O2 -I/Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/i386-apple-darwin14.0.0  -no-cpp-precomp  ) -DHAVE_CONFIG_H  -DUSE_THREADS -D_THREAD_SAFE -D_REENTRANT -DERLANG_INTEGRATION 

endif # win32

OBJDIR = $(ERL_TOP)/erts/emulator/pcre/obj/$(TARGET)/$(TYPE)

include $(ERL_TOP)/make/$(TARGET)/otp.mk

ifeq ($(TARGET), win32)
LIBRARY=$(OBJDIR)/epcre.lib
else
LIBRARY=$(OBJDIR)/libepcre.a
endif

all: $(LIBRARY)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

tests release_tests:

docs release_docs release_docs_spec:

clean:
	rm -f $(OBJS) $(OBJDIR)/libepcre.a

#### end OTP targets

ifeq ($(TARGET), win32)
$(LIBRARY): $(OBJS)
	$(AR) -out:$@ $(OBJS)
else
$(LIBRARY): $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	-@ ($(RANLIB) $@ || true) 2>/dev/null
endif

$(OBJDIR)/%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

$(GENINC): pcre_exec.c
	for x in `grep -n COST_CHK pcre_exec.c | grep -v 'COST_CHK(N)' | awk -F: '{print $$1}'`; \
	do \
		N=`expr $$x + 100`; \
		echo "case $$N: goto L_LOOP_COUNT_$${x};"; \
	done > $(GENINC)     

table: ./gen_table
	./gen_table pcre_latin_1_table.c

./gen_table: pcre_make_latin1_default.c make_latin1_table.c
	$(CC) $(CFLAGS) -o gen_table  pcre_make_latin1_default.c make_latin1_table.c

# DO NOT DELETE THIS LINE -- make depend depends on it.

$(OBJDIR)/pcre_chartables.o: pcre_chartables.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_compile.o: pcre_compile.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_config.o: pcre_config.c pcre_internal.h local_config.h pcre.h \
  ucp.h
$(OBJDIR)/pcre_dfa_exec.o: pcre_dfa_exec.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_exec.o: pcre_exec.c pcre_internal.h local_config.h pcre.h ucp.h \
  $(GENINC)
$(OBJDIR)/pcre_fullinfo.o: pcre_fullinfo.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_get.o: pcre_get.c pcre_internal.h local_config.h pcre.h ucp.h
$(OBJDIR)/pcre_globals.o: pcre_globals.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_info.o: pcre_info.c pcre_internal.h local_config.h pcre.h ucp.h
$(OBJDIR)/pcre_maketables.o: pcre_maketables.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_newline.o: pcre_newline.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_ord2utf8.o: pcre_ord2utf8.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_refcount.o: pcre_refcount.c pcre_internal.h local_config.h \
  pcre.h ucp.h
$(OBJDIR)/pcre_study.o: pcre_study.c pcre_internal.h local_config.h pcre.h \
  ucp.h
$(OBJDIR)/pcre_tables.o: pcre_tables.c pcre_internal.h local_config.h pcre.h \
 ucp.h
$(OBJDIR)/pcre_try_flipped.o: pcre_try_flipped.c pcre_internal.h \
  local_config.h pcre.h ucp.h
$(OBJDIR)/pcre_ucp_searchfuncs.o: pcre_ucp_searchfuncs.c pcre_internal.h \
  local_config.h pcre.h ucp.h ucpinternal.h ucptable.h
$(OBJDIR)/pcre_valid_utf8.o: pcre_valid_utf8.c pcre_internal.h local_config.h \
  pcre.h ucp.h
pcre_version.o: pcre_version.c pcre_internal.h local_config.h pcre.h \
  ucp.h
$(OBJDIR)/pcre_xclass.o: pcre_xclass.c pcre_internal.h local_config.h pcre.h \
  ucp.h
