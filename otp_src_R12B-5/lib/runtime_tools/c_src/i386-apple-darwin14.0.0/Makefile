# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(RUNTIME_TOOLS_VSN)

# ----------------------------------------------------
# The following variables differ on different systems, we set
# reasonable defaults, if something different is needed it should
# be set for that system only.
# ----------------------------------------------------
CC = gcc
CFLAGS = -g -O2 -I/Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/i386-apple-darwin14.0.0  -no-cpp-precomp    -DUSE_THREADS -D_THREAD_SAFE -D_REENTRANT -fPIC -fno-common
LD = gcc
SHELL = /bin/sh
LIBS = -lutil -ldl -lm  
LDFLAGS = -m64 -bundle -flat_namespace -undefined suppress
ERLANG_OSTYPE = unix

SYSINCLUDE = 	-I$(ERL_TOP)/erts/emulator/beam \
		-I$(ERL_TOP)/erts/emulator/sys/$(ERLANG_OSTYPE) \
		-I$(ERL_TOP)/erts/include/internal \
		-I$(ERL_TOP)/erts/include/internal/$(ERLANG_OSTYPE) \
		-I$(ERL_TOP)/erts/include \
		-I$(ERL_TOP)/erts/include/$(ERLANG_OSTYPE)
ifeq ($(findstring vxworks,$(TARGET)),vxworks)
	SYSINCLUDE += -I$(ERL_TOP)/erts/etc/vxworks
endif

TRACE_DRV_INCLUDES = $(SYSINCLUDE)

ALL_CFLAGS = $(CFLAGS) -DHAVE_CONFIG_H $(TYPE_FLAGS) $(TRACE_DRV_INCLUDES)


ifeq ($(TYPE),debug)
TYPEMARKER = .debug
TYPE_FLAGS = -g -DDEBUG -g
else
TYPEMARKER =
TYPE_FLAGS = -O2
endif

ROOTDIR = $(ERL_TOP)/lib
PRIVDIR = ../priv
LIBDIR = $(PRIVDIR)/lib/$(TARGET)
OBJDIR = $(PRIVDIR)/obj/$(TARGET)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/runtime_tools-$(VSN)

# ----------------------------------------------------
# Misc Macros
# ----------------------------------------------------
TRACE_IP_DRV_OBJS = \
	$(OBJDIR)/trace_ip_drv.o

TRACE_FILE_DRV_OBJS = \
	$(OBJDIR)/trace_file_drv.o

ifeq ($(findstring win32,$(TARGET)), win32)
SOLIBS = $(LIBDIR)/trace_ip_drv.dll $(LIBDIR)/trace_file_drv.dll
LN=cp
else
ifeq ($(findstring vxworks,$(TARGET)),vxworks)
SOLIBS = $(LIBDIR)/trace_ip_drv.eld $(LIBDIR)/trace_file_drv.eld
else
SOLIBS = $(LIBDIR)/trace_ip_drv.so $(LIBDIR)/trace_file_drv.so
endif
endif
# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(OBJDIR) $(BINDIR) $(SOLIBS)

$(OBJDIR):
	-@mkdir -p $(OBJDIR)

$(BINDIR):
	-@mkdir -p $(BINDIR)

$(OBJDIR)/%.o: %.c
	$(INSTALL_DIR) $(OBJDIR)
	$(CC) -c -o $@ $(ALL_CFLAGS) $<

$(LIBDIR)/trace_ip_drv.so: $(TRACE_IP_DRV_OBJS)
	$(INSTALL_DIR) $(LIBDIR)
	$(LD) $(LDFLAGS) -o $@ $^ -lc $(LIBS)

$(LIBDIR)/trace_file_drv.so: $(TRACE_FILE_DRV_OBJS)
	$(INSTALL_DIR) $(LIBDIR)
	$(LD) $(LDFLAGS) -o $@ $^ -lc $(LIBS)

$(LIBDIR)/trace_ip_drv.dll: $(TRACE_IP_DRV_OBJS)
	$(INSTALL_DIR) $(LIBDIR)
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBS)
$(LIBDIR)/trace_file_drv.dll: $(TRACE_FILE_DRV_OBJS)
	$(INSTALL_DIR) $(LIBDIR)
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBS)
#
# VxWorks is simply to different from Unix in this sense.
# Here are the inference rules for VxWorks
#
$(LIBDIR)/trace_ip_drv.eld: $(TRACE_IP_DRV_OBJS)
	$(INSTALL_DIR) $(LIBDIR)
	$(LD) $(LDFLAGS) -o $@ $^ 

$(LIBDIR)/trace_file_drv.eld: $(TRACE_FILE_DRV_OBJS)
	$(INSTALL_DIR) $(LIBDIR)
	$(LD) $(LDFLAGS) -o $@ $^ 

clean:
	rm -f $(SOLIBS) $(TRACE_IP_DRV_OBJS) $(TRACE_FILE_DRV_OBJS)
	rm -f core *~

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv/lib
	$(INSTALL_PROGRAM) $(SOLIBS) $(RELSYSDIR)/priv/lib

release_docs_spec:

