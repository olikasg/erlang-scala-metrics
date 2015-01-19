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


ifeq ($(TYPE),debug)
PURIFY     =
TYPEMARKER = .debug
TYPE_FLAGS = -DDEBUG -g
else
ifeq ($(TYPE),purify)
PURIFY     = purify
TYPEMARKER =
ifeq ($(findstring ose,$(TARGET)),ose)
  TYPE_FLAGS = -DPURIFY
else
  TYPE_FLAGS = -O2 -DPURIFY
endif
else
PURIFY     =
TYPEMARKER =
ifeq ($(findstring ose,$(TARGET)),ose)
  TYPE_FLAGS =
else
  TYPE_FLAGS = -O2
endif
endif
endif

include $(ERL_TOP)/make/$(TARGET)/otp.mk
include ../../vsn.mk
include ../epmd.mk

BINDIR  = $(ERL_TOP)/bin/$(TARGET)
OBJDIR  = $(ERL_TOP)/erts/obj$(TYPEMARKER)/$(TARGET)

CC      = gcc
WFLAGS  = -Wall -Wstrict-prototypes -Wmissing-prototypes -Wdeclaration-after-statement
CFLAGS  = -g -O2 -I/Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/i386-apple-darwin14.0.0  -no-cpp-precomp   -DHAVE_CONFIG_H $(TYPE_FLAGS) $(WFLAGS)
LD      = $(CC)
LIBS    = -lutil -ldl -lm  
LDFLAGS = -m64  


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

# The targets
ifeq ($(findstring win32,$(TARGET)),win32)
EPMD = epmd.exe
else
EPMD = epmd
endif

INSTALL_PROGS = $(BINDIR)/$(EPMD)

#---------------------------------
# Options
#---------------------------------
 
EPMD_FLAGS = -DEPMD_PORT_NO=$(EPMD_PORT_NO)

#---------------------------------
# source and object file information
#---------------------------------

EPMD_OBJS = $(OBJDIR)/epmd.o       \
               $(OBJDIR)/epmd_cli.o   \
               $(OBJDIR)/epmd_srv.o 

#---------------------------------
# Build targets
#---------------------------------


all: $(BINDIR)/$(EPMD)

docs:

clean:
	rm -f $(BINDIR)/$(EPMD) 
	rm -f $(ERL_TOP)/erts/obj/$(TARGET)/epmd.o
	rm -f $(ERL_TOP)/erts/obj/$(TARGET)/epmd_cli.o
	rm -f $(ERL_TOP)/erts/obj/$(TARGET)/epmd_srv.o 
	rm -f *.o
	rm -f *~ core

#
# Objects & executables
#

$(BINDIR)/$(EPMD): $(EPMD_OBJS)
	$(PURIFY) $(LD) $(LDFLAGS) -o $@ $(EPMD_OBJS) $(LIBS)

$(OBJDIR)/%.o: %.c
	$(CC) $(CFLAGS) $(EPMD_FLAGS) -o $@ -c $<

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: all
	$(INSTALL_DIR)     $(RELEASE_PATH)/erts-$(VSN)/bin
	$(INSTALL_PROGRAM) $(INSTALL_PROGS) $(RELEASE_PATH)/erts-$(VSN)/bin


release_docs_spec:

