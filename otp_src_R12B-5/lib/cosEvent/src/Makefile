#<copyright>
# <year>1997-2008</year>
# <holder>Ericsson AB, All Rights Reserved</holder>
#</copyright>
#<legalnotice>
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# The Initial Developer of the Original Code is Ericsson AB.
#</legalnotice>
#
include $(ERL_TOP)/make/target.mk
ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif
EBIN=../ebin
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSEVENT_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/cosEvent-$(VSN)
# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	CosEventChannelAdmin_ProxyPullConsumer_impl \
	CosEventChannelAdmin_ProxyPushConsumer_impl \
	CosEventChannelAdmin_SupplierAdmin_impl \
	oe_CosEventComm_CAdmin_impl \
	oe_CosEventComm_Channel_impl \
	oe_CosEventComm_PullerS_impl \
	oe_CosEventComm_PusherS_impl \
	cosEventApp



ERL_FILES = $(MODULES:%=%.erl) 
HRL_FILES = cosEventApp.hrl


GEN_ERL_FILES1 = \
	oe_CosEventChannelAdmin.erl \
	CosEventChannelAdmin_ConsumerAdmin.erl \
	CosEventChannelAdmin_EventChannel.erl \
	CosEventChannelAdmin_ProxyPullConsumer.erl \
	CosEventChannelAdmin_ProxyPullSupplier.erl \
	CosEventChannelAdmin_ProxyPushConsumer.erl \
	CosEventChannelAdmin_ProxyPushSupplier.erl \
	CosEventChannelAdmin_SupplierAdmin.erl \
	CosEventChannelAdmin_AlreadyConnected.erl \
	CosEventChannelAdmin_TypeError.erl

GEN_ERL_FILES2 = \
	oe_CosEventComm_CAdmin.erl \
	oe_CosEventComm_Channel.erl \
	oe_CosEventComm_Event.erl \
	oe_CosEventComm_PullerS.erl \
	oe_CosEventComm_PusherS.erl \
	oe_cosEventApp.erl

GEN_ERL_FILES3 = \
	oe_CosEventComm.erl \
	CosEventComm_Disconnected.erl \
	CosEventComm_PullConsumer.erl \
	CosEventComm_PullSupplier.erl \
	CosEventComm_PushConsumer.erl \
	CosEventComm_PushSupplier.erl

GEN_ERL_FILES =  \
	$(GEN_ERL_FILES1) $(GEN_ERL_FILES2) $(GEN_ERL_FILES3)

EXTERNAL_INC_PATH = ../include

GEN_HRL_FILES1 = \
	oe_CosEventChannelAdmin.hrl \
	CosEventChannelAdmin.hrl \
	CosEventChannelAdmin_ConsumerAdmin.hrl \
	CosEventChannelAdmin_EventChannel.hrl \
	CosEventChannelAdmin_ProxyPullConsumer.hrl \
	CosEventChannelAdmin_ProxyPullSupplier.hrl \
	CosEventChannelAdmin_ProxyPushConsumer.hrl \
	CosEventChannelAdmin_ProxyPushSupplier.hrl \
	CosEventChannelAdmin_SupplierAdmin.hrl

EXTERNAL_GEN_HRL_FILES1 = $(GEN_HRL_FILES1:%=$(EXTERNAL_INC_PATH)/%)

GEN_HRL_FILES2 = \
	oe_CosEventComm_PullerS.hrl \
	oe_CosEventComm_CAdmin.hrl \
	oe_CosEventComm_PusherS.hrl \
	oe_CosEventComm_Channel.hrl \
	oe_cosEventApp.hrl \
	oe_CosEventComm_Event.hrl

GEN_HRL_FILES3 = \
	oe_CosEventComm.hrl \
	CosEventComm.hrl \
	CosEventComm_PullConsumer.hrl \
	CosEventComm_PullSupplier.hrl \
	CosEventComm_PushConsumer.hrl \
	CosEventComm_PushSupplier.hrl

EXTERNAL_GEN_HRL_FILES3 = $(GEN_HRL_FILES3:%=$(EXTERNAL_INC_PATH)/%)

GEN_HRL_FILES = \
	$(EXTERNAL_GEN_HRL_FILES1) $(GEN_HRL_FILES2) $(EXTERNAL_GEN_HRL_FILES3)

TARGET_FILES = \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) 

GEN_FILES = $(GEN_HRL_FILES) $(GEN_ERL_FILES)

IDL_FILES = \
	CosEventChannelAdmin.idl \
	CosEventComm.idl \
	cosEventApp.idl

APPUP_FILE = cosEvent.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

APP_FILE = cosEvent.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/cosEvent/ebin -pa $(ERL_TOP)/lib/ic/ebin
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += \
	$(ERL_IDL_FLAGS) \
	-I$(ERL_TOP)/lib/orber/include \
	-I$(ERL_TOP)/lib/cosEvent/include \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,"cosEvent_$(COSEVENT_VSN)"}' 

YRL_FLAGS = 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

debug:
	@${MAKE} TYPE=debug opt

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC)
	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)
$(APPUP_TARGET): $(APPUP_SRC)
	sed -e 's;%VSN%;$(VSN);' $(APPUP_SRC) > $(APPUP_TARGET)

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(GEN_ERL_FILES1) $(EXTERNAL_GEN_HRL_FILES1): CosEventChannelAdmin.idl
	erlc $(ERL_IDL_FLAGS) +'{cfgfile,"CosEventChannelAdmin.cfg"}' CosEventChannelAdmin.idl
	mv $(GEN_HRL_FILES1) $(EXTERNAL_INC_PATH)

$(GEN_ERL_FILES2) $(GEN_HRL_FILES2): cosEventApp.idl
	erlc $(ERL_IDL_FLAGS) +'{cfgfile,"cosEventApp.cfg"}' cosEventApp.idl

$(GEN_ERL_FILES3) $(EXTERNAL_GEN_HRL_FILES3): CosEventComm.idl
	erlc $(ERL_IDL_FLAGS) CosEventComm.idl
	mv $(GEN_HRL_FILES3) $(EXTERNAL_INC_PATH)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) ../info $(RELSYSDIR)
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(GEN_ERL_FILES) $(IDL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(GEN_HRL_FILES) $(RELSYSDIR)/include


release_docs_spec:






